package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.{Path, Files}

/** Scan all _obj.s3d and zone .s3d files for textures that look like fire/glow effects.
  * Prints texture name, material type, and which object model uses it.
  * Usage: runMain opennorrath.tools.FireTextureScanner [assets/EverQuest]
  */
object FireTextureScanner:
  // Known fire prefixes we already handle in effectiveMaterialType
  private val handledPrefixes = List("fire", "bfire", "flame")

  // Broader pattern: anything that looks like it could be a glow/fire/lava effect
  private val suspectPatterns = List(
    "fire", "flame", "torch", "lava", "glow", "ember", "coal", "burn",
    "spark", "flare", "blaze", "pyre", "magma", "molten", "bfire",
    "brazlite", "campfir", "lite", "light",
  )

  def main(args: Array[String]): Unit =
    val assetsDir = if args.nonEmpty then args(0) else "assets/EverQuest"
    val dir = Path.of(assetsDir)
    if !Files.isDirectory(dir) then
      println(s"Directory not found: $assetsDir")
      return

    val findings = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]

    // Scan _obj.s3d files
    val objFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter(p => p.getFileName.toString.endsWith("_obj.s3d"))
      .sorted

    println(s"Scanning ${objFiles.length} _obj.s3d files...")
    for file <- objFiles do
      scanObjFile(file, findings)

    // Scan zone .s3d files
    val zoneFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString
        name.endsWith(".s3d") && !name.contains("_obj") && !name.contains("_chr") && !name.contains("global")
      }.sorted

    println(s"Scanning ${zoneFiles.length} zone .s3d files...")
    for file <- zoneFiles do
      scanZoneFile(file, findings)

    // Report
    println(s"\n=== Fire/glow textures found ===\n")
    val sorted = findings.toList.sortBy(_._1)
    val handled = sorted.filter { case (key, _) =>
      val texName = key.split(":")(0).toLowerCase
      handledPrefixes.exists(p => texName.startsWith(p))
    }
    val unhandled = sorted.filter { case (key, _) =>
      val texName = key.split(":")(0).toLowerCase
      !handledPrefixes.exists(p => texName.startsWith(p))
    }

    println(s"--- Already handled (${handled.size}) ---")
    for (key, zones) <- handled do
      val sample = zones.take(3).mkString(", ") + (if zones.size > 3 then s" +${zones.size - 3} more" else "")
      println(s"  $key  ($sample)")

    println(s"\n--- NOT handled (${unhandled.size}) ---")
    for (key, zones) <- unhandled do
      val matType = key.split(":")(1)
      val marker = if matType == "Diffuse" then " *** LIKELY NEEDS FIX ***" else ""
      val sample = zones.take(3).mkString(", ") + (if zones.size > 3 then s" +${zones.size - 3} more" else "")
      println(s"  $key$marker  ($sample)")

  private def scanObjFile(file: Path, findings: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]): Unit =
    try
      val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
      val wldEntry = entries.find(_.extension == "wld")
      if wldEntry.isEmpty then return
      val wld = WldFile(wldEntry.get.data)
      val actors = wld.fragmentsOfType[Fragment14_Actor]
      val zone = file.getFileName.toString.replace("_obj.s3d", "")

      for actor <- actors do
        val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
        val meshFragments = resolveMeshes(wld, actor)
        for mesh <- meshFragments do
          for group <- mesh.renderGroups do
            val textureName = ZoneGeometry.resolveTextureName(wld, mesh.materialListRef, group.materialIndex)
            val matType = ZoneGeometry.resolveMaterialType(wld, mesh.materialListRef, group.materialIndex)
            val lower = textureName.toLowerCase
            if suspectPatterns.exists(p => lower.contains(p)) then
              val key = s"$textureName:$matType"
              findings.getOrElseUpdate(key, scala.collection.mutable.Set.empty) += s"$zone/$actorKey"
    catch
      case _: Exception => ()

  private def scanZoneFile(file: Path, findings: scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]): Unit =
    try
      val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
      val wldEntry = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
      if wldEntry.isEmpty then return
      val wld = WldFile(wldEntry.get.data)
      val zone = file.getFileName.toString.replace(".s3d", "")
      val zm = ZoneGeometry.extract(wld)
      for group <- zm.groups do
        val lower = group.textureName.toLowerCase
        if suspectPatterns.exists(p => lower.contains(p)) then
          val key = s"${group.textureName}:${group.materialType}"
          findings.getOrElseUpdate(key, scala.collection.mutable.Set.empty) += s"$zone/zone"
    catch
      case _: Exception => ()

  private def resolveMeshes(wld: WldFile, actor: Fragment14_Actor): List[Fragment36_Mesh] =
    actor.componentRefs.flatMap { ref =>
      try
        wld.fragment(ref) match
          case mr: Fragment2D_MeshReference =>
            wld.fragment(mr.meshRef) match
              case m: Fragment36_Mesh => Some(m)
              case _ => None
          case _ => None
      catch
        case _: Exception => None
    }
