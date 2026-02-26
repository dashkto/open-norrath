package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import opennorrath.world.ZoneRenderer
import java.nio.file.{Path, Files}

/** Dump character texture names and BMP inventory from global_chr.s3d.
  * Usage: runMain opennorrath.tools.EquipDebug [assets/EverQuest] [hum]
  */
object EquipDebug:
  def main(args: Array[String]): Unit =
    val dir = if args.length > 0 then args(0) else "assets/EverQuest"
    val filterRace = if args.length > 1 then Some(args(1).toLowerCase) else None
    val globalPath = Path.of(dir, "global_chr.s3d")

    if !Files.exists(globalPath) then
      println(s"Not found: $globalPath")
      return

    val entries = PfsArchive.load(globalPath)
    val wldEntry = entries.find(_.extension == "wld").getOrElse { println("No WLD"); return }
    val wld = WldFile(wldEntry.data)

    // All BMP files in the archive
    val bmpNames = entries.filter(_.extension == "bmp").map(_.name.toLowerCase).sorted
    println(s"=== BMP files in global_chr.s3d: ${bmpNames.size} ===")
    filterRace match
      case Some(race) =>
        val matching = bmpNames.filter(_.startsWith(race))
        println(s"  Matching '$race': ${matching.size}")
        for name <- matching do println(s"    $name")
      case None =>
        // Group by 3-char prefix
        val byPrefix = bmpNames.groupBy(n => if n.length >= 3 then n.take(3) else n)
        for (prefix, names) <- byPrefix.toList.sortBy(_._1) do
          println(s"  $prefix: ${names.size} files")
          for name <- names.take(5) do println(s"    $name")
          if names.size > 5 then println(s"    ... and ${names.size - 5} more")

    // Actor render groups with texture names
    val actors = wld.fragmentsOfType[Fragment14_Actor]
    println(s"\n=== Actor render group textures ===")
    for actor <- actors do
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      if filterRace.forall(_ == actorKey) then
        val (_, meshFragments) = ZoneRenderer.resolveActorMeshes(wld, actor)
        if meshFragments.nonEmpty then
          println(s"\n  $actorKey: ${meshFragments.size} mesh(es)")
          for mesh <- meshFragments do
            println(s"    Mesh: ${mesh.name}, ${mesh.renderGroups.length} render groups")
            for (group, idx) <- mesh.renderGroups.zipWithIndex do
              val texName = ZoneGeometry.resolveTextureName(wld, mesh.materialListRef, group.materialIndex)
              println(s"      [$idx] ${group.polyCount} polys, material=${group.materialIndex} → $texName")
