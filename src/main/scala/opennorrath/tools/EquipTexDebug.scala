package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import opennorrath.world.ZoneRenderer
import java.nio.file.{Path, Files}

/** Check whether equipment model texture names match loaded BMP entries.
  * Reports any texture names that can't be found in the archive.
  *
  * Usage: runMain opennorrath.tools.EquipTexDebug [assets/EverQuest] [itNum]
  */
object EquipTexDebug:

    def main(args: Array[String]): Unit =
        val dir = if args.length > 0 then args(0) else "assets/EverQuest"
        val filterIt = if args.length > 1 then Some(args(1).toInt) else None
        val assetsDir = Path.of(dir)

        if !Files.isDirectory(assetsDir) then
            println(s"Assets directory not found: $assetsDir")
            return

        val gequipFiles = Files.list(assetsDir).toArray.map(_.asInstanceOf[Path])
            .filter { p =>
                val name = p.getFileName.toString.toLowerCase
                name.startsWith("gequip") && name.endsWith(".s3d")
            }.sorted

        val itPattern = "^it(\\d+)$".r
        // Collect ALL bmp names across all gequip archives (textures are shared)
        val allBmps = scala.collection.mutable.Set[String]()
        var missingCount = 0
        var totalGroups = 0
        var modelsChecked = 0

        // First pass: collect all BMP names
        for file <- gequipFiles do
            try
                val entries = PfsArchive.load(file)
                for entry <- entries if entry.extension == "bmp" do
                    allBmps += entry.name.toLowerCase
            catch case _: Exception => ()

        println(s"Total BMP textures across all gequip archives: ${allBmps.size}")
        println()

        // Second pass: check each model's texture references
        var seen = Set.empty[Int]
        for file <- gequipFiles do
            try
                val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
                entries.find(_.extension == "wld").foreach { wldEntry =>
                    val wld = WldFile(wldEntry.data)
                    val actors = wld.fragmentsOfType[Fragment14_Actor]
                    for actor <- actors do
                        val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
                        actorKey match
                            case itPattern(numStr) =>
                                val itNum = numStr.toInt
                                if !seen.contains(itNum) && filterIt.forall(_ == itNum) then
                                    seen += itNum
                                    checkModel(wld, actor, itNum, file.getFileName.toString, allBmps) match
                                        case (total, missing) =>
                                            totalGroups += total
                                            missingCount += missing
                                            modelsChecked += 1
                            case _ => ()
                }
            catch case _: Exception => ()

        println()
        println(s"=== Summary: $modelsChecked models, $totalGroups render groups, " +
            s"$missingCount missing textures ===")

    private def checkModel(wld: WldFile, actor: Fragment14_Actor, itNum: Int,
        sourceFile: String, allBmps: scala.collection.Set[String]): (Int, Int) =
        var meshFragments = List.empty[Fragment36_Mesh]

        for ref <- actor.componentRefs do
            try
                wld.fragment(ref) match
                    case mr: Fragment2D_MeshReference =>
                        wld.fragment(mr.meshRef) match
                            case m: Fragment36_Mesh => meshFragments = meshFragments :+ m
                            case _ =>
                    case skelRef: Fragment11_SkeletonHierarchyRef =>
                        wld.fragment(skelRef.skeletonRef) match
                            case skel: Fragment10_SkeletonHierarchy =>
                                for mr <- skel.meshRefs do
                                    try
                                        wld.fragment(mr) match
                                            case meshRef: Fragment2D_MeshReference =>
                                                wld.fragment(meshRef.meshRef) match
                                                    case m: Fragment36_Mesh =>
                                                        meshFragments = meshFragments :+ m
                                                    case _ =>
                                            case _ =>
                                    catch case _: Exception => ()
                            case _ =>
                    case _ =>
            catch case _: Exception => ()

        var totalGroups = 0
        var missingCount = 0
        val issues = scala.collection.mutable.ListBuffer[String]()

        for mesh <- meshFragments do
            for group <- mesh.renderGroups do
                totalGroups += 1
                val texName = ZoneGeometry.resolveTextureName(wld, mesh.materialListRef, group.materialIndex)
                val matType = ZoneGeometry.resolveMaterialType(wld, mesh.materialListRef, group.materialIndex)
                val key = texName.toLowerCase
                val found = key.isEmpty || matType == MaterialType.Invisible || allBmps.contains(key)
                if !found then
                    missingCount += 1
                    issues += s"  ${mesh.name} group[mat=${ group.materialIndex}] → '$texName' NOT FOUND"

        if issues.nonEmpty then
            println(s"IT$itNum ($sourceFile): ${issues.size} MISSING textures out of $totalGroups groups")
            for issue <- issues do println(issue)
        (totalGroups, missingCount)
