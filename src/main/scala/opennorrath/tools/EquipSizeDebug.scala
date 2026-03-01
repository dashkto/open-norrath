package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.{Path, Files}

/** Scan all gequip*.s3d equipment models and report bounding box extents.
  * Flags models with anomalously large dimensions that might render oversized in-game.
  *
  * Usage: runMain opennorrath.tools.EquipSizeDebug [assets/EverQuest] [threshold]
  *   threshold: max extent in any axis before flagging (default: 20.0 EQ units)
  */
object EquipSizeDebug:

    case class EquipStats(
        itNum: Int,
        sourceFile: String,
        vertexCount: Int,
        polyCount: Int,
        meshCount: Int,
        hasSkeleton: Boolean,
        isShield: Boolean,
        center: (Float, Float, Float),
        extents: (Float, Float, Float),   // width, depth, height in EQ S3D space
        maxExtent: Float,
        minBounds: (Float, Float, Float),
        maxBounds: (Float, Float, Float),
    )

    def main(args: Array[String]): Unit =
        val dir = if args.length > 0 then args(0) else "assets/EverQuest"
        val threshold = if args.length > 1 then args(1).toFloat else 20.0f
        val assetsDir = Path.of(dir)

        if !Files.isDirectory(assetsDir) then
            println(s"Assets directory not found: $assetsDir")
            return

        val gequipFiles = Files.list(assetsDir).toArray.map(_.asInstanceOf[Path])
            .filter { p =>
                val name = p.getFileName.toString.toLowerCase
                name.startsWith("gequip") && name.endsWith(".s3d")
            }.sorted

        if gequipFiles.isEmpty then
            println(s"No gequip*.s3d files found in $assetsDir")
            return

        println(s"Scanning ${gequipFiles.length} gequip archives in $assetsDir")
        println(s"Anomaly threshold: ${threshold} EQ units max extent")
        println()

        val itPattern = "^it(\\d+)$".r
        val allStats = scala.collection.mutable.ListBuffer[EquipStats]()
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
                                if !seen.contains(itNum) then
                                    seen += itNum
                                    analyzeEquipModel(wld, actor, itNum, file.getFileName.toString)
                                        .foreach(allStats += _)
                            case _ => ()
                }
            catch case e: Exception =>
                println(s"  Warning: failed to load ${file.getFileName}: ${e.getMessage}")

        println(s"=== Summary: ${allStats.size} equipment models scanned ===")
        println()

        // Sort by max extent descending to show the biggest offenders first
        val sorted = allStats.toList.sortBy(-_.maxExtent)

        // Report anomalies
        val anomalies = sorted.filter(_.maxExtent > threshold)
        if anomalies.nonEmpty then
            println(s"!!! ${anomalies.size} models exceed ${threshold} EQ units !!!")
            println()
            for s <- anomalies do
                printStats(s, flagged = true)
            println()

        // Histogram of max extents
        println("=== Extent distribution ===")
        val buckets = List(0f, 2f, 5f, 10f, 20f, 50f, 100f, 500f, Float.MaxValue)
        for i <- 0 until buckets.length - 1 do
            val lo = buckets(i); val hi = buckets(i + 1)
            val count = sorted.count(s => s.maxExtent >= lo && s.maxExtent < hi)
            val label = if hi == Float.MaxValue then s"${lo}+" else s"${lo}-${hi}"
            val bar = "#" * math.min(count, 80)
            println(f"  $label%-10s $count%4d $bar")

        // Also show the top 20 largest regardless of threshold
        println()
        println("=== Top 20 largest models ===")
        for s <- sorted.take(20) do
            printStats(s, flagged = s.maxExtent > threshold)

        // Models with very large center offsets (potential rendering issues)
        println()
        println("=== Models with large center offsets (>5 units) ===")
        val largeCenter = sorted.filter { s =>
            val (cx, cy, cz) = s.center
            math.sqrt((cx * cx + cy * cy + cz * cz).toDouble) > 5.0
        }
        if largeCenter.isEmpty then println("  None found.")
        else for s <- largeCenter do
            val (cx, cy, cz) = s.center
            val dist = math.sqrt((cx * cx + cy * cy + cz * cz).toDouble)
            println(f"  IT${s.itNum}%-5d center=(${cx}%.1f, ${cy}%.1f, ${cz}%.1f) dist=${dist}%.1f  extents=(${s.extents._1}%.1f, ${s.extents._2}%.1f, ${s.extents._3}%.1f)")

    private def analyzeEquipModel(wld: WldFile, actor: Fragment14_Actor, itNum: Int,
        sourceFile: String): Option[EquipStats] =
        var meshFragments = List.empty[Fragment36_Mesh]
        var hasSkeleton = false

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
                                hasSkeleton = true
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

        if meshFragments.isEmpty then return None

        // Compute bounding box from raw vertices across all mesh fragments
        var minX = Float.MaxValue; var maxX = Float.MinValue
        var minY = Float.MaxValue; var maxY = Float.MinValue
        var minZ = Float.MaxValue; var maxZ = Float.MinValue
        var totalVerts = 0
        var totalPolys = 0

        for mesh <- meshFragments do
            for v <- mesh.vertices do
                if v.x < minX then minX = v.x; if v.x > maxX then maxX = v.x
                if v.y < minY then minY = v.y; if v.y > maxY then maxY = v.y
                if v.z < minZ then minZ = v.z; if v.z > maxZ then maxZ = v.z
            totalVerts += mesh.vertices.length
            totalPolys += mesh.polygons.length

        if totalVerts == 0 then return None

        val extX = maxX - minX; val extY = maxY - minY; val extZ = maxZ - minZ
        val maxExt = math.max(extX, math.max(extY, extZ))
        val minExt = math.min(extX, math.min(extY, extZ))
        val cx = meshFragments.map(_.center.x).sum / meshFragments.size
        val cy = meshFragments.map(_.center.y).sum / meshFragments.size
        val cz = meshFragments.map(_.center.z).sum / meshFragments.size
        val isShield = maxExt > 0f && minExt / maxExt < 0.25f &&
            math.sqrt((cx * cx + cy * cy + cz * cz).toDouble) < 0.5 && !hasSkeleton

        Some(EquipStats(
            itNum = itNum,
            sourceFile = sourceFile,
            vertexCount = totalVerts,
            polyCount = totalPolys,
            meshCount = meshFragments.size,
            hasSkeleton = hasSkeleton,
            isShield = isShield,
            center = (cx, cy, cz),
            extents = (extX, extY, extZ),
            maxExtent = maxExt,
            minBounds = (minX, minY, minZ),
            maxBounds = (maxX, maxY, maxZ),
        ))

    private def printStats(s: EquipStats, flagged: Boolean): Unit =
        val flag = if flagged then ">>> " else "    "
        val skel = if s.hasSkeleton then " [skeleton]" else ""
        val shield = if s.isShield then " [shield]" else ""
        println(f"${flag}IT${s.itNum}%-5d  max=${s.maxExtent}%7.1f  extents=(${s.extents._1}%6.1f, ${s.extents._2}%6.1f, ${s.extents._3}%6.1f)  " +
            f"center=(${s.center._1}%5.1f, ${s.center._2}%5.1f, ${s.center._3}%5.1f)  " +
            f"verts=${s.vertexCount}%5d  polys=${s.polyCount}%5d  meshes=${s.meshCount}  " +
            f"${s.sourceFile}$skel$shield")
