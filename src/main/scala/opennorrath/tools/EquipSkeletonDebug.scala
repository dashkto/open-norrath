package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f, Quaternionf}
import java.nio.file.{Path, Files}

/** Deep-dive into skeleton structures of oversized equipment models.
  * For each flagged IT model, dumps bone hierarchy, rest-pose transforms,
  * scale factors, vertex-to-bone assignments, and per-bone bounding boxes.
  *
  * Usage: runMain opennorrath.tools.EquipSkeletonDebug [assets/EverQuest] [it1,it2,...]
  *   Default: analyzes the known oversized models (IT140-160)
  */
object EquipSkeletonDebug:

    // The oversized models found by EquipSizeDebug
    val DefaultTargets = Set(155, 145, 151, 140, 148, 150, 160, 149, 141, 142)

    def main(args: Array[String]): Unit =
        val dir = if args.length > 0 then args(0) else "assets/EverQuest"
        val targets = if args.length > 1 then
            args(1).split(",").map(_.trim.toInt).toSet
        else DefaultTargets
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
        var found = Set.empty[Int]

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
                                if targets.contains(itNum) && !found.contains(itNum) then
                                    found += itNum
                                    analyzeModel(wld, actor, itNum, file.getFileName.toString)
                            case _ => ()
                }
            catch case e: Exception =>
                println(s"Warning: failed to load ${file.getFileName}: ${e.getMessage}")

        val missing = targets -- found
        if missing.nonEmpty then
            println(s"\nNot found: ${missing.toList.sorted.map(n => s"IT$n").mkString(", ")}")

    private def analyzeModel(wld: WldFile, actor: Fragment14_Actor, itNum: Int,
        sourceFile: String): Unit =
        println(s"${"=" * 80}")
        println(s"IT$itNum  ($sourceFile)  actor=${actor.name}")
        println(s"  Component refs: ${actor.componentRefs.mkString(", ")}")

        var meshFragments = List.empty[Fragment36_Mesh]
        var skeletonOpt: Option[Fragment10_SkeletonHierarchy] = None

        for ref <- actor.componentRefs do
            try
                wld.fragment(ref) match
                    case mr: Fragment2D_MeshReference =>
                        wld.fragment(mr.meshRef) match
                            case m: Fragment36_Mesh =>
                                meshFragments = meshFragments :+ m
                                println(s"  Direct mesh: ${m.name}, ${m.vertices.length} verts, " +
                                    s"${m.polygons.length} polys, center=(${m.center.x}%.1f, ${m.center.y}%.1f, ${m.center.z}%.1f)")
                            case other =>
                                println(s"  MeshRef -> unexpected: ${other.getClass.getSimpleName}")
                    case skelRef: Fragment11_SkeletonHierarchyRef =>
                        wld.fragment(skelRef.skeletonRef) match
                            case skel: Fragment10_SkeletonHierarchy =>
                                skeletonOpt = Some(skel)
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
                    case other =>
                        println(s"  Component: ${other.getClass.getSimpleName} (${other.name})")
            catch case e: Exception =>
                println(s"  Component ref $ref failed: ${e.getMessage}")

        skeletonOpt match
            case None =>
                println(s"  NO SKELETON")
            case Some(skel) =>
                analyzeSkeleton(wld, skel, meshFragments)

        // Overall mesh stats
        if meshFragments.nonEmpty then
            println(s"\n  --- Mesh Summary ---")
            println(s"  Total meshes: ${meshFragments.size}")
            for (mesh, idx) <- meshFragments.zipWithIndex do
                val verts = mesh.vertices
                if verts.nonEmpty then
                    var minX = Float.MaxValue; var maxX = Float.MinValue
                    var minY = Float.MaxValue; var maxY = Float.MinValue
                    var minZ = Float.MaxValue; var maxZ = Float.MinValue
                    for v <- verts do
                        if v.x < minX then minX = v.x; if v.x > maxX then maxX = v.x
                        if v.y < minY then minY = v.y; if v.y > maxY then maxY = v.y
                        if v.z < minZ then minZ = v.z; if v.z > maxZ then maxZ = v.z
                    println(f"  Mesh[$idx] ${mesh.name}: ${verts.length} verts, " +
                        f"bounds=($minX%.1f..$maxX%.1f, $minY%.1f..$maxY%.1f, $minZ%.1f..$maxZ%.1f) " +
                        f"center=(${mesh.center.x}%.1f, ${mesh.center.y}%.1f, ${mesh.center.z}%.1f)")
        println()

    private def analyzeSkeleton(wld: WldFile, skel: Fragment10_SkeletonHierarchy,
        meshFragments: List[Fragment36_Mesh]): Unit =
        println(s"\n  --- Skeleton: ${skel.name}, ${skel.bones.length} bones ---")

        // Compute all world transforms
        val worldTransforms = skel.boneWorldTransforms(wld)

        // Dump each bone
        for (bone, idx) <- skel.bones.zipWithIndex do
            val boneName = try
                val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
                val trackDef = wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
                trackDef.cleanName
            catch case _: Exception => "?"

            val frame = try
                val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
                val trackDef = wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
                Some(trackDef.frames(0))
            catch case _: Exception => None

            val world = worldTransforms(idx)
            // Extract world-space translation and scale from the composed matrix
            val wx = world.m30(); val wy = world.m31(); val wz = world.m32()
            // Approximate scale from column lengths
            val scaleX = math.sqrt((world.m00() * world.m00() + world.m01() * world.m01() + world.m02() * world.m02()).toDouble).toFloat
            val scaleY = math.sqrt((world.m10() * world.m10() + world.m11() * world.m11() + world.m12() * world.m12()).toDouble).toFloat
            val scaleZ = math.sqrt((world.m20() * world.m20() + world.m21() * world.m21() + world.m22() * world.m22()).toDouble).toFloat

            val indent = "    "
            val parentStr = if bone.parentIndex < 0 then "ROOT" else s"parent=${bone.parentIndex}"
            val childStr = if bone.children.nonEmpty then s" children=${bone.children.mkString(",")}" else ""

            frame match
                case Some(f) =>
                    val t = f.translation; val r = f.rotation
                    println(f"${indent}Bone[$idx] $boneName%-20s $parentStr$childStr")
                    println(f"$indent  local: translate=(${t.x}%8.2f, ${t.y}%8.2f, ${t.z}%8.2f)  " +
                        f"rot=(${r.x}%.3f, ${r.y}%.3f, ${r.z}%.3f, ${r.w}%.3f)  scale=${f.scale}%.4f")
                    println(f"$indent  world: pos=($wx%8.2f, $wy%8.2f, $wz%8.2f)  " +
                        f"scale=($scaleX%.4f, $scaleY%.4f, $scaleZ%.4f)")
                case None =>
                    println(f"${indent}Bone[$idx] $boneName%-20s $parentStr$childStr")
                    println(f"$indent  world: pos=($wx%8.2f, $wy%8.2f, $wz%8.2f)")

        // Per-bone vertex bounding boxes (shows which bones own the far-flung vertices)
        println(s"\n  --- Per-bone vertex bounds ---")
        // Build vertex-to-bone mapping from vertexPieces
        for (mesh, mIdx) <- meshFragments.zipWithIndex do
            if mesh.vertexPieces.nonEmpty then
                val vertBone = new Array[Int](mesh.vertices.length)
                var vi = 0
                for piece <- mesh.vertexPieces do
                    for _ <- 0 until piece.count do
                        if vi < vertBone.length then
                            vertBone(vi) = piece.boneIndex
                            vi += 1

                // Group vertices by bone and compute bounds
                val byBone = mesh.vertices.indices.groupBy(i => vertBone(i))
                for (boneIdx, vertIndices) <- byBone.toList.sortBy(_._1) do
                    val verts = vertIndices.map(mesh.vertices(_))
                    var minX = Float.MaxValue; var maxX = Float.MinValue
                    var minY = Float.MaxValue; var maxY = Float.MinValue
                    var minZ = Float.MaxValue; var maxZ = Float.MinValue
                    for v <- verts do
                        if v.x < minX then minX = v.x; if v.x > maxX then maxX = v.x
                        if v.y < minY then minY = v.y; if v.y > maxY then maxY = v.y
                        if v.z < minZ then minZ = v.z; if v.z > maxZ then maxZ = v.z
                    val extX = maxX - minX; val extY = maxY - minY; val extZ = maxZ - minZ
                    val maxExt = math.max(extX, math.max(extY, extZ))
                    val boneName = try
                        val bone = skel.bones(boneIdx)
                        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
                        val trackDef = wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
                        trackDef.cleanName
                    catch case _: Exception => "?"
                    val flag = if maxExt > 20f then " <<<" else ""
                    println(f"    Mesh[$mIdx] bone[$boneIdx] $boneName%-20s ${vertIndices.size}%4d verts  " +
                        f"extent=($extX%7.1f, $extY%7.1f, $extZ%7.1f)  max=$maxExt%7.1f" +
                        f"  bounds=($minX%7.1f..$maxX%7.1f, $minY%7.1f..$maxY%7.1f, $minZ%7.1f..$maxZ%7.1f)$flag")
            else
                println(f"    Mesh[$mIdx]: no vertexPieces (unskinned)")

        // Show what rootBoneTransform(0) produces — this is what loadEquipmentModels uses
        println(s"\n  --- Root bone transform (used by renderer) ---")
        val rootXform = skel.restPoseBoneTransform(0, wld)
        println(f"    rootBoneTransform(0):")
        println(f"      | ${rootXform.m00()}%8.4f ${rootXform.m10()}%8.4f ${rootXform.m20()}%8.4f ${rootXform.m30()}%8.4f |")
        println(f"      | ${rootXform.m01()}%8.4f ${rootXform.m11()}%8.4f ${rootXform.m21()}%8.4f ${rootXform.m31()}%8.4f |")
        println(f"      | ${rootXform.m02()}%8.4f ${rootXform.m12()}%8.4f ${rootXform.m22()}%8.4f ${rootXform.m32()}%8.4f |")
        println(f"      | ${rootXform.m03()}%8.4f ${rootXform.m13()}%8.4f ${rootXform.m23()}%8.4f ${rootXform.m33()}%8.4f |")
        val rootScale = math.sqrt((rootXform.m00() * rootXform.m00() + rootXform.m01() * rootXform.m01() + rootXform.m02() * rootXform.m02()).toDouble)
        println(f"    Effective scale: $rootScale%.4f")
        println(f"    Translation: (${rootXform.m30()}%.2f, ${rootXform.m31()}%.2f, ${rootXform.m32()}%.2f)")
