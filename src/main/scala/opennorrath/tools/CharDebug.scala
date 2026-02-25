package opennorrath.tools

import opennorrath.animation.AnimatedCharacter
import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.{Path, Files}

/** Debug _chr.s3d: actors, skeletons, animations.
  * Usage: runMain opennorrath.tools.CharDebug [assets/arena.s3d]
  */
object CharDebug:
  def main(args: Array[String]): Unit =
    val s3dPath = if args.nonEmpty then args(0) else "assets/arena.s3d"
    val chrS3dPath = s3dPath.replace(".s3d", "_chr.s3d")

    if !Files.exists(Path.of(chrS3dPath)) then
      println(s"Character S3D not found: $chrS3dPath")
      return

    val entries = PfsArchive.load(Path.of(chrS3dPath))
    println(s"=== Character S3D: $chrS3dPath ===")
    println(s"  Entries: ${entries.size}")
    for e <- entries do
      println(s"    ${e.name} (${e.data.length} bytes)")

    val wldEntry = entries.find(_.extension == "wld")
    if wldEntry.isEmpty then
      println("  No WLD found")
      return

    val wld = WldFile(wldEntry.get.data)
    println(s"\n  Total fragments: ${wld.fragments.size}")
    val byType = wld.fragments.groupBy(_.getClass.getSimpleName)
    for (typeName, frags) <- byType.toList.sortBy(_._1) do
      println(s"    $typeName: ${frags.size}")

    // Actors and their skeletons/animations
    val actors = wld.fragmentsOfType[Fragment14_Actor]
    println(s"\n=== Actors: ${actors.size} ===")
    for actor <- actors do
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      println(s"\n  $actorKey (refs: ${actor.componentRefs.mkString(", ")})")

      // Find skeleton
      val skeletonOpt = actor.componentRefs.flatMap { ref =>
        try wld.fragment(ref) match
          case sr: Fragment11_SkeletonHierarchyRef =>
            wld.fragment(sr.skeletonRef) match
              case sk: Fragment10_SkeletonHierarchy => Some(sk)
              case _ => None
          case _ => None
        catch case _: Exception => None
      }.headOption

      skeletonOpt match
        case Some(sk) =>
          println(s"    Skeleton: ${sk.name}, ${sk.bones.length} bones")
          for (bone, i) <- sk.bones.zipWithIndex do
            println(s"      [$i] parent=${bone.parentIndex}, trackRef=${bone.trackRef}, children=${bone.children.mkString(",")}")

          // Meshes via skeleton refs
          val meshRefs = sk.meshRefs.flatMap { mr =>
            try wld.fragment(mr) match
              case m: Fragment2D_MeshReference =>
                wld.fragment(m.meshRef) match
                  case mesh: Fragment36_Mesh => Some(mesh)
                  case _ => None
              case _ => None
            catch case _: Exception => None
          }
          println(s"    Meshes: ${meshRefs.size}")
          for mesh <- meshRefs do
            println(s"      ${mesh.name}: ${mesh.vertices.length} verts, ${mesh.polygons.length} polys, ${mesh.vertexPieces.length} bone pieces")

          // Animations
          val clips = AnimatedCharacter.discoverAnimations(wld, sk)
          if clips.nonEmpty then
            println(s"    Animations: ${clips.size}")
            for (code, clip) <- clips.toList.sortBy(_._1) do
              println(s"      $code: ${clip.frameCount} frames")
          else
            println(s"    Animations: none")

        case None =>
          println(s"    No skeleton found")

    // Textures in S3D
    val bmpEntries = entries.filter(_.extension == "bmp")
    println(s"\n=== Textures: ${bmpEntries.size} ===")
    for e <- bmpEntries do
      println(s"  ${e.name} (${e.data.length} bytes)")
