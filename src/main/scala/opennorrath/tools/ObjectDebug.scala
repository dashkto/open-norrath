package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.{Path, Files}

/** Debug _obj.s3d: actors, meshes, materials.
  * Usage: runMain opennorrath.tools.ObjectDebug [assets/arena.s3d]
  */
object ObjectDebug:
  def main(args: Array[String]): Unit =
    val s3dPath = if args.nonEmpty then args(0) else "assets/arena.s3d"
    val objS3dPath = s3dPath.replace(".s3d", "_obj.s3d")

    if !Files.exists(Path.of(objS3dPath)) then
      println(s"Object S3D not found: $objS3dPath")
      return

    val entries = PfsArchive.load(Path.of(objS3dPath))
    println(s"=== Object S3D: $objS3dPath ===")
    println(s"  Entries: ${entries.size}")

    val wldEntry = entries.find(_.extension == "wld")
    if wldEntry.isEmpty then
      println("  No WLD found")
      return

    val wld = WldFile(wldEntry.get.data)

    // Actors
    val actors = wld.fragmentsOfType[Fragment14_Actor]
    println(s"\n=== Actors: ${actors.size} ===")
    for actor <- actors do
      val key = actor.name.replace("_ACTORDEF", "").toLowerCase
      println(s"  $key (refs: ${actor.componentRefs.mkString(", ")})")

    // Meshes
    val meshes = wld.fragmentsOfType[Fragment36_Mesh]
    println(s"\n=== Meshes: ${meshes.size} ===")
    for mesh <- meshes do
      val xs = mesh.vertices.map(_.x)
      val ys = mesh.vertices.map(_.y)
      val zs = mesh.vertices.map(_.z)
      println(s"  ${mesh.name}: ${mesh.vertices.length} verts, ${mesh.polygons.length} polys, ${mesh.renderGroups.length} groups")
      if mesh.vertices.nonEmpty then
        println(f"    X: ${xs.min}%.1f to ${xs.max}%.1f, Y: ${ys.min}%.1f to ${ys.max}%.1f, Z: ${zs.min}%.1f to ${zs.max}%.1f")

    // Materials
    val materials = wld.fragmentsOfType[Fragment30_Material]
    println(s"\n=== Materials: ${materials.size} ===")
    for mat <- materials do
      println(s"  ${mat.name}: type=${mat.shaderType}, bitmapRef=${mat.bitmapInfoRefIndex}")

    // Textures in S3D
    val bmpEntries = entries.filter(_.extension == "bmp")
    println(s"\n=== Textures: ${bmpEntries.size} ===")
    for e <- bmpEntries do
      println(s"  ${e.name} (${e.data.length} bytes)")
