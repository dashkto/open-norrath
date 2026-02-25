package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.Path

object ObjDebug:
  def main(args: Array[String]): Unit =
    // objects.wld placements
    val zoneEntries = PfsArchive.load(Path.of("assets/arena.s3d"))
    val objectsWld = WldFile(zoneEntries.find(_.name == "objects.wld").get.data)

    println("=== Placements (objects.wld) ===")
    for f <- objectsWld.fragmentsOfType[Fragment15_ObjectInstance] do
      println(s"  '${f.actorName}' at (${f.position.x}, ${f.position.y}, ${f.position.z})")

    // actors in objects.wld
    println("\n=== Actors in objects.wld ===")
    for f <- objectsWld.fragmentsOfType[Fragment14_Actor] do
      println(s"  '${f.name}' refs=${f.componentRefs}")

    // arena_obj.wld actors
    val objEntries = PfsArchive.load(Path.of("assets/arena_obj.s3d"))
    val objWld = WldFile(objEntries.find(_.extension == "wld").get.data)

    println("\n=== Actors in arena_obj.wld ===")
    for f <- objWld.fragmentsOfType[Fragment14_Actor] do
      val key = f.name.replace("_ACTORDEF", "").toLowerCase
      println(s"  '${f.name}' → key='$key' refs=${f.componentRefs}")

    println("\n=== MeshReferences in arena_obj.wld ===")
    for (f, i) <- objWld.fragments.zipWithIndex do
      f match
        case mr: Fragment2D_MeshReference => println(s"  [${i+1}] '${mr.name}' meshRef=${mr.meshRef}")
        case _ =>

    println("\n=== Meshes in arena_obj.wld ===")
    for (f, i) <- objWld.fragments.zipWithIndex do
      f match
        case m: Fragment36_Mesh => println(s"  [${i+1}] '${m.name}' verts=${m.vertices.length} polys=${m.polygons.length}")
        case _ =>

    // Character models
    val chrEntries = PfsArchive.load(Path.of("assets/arena_chr.s3d"))
    val chrWld = WldFile(chrEntries.find(_.extension == "wld").get.data)

    // Animation track names
    println("\n=== TrackDef names (Fragment12) in arena_chr.wld ===")
    val trackDefs = chrWld.fragmentsOfType[Fragment12_TrackDef]
    for td <- trackDefs do
      println(s"  '${td.name}' frames=${td.frames.length}")

    println(s"\n  Total TrackDefs: ${trackDefs.size}")

    // Group by prefix to see patterns
    println("\n=== TrackDef names grouped by model prefix ===")
    val grouped = trackDefs.groupBy { td =>
      val name = td.name.toUpperCase
      name.takeWhile(c => c.isLetter)
    }
    for (prefix, tds) <- grouped.toList.sortBy(_._1) do
      println(s"  $prefix: ${tds.size} tracks")
      // Show unique frame counts
      val frameCounts = tds.map(_.frames.length).distinct.sorted
      println(s"    frame counts: ${frameCounts.mkString(", ")}")
