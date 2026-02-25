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

    // Unknown fragment types
    println("\n=== Unknown types in objects.wld ===")
    val unknowns = objectsWld.fragments.collect { case u: UnknownFragment => u }
    val typeCounts = unknowns.groupBy(_.fragmentType).view.mapValues(_.length).toList.sortBy(_._1)
    for (t, c) <- typeCounts do
      printf("  0x%02X: %d%n", t, c)
