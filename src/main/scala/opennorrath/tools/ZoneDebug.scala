package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.Path

/** Debug zone S3D: WLD fragment stats, objects.wld placements.
  * Usage: runMain opennorrath.tools.ZoneDebug [assets/arena.s3d]
  */
object ZoneDebug:
  def main(args: Array[String]): Unit =
    val s3dPath = if args.nonEmpty then args(0) else "assets/arena.s3d"
    val entries = PfsArchive.load(Path.of(s3dPath))

    println(s"=== Zone S3D: $s3dPath ===")
    println(s"  Entries: ${entries.size}")
    for e <- entries do
      println(s"    ${e.name} (${e.data.length} bytes)")

    // Zone WLD
    val zoneWldOpt = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
    zoneWldOpt.foreach { entry =>
      val wld = WldFile(entry.data)
      println(s"\n=== Zone WLD: ${entry.name} ===")
      println(s"  Total fragments: ${wld.fragments.size}")
      val byType = wld.fragments.groupBy(_.getClass.getSimpleName)
      for (typeName, frags) <- byType.toList.sortBy(_._1) do
        println(s"    $typeName: ${frags.size}")
    }

    // Objects WLD (placements)
    val objectsWldOpt = entries.find(_.name == "objects.wld")
    objectsWldOpt.foreach { entry =>
      val wld = WldFile(entry.data)
      val placements = wld.fragmentsOfType[Fragment15_ObjectInstance]
      println(s"\n=== Object placements: ${placements.size} ===")
      val byActor = placements.groupBy(_.actorName)
      for (actor, instances) <- byActor.toList.sortBy(_._1) do
        println(s"  $actor: ${instances.size} instances")
        for inst <- instances do
          val p = inst.position; val r = inst.rotation; val s = inst.scale
          println(f"    EQ(${p.x}%.1f, ${p.y}%.1f, ${p.z}%.1f) rot(${r.x}%.1f, ${r.y}%.1f, ${r.z}%.1f) scale=${s.y}%.2f")
    }
