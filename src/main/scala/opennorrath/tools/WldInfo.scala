package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.Path

object WldInfo:

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println("Usage: WldInfo <file.s3d> [file2.s3d ...]")
      sys.exit(1)

    for path <- args do
      println(s"\n=== $path ===")
      try
        val entries = PfsArchive.load(Path.of(path))
        val wldEntries = entries.filter(_.extension == "wld")

        if wldEntries.isEmpty then
          println("  No WLD files found in archive")
        else
          for entry <- wldEntries do
            println(s"\n  --- ${entry.name} (${entry.data.length} bytes) ---")
            val wld = WldFile(entry.data)

            // Fragment type summary
            val typeCounts = wld.fragments.groupBy(_.getClass.getSimpleName).view.mapValues(_.length).toList.sortBy(_._1)
            println(s"  Fragments: ${wld.fragments.length} total")
            for (typeName, count) <- typeCounts do
              printf("    %-30s %d%n", typeName, count)

            // Mesh details
            val meshes = wld.fragmentsOfType[Fragment36_Mesh]
            if meshes.nonEmpty then
              val totalVerts = meshes.map(_.vertices.length).sum
              val totalPolys = meshes.map(_.polygons.length).sum
              println(s"\n  Meshes: ${meshes.size}")
              println(s"  Total vertices: $totalVerts")
              println(s"  Total triangles: $totalPolys")

              // Extract full zone geometry
              val zone = ZoneGeometry.extract(wld)
              println(s"  Combined: ${zone.vertices.length / 3} verts, ${zone.indices.length / 3} tris, ${zone.groups.size} render groups")

              // Textures used
              val textures = zone.groups.map(_.textureName).filter(_.nonEmpty).distinct.sorted
              if textures.nonEmpty then
                println(s"\n  Textures (${textures.size}):")
                for tex <- textures do
                  println(s"    $tex")
      catch
        case e: Exception =>
          println(s"  Error: ${e.getMessage}")
          e.printStackTrace()
