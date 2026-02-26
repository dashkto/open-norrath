package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.{Path, Files}

/** Search global S3D files for tracks matching given model prefixes. */
object TrackSearch:
  def main(args: Array[String]): Unit =
    val dir = if args.length > 0 then args(0) else "assets/EverQuest"
    val prefixes = if args.length > 1 then args.drop(1).toList else List("BRF", "BRM", "PIF", "RIF", "RIM")

    val globalFiles = Files.list(Path.of(dir)).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted

    for prefix <- prefixes do
      println(s"\n=== Searching for '$prefix' tracks ===")
      var found = 0
      for file <- globalFiles do
        try
          val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
          entries.find(_.extension == "wld").foreach { wldEntry =>
            val wld = WldFile(wldEntry.data)
            val tracks = wld.fragmentsOfType[Fragment12_TrackDef]
              .filter(_.name.toUpperCase.replace("_TRACKDEF", "").contains(prefix))
            if tracks.nonEmpty then
              println(s"  ${file.getFileName}: ${tracks.size} tracks")
              for td <- tracks.sortBy(_.name).take(5) do
                println(s"    ${td.name} (${td.frames.length} frames)")
              if tracks.size > 5 then println(s"    ... and ${tracks.size - 5} more")
              found += tracks.size
          }
        catch case _: Exception => ()
      if found == 0 then println("  NO TRACKS FOUND in any global file")
