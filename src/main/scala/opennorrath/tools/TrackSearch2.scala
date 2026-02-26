package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.{Path, Files}

/** Search for tracks matching a model prefix, showing frame counts. */
object TrackSearch2:
  def main(args: Array[String]): Unit =
    val dir = if args.length > 0 then args(0) else "assets/EverQuest"
    val prefix = if args.length > 1 then args(1).toUpperCase else "HAF"

    val globalFiles = Files.list(Path.of(dir)).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted

    // Also check zone _chr.s3d
    val allFiles = globalFiles

    for file <- allFiles do
      try
        val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
        entries.find(_.extension == "wld").foreach { wldEntry =>
          val wld = WldFile(wldEntry.data)
          val tracks = wld.fragmentsOfType[Fragment12_TrackDef]
          // Match tracks where the name (without _TRACKDEF) starts with an anim code + prefix
          // i.e., the prefix appears starting at position 3 (after 3-char anim code)
          val animTracks = tracks.filter { td =>
            val name = td.name.toUpperCase.replace("_TRACKDEF", "")
            name.length > 3 && name.substring(3).startsWith(prefix)
          }
          val multiFrame = animTracks.filter(_.frames.length > 1)
          if animTracks.nonEmpty then
            println(s"\n${file.getFileName}: ${animTracks.size} anim tracks (${multiFrame.size} multi-frame)")
            // Group by anim code (first 3 chars)
            val byCode = animTracks.groupBy(_.name.toUpperCase.replace("_TRACKDEF", "").take(3))
            for (code, tds) <- byCode.toList.sortBy(_._1).take(10) do
              val maxFrames = tds.map(_.frames.length).max
              println(f"  $code: ${tds.size}%3d tracks, max $maxFrames frames")
        }
      catch case _: Exception => ()
