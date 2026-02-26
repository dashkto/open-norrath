package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.Path

/** Dump all TrackDef names and frame counts from a single S3D file. */
object DumpTracks:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val entries = PfsArchive.load(Path.of(file), extensionFilter = Some(Set("wld")))
    entries.find(_.extension == "wld").foreach { wldEntry =>
      val wld = WldFile(wldEntry.data)
      val tracks = wld.fragmentsOfType[Fragment12_TrackDef]
      println(s"Total tracks: ${tracks.size}")
      val multiFrame = tracks.filter(_.frames.length > 1)
      println(s"Multi-frame tracks: ${multiFrame.size}")
      println()
      // Show all multi-frame tracks
      for td <- multiFrame.sortBy(_.name).take(30) do
        println(f"  ${td.name}%-50s ${td.frames.length}%4d frames")
      if multiFrame.size > 30 then println(s"  ... and ${multiFrame.size - 30} more")
      println()
      // Show unique prefixes (first 3 chars) of multi-frame tracks
      val prefixes = multiFrame.map(_.name.toUpperCase.replace("_TRACKDEF", "").take(3)).distinct.sorted
      println(s"Unique 3-char prefixes of multi-frame tracks: ${prefixes.mkString(", ")}")
      println()
      // Show unique prefixes at various lengths
      for len <- 3 to 6 do
        val p = multiFrame.map(_.name.toUpperCase.replace("_TRACKDEF", "").take(len)).distinct.sorted
        println(s"  ${len}-char prefixes: ${p.take(20).mkString(", ")}${if p.size > 20 then s" ... (${p.size} total)" else ""}")
      // Show unique model codes (chars 4-6, after 3-char anim code)
      val models = multiFrame.map(_.name.toUpperCase.replace("_TRACKDEF", ""))
        .filter(_.length > 6).map(_.substring(3, 6)).distinct.sorted
      println(s"\nUnique model codes (chars 4-6): ${models.mkString(", ")}")
    }
