package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.Path

/** Dump multi-frame track samples grouped by model prefix pattern. */
object DumpTracks2:
  def main(args: Array[String]): Unit =
    val file = args(0)
    val entries = PfsArchive.load(Path.of(file), extensionFilter = Some(Set("wld")))
    entries.find(_.extension == "wld").foreach { wldEntry =>
      val wld = WldFile(wldEntry.data)
      val tracks = wld.fragmentsOfType[Fragment12_TrackDef]
      val multiFrame = tracks.filter(_.frames.length > 1)
      println(s"Total: ${tracks.size}, Multi-frame: ${multiFrame.size}")

      // Show sample tracks for first anim code of each model prefix length
      val names = multiFrame.map(_.name.toUpperCase.replace("_TRACKDEF", "")).sorted
      // Group by first 6 chars to see model patterns
      val byPrefix6 = names.groupBy(_.take(6))
      println(s"\nSample tracks by 6-char prefix:")
      for (prefix, ns) <- byPrefix6.toList.sortBy(_._1).take(20) do
        println(s"  $prefix: ${ns.take(3).mkString(", ")}  (${ns.size} total)")
    }
