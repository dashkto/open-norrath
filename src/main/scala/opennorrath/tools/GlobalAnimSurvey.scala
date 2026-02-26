package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.{Path, Files}

object GlobalAnimSurvey:
  def main(args: Array[String]): Unit =
    val dir = if args.nonEmpty then args(0) else "assets/EverQuest"
    val globalFiles = List(
      "global_chr.s3d", "global2_chr.s3d", "global3_chr.s3d",
      "global4_chr.s3d", "global5_chr.s3d", "global6_chr.s3d", "global7_chr.s3d"
    )
    
    for file <- globalFiles do
      val path = Path.of(dir, file)
      if Files.exists(path) then
        println(s"\n=== $file ===")
        try
          val entries = PfsArchive.load(path)
          val wldEntry = entries.find(_.extension == "wld")
          if wldEntry.isDefined then
            val wld = WldFile(wldEntry.get.data)
            val trackDefs = wld.fragmentsOfType[Fragment12_TrackDef]
            val tigTracks = trackDefs.filter(_.name.toUpperCase.contains("TIG"))
            if tigTracks.nonEmpty then
              println(s"  Found ${tigTracks.size} TIG tracks:")
              for td <- tigTracks.sortBy(_.name) do
                println(s"    ${td.name} (${td.frames.length} frames)")
            else
              println(s"  No TIG tracks (${trackDefs.size} total tracks)")
          else
            println("  No WLD entry found")
        catch case e: Exception => println(s"  Error: ${e.getMessage}")
      else
        println(s"\n=== $file === (not found)")
