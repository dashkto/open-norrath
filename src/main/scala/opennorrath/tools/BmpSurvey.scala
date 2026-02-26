package opennorrath.tools

import opennorrath.archive.PfsArchive
import java.nio.file.{Path, Files}

/** Scan all S3D files and report BMP bit depths and color key colors. */
object BmpSurvey:

  def main(args: Array[String]): Unit =
    val dir = if args.nonEmpty then args(0) else "assets/EverQuest"
    val s3dFiles = Files.list(Path.of(dir)).toArray.map(_.asInstanceOf[Path])
      .filter(_.toString.endsWith(".s3d")).sorted

    var total = 0
    var paletted8 = 0
    var paletted4 = 0
    var other = 0
    val colorKeys = scala.collection.mutable.Map[(Int, Int, Int), Int]().withDefaultValue(0)
    val nonBlackExamples = scala.collection.mutable.ListBuffer[(String, String, Int, Int, Int)]()

    for s3d <- s3dFiles do
      try
        val entries = PfsArchive.load(s3d)
        for entry <- entries if entry.extension == "bmp" do
          val data = entry.data
          if data.length >= 30 && data(0) == 'B' && data(1) == 'M' then
            val bpp = (data(28) & 0xFF) | ((data(29) & 0xFF) << 8)
            total += 1
            bpp match
              case 8 =>
                paletted8 += 1
                val dibSize = (data(14) & 0xFF) | ((data(15) & 0xFF) << 8) |
                  ((data(16) & 0xFF) << 16) | ((data(17) & 0xFF) << 24)
                val palOff = 14 + dibSize
                if data.length >= palOff + 4 then
                  val b = data(palOff) & 0xFF
                  val g = data(palOff + 1) & 0xFF
                  val r = data(palOff + 2) & 0xFF
                  colorKeys((r, g, b)) += 1
                  if r + g + b >= 10 then
                    nonBlackExamples += ((s3d.getFileName.toString, entry.name, r, g, b))
              case 4 => paletted4 += 1
              case _ => other += 1
      catch case _: Exception => ()

    println(s"Total BMPs scanned: $total")
    println(s"  8-bit paletted: $paletted8")
    println(s"  4-bit paletted: $paletted4")
    println(s"  other:          $other")
    println()
    println("Color key distribution (palette[0] for 8-bit BMPs):")
    for ((r, g, b), count) <- colorKeys.toList.sortBy(-_._2) do
      val label = if r + g + b < 10 then " (black)" else " << NON-BLACK"
      println(f"  RGB($r%3d,$g%3d,$b%3d): $count%5d$label")
    println()
    if nonBlackExamples.nonEmpty then
      println(s"Non-black color key examples (${nonBlackExamples.size} total, showing first 20):")
      for (s3d, bmp, r, g, b) <- nonBlackExamples.take(20) do
        println(f"  $s3d%-40s $bmp%-30s RGB($r%3d,$g%3d,$b%3d)")
