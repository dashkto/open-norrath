package opennorrath.tools

import java.nio.file.{Path, Files}

/** Debug zone .txt file: line lights and portals.
  * Usage: runMain opennorrath.tools.LightDebug [assets/arena.txt]
  */
object LightDebug:
  def main(args: Array[String]): Unit =
    val txtPath = if args.nonEmpty then args(0) else "assets/arena.txt"

    if !Files.exists(Path.of(txtPath)) then
      println(s"Light/portal file not found: $txtPath")
      return

    val source = scala.io.Source.fromFile(txtPath)
    val lines = try source.getLines().toList finally source.close()

    println(s"=== Light/Portal File: $txtPath ===")
    println(s"  Total lines: ${lines.size}")

    val lightLines = lines.filter(_.trim.startsWith("L "))
    val portalLines = lines.filter(_.trim.startsWith("P "))
    val otherLines = lines.filterNot(l => l.trim.startsWith("L ") || l.trim.startsWith("P ") || l.trim.isEmpty)

    println(s"\n=== Line Lights: ${lightLines.size} ===")
    for (line, i) <- lightLines.zipWithIndex do
      val parts = line.trim.drop(2).split(",").map(_.trim)
      if parts.length == 9 then
        try
          val x1 = parts(0).toFloat; val y1 = parts(1).toFloat; val z1 = parts(2).toFloat
          val x2 = parts(3).toFloat; val y2 = parts(4).toFloat; val z2 = parts(5).toFloat
          val r = parts(6).toFloat; val g = parts(7).toFloat; val b = parts(8).toFloat
          println(f"  [$i] EQ($x1%.1f,$y1%.1f,$z1%.1f)-($x2%.1f,$y2%.1f,$z2%.1f) RGB($r%.0f,$g%.0f,$b%.0f)")
        catch case _: NumberFormatException =>
          println(s"  [$i] PARSE ERROR: $line")
      else
        println(s"  [$i] UNEXPECTED FORMAT (${parts.length} parts): $line")

    if portalLines.nonEmpty then
      println(s"\n=== Portals: ${portalLines.size} ===")
      for (line, i) <- portalLines.zipWithIndex do
        println(s"  [$i] $line")

    if otherLines.nonEmpty then
      println(s"\n=== Other Lines: ${otherLines.size} ===")
      for line <- otherLines do
        println(s"  $line")
