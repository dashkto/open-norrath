package opennorrath.tools

import java.nio.file.{Path, Files}

/** Debug _EnvironmentEmitters.txt: particle emitter positions.
  * Usage: runMain opennorrath.tools.EmitterDebug [assets/arena_EnvironmentEmitters.txt.backup]
  */
object EmitterDebug:
  def main(args: Array[String]): Unit =
    val path = if args.nonEmpty then args(0) else "assets/arena_EnvironmentEmitters.txt.backup"

    if !Files.exists(Path.of(path)) then
      println(s"Emitter file not found: $path")
      return

    val source = scala.io.Source.fromFile(path)
    val lines = try source.getLines().toList finally source.close()

    println(s"=== Emitter File: $path ===")
    println(s"  Total lines: ${lines.size}")

    if lines.nonEmpty then
      println(s"  Header: ${lines.head}")

    println(s"\n=== Emitters: ${lines.size - 1} ===")
    for (line, i) <- lines.drop(1).zipWithIndex do
      val parts = line.split("\\^")
      if parts.length >= 5 then
        try
          val name = parts(0)
          val id = parts(1)
          val x = parts(2).toFloat
          val y = parts(3).toFloat
          val z = parts(4).toFloat
          println(f"  [$i] '$name' id=$id EQ($x%.1f, $y%.1f, $z%.1f) → GL($x%.1f, $z%.1f, ${-y}%.1f)")
        catch case _: NumberFormatException =>
          println(s"  [$i] PARSE ERROR: $line")
      else
        println(s"  [$i] UNEXPECTED FORMAT (${parts.length} parts): $line")
