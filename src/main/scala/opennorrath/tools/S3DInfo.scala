package opennorrath.tools

import opennorrath.archive.PfsArchive
import java.nio.file.Path

object S3DInfo:

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println("Usage: S3DInfo <file.s3d> [file2.s3d ...]")
      sys.exit(1)

    for path <- args do
      println(s"\n=== $path ===")
      try
        val entries = PfsArchive.load(Path.of(path))
        println(s"${entries.size} files:\n")
        printf("  %-40s %10s%n", "Name", "Size")
        printf("  %-40s %10s%n", "-" * 40, "-" * 10)
        var totalSize = 0L
        for entry <- entries.sortBy(_.name) do
          printf("  %-40s %10d%n", entry.name, entry.data.length)
          totalSize += entry.data.length
        printf("  %-40s %10s%n", "-" * 40, "-" * 10)
        printf("  %-40s %10d%n", s"Total (${entries.size} files)", totalSize)
      catch
        case e: Exception =>
          println(s"Error: ${e.getMessage}")
