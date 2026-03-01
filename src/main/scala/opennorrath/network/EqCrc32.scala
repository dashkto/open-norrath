package opennorrath.network

import java.nio.file.{Files, Path}
import java.util.zip.CRC32

object EqCrc32:

  def compute(data: Array[Byte], offset: Int, length: Int): Int =
    val crc = CRC32()
    crc.update(data, offset, length)
    crc.getValue.toInt

  /** Compute CRC32 of an entire file, returned as uint64 (zero-extended).
    * Returns 0 if the file doesn't exist.
    */
  def computeFile(path: Path): Long =
    if !Files.exists(path) then
      println(s"[EqCrc32] File not found, using checksum 0: $path")
      return 0L
    val crc = CRC32()
    val data = Files.readAllBytes(path)
    crc.update(data)
    crc.getValue
