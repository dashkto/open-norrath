package opennorrath.network

import java.util.zip.CRC32

object EqCrc32:

  def compute(data: Array[Byte], offset: Int, length: Int): Int =
    val crc = CRC32()
    crc.update(data, offset, length)
    crc.getValue.toInt
