package opennorrath.wld

class WldStringHash(encoded: Array[Byte]):

  private val HashKey = Array[Byte](
    0x95.toByte, 0x3A.toByte, 0xC5.toByte, 0x2A.toByte,
    0x95.toByte, 0x7A.toByte, 0x95.toByte, 0x6A.toByte,
  )

  private val decoded: Array[Byte] =
    encoded.zipWithIndex.map { (b, i) =>
      (b ^ HashKey(i % HashKey.length)).toByte
    }

  def lookup(negativeOffset: Int): String =
    val offset = -negativeOffset
    if offset < 0 || offset >= decoded.length then return ""
    val end = decoded.indexOf(0.toByte, offset)
    val len = if end < 0 then decoded.length - offset else end - offset
    String(decoded, offset, len, "US-ASCII")
