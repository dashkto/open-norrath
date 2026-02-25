package opennorrath.archive

object PfsCrc:

  private val Polynomial = 0x04C11DB7

  private val Table: Array[Int] =
    val t = new Array[Int](256)
    for i <- 0 until 256 do
      var crc = i << 24
      for _ <- 0 until 8 do
        if (crc & 0x80000000) != 0 then
          crc = (crc << 1) ^ Polynomial
        else
          crc = crc << 1
      t(i) = crc
    t

  def compute(filename: String): Int =
    var crc = 0
    // Process each byte of the filename including a null terminator
    val bytes = filename.toLowerCase.getBytes("US-ASCII")
    for b <- bytes do
      val index = ((crc >>> 24) ^ (b & 0xFF)) & 0xFF
      crc = (crc << 8) ^ Table(index)
    // Process null terminator
    val index = ((crc >>> 24) ^ 0) & 0xFF
    crc = (crc << 8) ^ Table(index)
    crc
