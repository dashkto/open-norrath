package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}
import java.util.zip.Inflater

/** Decrypt and decompress zone protocol packets.
  *
  * The Mac zone server applies encryption and/or zlib compression to several
  * packet types before sending them. This object reverses those transforms.
  *
  * Reference: EQMacDocker/Server/common/packet_functions.cpp
  *            EQMacDocker/Server/common/patches/mac.cpp
  */
object PacketCrypto:

  /** Decrypt OP_PlayerProfile (reverses EncryptProfilePacket).
    *
    * Server pipeline: raw struct → CRC32 → DeflatePacket → EncryptProfilePacket
    * Client reverses: decrypt → inflate
    */
  def decryptProfilePacket(data: Array[Byte]): Array[Byte] =
    val len = data.length / 8
    if len < 2 then return data

    val buf = ByteBuffer.wrap(data.clone()).order(ByteOrder.LITTLE_ENDIAN)
    val blocks = Array.fill(len)(buf.getLong())

    // Reverse the per-block encryption (forward pass, same order as encrypt)
    var crypt = 0x659365E7L
    for i <- 0 until len do
      val s3 = blocks(i) + crypt              // undo: data[i] = data[i] - crypt
      val s2 = ror64(s3, 7)                   // undo: data[i] = rol(data[i], 7)
      val orig = rol64(s2 - 0x422437A9L, 25)  // undo: data[i] = ror(data[i], 25) + 0x422437A9
      blocks(i) = orig
      crypt = crypt + orig - 0x422437A9L       // same chain as encrypt

    // Reverse the initial swap
    val swap = blocks(0)
    blocks(0) = blocks(len / 2)
    blocks(len / 2) = swap

    // Write back to byte array
    val out = ByteBuffer.allocate(data.length).order(ByteOrder.LITTLE_ENDIAN)
    for b <- blocks do out.putLong(b)
    // Copy any trailing bytes (< 8) unchanged
    val trailingStart = len * 8
    if trailingStart < data.length then
      System.arraycopy(data, trailingStart, out.array(), trailingStart, data.length - trailingStart)
    out.array()

  /** Decrypt OP_ZoneSpawns / OP_NewSpawn (reverses EncryptZoneSpawnPacket).
    *
    * Server pipeline: spawn structs → DeflatePacket → EncryptZoneSpawnPacket
    * Client reverses: decrypt → inflate
    */
  def decryptZoneSpawnPacket(data: Array[Byte]): Array[Byte] =
    val len = data.length / 8
    if len < 2 then return data

    val buf = ByteBuffer.wrap(data.clone()).order(ByteOrder.LITTLE_ENDIAN)
    val blocks = Array.fill(len)(buf.getLong())

    var crypt = 0x0000L
    for i <- 0 until len do
      val s3 = blocks(i) + crypt              // undo: data[i] = data[i] - crypt
      val s2 = ror64(s3, 14)                  // undo: data[i] = rol(data[i], 14)
      val orig = ror64(s2 - 0x659365E7L, 29)  // undo: data[i] = rol(data[i], 29) + 0x659365E7
      blocks(i) = orig
      crypt = crypt + orig - 0x659365E7L

    // Reverse the initial swap
    val swap = blocks(0)
    blocks(0) = blocks(len / 2)
    blocks(len / 2) = swap

    val out = ByteBuffer.allocate(data.length).order(ByteOrder.LITTLE_ENDIAN)
    for b <- blocks do out.putLong(b)
    val trailingStart = len * 8
    if trailingStart < data.length then
      System.arraycopy(data, trailingStart, out.array(), trailingStart, data.length - trailingStart)
    out.array()

  /** Inflate (zlib decompress) a packet.
    *
    * Server uses standard zlib deflate (not raw deflate), so the data
    * starts with a zlib header (typically 78 XX).
    *
    * @param maxSize maximum output buffer size (default 64KB, PlayerProfile needs ~9KB)
    */
  def inflatePacket(data: Array[Byte], maxSize: Int = 65536): Option[Array[Byte]] =
    try
      val inflater = new Inflater() // zlib format (with header)
      inflater.setInput(data)
      val output = new Array[Byte](maxSize)
      val len = inflater.inflate(output)
      inflater.end()
      if len > 0 then Some(output.take(len))
      else
        println(s"[PacketCrypto] Inflate produced 0 bytes from ${data.length}B input")
        None
    catch
      case e: Exception =>
        println(s"[PacketCrypto] Inflate failed: ${e.getMessage}")
        None

  /** Decrypt and inflate a PlayerProfile packet. */
  def decryptAndInflateProfile(data: Array[Byte]): Option[Array[Byte]] =
    val decrypted = decryptProfilePacket(data)
    inflatePacket(decrypted, maxSize = 16384) // PlayerProfile_Struct is 8460 bytes

  /** Decrypt and inflate a ZoneSpawns/NewSpawn packet. */
  def decryptAndInflateSpawns(data: Array[Byte]): Option[Array[Byte]] =
    val decrypted = decryptZoneSpawnPacket(data)
    inflatePacket(decrypted, maxSize = 131072) // spawns can be large

  /** Decompress CharInventory data.
    * Format: uint16(item_count) + zlib compressed item data
    */
  def inflateInventory(data: Array[Byte]): Option[(Int, Array[Byte])] =
    if data.length < 4 then return None
    val itemCount = (data(0) & 0xFF) | ((data(1) & 0xFF) << 8) // LE uint16
    val compressed = data.drop(2)
    inflatePacket(compressed, maxSize = 262144).map(d => (itemCount, d))

  // 64-bit rotate helpers
  private inline def rol64(x: Long, n: Int): Long = (x << n) | (x >>> (64 - n))
  private inline def ror64(x: Long, n: Int): Long = (x >>> n) | (x << (64 - n))
