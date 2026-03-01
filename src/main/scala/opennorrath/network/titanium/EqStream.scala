package opennorrath.network.titanium

import java.nio.{ByteBuffer, ByteOrder}
import java.util.zip.{Deflater, Inflater}

import opennorrath.network.{FragmentInfo, InboundPacket}

/** EQEmu "Reliable Stream" session protocol (Titanium wire format).
  *
  * This is fundamentally different from the Mac "OldPacket" protocol:
  * - Session negotiation handshake (SessionRequest/SessionResponse)
  * - 2-byte protocol header: [0x00][opcode]
  * - Sequences and ACKs at the session layer (not per-app-packet)
  * - CRC32-with-key, optional XOR encoding, optional zlib compression
  * - App opcodes are little-endian (Mac uses big-endian)
  *
  * Reference: EQEmu/common/net/reliable_stream_connection.cpp
  *            EQEmu/common/net/reliable_stream_structs.h
  */
object EqStream:

  // =========================================================================
  // Protocol opcodes (first byte is always 0x00)
  // =========================================================================

  val OP_SessionRequest: Byte    = 0x01
  val OP_SessionResponse: Byte   = 0x02
  val OP_Combined: Byte          = 0x03
  val OP_SessionDisconnect: Byte = 0x05
  val OP_KeepAlive: Byte         = 0x06
  val OP_SessionStatRequest: Byte  = 0x07
  val OP_SessionStatResponse: Byte = 0x08
  val OP_Packet: Byte            = 0x09
  val OP_Fragment: Byte          = 0x0d
  val OP_OutOfOrderAck: Byte     = 0x11
  val OP_Ack: Byte               = 0x15
  val OP_AppCombined: Byte       = 0x19

  // Encode pass types from SessionResponse
  val EncodeNone: Byte        = 0
  val EncodeCompression: Byte = 1
  val EncodeXOR: Byte         = 4

  val ProtocolVersion: Int = 3
  val DefaultMaxPacketSize: Int = 512

  // =========================================================================
  // Session negotiation
  // =========================================================================

  /** Build OP_SessionRequest (14 bytes). All multi-byte fields are big-endian on wire.
    * @param connectCode random uint32 to identify this session
    * @param maxPacketSize typically 512
    */
  def encodeSessionRequest(connectCode: Int, maxPacketSize: Int = DefaultMaxPacketSize): Array[Byte] =
    val buf = ByteBuffer.allocate(14).order(ByteOrder.BIG_ENDIAN)
    buf.put(0: Byte)               // zero
    buf.put(OP_SessionRequest)     // opcode
    buf.putInt(ProtocolVersion)    // protocol_version (BE)
    buf.putInt(connectCode)        // connect_code (BE)
    buf.putInt(maxPacketSize)      // max_packet_size (BE)
    buf.array()

  /** Parsed OP_SessionResponse from server. */
  case class SessionParams(
    connectCode: Int,
    encodeKey: Int,
    crcBytes: Int,        // 0, 2, or 4
    encodePass1: Int,     // 0=None, 1=Compression, 4=XOR
    encodePass2: Int,
    maxPacketSize: Int,
  )

  /** Parse OP_SessionResponse (17 bytes). All multi-byte fields are big-endian. */
  def decodeSessionResponse(data: Array[Byte]): Option[SessionParams] =
    if data.length < 17 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.BIG_ENDIAN)
    val zero = buf.get()     // 0x00
    val opcode = buf.get()   // 0x02
    if zero != 0 || opcode != OP_SessionResponse then return None
    Some(SessionParams(
      connectCode = buf.getInt,
      encodeKey = buf.getInt,
      crcBytes = buf.get() & 0xFF,
      encodePass1 = buf.get() & 0xFF,
      encodePass2 = buf.get() & 0xFF,
      maxPacketSize = buf.getInt,
    ))

  // =========================================================================
  // Control packets
  // =========================================================================

  /** Build OP_SessionDisconnect (6 bytes). */
  def encodeDisconnect(connectCode: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(6).order(ByteOrder.BIG_ENDIAN)
    buf.put(0: Byte)
    buf.put(OP_SessionDisconnect)
    buf.putInt(connectCode)
    buf.array()

  /** Build OP_KeepAlive (2 bytes). */
  def encodeKeepAlive(): Array[Byte] = Array[Byte](0, OP_KeepAlive)

  /** Build OP_Ack for stream 0 (4 bytes). Sequence is big-endian. */
  def encodeAck(sequence: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
    buf.put(0: Byte)
    buf.put(OP_Ack)
    buf.putShort((sequence & 0xFFFF).toShort)
    buf.array()

  /** Build OP_OutOfOrderAck for stream 0 (4 bytes). */
  def encodeOutOfOrderAck(sequence: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
    buf.put(0: Byte)
    buf.put(OP_OutOfOrderAck)
    buf.putShort((sequence & 0xFFFF).toShort)
    buf.array()

  /** Build OP_SessionStatResponse from a SessionStatRequest. */
  def encodeSessionStatResponse(requestData: Array[Byte], sentPackets: Long, recvPackets: Long): Array[Byte] =
    if requestData.length < 40 then return Array.emptyByteArray
    val buf = ByteBuffer.allocate(40).order(ByteOrder.BIG_ENDIAN)
    buf.put(0: Byte)
    buf.put(OP_SessionStatResponse)
    // Echo timestamp from request (offset 2-3)
    buf.putShort(ByteBuffer.wrap(requestData, 2, 2).order(ByteOrder.BIG_ENDIAN).getShort)
    // Our timestamp
    buf.putInt((System.currentTimeMillis() & 0xFFFFFFFFL).toInt)
    // Echo client_sent (offset 24-31) and client_recv (offset 32-39)
    buf.put(requestData, 24, 8)
    buf.put(requestData, 32, 8)
    // Server sent/recv — not needed for client, fill with zeros
    buf.putLong(sentPackets)
    buf.putLong(recvPackets)
    buf.array()

  // =========================================================================
  // Reliable data packets (OP_Packet / OP_Fragment)
  // =========================================================================

  /** Wrap an application packet in OP_Packet for stream 0.
    * App opcodes are 2-byte little-endian, prepended to the payload.
    * If the first byte happens to be 0x00 (collision with protocol header),
    * prepend an extra 0x00 padding byte.
    *
    * @param appOpcode application-level opcode (little-endian)
    * @param appPayload raw application payload
    * @param sequence outbound sequence number
    * @return the OP_Packet bytes (before encoding/CRC)
    */
  def encodePacket(appOpcode: Short, appPayload: Array[Byte], sequence: Int): Array[Byte] =
    // Build the inner app data: [2-byte LE opcode][payload]
    val appLen = 2 + appPayload.length
    val needsPadding = (appOpcode & 0xFF) == 0  // first byte on wire would be 0x00
    val innerLen = if needsPadding then appLen + 1 else appLen
    val inner = ByteBuffer.allocate(innerLen).order(ByteOrder.LITTLE_ENDIAN)
    if needsPadding then inner.put(0: Byte) // OP_Padding escape
    inner.putShort(appOpcode)
    if appPayload.nonEmpty then inner.put(appPayload)

    // Wrap in OP_Packet header: [0x00][0x09][sequence BE]
    val result = ByteBuffer.allocate(4 + innerLen).order(ByteOrder.BIG_ENDIAN)
    result.put(0: Byte)
    result.put(OP_Packet)
    result.putShort((sequence & 0xFFFF).toShort)
    result.put(inner.array())
    result.array()

  /** Fragment a large application packet into OP_Fragment packets for stream 0.
    * @param appOpcode application-level opcode
    * @param appPayload raw application payload
    * @param startSequence first sequence number to use
    * @param maxPacketSize max UDP packet size (typically 512)
    * @param crcBytes number of CRC bytes (typically 2)
    * @return list of (raw OP_Fragment packet bytes, sequence) pairs
    */
  def encodeFragments(
    appOpcode: Short,
    appPayload: Array[Byte],
    startSequence: Int,
    maxPacketSize: Int,
    crcBytes: Int,
  ): Vector[(Array[Byte], Int)] =
    // Build inner app data: [2-byte LE opcode][payload], with padding if needed
    val needsPadding = (appOpcode & 0xFF) == 0
    val innerLen = (if needsPadding then 1 else 0) + 2 + appPayload.length
    val innerBuf = ByteBuffer.allocate(innerLen).order(ByteOrder.LITTLE_ENDIAN)
    if needsPadding then innerBuf.put(0: Byte)
    innerBuf.putShort(appOpcode)
    if appPayload.nonEmpty then innerBuf.put(appPayload)
    val inner = innerBuf.array()

    val totalSize = inner.length
    // First fragment: 8-byte header (0x00, 0x0d, seq[2], total_size[4]) + compress flag room
    val firstDataSize = maxPacketSize - crcBytes - 8 - 1
    // Subsequent fragments: 4-byte header + compress flag room
    val laterDataSize = maxPacketSize - crcBytes - 4 - 1

    val fragments = Vector.newBuilder[(Array[Byte], Int)]
    var offset = 0
    var seq = startSequence

    // First fragment
    val firstChunkSize = Math.min(firstDataSize, totalSize)
    val firstBuf = ByteBuffer.allocate(8 + firstChunkSize).order(ByteOrder.BIG_ENDIAN)
    firstBuf.put(0: Byte)
    firstBuf.put(OP_Fragment)
    firstBuf.putShort((seq & 0xFFFF).toShort)
    firstBuf.putInt(totalSize)  // total_size (BE)
    firstBuf.put(inner, 0, firstChunkSize)
    fragments += ((firstBuf.array(), seq))
    offset += firstChunkSize
    seq = (seq + 1) & 0xFFFF

    // Subsequent fragments
    while offset < totalSize do
      val chunkSize = Math.min(laterDataSize, totalSize - offset)
      val fragBuf = ByteBuffer.allocate(4 + chunkSize).order(ByteOrder.BIG_ENDIAN)
      fragBuf.put(0: Byte)
      fragBuf.put(OP_Fragment)
      fragBuf.putShort((seq & 0xFFFF).toShort)
      fragBuf.put(inner, offset, chunkSize)
      fragments += ((fragBuf.array(), seq))
      offset += chunkSize
      seq = (seq + 1) & 0xFFFF

    fragments.result()

  // =========================================================================
  // Decoding incoming packets
  // =========================================================================

  /** Identify the protocol opcode of a raw UDP packet.
    * Returns None if the packet is a raw app packet (first byte != 0x00).
    */
  def protocolOpcode(data: Array[Byte]): Option[Byte] =
    if data.length >= 2 && data(0) == 0 then Some(data(1))
    else None

  /** Check if a packet is a protocol-level packet (first byte == 0x00). */
  def isProtocolPacket(data: Array[Byte]): Boolean =
    data.length >= 2 && data(0) == 0

  /** Extract the sequence number from an OP_Packet or OP_Fragment (offset 2-3, BE). */
  def readSequence(data: Array[Byte]): Int =
    if data.length < 4 then return -1
    ((data(2) & 0xFF) << 8) | (data(3) & 0xFF)

  /** Extract the total_size from the first OP_Fragment (offset 4-7, BE). */
  def readFragmentTotalSize(data: Array[Byte]): Int =
    if data.length < 8 then return -1
    ByteBuffer.wrap(data, 4, 4).order(ByteOrder.BIG_ENDIAN).getInt

  /** Extract the app-level data from an OP_Packet (after 4-byte header).
    * Returns (appOpcode, appPayload) or None if too short.
    * Handles the 0x00 padding escape: if first byte after header is 0x00,
    * it's a padding byte and the real app data starts at the next byte.
    */
  def decodeAppData(data: Array[Byte], headerSize: Int): Option[InboundPacket] =
    if data.length <= headerSize then return None
    var offset = headerSize
    // Check for OP_Padding escape (0x00 prefix on app data whose opcode starts with 0x00)
    if data(offset) == 0 then offset += 1
    if data.length - offset < 2 then return None
    // App opcode is 2-byte little-endian
    val opcode = ((data(offset) & 0xFF) | ((data(offset + 1) & 0xFF) << 8)).toShort
    offset += 2
    val payload = if offset < data.length then
      val buf = new Array[Byte](data.length - offset)
      System.arraycopy(data, offset, buf, 0, buf.length)
      buf
    else Array.emptyByteArray
    val seq = readSequence(data)
    Some(InboundPacket(seq = seq, arsp = None, arq = None, opcode = opcode, payload = payload))

  /** Unpack an OP_Combined packet into its sub-packets.
    * Each sub-packet is prefixed by a 1-byte length.
    */
  def decodeCombined(data: Array[Byte]): Vector[Array[Byte]] =
    val result = Vector.newBuilder[Array[Byte]]
    var offset = 2 // skip protocol header [0x00, 0x03]
    while offset < data.length do
      val len = data(offset) & 0xFF
      offset += 1
      if offset + len <= data.length then
        val sub = new Array[Byte](len)
        System.arraycopy(data, offset, sub, 0, len)
        result += sub
        offset += len
      else
        offset = data.length // malformed, stop
    result.result()

  /** Unpack an OP_AppCombined packet into its sub-packets.
    * Each sub-packet uses a variable-length size prefix:
    * - 0-254: 1 byte
    * - 255-65534: 0xFF + 2 bytes BE
    * - 65535+: 0xFF 0xFF 0xFF + 4 bytes BE
    */
  def decodeAppCombined(data: Array[Byte]): Vector[Array[Byte]] =
    val result = Vector.newBuilder[Array[Byte]]
    var offset = 2 // skip protocol header
    while offset < data.length do
      val first = data(offset) & 0xFF
      offset += 1
      val len =
        if first < 0xFF then first
        else if offset + 2 <= data.length && (data(offset) & 0xFF) != 0xFF then
          val v = ((data(offset) & 0xFF) << 8) | (data(offset + 1) & 0xFF)
          offset += 2
          v
        else
          offset += 2 // skip 0xFF 0xFF
          if offset + 4 <= data.length then
            val v = ByteBuffer.wrap(data, offset, 4).order(ByteOrder.BIG_ENDIAN).getInt
            offset += 4
            v
          else
            offset = data.length; 0
      if len > 0 && offset + len <= data.length then
        val sub = new Array[Byte](len)
        System.arraycopy(data, offset, sub, 0, len)
        result += sub
        offset += len
      else if len > 0 then
        offset = data.length // malformed
    result.result()

  // =========================================================================
  // CRC with encode key
  // =========================================================================

  /** CRC32 with encode key, matching EQEmu's CRC32::Update(data, len, key).
    * Standard Ethernet CRC32 polynomial (0xEDB88320 reflected).
    * Key bytes are folded in first (little-endian order), then data bytes.
    */
  def crc32WithKey(data: Array[Byte], offset: Int, length: Int, key: Int): Int =
    var crc = 0xFFFFFFFF
    // Fold in the 4 key bytes (little-endian order)
    for i <- 0 until 4 do
      val keyByte = (key >>> (i * 8)) & 0xFF
      crc = (crc >>> 8) ^ Crc32Table((crc ^ keyByte) & 0xFF)
    // Fold in the data bytes
    for i <- offset until offset + length do
      crc = (crc >>> 8) ^ Crc32Table((crc ^ (data(i) & 0xFF)) & 0xFF)
    ~crc

  /** Validate the CRC at the end of an encoded packet.
    * @return true if CRC matches (or crcBytes == 0)
    */
  def validateCrc(data: Array[Byte], crcBytes: Int, key: Int): Boolean =
    if crcBytes == 0 then return true
    if data.length <= crcBytes then return false
    val dataLen = data.length - crcBytes
    val computed = crc32WithKey(data, 0, dataLen, key)
    if crcBytes == 2 then
      val expected = ((data(dataLen) & 0xFF) << 8) | (data(dataLen + 1) & 0xFF) // BE
      (computed & 0xFFFF) == expected
    else // crcBytes == 4
      val expected = ByteBuffer.wrap(data, dataLen, 4).order(ByteOrder.BIG_ENDIAN).getInt
      computed == expected

  /** Append CRC to a packet. Returns a new array with CRC appended (big-endian). */
  def appendCrc(data: Array[Byte], crcBytes: Int, key: Int): Array[Byte] =
    if crcBytes == 0 then return data
    val crc = crc32WithKey(data, 0, data.length, key)
    val result = new Array[Byte](data.length + crcBytes)
    System.arraycopy(data, 0, result, 0, data.length)
    if crcBytes == 2 then
      result(data.length) = ((crc >>> 8) & 0xFF).toByte   // BE high byte
      result(data.length + 1) = (crc & 0xFF).toByte       // BE low byte
    else
      ByteBuffer.wrap(result, data.length, 4).order(ByteOrder.BIG_ENDIAN).putInt(crc)
    result

  // =========================================================================
  // XOR encode/decode
  // =========================================================================

  /** XOR decode (CBC-like). Key is updated to the ciphertext before XOR.
    * Modifies the buffer in place.
    * @param data buffer to decode
    * @param offset start of region to decode
    * @param length length of region to decode
    * @param key initial XOR key (encode_key from session)
    */
  def xorDecode(data: Array[Byte], offset: Int, length: Int, key: Int): Unit =
    var k = key
    var i = offset
    val end4 = offset + (length / 4) * 4
    while i < end4 do
      // Read 4-byte ciphertext block (native/LE order)
      val ct = (data(i) & 0xFF) | ((data(i + 1) & 0xFF) << 8) |
               ((data(i + 2) & 0xFF) << 16) | ((data(i + 3) & 0xFF) << 24)
      val pt = ct ^ k
      k = ct // key becomes ciphertext
      data(i) = (pt & 0xFF).toByte
      data(i + 1) = ((pt >>> 8) & 0xFF).toByte
      data(i + 2) = ((pt >>> 16) & 0xFF).toByte
      data(i + 3) = ((pt >>> 24) & 0xFF).toByte
      i += 4
    // Remaining bytes: XOR with lowest byte of key
    val kc = (k & 0xFF).toByte
    while i < offset + length do
      data(i) = (data(i) ^ kc).toByte
      i += 1

  /** XOR encode (CBC-like). Key is updated to the ciphertext after XOR.
    * Modifies the buffer in place.
    */
  def xorEncode(data: Array[Byte], offset: Int, length: Int, key: Int): Unit =
    var k = key
    var i = offset
    val end4 = offset + (length / 4) * 4
    while i < end4 do
      val pt = (data(i) & 0xFF) | ((data(i + 1) & 0xFF) << 8) |
               ((data(i + 2) & 0xFF) << 16) | ((data(i + 3) & 0xFF) << 24)
      val ct = pt ^ k
      k = ct // key becomes ciphertext
      data(i) = (ct & 0xFF).toByte
      data(i + 1) = ((ct >>> 8) & 0xFF).toByte
      data(i + 2) = ((ct >>> 16) & 0xFF).toByte
      data(i + 3) = ((ct >>> 24) & 0xFF).toByte
      i += 4
    val kc = (k & 0xFF).toByte
    while i < offset + length do
      data(i) = (data(i) ^ kc).toByte
      i += 1

  // =========================================================================
  // Compression
  // =========================================================================

  private val CompressFlag: Byte = 0x5a  // 'Z' — data is zlib compressed
  private val NoCompressFlag: Byte = 0xa5.toByte // data is raw (flag stripped)

  /** Decompress a payload that has a 1-byte compression flag prefix.
    * Returns the decompressed data (flag stripped).
    */
  def decompress(data: Array[Byte], offset: Int, length: Int): Array[Byte] =
    if length < 1 then return Array.emptyByteArray
    val flag = data(offset)
    if flag == CompressFlag then
      val inflater = Inflater()
      try
        inflater.setInput(data, offset + 1, length - 1)
        val out = new Array[Byte](65536)
        val n = inflater.inflate(out)
        val result = new Array[Byte](n)
        System.arraycopy(out, 0, result, 0, n)
        result
      finally inflater.end()
    else
      // No compression — just strip the flag byte
      val result = new Array[Byte](length - 1)
      System.arraycopy(data, offset + 1, result, 0, length - 1)
      result

  /** Compress a payload, prepending the compression flag.
    * Only compresses if the result is smaller; otherwise prepends 0xA5 + raw data.
    */
  def compress(data: Array[Byte]): Array[Byte] =
    if data.length <= 30 then
      // Too small to bother compressing
      val result = new Array[Byte](1 + data.length)
      result(0) = NoCompressFlag
      System.arraycopy(data, 0, result, 1, data.length)
      return result

    val deflater = Deflater(Deflater.BEST_SPEED)
    try
      deflater.setInput(data)
      deflater.finish()
      val out = new Array[Byte](data.length + 64)
      val n = deflater.deflate(out)
      if n > 0 && n < data.length then
        val result = new Array[Byte](1 + n)
        result(0) = CompressFlag
        System.arraycopy(out, 0, result, 1, n)
        result
      else
        val result = new Array[Byte](1 + data.length)
        result(0) = NoCompressFlag
        System.arraycopy(data, 0, result, 1, data.length)
        result
    finally deflater.end()

  // =========================================================================
  // Full encode/decode pipelines
  // =========================================================================

  /** Determine the decode region offset for a packet.
    * Protocol packets (first byte 0x00): decode starts at offset 2 (skip header).
    * App packets (first byte != 0x00): decode starts at offset 1.
    */
  private def decodeRegionOffset(data: Array[Byte]): Int =
    if data(0) == 0 then 2 else 1

  /** Whether a packet can be encoded (CRC/XOR/compress applied).
    * SessionRequest, SessionResponse, and OutOfSession are NOT encoded.
    */
  def packetCanBeEncoded(data: Array[Byte]): Boolean =
    if data.length < 2 then return false
    if data(0) != 0 then return true // app packet — always encodable
    val op = data(1)
    op != OP_SessionRequest && op != OP_SessionResponse && op != 0x1d // OP_OutOfSession

  /** Full decode pipeline for an incoming packet:
    * 1. Validate CRC (if encodable)
    * 2. Strip CRC
    * 3. Apply decode passes in reverse order (pass2 then pass1)
    * @return decoded packet data, or None if CRC failed
    */
  /** Full decode pipeline for an incoming packet:
    * 1. Validate CRC (if encodable)
    * 2. Strip CRC
    * 3. Apply decode passes in reverse order (pass2 then pass1)
    *
    * The EQEmu server applies compression to the "region" of each UDP packet:
    * - Protocol packets (first byte 0x00): region starts at offset 2 (after [0x00, opcode])
    * - App packets (first byte != 0x00): region starts at offset 1
    * Compression prepends a 1-byte flag: 0x5a=zlib, 0xa5=raw. The flag covers
    * everything after the protocol header, including sequence numbers, so it MUST
    * be handled before parsing sequences or app data.
    *
    * @return decoded packet data, or None if CRC failed
    */
  def decodePacket(
    raw: Array[Byte],
    params: SessionParams,
  ): Option[Array[Byte]] =
    if !packetCanBeEncoded(raw) then return Some(raw)

    // Validate CRC
    if !validateCrc(raw, params.crcBytes, params.encodeKey) then
      println(s"[EqStream] CRC validation failed (len=${raw.length})")
      return None

    // Strip CRC
    var data = new Array[Byte](raw.length - params.crcBytes)
    System.arraycopy(raw, 0, data, 0, data.length)

    val regionOffset = decodeRegionOffset(data)
    if data.length - regionOffset <= 0 then return Some(data)

    // Decode passes in reverse order (pass2 first, then pass1).
    // XOR is in-place; compression changes the buffer, so it produces a new array.
    for pass <- Array(params.encodePass2, params.encodePass1) do
      pass match
        case p if p == EncodeXOR =>
          xorDecode(data, regionOffset, data.length - regionOffset, params.encodeKey)
        case p if p == EncodeCompression =>
          data = decompressRegion(data, regionOffset)
        case _ => ()

    Some(data)

  /** Strip the compress flag from a region and decompress if needed.
    * Returns a new array with the header preserved and the region decompressed.
    * Compress flag values: 0x5a = zlib compressed, 0xa5 = raw (flag only).
    * If the flag is neither, the data is returned unchanged (matches EQEmu behavior).
    */
  private def decompressRegion(data: Array[Byte], regionOffset: Int): Array[Byte] =
    val regionLen = data.length - regionOffset
    if regionLen < 1 then return data
    val flag = data(regionOffset)
    if flag == CompressFlag then
      // Zlib compressed: inflate the data after the flag byte
      val inflater = Inflater()
      try
        inflater.setInput(data, regionOffset + 1, regionLen - 1)
        val out = new Array[Byte](65536)
        val n = inflater.inflate(out)
        val result = new Array[Byte](regionOffset + n)
        System.arraycopy(data, 0, result, 0, regionOffset)
        System.arraycopy(out, 0, result, regionOffset, n)
        result
      finally inflater.end()
    else if flag == NoCompressFlag then
      // Not compressed: strip the flag byte, keep the rest
      val result = new Array[Byte](data.length - 1)
      System.arraycopy(data, 0, result, 0, regionOffset)
      System.arraycopy(data, regionOffset + 1, result, regionOffset, regionLen - 1)
      result
    else
      // Unknown flag — leave data as-is (no compress flag to strip)
      data

  /** Insert a compress flag (0xa5 = not compressed) into the encode region.
    * The EQEmu server expects the flag when compression is configured, even
    * if we don't actually compress. Returns a new array with the flag inserted.
    */
  private def compressRegion(data: Array[Byte], regionOffset: Int): Array[Byte] =
    val result = new Array[Byte](data.length + 1)
    System.arraycopy(data, 0, result, 0, regionOffset)
    result(regionOffset) = NoCompressFlag
    System.arraycopy(data, regionOffset, result, regionOffset + 1, data.length - regionOffset)
    result

  /** Full encode pipeline for an outgoing packet:
    * 1. Apply encode passes in forward order (pass1 then pass2)
    * 2. Append CRC
    * @return encoded packet data ready to send on wire
    */
  def encodeForWire(
    data: Array[Byte],
    params: SessionParams,
  ): Array[Byte] =
    if !packetCanBeEncoded(data) then return data

    var encoded = data.clone()
    val regionOffset = decodeRegionOffset(encoded)

    // Encode passes in forward order (pass1 then pass2)
    for pass <- Array(params.encodePass1, params.encodePass2) do
      pass match
        case p if p == EncodeCompression =>
          encoded = compressRegion(encoded, regionOffset)
        case p if p == EncodeXOR =>
          xorEncode(encoded, regionOffset, encoded.length - regionOffset, params.encodeKey)
        case _ => ()

    // Append CRC
    appendCrc(encoded, params.crcBytes, params.encodeKey)

  // =========================================================================
  // Sequence comparison (handles wraparound)
  // =========================================================================

  /** Compare two sequence numbers with wraparound.
    * Returns negative if seq1 is past, 0 if equal, positive if seq1 is future.
    */
  def compareSequence(seq1: Int, seq2: Int): Int =
    val diff = (seq1 - seq2).toShort.toInt // wrap to signed 16-bit
    if diff == 0 then 0
    else if diff > 0 && diff <= 10000 then 1   // future
    else if diff < 0 && diff >= -10000 then -1  // past
    else if diff > 10000 then -1                 // wrapped past
    else 1                                       // wrapped future

  // =========================================================================
  // CRC32 lookup table (standard Ethernet polynomial 0xEDB88320, reflected)
  // =========================================================================

  private val Crc32Table: Array[Int] = {
    val table = new Array[Int](256)
    for i <- 0 until 256 do
      var crc = i
      for _ <- 0 until 8 do
        if (crc & 1) != 0 then crc = (crc >>> 1) ^ 0xEDB88320
        else crc = crc >>> 1
      table(i) = crc
    table
  }
