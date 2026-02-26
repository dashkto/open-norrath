package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}

/** Fragment header from a fragmented old-protocol packet. */
case class FragmentInfo(seq: Int, current: Int, total: Int)

/** Decoded inbound old-protocol packet. */
case class InboundPacket(
  seq: Int,
  arsp: Option[Int],
  arq: Option[Int],
  opcode: Short,
  payload: Array[Byte],
  fragment: Option[FragmentInfo] = None,
)

/** EQ "old protocol" packet encode/decode.
  *
  * Wire format:
  *   [HDR 2B][SEQ 2B][ARSP? 2B][ARQ? 2B][frag? 6B][ASQ? 1-2B][opcode 2B][payload][CRC32 4B]
  *
  * HDR is a 2-byte bitfield: byte 0 = a-flags, byte 1 = b-flags.
  * All 16-bit fields are big-endian on the wire (server uses ntohs/htons).
  * CRC32 is standard Ethernet CRC32, written big-endian.
  *
  * Reference: EQMacDocker/Server/common/eq_packet.cpp DecodePacket/ReturnPacket
  */
object OldPacket:

  val MinPacketLen = 10

  // HDR byte 0 bit positions
  private val A1_ARQ      = 1
  private val A2_CLOSING  = 2
  private val A3_FRAGMENT = 3
  private val A4_ASQ      = 4
  private val A5_SEQSTART = 5
  private val A6_CLOSING  = 6

  // HDR byte 1 bit positions
  private val B2_ARSP = 2
  private val B3_UNKNOWN = 3

  private def bit(byte: Int, pos: Int): Boolean = ((byte >> pos) & 1) != 0
  private def setBit(byte: Int, pos: Int): Int = byte | (1 << pos)

  /** Decode a raw UDP datagram. */
  def decode(data: Array[Byte], length: Int): Option[InboundPacket] =
    if length < MinPacketLen then return None

    val hdr0 = data(0) & 0xFF
    val hdr1 = data(1) & 0xFF

    // Validate CRC32 (last 4 bytes)
    val computedCrc = EqCrc32.compute(data, 0, length - 4)
    val storedCrc = ByteBuffer.wrap(data, length - 4, 4).order(ByteOrder.BIG_ENDIAN).getInt
    if computedCrc != storedCrc then
      // Log but don't reject — server doesn't validate incoming CRC either
      println(f"[OldPacket] CRC mismatch: computed=0x$computedCrc%08x stored=0x$storedCrc%08x")

    var offset = 2
    val seq = readShort(data, offset); offset += 2

    // b2_ARSP
    val arsp =
      if bit(hdr1, B2_ARSP) then
        val v = readShort(data, offset); offset += 2; Some(v)
      else None

    // b3_Unknown (resend request)
    if bit(hdr1, B3_UNKNOWN) then offset += 2

    // b4-b7 ack bitfield size
    val b4size = ((hdr1 >> 4) & 0xF)
    if b4size > 0 then offset += b4size

    // a1_ARQ
    val arq =
      if bit(hdr0, A1_ARQ) then
        if offset + 2 > length then return None
        val v = readShort(data, offset); offset += 2; Some(v)
      else None

    // a3_Fragment
    val fragInfo =
      if bit(hdr0, A3_FRAGMENT) then
        if offset + 6 > length then return None
        val fragSeq = readShort(data, offset); offset += 2
        val fragCurr = readShort(data, offset); offset += 2
        val fragTotal = readShort(data, offset); offset += 2
        Some(FragmentInfo(fragSeq, fragCurr, fragTotal))
      else None

    // a4_ASQ
    if bit(hdr0, A4_ASQ) then
      if bit(hdr0, A1_ARQ) then offset += 2
      else offset += 1

    // Opcode and payload
    val dataEnd = length - 4 // exclude CRC32
    if !(bit(hdr0, A2_CLOSING) && bit(hdr0, A6_CLOSING)) then
      // Opcode only present if: not a fragment, or first fragment (current == 0)
      val hasOpcode = fragInfo.forall(_.current == 0)
      val opcode =
        if hasOpcode && offset + 2 <= dataEnd then
          val op = readShortRaw(data, offset); offset += 2; op
        else 0.toShort
      val payloadLen = dataEnd - offset
      val payload =
        if payloadLen > 0 then
          val buf = new Array[Byte](payloadLen)
          System.arraycopy(data, offset, buf, 0, payloadLen)
          buf
        else Array.emptyByteArray
      Some(InboundPacket(seq, arsp, arq, opcode, payload, fragInfo))
    else
      // Pure ACK — no opcode
      Some(InboundPacket(seq, arsp, arq, 0, Array.emptyByteArray))

  /** Encode an outgoing packet with opcode and payload. */
  def encode(
    opcode: Short,
    payload: Array[Byte],
    seq: Int,
    arq: Option[Int] = None,
    arsp: Option[Int] = None,
    asqHigh: Byte = 0,
    asqLow: Byte = 0,
    includeAsq: Boolean = false,
    seqStart: Boolean = false,
  ): Array[Byte] =
    val maxSize = 2 + 2 + 2 + 2 + 2 + 2 + payload.length + 4 + 4 // generous
    val buf = ByteBuffer.allocate(maxSize).order(ByteOrder.BIG_ENDIAN)
    var hdr0 = 0
    var hdr1 = 0

    // Reserve HDR (2 bytes) — written last
    buf.position(2)

    // SEQ
    buf.putShort((seq & 0xFFFF).toShort)

    // ARSP
    arsp.foreach { v =>
      hdr1 = setBit(hdr1, B2_ARSP)
      buf.putShort((v & 0xFFFF).toShort)
    }

    // ARQ
    arq.foreach { v =>
      hdr0 = setBit(hdr0, A1_ARQ)
      buf.putShort((v & 0xFFFF).toShort)
    }

    // SEQStart (must be set on first packet to initialize server sequence tracking)
    if seqStart then
      hdr0 = setBit(hdr0, A5_SEQSTART)

    // ASQ
    if includeAsq then
      hdr0 = setBit(hdr0, A4_ASQ)
      if arq.isDefined then
        buf.put(asqHigh)
        buf.put(asqLow)
      else
        buf.put(asqHigh)

    // Opcode
    if opcode != 0 then
      buf.putShort(opcode)

    // Payload
    if payload.nonEmpty then
      buf.put(payload)

    // Write HDR at position 0
    val dataEnd = buf.position()
    buf.put(0, hdr0.toByte)
    buf.put(1, hdr1.toByte)

    // Compute and append CRC32
    val bytes = new Array[Byte](dataEnd)
    buf.position(0)
    buf.get(bytes)
    val crc = EqCrc32.compute(bytes, 0, dataEnd)

    val result = new Array[Byte](dataEnd + 4)
    System.arraycopy(bytes, 0, result, 0, dataEnd)
    ByteBuffer.wrap(result, dataEnd, 4).order(ByteOrder.BIG_ENDIAN).putInt(crc)
    result

  /** Encode a fragmented packet. */
  def encodeFragment(
    frag: FragmentData,
    seq: Int,
    arq: Option[Int] = None,
    arsp: Option[Int] = None,
    includeAsq: Boolean = false,
    seqStart: Boolean = false,
  ): Array[Byte] =
    val maxSize = 2 + 2 + 2 + 2 + 6 + 2 + 2 + frag.payload.length + 4 + 4
    val buf = ByteBuffer.allocate(maxSize).order(ByteOrder.BIG_ENDIAN)
    var hdr0 = 0
    var hdr1 = 0

    hdr0 = setBit(hdr0, A3_FRAGMENT)
    buf.position(2) // reserve HDR

    // SEQ
    buf.putShort((seq & 0xFFFF).toShort)

    // ARSP
    arsp.foreach { v =>
      hdr1 = setBit(hdr1, B2_ARSP)
      buf.putShort((v & 0xFFFF).toShort)
    }

    // ARQ
    arq.foreach { v =>
      hdr0 = setBit(hdr0, A1_ARQ)
      buf.putShort((v & 0xFFFF).toShort)
    }

    if seqStart then hdr0 = setBit(hdr0, A5_SEQSTART)

    // Fragment header: seq, current, total
    buf.putShort((frag.fragSeq & 0xFFFF).toShort)
    buf.putShort((frag.current & 0xFFFF).toShort)
    buf.putShort((frag.total & 0xFFFF).toShort)

    // ASQ
    if includeAsq then
      hdr0 = setBit(hdr0, A4_ASQ)
      if arq.isDefined then buf.putShort(0.toShort)
      else buf.put(0.toByte)

    // Opcode (only first fragment)
    if frag.current == 0 && frag.opcode != 0 then
      buf.putShort(frag.opcode)

    // Payload
    if frag.payload.nonEmpty then buf.put(frag.payload)

    // Write HDR
    val dataEnd = buf.position()
    buf.put(0, hdr0.toByte)
    buf.put(1, hdr1.toByte)

    // CRC32
    val bytes = new Array[Byte](dataEnd)
    buf.position(0)
    buf.get(bytes)
    val crc = EqCrc32.compute(bytes, 0, dataEnd)

    val result = new Array[Byte](dataEnd + 4)
    System.arraycopy(bytes, 0, result, 0, dataEnd)
    ByteBuffer.wrap(result, dataEnd, 4).order(ByteOrder.BIG_ENDIAN).putInt(crc)
    result

  /** Encode a pure ACK (ARSP only, no opcode or payload). */
  def encodeAck(seq: Int, arsp: Int): Array[Byte] =
    // HDR(2) + SEQ(2) + ARSP(2) + CRC32(4) = 10 bytes
    val buf = ByteBuffer.allocate(10).order(ByteOrder.BIG_ENDIAN)
    var hdr1 = 0
    hdr1 = setBit(hdr1, B2_ARSP)

    buf.put(0.toByte) // hdr0
    buf.put(hdr1.toByte)
    buf.putShort((seq & 0xFFFF).toShort)
    buf.putShort((arsp & 0xFFFF).toShort)

    val bytes = new Array[Byte](6)
    buf.position(0)
    buf.get(bytes)
    val crc = EqCrc32.compute(bytes, 0, 6)

    val result = new Array[Byte](10)
    System.arraycopy(bytes, 0, result, 0, 6)
    ByteBuffer.wrap(result, 6, 4).order(ByteOrder.BIG_ENDIAN).putInt(crc)
    result

  /** Read a 16-bit value in big-endian (network byte order) as unsigned int. */
  private def readShort(data: Array[Byte], offset: Int): Int =
    ((data(offset) & 0xFF) << 8) | (data(offset + 1) & 0xFF)

  /** Read a 16-bit value in big-endian as a Short (for opcodes). */
  private def readShortRaw(data: Array[Byte], offset: Int): Short =
    (((data(offset) & 0xFF) << 8) | (data(offset + 1) & 0xFF)).toShort

  /** Hex dump for debugging. */
  def hexDump(data: Array[Byte], length: Int): String =
    data.take(length).map(b => f"${b & 0xFF}%02x").mkString(" ")
