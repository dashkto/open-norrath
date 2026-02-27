package opennorrath.network

import scala.collection.mutable

/** Reassembles fragmented old-protocol packets.
  *
  * The EQ old protocol fragments large payloads across multiple UDP packets.
  * Each fragment has a 6-byte header: dwSeq(u16) + dwCurr(u16) + dwTotal(u16).
  * The opcode is only in the first fragment (dwCurr == 0).
  *
  * For sending, large payloads are split: first fragment = 510 bytes of app data
  * (plus opcode), subsequent = 512 bytes each.
  */
class FragmentAssembler:
  private val groups = mutable.Map[Int, FragmentGroup]()

  /** Process an inbound packet. Returns the reassembled packet when all
    * fragments have arrived, or the packet unchanged if not fragmented.
    */
  def process(packet: InboundPacket): Option[InboundPacket] =
    packet.fragment match
      case None => Some(packet)
      case Some(frag) =>
        val group = groups.getOrElseUpdate(frag.seq, FragmentGroup(frag.total, packet.opcode))
        // First fragment carries the opcode
        if frag.current == 0 && packet.opcode != 0 then
          group.opcode = packet.opcode
        group.add(frag.current, packet.payload)
        if group.isComplete then
          groups.remove(frag.seq)
          val assembled = group.assemble()
          Some(InboundPacket(packet.seq, packet.arsp, packet.arq, group.opcode, assembled))
        else
          None

  /** Split a large payload into fragment packets for sending.
    * Returns a sequence of (fragmentHeader, opcodeIfFirst, payloadChunk) tuples.
    */
  def fragment(opcode: Short, payload: Array[Byte], fragSeq: Int): Vector[FragmentData] =
    // First fragment: 510 bytes of app data (opcode is separate)
    // Subsequent: 512 bytes each
    val firstSize = 510
    val restSize = 512
    val total =
      if payload.length <= firstSize then 1
      else 1 + ((payload.length - firstSize + restSize - 1) / restSize)

    val result = Vector.newBuilder[FragmentData]
    var offset = 0
    for i <- 0 until total do
      val chunkSize =
        if i == 0 then Math.min(firstSize, payload.length)
        else Math.min(restSize, payload.length - offset)
      val chunk = new Array[Byte](chunkSize)
      System.arraycopy(payload, offset, chunk, 0, chunkSize)
      offset += chunkSize
      result += FragmentData(
        fragSeq = fragSeq,
        current = i,
        total = total,
        opcode = if i == 0 then opcode else 0.toShort,
        payload = chunk,
      )
    result.result()

/** Data for one fragment to be encoded into a packet. */
case class FragmentData(
  fragSeq: Int,
  current: Int,
  total: Int,
  opcode: Short,
  payload: Array[Byte],
)

private class FragmentGroup(val total: Int, var opcode: Short):
  private val fragments = new Array[Array[Byte]](total)
  private var count = 0

  def add(index: Int, data: Array[Byte]): Unit =
    if index < total && fragments(index) == null then
      fragments(index) = data
      count += 1

  def isComplete: Boolean = count >= total

  def assemble(): Array[Byte] =
    val totalSize = fragments.map(f => if f != null then f.length else 0).sum
    val result = new Array[Byte](totalSize)
    var offset = 0
    for f <- fragments if f != null do
      System.arraycopy(f, 0, result, offset, f.length)
      offset += f.length
    result
