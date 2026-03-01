package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

/** Abstraction for protocol state machines that run on a NetworkThread.
  * Both LoginClient and WorldClient implement this.
  */
trait PacketHandler:
  /** Error events queue — NetworkThread posts errors here. */
  val errors: ConcurrentLinkedQueue[String]

  /** App-level outgoing packet queue for Titanium transport.
    * Client classes add (opcode, payload) tuples here when in Titanium mode;
    * TitaniumNetworkThread handles framing (OP_Packet/OP_Fragment + sequencing + CRC).
    * Unused in Mac mode — Mac transport uses pollOutgoing() for OldPacket-encoded data.
    */
  val appOutQueue: ConcurrentLinkedQueue[(Short, Array[Byte])] = ConcurrentLinkedQueue()

  /** Expected payload sizes for fixed-size packets. Override in subclasses.
    * Key = opcode, Value = (expected bytes, human-readable name).
    * Variable-length packets should NOT be included — only packets that must be
    * an exact size. TitaniumNetworkThread checks these on send/receive,
    * logging a warning on mismatch (but never blocking the packet).
    *
    * Separate maps for outgoing vs incoming because bidirectional opcodes
    * (e.g. ApproveName: 72 bytes out, 1 byte back) have different sizes.
    *
    * Hard-won lesson: the Titanium world connection hung silently for hours because
    * LoginInfo_Struct was 488 bytes instead of 464. The server's CheckSignature
    * rejected the size mismatch with no visible client-side error.
    */
  val expectedOutgoingSizes: Map[Short, (Int, String)] = Map.empty
  val expectedIncomingSizes: Map[Short, (Int, String)] = Map.empty

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit

  /** Called periodically from network thread. Produce ACKs etc. */
  def tick(): Unit

  /** Dequeue next outgoing raw packet. Called from network thread. */
  def pollOutgoing(): Option[Array[Byte]]
