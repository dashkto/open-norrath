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

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit

  /** Called periodically from network thread. Produce ACKs etc. */
  def tick(): Unit

  /** Dequeue next outgoing raw packet. Called from network thread. */
  def pollOutgoing(): Option[Array[Byte]]
