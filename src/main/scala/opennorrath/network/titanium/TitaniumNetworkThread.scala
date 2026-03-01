package opennorrath.network.titanium

import scala.collection.mutable
import scala.compiletime.uninitialized

import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketTimeoutException}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

import opennorrath.network.{EqNetworkThread, InboundPacket, NetCommand, PacketHandler}

/** Background UDP I/O thread for the Titanium "EqStream" protocol.
  *
  * Replaces NetworkThread for Titanium connections. Handles session negotiation,
  * encoding/decoding, sequencing, ACKs, and fragment reassembly at the transport
  * layer — delivering the same InboundPacket to the same PacketHandler trait.
  *
  * Key differences from Mac's NetworkThread + OldPacket:
  * - Session handshake (SessionRequest/SessionResponse) before any app data
  * - Transport-layer sequencing and ACKs (not in the app client classes)
  * - CRC32-with-key, XOR encoding, compression at transport level
  * - App opcodes are little-endian (Mac is big-endian)
  */
class TitaniumNetworkThread(handler: PacketHandler) extends EqNetworkThread:
  private val running = AtomicBoolean(false)
  private val commands = ConcurrentLinkedQueue[NetCommand]()
  private var socket: DatagramSocket = uninitialized
  private var address: InetSocketAddress = uninitialized
  private var thread: Thread = uninitialized

  // Session state
  private var sessionParams: EqStream.SessionParams = uninitialized
  private var sessionEstablished = false
  private val connectCode = scala.util.Random.nextInt()

  // Outbound sequencing (transport-layer, managed here not in client classes)
  private var sequenceOut: Int = 0
  // Inbound sequencing
  private var sequenceIn: Int = 0
  private var needAck = false
  private var highestAckedSeq: Int = -1

  // Fragment reassembly for incoming
  private var fragmentBuffer: Array[Byte] = uninitialized
  private var fragmentTotalBytes: Int = 0
  private var fragmentCurrentBytes: Int = 0

  // Pending raw packets to send on wire
  private val wireOutQueue = ConcurrentLinkedQueue[Array[Byte]]()

  // Sent packets awaiting ACK (sequence -> raw packet for retransmission)
  private val sentPackets = mutable.LinkedHashMap[Int, (Array[Byte], Long)]()

  // Timers
  private var lastSentMs: Long = 0
  private var lastConnectAttemptMs: Long = 0
  private val KeepaliveIntervalMs = 9000L
  private val ConnectRetryMs = 500L
  private val ResendTimeoutMs = 500L
  private val StaleConnectionMs = 60000L

  // Stats
  private var packetsSent: Long = 0
  private var packetsRecv: Long = 0

  def start(): Unit =
    if running.get() then return
    running.set(true)
    thread = Thread(() => run(), "eq-titanium-network")
    thread.setDaemon(true)
    thread.start()

  def stop(): Unit =
    running.set(false)
    if socket != null then
      try socket.close() catch case _: Exception => ()
    if thread != null then thread.join(2000)

  def send(cmd: NetCommand): Unit = commands.add(cmd)

  private def run(): Unit =
    val recvBuf = new Array[Byte](4096)
    while running.get() do
      try
        processCommands()
        receivePackets(recvBuf)
        processOutgoing()
        handleTimers()
      catch
        case e: Exception =>
          if running.get() then
            println(s"[TitaniumNetwork] Error: ${e.getClass.getSimpleName}: ${e.getMessage}")
            handler.errors.add(s"Network error: ${e.getMessage}")

    println("[TitaniumNetwork] Thread exiting")
    if socket != null && !socket.isClosed then
      try socket.close() catch case _: Exception => ()

  private def processCommands(): Unit =
    var cmd = commands.poll()
    while cmd != null do
      cmd match
        case NetCommand.Connect(host, port) =>
          if socket != null then try socket.close() catch case _: Exception => ()
          address = InetSocketAddress(host, port)
          socket = DatagramSocket()
          socket.setSoTimeout(50)
          // Reset session state
          sessionEstablished = false
          sequenceOut = 0
          sequenceIn = 0
          needAck = false
          highestAckedSeq = -1
          fragmentTotalBytes = 0
          fragmentCurrentBytes = 0
          sentPackets.clear()
          // Initiate session handshake
          sendRaw(EqStream.encodeSessionRequest(connectCode))
          lastConnectAttemptMs = System.currentTimeMillis()

        case NetCommand.SendRaw(data) =>
          sendRaw(data)

        case NetCommand.Disconnect =>
          if sessionEstablished then
            sendRaw(EqStream.encodeDisconnect(connectCode))
          if socket != null then
            try socket.close() catch case _: Exception => ()
            socket = null
          sessionEstablished = false
          println("[TitaniumNetwork] Disconnected")
      cmd = commands.poll()

  private def receivePackets(recvBuf: Array[Byte]): Unit =
    if socket == null || socket.isClosed then return
    try
      val packet = DatagramPacket(recvBuf, recvBuf.length)
      socket.receive(packet)
      val raw = new Array[Byte](packet.getLength)
      System.arraycopy(recvBuf, 0, raw, 0, packet.getLength)
      packetsRecv += 1
      handleIncoming(raw)
    catch
      case _: SocketTimeoutException => ()

  /** Process a raw incoming UDP packet through the EqStream pipeline. */
  private def handleIncoming(raw: Array[Byte]): Unit =
    if raw.length < 2 then return

    if !sessionEstablished then
      // During handshake, only look for SessionResponse
      EqStream.decodeSessionResponse(raw).foreach { params =>
        sessionParams = params
        sessionEstablished = true
        println(s"[TitaniumNetwork] Session established: " +
          s"crcBytes=${params.crcBytes}, pass1=${params.encodePass1}, " +
          s"pass2=${params.encodePass2}, maxPacket=${params.maxPacketSize}")
        // Now process any buffered outgoing app packets
      }
      return

    // Decode the packet (CRC validation, XOR decode, etc.)
    val decoded = EqStream.decodePacket(raw, sessionParams) match
      case Some(d) => d
      case None => return // CRC failure

    processDecodedPacket(decoded)

  /** Route a decoded packet based on its protocol opcode. */
  private def processDecodedPacket(data: Array[Byte]): Unit =
    if data.length < 2 then return

    if !EqStream.isProtocolPacket(data) then
      // Raw app packet (first byte != 0x00) — deliver to handler
      deliverAppPacket(data, 0)
      return

    val opcode = data(1)
    opcode match
      case EqStream.OP_Combined =>
        // Recursively process each sub-packet
        for sub <- EqStream.decodeCombined(data) do
          processDecodedPacket(sub)

      case EqStream.OP_AppCombined =>
        for sub <- EqStream.decodeAppCombined(data) do
          processDecodedPacket(sub)

      case EqStream.OP_Packet =>
        handleReliablePacket(data, isFragment = false)

      case EqStream.OP_Fragment =>
        handleReliablePacket(data, isFragment = true)

      case EqStream.OP_Ack =>
        val ackSeq = EqStream.readSequence(data)
        handleAck(ackSeq)

      case EqStream.OP_OutOfOrderAck =>
        val ackSeq = EqStream.readSequence(data)
        sentPackets.remove(ackSeq)

      case EqStream.OP_KeepAlive =>
        () // consumed, no response needed

      case EqStream.OP_SessionStatRequest =>
        val response = EqStream.encodeSessionStatResponse(data, packetsSent, packetsRecv)
        if response.nonEmpty then sendEncoded(response)

      case EqStream.OP_SessionDisconnect =>
        println("[TitaniumNetwork] Server disconnected session")
        handler.errors.add("Server disconnected")

      case _ =>
        println(f"[TitaniumNetwork] Unknown protocol opcode: 0x${opcode & 0xFF}%02x")

  /** Handle OP_Packet or OP_Fragment with sequencing. */
  private def handleReliablePacket(data: Array[Byte], isFragment: Boolean): Unit =
    val seq = EqStream.readSequence(data)
    val cmp = EqStream.compareSequence(seq, sequenceIn)

    if cmp < 0 then
      // Past — duplicate, ignore
      return
    else if cmp > 0 then
      // Future — out of order, send OutOfOrderAck
      sendRaw(EqStream.encodeForWire(EqStream.encodeOutOfOrderAck(seq), sessionParams))
      return

    // Expected sequence — process it
    sequenceIn = (sequenceIn + 1) & 0xFFFF
    needAck = true

    if isFragment then
      handleFragment(data)
    else
      // OP_Packet: payload starts after 4-byte header
      if data.length > 4 then
        // The inner data could be another protocol packet or an app packet
        val inner = new Array[Byte](data.length - 4)
        System.arraycopy(data, 4, inner, 0, inner.length)
        processDecodedPacket(inner)

  /** Handle an OP_Fragment packet, reassembling into a complete packet. */
  private def handleFragment(data: Array[Byte]): Unit =
    if fragmentTotalBytes == 0 then
      // First fragment — has total_size at offset 4-7
      fragmentTotalBytes = EqStream.readFragmentTotalSize(data)
      if fragmentTotalBytes <= 0 || fragmentTotalBytes > 1048576 then
        println(s"[TitaniumNetwork] Invalid fragment total size: $fragmentTotalBytes")
        fragmentTotalBytes = 0
        return
      fragmentBuffer = new Array[Byte](fragmentTotalBytes)
      fragmentCurrentBytes = 0
      // Data starts after 8-byte header (first fragment)
      val dataOffset = 8
      val dataLen = data.length - dataOffset
      if dataLen > 0 then
        System.arraycopy(data, dataOffset, fragmentBuffer, 0, Math.min(dataLen, fragmentTotalBytes))
        fragmentCurrentBytes += dataLen
    else
      // Subsequent fragment — data starts after 4-byte header
      val dataOffset = 4
      val dataLen = data.length - dataOffset
      if dataLen > 0 && fragmentCurrentBytes + dataLen <= fragmentTotalBytes then
        System.arraycopy(data, dataOffset, fragmentBuffer, fragmentCurrentBytes, dataLen)
        fragmentCurrentBytes += dataLen

    // Check if reassembly is complete
    if fragmentCurrentBytes >= fragmentTotalBytes then
      val assembled = fragmentBuffer
      fragmentTotalBytes = 0
      fragmentCurrentBytes = 0
      fragmentBuffer = null
      // Process the reassembled data as a packet
      processDecodedPacket(assembled)

  /** Deliver an app-level packet to the handler. */
  private def deliverAppPacket(data: Array[Byte], headerOffset: Int): Unit =
    var offset = headerOffset
    // Handle OP_Padding escape (0x00 prefix)
    if offset < data.length && data(offset) == 0 then offset += 1
    if data.length - offset < 2 then return
    // App opcode is 2-byte little-endian
    val opcode = ((data(offset) & 0xFF) | ((data(offset + 1) & 0xFF) << 8)).toShort
    offset += 2
    val payload = if offset < data.length then
      val buf = new Array[Byte](data.length - offset)
      System.arraycopy(data, offset, buf, 0, buf.length)
      buf
    else Array.emptyByteArray
    // Validate incoming packet size against expected fixed sizes
    handler.expectedIncomingSizes.get(opcode).foreach { (expected, name) =>
      if payload.length != expected then
        println(f"[PacketSize] WARNING: Incoming 0x${opcode & 0xFFFF}%04x ($name) " +
          s"expected $expected bytes but received ${payload.length} bytes")
    }
    handler.handlePacket(InboundPacket(seq = 0, arsp = None, arq = None, opcode = opcode, payload = payload))

  /** Handle an ACK — remove all sent packets up to and including the acked sequence. */
  private def handleAck(ackSeq: Int): Unit =
    val toRemove = sentPackets.keys.takeWhile { seq =>
      EqStream.compareSequence(seq, ackSeq) <= 0
    }.toVector
    toRemove.foreach(sentPackets.remove)
    if EqStream.compareSequence(ackSeq, highestAckedSeq) > 0 then
      highestAckedSeq = ackSeq

  /** Process outgoing app packets from the handler and the app queue. */
  private def processOutgoing(): Unit =
    if !sessionEstablished then return

    // Send pending ACK
    if needAck then
      val ackSeq = (sequenceIn - 1) & 0xFFFF
      sendEncoded(EqStream.encodeAck(ackSeq))
      needAck = false

    // Process app packets from the handler's outgoing queue
    // (For Titanium, the handler's pollOutgoing returns raw OldPacket-encoded bytes
    //  which we don't want. Instead, app packets come through queueAppPacket.)
    // Drain and discard any handler-generated OldPacket data
    var old = handler.pollOutgoing()
    while old.isDefined do old = handler.pollOutgoing()

    // Process handler tick (for keepalive logic in client classes)
    handler.tick()

    // Send queued app packets (client classes add to handler.appOutQueue)
    var appPkt = handler.appOutQueue.poll()
    while appPkt != null do
      val (opcode, payload) = appPkt
      sendAppPacket(opcode, payload)
      appPkt = handler.appOutQueue.poll()

  /** Send an application packet wrapped in OP_Packet or OP_Fragment. */
  private def sendAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    // Validate outgoing packet size against expected fixed sizes
    handler.expectedOutgoingSizes.get(opcode).foreach { (expected, name) =>
      if payload.length != expected then
        println(f"[PacketSize] WARNING: Outgoing 0x${opcode & 0xFFFF}%04x ($name) " +
          s"expected $expected bytes but sending ${payload.length} bytes")
    }
    // Calculate if fragmentation is needed
    val maxRawSize = sessionParams.maxPacketSize - sessionParams.crcBytes - 4 - 1

    // Build inner app data: [2-byte LE opcode][payload], with padding if needed
    val needsPadding = (opcode & 0xFF) == 0
    val innerLen = (if needsPadding then 1 else 0) + 2 + payload.length
    val inner = ByteBuffer.allocate(innerLen).order(ByteOrder.LITTLE_ENDIAN)
    if needsPadding then inner.put(0: Byte)
    inner.putShort(opcode)
    if payload.nonEmpty then inner.put(payload)
    val innerBytes = inner.array()

    if innerBytes.length <= maxRawSize then
      // Fits in one OP_Packet
      val pkt = ByteBuffer.allocate(4 + innerBytes.length).order(ByteOrder.BIG_ENDIAN)
      pkt.put(0: Byte)
      pkt.put(EqStream.OP_Packet)
      pkt.putShort((sequenceOut & 0xFFFF).toShort)
      pkt.put(innerBytes)
      val raw = pkt.array()
      sentPackets(sequenceOut) = (raw, System.currentTimeMillis())
      sequenceOut = (sequenceOut + 1) & 0xFFFF
      sendEncoded(raw)
    else
      // Fragment
      val frags = EqStream.encodeFragments(opcode, payload, sequenceOut, sessionParams.maxPacketSize, sessionParams.crcBytes)
      for (frag, seq) <- frags do
        sentPackets(seq) = (frag, System.currentTimeMillis())
        sendEncoded(frag)
      sequenceOut = (sequenceOut + frags.length) & 0xFFFF

  /** Handle timers: keepalive, retransmission, connect retry. */
  private def handleTimers(): Unit =
    val now = System.currentTimeMillis()

    if !sessionEstablished then
      // Retry session connect
      if now - lastConnectAttemptMs > ConnectRetryMs then
        sendRaw(EqStream.encodeSessionRequest(connectCode))
        lastConnectAttemptMs = now
      return

    // Keepalive
    if now - lastSentMs > KeepaliveIntervalMs then
      sendEncoded(EqStream.encodeKeepAlive())

    // Retransmit un-ACKed packets
    val retransmitThreshold = now - ResendTimeoutMs
    for (seq, (pkt, sentTime)) <- sentPackets if sentTime < retransmitThreshold do
      sendEncoded(pkt)
      sentPackets(seq) = (pkt, now)

  /** Send a raw UDP datagram (no encoding). Used for session negotiation. */
  private def sendRaw(data: Array[Byte]): Unit =
    if socket != null && address != null && !socket.isClosed then
      val packet = DatagramPacket(data, data.length, address)
      socket.send(packet)
      lastSentMs = System.currentTimeMillis()
      packetsSent += 1

  /** Encode a packet (CRC/XOR) and send it. */
  private def sendEncoded(data: Array[Byte]): Unit =
    val encoded = EqStream.encodeForWire(data, sessionParams)
    sendRaw(encoded)
