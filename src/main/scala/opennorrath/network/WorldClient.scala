package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

enum WorldState:
  case Disconnected, Connecting, Authenticated, CharacterSelect, EnteringZone, Failed

enum WorldEvent:
  case StateChanged(state: WorldState)
  case CharacterList(characters: Vector[CharacterInfo])
  case ZoneInfo(address: ZoneAddress)
  case Error(message: String)

/** World server protocol state machine.
  *
  * Thread safety model (mirrors LoginClient):
  * - Game thread calls: connect(), enterWorld(), pollEvent(), state
  * - Network thread calls: handlePacket(), tick(), pollOutgoing()
  * - Cross-thread communication via ConcurrentLinkedQueue (lock-free)
  */
class WorldClient extends PacketHandler:
  @volatile var state: WorldState = WorldState.Disconnected
  val events = ConcurrentLinkedQueue[WorldEvent]()
  val errors = ConcurrentLinkedQueue[String]()

  private val outQueue = ConcurrentLinkedQueue[Array[Byte]]()

  // Sequence tracking (network thread only)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true

  // Login data
  var characters: Vector[CharacterInfo] = Vector.empty

  // Pending connection info (set from game thread, consumed by network thread)
  @volatile private var pendingAccountId = 0
  @volatile private var pendingSessionKey = ""

  /** Initiate world connection. Called from game thread. */
  def connect(accountId: Int, sessionKey: String): Unit =
    pendingAccountId = accountId
    pendingSessionKey = sessionKey
    state = WorldState.Connecting
    emit(WorldEvent.StateChanged(state))
    queueAppPacket(WorldOpcodes.SendLoginInfo, WorldCodec.encodeLoginInfo(accountId, sessionKey))

  /** Request to enter the world with a character. Called from game thread. */
  def enterWorld(charName: String): Unit =
    state = WorldState.EnteringZone
    emit(WorldEvent.StateChanged(state))
    queueAppPacket(WorldOpcodes.EnterWorld, WorldCodec.encodeEnterWorld(charName))

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit =
    // Track ARQs for acknowledgment
    packet.arq.foreach { arq =>
      lastInArq = arq
      needArsp = true
    }

    if packet.opcode == 0 then return // pure ACK

    println(s"[World] Recv ${WorldOpcodes.name(packet.opcode)} (${packet.payload.length}B)")

    packet.opcode match
      case WorldOpcodes.SendCharInfo =>
        characters = WorldCodec.decodeCharacterSelect(packet.payload)
        println(s"[World] ${characters.size} character(s): ${characters.map(c => s"${c.name} L${c.level}").mkString(", ")}")
        state = WorldState.CharacterSelect
        emit(WorldEvent.CharacterList(characters))
        emit(WorldEvent.StateChanged(state))

      case WorldOpcodes.ApproveWorld =>
        println("[World] World approved")
        state = WorldState.Authenticated
        emit(WorldEvent.StateChanged(state))

      case WorldOpcodes.GuildsList =>
        println(s"[World] Guilds list received (${packet.payload.length}B)")

      case WorldOpcodes.LogServer =>
        println(s"[World] LogServer received (${packet.payload.length}B)")

      case WorldOpcodes.ExpansionInfo =>
        println(s"[World] ExpansionInfo received (${packet.payload.length}B)")

      case WorldOpcodes.MOTD =>
        val msg = new String(packet.payload, java.nio.charset.StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
        println(s"[World] MOTD: $msg")

      case WorldOpcodes.SetChatServer =>
        println(s"[World] Chat server info received (${packet.payload.length}B)")

      case WorldOpcodes.ZoneServerInfo =>
        WorldCodec.decodeZoneServerInfo(packet.payload) match
          case Some(addr) =>
            println(s"[World] Zone server: ${addr.ip}:${addr.port}")
            emit(WorldEvent.ZoneInfo(addr))
          case None =>
            println("[World] Failed to decode zone server info")

      case other =>
        println(s"[World] Unhandled opcode: ${WorldOpcodes.name(other)}")

  /** Called periodically from network thread. Produces ACK if needed. */
  def tick(): Unit =
    if needArsp && outQueue.isEmpty then
      val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
      outQueue.add(ack)
      needArsp = false

  /** Dequeue next outgoing raw packet. Called from network thread. */
  def pollOutgoing(): Option[Array[Byte]] =
    Option(outQueue.poll())

  /** Poll next event. Called from game thread. */
  def pollEvent(): Option[WorldEvent] =
    var err = errors.poll()
    while err != null do
      events.add(WorldEvent.Error(err))
      err = errors.poll()
    Option(events.poll())

  private def queueAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    val arsp = if needArsp then Some(lastInArq) else None
    needArsp = false
    val isFirst = firstPacket
    firstPacket = false
    val packet = OldPacket.encode(
      opcode = opcode,
      payload = payload,
      seq = nextSeq(),
      arq = Some(nextArq()),
      arsp = arsp,
      includeAsq = true,
      seqStart = isFirst,
    )
    outQueue.add(packet)

  private def nextSeq(): Int =
    val s = outSeq
    outSeq = (outSeq + 1) & 0xFFFF
    s

  private def nextArq(): Int =
    val a = outArq
    outArq = (outArq + 1) & 0xFFFF
    a

  private def emit(event: WorldEvent): Unit = events.add(event)
