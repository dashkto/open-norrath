package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

enum WorldState:
  case Disconnected, Connecting, Authenticated, CharacterSelect, EnteringZone, Failed

enum WorldEvent:
  case StateChanged(state: WorldState)
  case CharacterList(characters: Vector[CharacterInfo])
  case ZoneInfo(address: ZoneAddress)
  case NameApproved(approved: Boolean)
  case GuildsReceived(guilds: Vector[GuildInfo])
  case MOTDReceived(message: String)
  case ExpansionReceived(expansions: ExpansionFlags)
  case ChatServerReceived(host: String)
  case LogServerReceived(host: String)
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
  private val pendingApps = ConcurrentLinkedQueue[(Short, Array[Byte])]()

  // Sequence tracking (network thread only — never touch from game thread)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true
  private var fragSeq: Int = 0

  // Fragment reassembly
  private val assembler = FragmentAssembler()

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

  /** Check if a name is available. Called from game thread. */
  def approveName(name: String, race: Int, classId: Int): Unit =
    queueAppPacket(WorldOpcodes.ApproveName, WorldCodec.encodeApproveName(name, race, classId))

  /** Create a character. Called from game thread. */
  def createCharacter(
    name: String,
    gender: Int, race: Int, classId: Int,
    str: Int, sta: Int, cha: Int, dex: Int, int_ : Int, agi: Int, wis: Int,
    startZone: Int, deity: Int,
    hairColor: Int = 0, beardColor: Int = 0,
    eyeColor1: Int = 0, eyeColor2: Int = 0,
    hairStyle: Int = 0, beard: Int = 0, face: Int = 0,
  ): Unit =
    queueAppPacket(WorldOpcodes.CharacterCreate, WorldCodec.encodeCharCreate(
      name, gender, race, classId, str, sta, cha, dex, int_, agi, wis,
      startZone, deity, hairColor, beardColor, eyeColor1, eyeColor2,
      hairStyle, beard, face,
    ))

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit =
    // Track ARQs for acknowledgment
    packet.arq.foreach { arq =>
      lastInArq = arq
      needArsp = true
    }

    // Fragment reassembly — may buffer and return None until complete
    val pkt = assembler.process(packet) match
      case Some(p) => p
      case None => return

    if pkt.opcode == 0 then return

    pkt.opcode match
      case WorldOpcodes.SendCharInfo =>
        characters = WorldCodec.decodeCharacterSelect(pkt.payload)
        state = WorldState.CharacterSelect
        emit(WorldEvent.CharacterList(characters))
        emit(WorldEvent.StateChanged(state))

      case WorldOpcodes.ApproveWorld =>
        state = WorldState.Authenticated
        emit(WorldEvent.StateChanged(state))

      case WorldOpcodes.GuildsList =>
        val guilds = WorldCodec.decodeGuildsList(pkt.payload)
        emit(WorldEvent.GuildsReceived(guilds))

      case WorldOpcodes.LogServer =>
        val host = WorldCodec.decodeLogServer(pkt.payload)
        emit(WorldEvent.LogServerReceived(host))

      case WorldOpcodes.ExpansionInfo =>
        val exp = WorldCodec.decodeExpansionInfo(pkt.payload)
        emit(WorldEvent.ExpansionReceived(exp))

      case WorldOpcodes.MOTD =>
        val msg = WorldCodec.decodeMOTD(pkt.payload)
        emit(WorldEvent.MOTDReceived(msg))

      case WorldOpcodes.SetChatServer =>
        val host = WorldCodec.decodeChatServer(pkt.payload)
        emit(WorldEvent.ChatServerReceived(host))

      case WorldOpcodes.ZoneServerInfo =>
        WorldCodec.decodeZoneServerInfo(pkt.payload) match
          case Some(addr) => emit(WorldEvent.ZoneInfo(addr))
          case None => ()

      case WorldOpcodes.ApproveName =>
        val approved = WorldCodec.decodeNameApproval(pkt.payload)
        emit(WorldEvent.NameApproved(approved))

      case _ => ()

  /** Called periodically from network thread. Builds queued app packets and ACKs. */
  def tick(): Unit =
    // Build pending app packets (queued from game thread, built here on network thread)
    var pending = pendingApps.poll()
    while pending != null do
      buildAppPacket(pending._1, pending._2)
      pending = pendingApps.poll()

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

  /** Queue an app packet for sending. Safe to call from game thread. */
  private def queueAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    pendingApps.add((opcode, payload))

  /** Build and enqueue a raw packet. Called from network thread only. */
  private def buildAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    if payload.length > 510 then
      buildFragmentedPacket(opcode, payload)
      return

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

  private def buildFragmentedPacket(opcode: Short, payload: Array[Byte]): Unit =
    val fSeq = nextFragSeq()
    val fragments = assembler.fragment(opcode, payload, fSeq)
    for (frag, i) <- fragments.zipWithIndex do
      val arsp = if i == 0 && needArsp then Some(lastInArq) else None
      if i == 0 then needArsp = false
      val isFirst = firstPacket
      if i == 0 then firstPacket = false
      val packet = OldPacket.encodeFragment(
        frag = frag,
        seq = nextSeq(),
        arq = if i == 0 then Some(nextArq()) else None,
        arsp = arsp,
        includeAsq = i == 0,
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

  private def nextFragSeq(): Int =
    val s = fragSeq
    fragSeq = (fragSeq + 1) & 0xFFFF
    s

  private def emit(event: WorldEvent): Unit = events.add(event)
