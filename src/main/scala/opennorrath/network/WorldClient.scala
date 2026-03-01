package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

import opennorrath.Game
import opennorrath.network.titanium.{TitaniumWorldCodec, TitaniumWorldOpcodes}

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
  * Supports both Mac (OldPacket) and Titanium (EqStream) protocols.
  * Mac mode uses OldPacket.encode + outQueue + FragmentAssembler.
  * Titanium mode uses appOutQueue — TitaniumNetworkThread handles framing.
  *
  * Thread safety model (mirrors LoginClient):
  * - Game thread calls: connect(), enterWorld(), pollEvent(), state
  * - Network thread calls: handlePacket(), tick(), pollOutgoing()
  * - Cross-thread communication via ConcurrentLinkedQueue (lock-free)
  */
class WorldClient extends PacketHandler:
  @volatile var state: WorldState = WorldState.Disconnected
  val events = ConcurrentLinkedQueue[WorldEvent]()

  // Fixed-size packet validation — catches struct size mismatches before they cause silent failures.
  override val expectedPacketSizes: Map[Short, (Int, String)] =
    if Game.macMode then Map.empty
    else Map(
      // Outgoing (client -> server)
      TitaniumWorldOpcodes.SendLoginInfo -> (464, "SendLoginInfo"),
      TitaniumWorldOpcodes.EnterWorld -> (72, "EnterWorld"),
      TitaniumWorldOpcodes.ApproveName -> (76, "ApproveName"),
      TitaniumWorldOpcodes.CharacterCreate -> (80, "CharacterCreate"),
      // Incoming (server -> client)
      TitaniumWorldOpcodes.SendCharInfo -> (1704, "SendCharInfo"),
      TitaniumWorldOpcodes.ZoneServerInfo -> (130, "ZoneServerInfo"),
      TitaniumWorldOpcodes.ExpansionInfo -> (4, "ExpansionInfo"),
    )
  val errors = ConcurrentLinkedQueue[String]()

  // Mac transport outgoing queues
  private val outQueue = ConcurrentLinkedQueue[Array[Byte]]()
  private val pendingApps = ConcurrentLinkedQueue[(Short, Array[Byte])]()

  // Sequence tracking for Mac transport (network thread only)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true
  private var fragSeq: Int = 0

  // Fragment reassembly (Mac only — Titanium handles fragments in TitaniumNetworkThread)
  private val assembler = FragmentAssembler()

  // Keepalive — send periodic ACKs so the server doesn't drop the connection (Mac only)
  private var lastSentMs: Long = 0
  private val KeepaliveIntervalMs = 5000L

  // Login data
  var characters: Vector[CharacterInfo] = Vector.empty

  // Pending connection info (set from game thread, consumed by network thread)
  @volatile private var pendingAccountId = 0
  @volatile private var pendingSessionKey = ""
  @volatile private var zoningCharName: String = ""  // non-empty = zoning mode

  /** Initiate world connection. Called from game thread. */
  def connect(accountId: Int, sessionKey: String): Unit =
    pendingAccountId = accountId
    pendingSessionKey = sessionKey
    state = WorldState.Connecting
    emit(WorldEvent.StateChanged(state))
    if Game.macMode then
      queueAppPacket(MacWorldOpcodes.SendLoginInfo, WorldCodec.encodeLoginInfo(accountId, sessionKey))
    else
      queueAppPacket(
        TitaniumWorldOpcodes.SendLoginInfo,
        TitaniumWorldCodec.encodeLoginInfo(accountId, sessionKey),
      )

  /** Initiate world reconnection for zone-to-zone. Called from game thread.
    * Server will recognize zoning=true and skip character select.
    */
  def connectForZoning(accountId: Int, sessionKey: String, charName: String): Unit =
    zoningCharName = charName
    pendingAccountId = accountId
    pendingSessionKey = sessionKey
    state = WorldState.Connecting
    emit(WorldEvent.StateChanged(state))
    if Game.macMode then
      // Mac LoginInfo_Struct doesn't have a separate zoning variant in our codec,
      // but the zoning byte at offset 192 can be set
      queueAppPacket(MacWorldOpcodes.SendLoginInfo, WorldCodec.encodeLoginInfo(accountId, sessionKey))
    else
      queueAppPacket(
        TitaniumWorldOpcodes.SendLoginInfo,
        TitaniumWorldCodec.encodeLoginInfoZoning(accountId, sessionKey),
      )

  /** Request to enter the world with a character. Called from game thread. */
  def enterWorld(charName: String): Unit =
    state = WorldState.EnteringZone
    emit(WorldEvent.StateChanged(state))
    if Game.macMode then
      queueAppPacket(MacWorldOpcodes.EnterWorld, WorldCodec.encodeEnterWorld(charName))
    else
      queueAppPacket(TitaniumWorldOpcodes.EnterWorld, TitaniumWorldCodec.encodeEnterWorld(charName))

  /** Check if a name is available. Called from game thread. */
  def approveName(name: String, race: Int, classId: Int): Unit =
    if Game.macMode then
      queueAppPacket(MacWorldOpcodes.ApproveName, WorldCodec.encodeApproveName(name, race, classId))
    else
      queueAppPacket(
        TitaniumWorldOpcodes.ApproveName,
        TitaniumWorldCodec.encodeApproveName(name, race, classId),
      )

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
    if Game.macMode then
      queueAppPacket(MacWorldOpcodes.CharacterCreate, WorldCodec.encodeCharCreate(
        name, gender, race, classId, str, sta, cha, dex, int_, agi, wis,
        startZone, deity, hairColor, beardColor, eyeColor1, eyeColor2,
        hairStyle, beard, face,
      ))
    else
      queueAppPacket(TitaniumWorldOpcodes.CharacterCreate, TitaniumWorldCodec.encodeCharCreate(
        name, gender, race, classId, str, sta, cha, dex, int_, agi, wis,
        startZone, deity, hairColor, beardColor, eyeColor1, eyeColor2,
        hairStyle, beard, face,
      ))

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit =
    if Game.macMode then
      handleMacPacket(packet)
    else
      handleTitaniumPacket(packet)

  // ---- Mac protocol handling ----

  private def handleMacPacket(packet: InboundPacket): Unit =
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

    println(f"[World] Received opcode: 0x${pkt.opcode & 0xFFFF}%04x " +
      s"(${MacWorldOpcodes.name(pkt.opcode)}) ${pkt.payload.length}B")
    dispatchMacOpcode(pkt)

  private def dispatchMacOpcode(pkt: InboundPacket): Unit =
    pkt.opcode match
      case MacWorldOpcodes.SendCharInfo =>
        characters = WorldCodec.decodeCharacterSelect(pkt.payload)
        state = WorldState.CharacterSelect
        emit(WorldEvent.CharacterList(characters))
        emit(WorldEvent.StateChanged(state))

      case MacWorldOpcodes.ApproveWorld =>
        state = WorldState.Authenticated
        emit(WorldEvent.StateChanged(state))

      case MacWorldOpcodes.GuildsList =>
        val guilds = WorldCodec.decodeGuildsList(pkt.payload)
        emit(WorldEvent.GuildsReceived(guilds))

      case MacWorldOpcodes.LogServer =>
        val host = WorldCodec.decodeLogServer(pkt.payload)
        emit(WorldEvent.LogServerReceived(host))

      case MacWorldOpcodes.ExpansionInfo =>
        val exp = WorldCodec.decodeExpansionInfo(pkt.payload)
        emit(WorldEvent.ExpansionReceived(exp))

      case MacWorldOpcodes.MOTD =>
        val msg = WorldCodec.decodeMOTD(pkt.payload)
        emit(WorldEvent.MOTDReceived(msg))

      case MacWorldOpcodes.SetChatServer =>
        val host = WorldCodec.decodeChatServer(pkt.payload)
        emit(WorldEvent.ChatServerReceived(host))

      case MacWorldOpcodes.ZoneServerInfo =>
        WorldCodec.decodeZoneServerInfo(pkt.payload) match
          case Some(addr) => emit(WorldEvent.ZoneInfo(addr))
          case None => ()

      case MacWorldOpcodes.ApproveName =>
        val approved = WorldCodec.decodeNameApproval(pkt.payload)
        emit(WorldEvent.NameApproved(approved))

      case MacWorldOpcodes.EnterWorld =>
        // Server sends OP_EnterWorld during zoning reconnect (pZoning=true).
        // Auto-respond with OP_EnterWorld to continue the zone-to-zone flow.
        if zoningCharName.nonEmpty then
          println(s"[World] Zoning: auto-responding OP_EnterWorld for '$zoningCharName'")
          queueAppPacket(MacWorldOpcodes.EnterWorld, WorldCodec.encodeEnterWorld(zoningCharName))
          state = WorldState.EnteringZone
          emit(WorldEvent.StateChanged(state))

      case other =>
        println(f"[World] Unhandled opcode: 0x${other & 0xFFFF}%04x (${pkt.payload.length} bytes)")

  // ---- Titanium protocol handling ----

  private def handleTitaniumPacket(packet: InboundPacket): Unit =
    if packet.opcode == 0 then return

    val op = packet.opcode
    println(f"[World] Received opcode: 0x${op & 0xFFFF}%04x " +
      s"(${TitaniumWorldOpcodes.name(op)}) ${packet.payload.length}B")

    if op == TitaniumWorldOpcodes.SendCharInfo then
      characters = TitaniumWorldCodec.decodeCharacterSelect(packet.payload)
      state = WorldState.CharacterSelect
      emit(WorldEvent.CharacterList(characters))
      emit(WorldEvent.StateChanged(state))

    else if op == TitaniumWorldOpcodes.ApproveWorld then
      state = WorldState.Authenticated
      emit(WorldEvent.StateChanged(state))

    else if op == TitaniumWorldOpcodes.GuildsList then
      val guilds = TitaniumWorldCodec.decodeGuildsList(packet.payload)
      emit(WorldEvent.GuildsReceived(guilds))

    else if op == TitaniumWorldOpcodes.LogServer then
      val host = TitaniumWorldCodec.decodeLogServer(packet.payload)
      emit(WorldEvent.LogServerReceived(host))

    else if op == TitaniumWorldOpcodes.ExpansionInfo then
      val exp = TitaniumWorldCodec.decodeExpansionInfo(packet.payload)
      emit(WorldEvent.ExpansionReceived(exp))

    else if op == TitaniumWorldOpcodes.MOTD then
      val msg = TitaniumWorldCodec.decodeMOTD(packet.payload)
      emit(WorldEvent.MOTDReceived(msg))

    else if op == TitaniumWorldOpcodes.SetChatServer then
      val host = TitaniumWorldCodec.decodeChatServer(packet.payload)
      emit(WorldEvent.ChatServerReceived(host))

    else if op == TitaniumWorldOpcodes.ZoneServerInfo then
      TitaniumWorldCodec.decodeZoneServerInfo(packet.payload) match
        case Some(addr) => emit(WorldEvent.ZoneInfo(addr))
        case None => ()

    else if op == TitaniumWorldOpcodes.ApproveName then
      val approved = TitaniumWorldCodec.decodeNameApproval(packet.payload)
      emit(WorldEvent.NameApproved(approved))

    else if op == TitaniumWorldOpcodes.EnterWorld then
      // Server sends OP_EnterWorld during zoning reconnect
      if zoningCharName.nonEmpty then
        println(s"[World] Zoning: auto-responding OP_EnterWorld for '$zoningCharName'")
        queueAppPacket(
          TitaniumWorldOpcodes.EnterWorld,
          TitaniumWorldCodec.encodeEnterWorld(zoningCharName),
        )
        state = WorldState.EnteringZone
        emit(WorldEvent.StateChanged(state))

    else
      println(f"[World] Unhandled Titanium opcode: 0x${op & 0xFFFF}%04x (${packet.payload.length} bytes)")

  // ---- Common ----

  /** Called periodically from network thread. Builds queued app packets and ACKs. */
  def tick(): Unit =
    if Game.macMode then
      // Mac: build pending app packets (queued from game thread, built here on network thread)
      var pending = pendingApps.poll()
      while pending != null do
        buildAppPacket(pending._1, pending._2)
        pending = pendingApps.poll()

      if needArsp then
        val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
        outQueue.add(ack)
        needArsp = false
        lastSentMs = System.currentTimeMillis()

      // Keepalive — send a periodic ACK to prevent server from dropping idle connection
      val now = System.currentTimeMillis()
      if lastInArq >= 0 && now - lastSentMs > KeepaliveIntervalMs then
        val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
        outQueue.add(ack)
        lastSentMs = now
    // Titanium: no tick work needed — TitaniumNetworkThread handles ACKs and keepalive

  /** Dequeue next outgoing raw packet. Called from network thread (Mac transport). */
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
    if Game.macMode then
      pendingApps.add((opcode, payload))
    else
      appOutQueue.add((opcode, payload))

  // ---- Mac transport helpers (network thread only) ----

  private def buildAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    if payload.length > 510 then
      buildFragmentedPacket(opcode, payload)
      return

    val arsp = if needArsp then Some(lastInArq) else None
    needArsp = false
    val isFirst = firstPacket
    firstPacket = false
    val seq = nextSeq()
    val arq = nextArq()
    val packet = OldPacket.encode(
      opcode = opcode,
      payload = payload,
      seq = seq,
      arq = Some(arq),
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
