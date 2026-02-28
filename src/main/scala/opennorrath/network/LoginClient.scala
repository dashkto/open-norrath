package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable

import opennorrath.Game
import opennorrath.network.titanium.{TitaniumLoginCodec, TitaniumLoginOpcodes}

enum LoginState:
  case Disconnected, Connecting, WaitingForLogin, LoggedIn
  case ServerListReceived, PlayRequested, Complete, Failed

enum LoginEvent:
  case StateChanged(state: LoginState)
  case DateReceived(date: String)
  case LoginSuccess(sessionId: String)
  case ServerNameReceived(ip: String)
  case ServerListUpdated(servers: Vector[ServerInfo])
  case BannerReceived(text: String)
  case PlayApproved(key: String)
  case LoginComplete
  case Error(message: String)

/** Login protocol state machine.
  *
  * Supports both Mac (OldPacket) and Titanium (EqStream) protocols.
  * Mac mode uses OldPacket.encode + outQueue for transport framing.
  * Titanium mode uses appOutQueue (opcode, payload) tuples — TitaniumNetworkThread
  * handles the EqStream framing (OP_Packet/OP_Fragment + sequencing + CRC).
  *
  * Thread safety model:
  * - Game thread calls: connect(), selectServer(), pollEvent(), state
  * - Network thread calls: handlePacket(), tick(), pollOutgoing()
  * - Cross-thread communication via ConcurrentLinkedQueue (lock-free)
  */
class LoginClient extends PacketHandler:
  @volatile var state: LoginState = LoginState.Disconnected
  val events = ConcurrentLinkedQueue[LoginEvent]()
  val errors = ConcurrentLinkedQueue[String]()

  // Outgoing packet queue for Mac transport (network thread drains this)
  private val outQueue = ConcurrentLinkedQueue[Array[Byte]]()

  // Sequence tracking for Mac transport (network thread only)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true

  // Login data
  var sessionId: String = ""
  var accountId: Int = 0
  var serverIp: String = ""
  var servers: Vector[ServerInfo] = Vector.empty
  var worldKey: String = ""

  // Pending credentials (set from game thread, consumed by network thread)
  @volatile private var pendingUser = ""
  @volatile private var pendingPass = ""

  /** Initiate connection and login. Called from game thread. */
  def connect(user: String, pass: String): Unit =
    // Reset protocol state for fresh connection
    outSeq = 0
    outArq = 0
    lastInArq = -1
    needArsp = false
    firstPacket = true
    outQueue.clear()
    appOutQueue.clear()
    pendingUser = user
    pendingPass = pass
    state = LoginState.Connecting
    emit(LoginEvent.StateChanged(state))

    if Game.macMode then
      queueAppPacket(LoginOpcodes.SessionReady, LoginCodec.encodeSessionReady)
    else
      queueAppPacket(TitaniumLoginOpcodes.SessionReady, TitaniumLoginCodec.encodeSessionReady)

  /** Request to play on a specific server. Called from game thread.
    * For Mac: sends the server IP string.
    * For Titanium: sends the server_number (worldId from server list).
    */
  def selectServer(ip: String, serverId: Int = 0): Unit =
    state = LoginState.PlayRequested
    emit(LoginEvent.StateChanged(state))
    if Game.macMode then
      queueAppPacket(LoginOpcodes.PlayEverquestRequest, LoginCodec.encodePlayRequest(ip))
    else
      queueAppPacket(
        TitaniumLoginOpcodes.PlayEverquestRequest,
        TitaniumLoginCodec.encodePlayRequest(serverId),
      )

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit =
    // Track ARQs for acknowledgment (Mac transport only — Titanium handles ACKs itself)
    if Game.macMode then
      packet.arq.foreach { arq =>
        lastInArq = arq
        needArsp = true
      }

    if packet.opcode == 0 then return // pure ACK

    if Game.macMode then handleMacPacket(packet)
    else handleTitaniumPacket(packet)

  // ---- Mac protocol handling (unchanged from original) ----

  private def handleMacPacket(packet: InboundPacket): Unit =
    packet.opcode match
      case LoginOpcodes.SessionReady =>
        val date = LoginCodec.decodeSessionReadyResponse(packet.payload)
        emit(LoginEvent.DateReceived(date))
        // Now send credentials
        if pendingUser.nonEmpty then
          queueAppPacket(LoginOpcodes.LoginOSX, LoginCodec.encodeLoginOSX(pendingUser, pendingPass))
          state = LoginState.WaitingForLogin
          emit(LoginEvent.StateChanged(state))

      case LoginOpcodes.LoginAccepted =>
        sessionId = LoginCodec.decodeLoginAccepted(packet.payload)
        state = LoginState.LoggedIn
        emit(LoginEvent.LoginSuccess(sessionId))
        emit(LoginEvent.StateChanged(state))

      case LoginOpcodes.ServerName =>
        serverIp = LoginCodec.decodeServerName(packet.payload)
        emit(LoginEvent.ServerNameReceived(serverIp))
        // Auto-request server list
        queueAppPacket(LoginOpcodes.ServerListRequest, LoginCodec.encodeServerListRequest)

      case LoginOpcodes.ServerListRequest =>
        // Server reuses the same opcode for the response
        val list = LoginCodec.decodeServerList(packet.payload)
        servers = list.servers
        state = LoginState.ServerListReceived
        emit(LoginEvent.ServerListUpdated(servers))
        emit(LoginEvent.StateChanged(state))

      case LoginOpcodes.PlayEverquestRequest =>
        // Server reuses same opcode for old client play response
        worldKey = LoginCodec.decodePlayResponse(packet.payload)
        emit(LoginEvent.PlayApproved(worldKey))
        // Send login complete
        queueAppPacket(LoginOpcodes.LoginComplete, LoginCodec.encodeLoginComplete)

      case LoginOpcodes.LoginComplete =>
        state = LoginState.Complete
        emit(LoginEvent.LoginComplete)
        emit(LoginEvent.StateChanged(state))

      case LoginOpcodes.ClientError =>
        val msg = LoginCodec.decodeClientError(packet.payload)
        state = LoginState.Failed
        emit(LoginEvent.Error(msg))
        emit(LoginEvent.StateChanged(state))

      case LoginOpcodes.LoginBanner =>
        if packet.payload.length > 4 then
          val text = new String(packet.payload, 4, packet.payload.length - 4,
            java.nio.charset.StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
          emit(LoginEvent.BannerReceived(text))

      case LoginOpcodes.LoginUnknown2 =>
        () // Expected response to LoginUnknown1, ignore

      case _ => ()

  // ---- Titanium protocol handling ----

  private def handleTitaniumPacket(packet: InboundPacket): Unit =
    val op = packet.opcode
    if op == TitaniumLoginOpcodes.ChatMessage then
      // Handshake reply to SessionReady
      val ok = TitaniumLoginCodec.decodeChatMessage(packet.payload)
      if ok then
        emit(LoginEvent.DateReceived("Titanium handshake OK"))
        // Now send DES-encrypted credentials
        if pendingUser.nonEmpty then
          queueAppPacket(TitaniumLoginOpcodes.Login, TitaniumLoginCodec.encodeLogin(pendingUser, pendingPass))
          state = LoginState.WaitingForLogin
          emit(LoginEvent.StateChanged(state))
      else
        state = LoginState.Failed
        emit(LoginEvent.Error("Login handshake failed"))
        emit(LoginEvent.StateChanged(state))

    else if op == TitaniumLoginOpcodes.LoginAccepted then
      val result = TitaniumLoginCodec.decodeLoginAccepted(packet.payload)
      if result.success then
        accountId = result.accountId
        worldKey = result.key
        sessionId = s"LS#${result.accountId}"
        state = LoginState.LoggedIn
        emit(LoginEvent.LoginSuccess(sessionId))
        emit(LoginEvent.StateChanged(state))
        // Auto-request server list (Titanium doesn't have the ServerName step)
        queueAppPacket(
          TitaniumLoginOpcodes.ServerListRequest,
          TitaniumLoginCodec.encodeServerListRequest(),
        )
      else
        state = LoginState.Failed
        emit(LoginEvent.Error(s"Login failed (error ${result.errorStrId})"))
        emit(LoginEvent.StateChanged(state))

    else if op == TitaniumLoginOpcodes.ServerListResponse then
      val list = TitaniumLoginCodec.decodeServerList(packet.payload)
      servers = list.servers
      state = LoginState.ServerListReceived
      emit(LoginEvent.ServerListUpdated(servers))
      emit(LoginEvent.StateChanged(state))

    else if op == TitaniumLoginOpcodes.PlayEverquestResponse then
      val (success, _serverId) = TitaniumLoginCodec.decodePlayResponse(packet.payload)
      if success then
        // Key was already saved from LoginAccepted — emit it now
        emit(LoginEvent.PlayApproved(worldKey))
        // Titanium has no LoginComplete step — go straight to Complete
        state = LoginState.Complete
        emit(LoginEvent.LoginComplete)
        emit(LoginEvent.StateChanged(state))
      else
        state = LoginState.Failed
        emit(LoginEvent.Error("Server rejected play request"))
        emit(LoginEvent.StateChanged(state))

    else if op == TitaniumLoginOpcodes.Poll then
      // Server keepalive poll — respond with PollResponse
      queueAppPacket(TitaniumLoginOpcodes.PollResponse, Array.emptyByteArray)

    else
      println(f"[LoginClient] Unknown Titanium opcode: 0x${op & 0xFFFF}%04x " +
        s"(${TitaniumLoginOpcodes.name(op)}) len=${packet.payload.length}")

  // ---- Common ----

  /** Called periodically from network thread. Produces ACK if needed (Mac only). */
  def tick(): Unit =
    if Game.macMode && needArsp then
      val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
      outQueue.add(ack)
      needArsp = false

  /** Dequeue next outgoing raw packet. Called from network thread (Mac transport). */
  def pollOutgoing(): Option[Array[Byte]] =
    Option(outQueue.poll())

  /** Poll next event. Called from game thread. */
  def pollEvent(): Option[LoginEvent] =
    // Drain network errors into events
    var err = errors.poll()
    while err != null do
      events.add(LoginEvent.Error(err))
      err = errors.poll()
    Option(events.poll())

  /** Queue an app-level packet for the appropriate transport.
    * Mac: OldPacket-encode and add to outQueue.
    * Titanium: add (opcode, payload) to appOutQueue for TitaniumNetworkThread.
    */
  private def queueAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    if Game.macMode then
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
    else
      appOutQueue.add((opcode, payload))

  private def nextSeq(): Int =
    val s = outSeq
    outSeq = (outSeq + 1) & 0xFFFF
    s

  private def nextArq(): Int =
    val a = outArq
    outArq = (outArq + 1) & 0xFFFF
    a

  private def emit(event: LoginEvent): Unit = events.add(event)
