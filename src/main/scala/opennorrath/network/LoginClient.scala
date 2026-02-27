package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable

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
  * Thread safety model:
  * - Game thread calls: connect(), selectServer(), pollEvent(), state
  * - Network thread calls: handlePacket(), tick(), pollOutgoing()
  * - Cross-thread communication via ConcurrentLinkedQueue (lock-free)
  */
class LoginClient extends PacketHandler:
  @volatile var state: LoginState = LoginState.Disconnected
  val events = ConcurrentLinkedQueue[LoginEvent]()
  val errors = ConcurrentLinkedQueue[String]()

  // Outgoing packet queue (network thread drains this)
  private val outQueue = ConcurrentLinkedQueue[Array[Byte]]()

  // Sequence tracking (network thread only)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true

  // Login data
  var sessionId: String = ""
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
    pendingUser = user
    pendingPass = pass
    state = LoginState.Connecting
    emit(LoginEvent.StateChanged(state))
    // Send OP_SessionReady as first packet
    queueAppPacket(LoginOpcodes.SessionReady, LoginCodec.encodeSessionReady)

  /** Request to play on a specific server. Called from game thread. */
  def selectServer(ip: String): Unit =
    state = LoginState.PlayRequested
    emit(LoginEvent.StateChanged(state))
    queueAppPacket(LoginOpcodes.PlayEverquestRequest, LoginCodec.encodePlayRequest(ip))

  /** Called from network thread when a decoded packet arrives. */
  def handlePacket(packet: InboundPacket): Unit =
    // Track ARQs for acknowledgment
    packet.arq.foreach { arq =>
      lastInArq = arq
      needArsp = true
    }

    if packet.opcode == 0 then return // pure ACK

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

  /** Called periodically from network thread. Produces ACK if needed. */
  def tick(): Unit =
    if needArsp then
      val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
      outQueue.add(ack)
      needArsp = false

  /** Dequeue next outgoing raw packet. Called from network thread. */
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

  private def emit(event: LoginEvent): Unit = events.add(event)
