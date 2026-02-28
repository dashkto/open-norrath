package opennorrath.network.titanium

/** Application opcodes for the Titanium (PC) login protocol.
  * Values from EQEmu/loginserver/login_util/login_opcodes.conf.
  * These are little-endian wire values (EqStream uses LE opcodes).
  */
object TitaniumLoginOpcodes:
  // Client -> Server
  val SessionReady: Short          = 0x0001.toShort
  val Login: Short                 = 0x0002.toShort  // DES-encrypted credentials
  val ServerListRequest: Short     = 0x0004.toShort
  val PlayEverquestRequest: Short  = 0x000d.toShort
  val PollResponse: Short          = 0x0011.toShort

  // Server -> Client
  val ChatMessage: Short           = 0x0016.toShort  // Handshake reply to SessionReady
  val LoginAccepted: Short         = 0x0017.toShort  // Login success/fail (DES-encrypted)
  val ServerListResponse: Short    = 0x0018.toShort
  val PlayEverquestResponse: Short = 0x0021.toShort
  val Poll: Short                  = 0x0029.toShort

  def name(op: Short): String = op match
    case SessionReady          => "SessionReady"
    case Login                 => "Login"
    case ServerListRequest     => "ServerListRequest"
    case PlayEverquestRequest  => "PlayEverquestRequest"
    case ChatMessage           => "ChatMessage"
    case LoginAccepted         => "LoginAccepted"
    case ServerListResponse    => "ServerListResponse"
    case PlayEverquestResponse => "PlayEverquestResponse"
    case Poll                  => "Poll"
    case other                 => f"Unknown(0x${other & 0xFFFF}%04x)"
