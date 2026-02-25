package opennorrath.network

/** Application opcodes for the EQ Mac "old" login protocol.
  * Values match login_opcodes_oldver.conf — these are the wire values
  * as read/written with big-endian ByteBuffer (equivalent to ntohs on LE).
  */
object LoginOpcodes:
  val SessionReady: Short          = 0x5900.toShort
  val LoginOSX: Short              = 0x8e00.toShort
  val LoginPC: Short               = 0x0100.toShort
  val ClientError: Short           = 0x0200.toShort
  val LoginAccepted: Short         = 0x0400.toShort
  val ServerListRequest: Short     = 0x4600.toShort
  val PlayEverquestRequest: Short  = 0x4700.toShort
  val LoginUnknown1: Short         = 0x4800.toShort
  val ServerName: Short            = 0x4900.toShort
  val LoginUnknown2: Short         = 0x4a00.toShort
  val LoginBanner: Short           = 0x5200.toShort
  val LoginComplete: Short         = 0x8800.toShort

  def name(op: Short): String = op match
    case SessionReady         => "SessionReady"
    case LoginOSX             => "LoginOSX"
    case LoginAccepted        => "LoginAccepted"
    case ServerListRequest    => "ServerListRequest"
    case PlayEverquestRequest => "PlayEverquestRequest"
    case ServerName           => "ServerName"
    case ClientError          => "ClientError"
    case LoginBanner          => "LoginBanner"
    case LoginComplete        => "LoginComplete"
    case other                => f"Unknown(0x${other & 0xFFFF}%04x)"
