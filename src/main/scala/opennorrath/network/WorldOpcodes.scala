package opennorrath.network

/** Application opcodes for the EQ Mac world server protocol.
  * Values from EQMacDocker/Server/utils/patches/patch_Mac.conf.
  * These are big-endian wire values (matching how OldPacket reads opcodes).
  */
object WorldOpcodes:
  val SendLoginInfo: Short   = 0x5818.toShort  // Client → World: auth
  val GuildsList: Short      = 0x9241.toShort  // World → Client
  val LogServer: Short       = 0xc341.toShort  // World → Client
  val ApproveWorld: Short    = 0x0710.toShort  // World → Client
  val EnterWorld: Short      = 0x0180.toShort  // Client → World: enter with char name
  val SendCharInfo: Short    = 0x4740.toShort  // World → Client: character select data
  val ExpansionInfo: Short   = 0xd841.toShort  // World → Client
  val ZoneServerInfo: Short  = 0x0480.toShort  // World → Client: zone IP+port
  val MOTD: Short            = 0xdd41.toShort  // World → Client: message of the day
  val SetChatServer: Short   = 0x0980.toShort  // World → Client
  val ZoneUnavail: Short     = 0x0580.toShort  // World → Client
  val WorldLogout: Short     = 0x2340.toShort  // Client → World
  val ClientError: Short     = 0x4841.toShort  // Client → World
  val ApproveName: Short     = 0x8B40.toShort  // Client → World: name check
  val CharacterCreate: Short = 0x4940.toShort  // Client → World: create character
  val DeleteCharacter: Short = 0x5a40.toShort  // Client → World: delete character

  def name(op: Short): String = op match
    case SendLoginInfo  => "SendLoginInfo"
    case GuildsList     => "GuildsList"
    case LogServer      => "LogServer"
    case ApproveWorld   => "ApproveWorld"
    case EnterWorld     => "EnterWorld"
    case SendCharInfo   => "SendCharInfo"
    case ExpansionInfo  => "ExpansionInfo"
    case ZoneServerInfo => "ZoneServerInfo"
    case MOTD           => "MOTD"
    case SetChatServer  => "SetChatServer"
    case ZoneUnavail    => "ZoneUnavail"
    case WorldLogout    => "WorldLogout"
    case ClientError    => "ClientError"
    case ApproveName    => "ApproveName"
    case CharacterCreate => "CharacterCreate"
    case DeleteCharacter => "DeleteCharacter"
    case other          => f"Unknown(0x${other & 0xFFFF}%04x)"
