package opennorrath.network.titanium

import opennorrath.network.WorldOpcodes

/** Application opcodes for the Titanium (PC) world server protocol.
  * Values from EQEmu/utils/patches/patch_Titanium.conf.
  * These are little-endian wire values (EqStream uses LE opcodes).
  */
object TitaniumWorldOpcodes extends WorldOpcodes:
  val SendLoginInfo: Short   = 0x4dd0.toShort  // Client -> World: auth
  val GuildsList: Short      = 0x6957.toShort  // World -> Client
  val LogServer: Short       = 0x0fa6.toShort  // World -> Client
  val ApproveWorld: Short    = 0x3c25.toShort  // World -> Client
  val EnterWorld: Short      = 0x7cba.toShort  // Client -> World: enter with char name
  val SendCharInfo: Short    = 0x4513.toShort  // World -> Client: character select data
  val ExpansionInfo: Short   = 0x04ec.toShort  // World -> Client
  val ZoneServerInfo: Short  = 0x61b6.toShort  // World -> Client: zone IP+port
  val MOTD: Short            = 0x024d.toShort  // World -> Client: message of the day
  val SetChatServer: Short   = 0x00d7.toShort  // World -> Client
  val ZoneUnavail: Short     = 0x407c.toShort  // World -> Client
  val ApproveName: Short     = 0x3ea6.toShort  // Client -> World: name check
  val CharacterCreate: Short = 0x10b2.toShort  // Client -> World: create character
  val DeleteCharacter: Short = 0x26c9.toShort  // Client -> World: delete character

  def name(op: Short): String = op match
    case SendLoginInfo   => "SendLoginInfo"
    case GuildsList      => "GuildsList"
    case LogServer       => "LogServer"
    case ApproveWorld    => "ApproveWorld"
    case EnterWorld      => "EnterWorld"
    case SendCharInfo    => "SendCharInfo"
    case ExpansionInfo   => "ExpansionInfo"
    case ZoneServerInfo  => "ZoneServerInfo"
    case MOTD            => "MOTD"
    case SetChatServer   => "SetChatServer"
    case ZoneUnavail     => "ZoneUnavail"
    case ApproveName     => "ApproveName"
    case CharacterCreate => "CharacterCreate"
    case DeleteCharacter => "DeleteCharacter"
    case other           => f"Unknown(0x${other & 0xFFFF}%04x)"
