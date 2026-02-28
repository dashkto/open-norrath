package opennorrath.network

/** Shared trait for world server opcodes — implemented by Mac and Titanium.
  * All opcodes used by WorldClient for protocol-agnostic dispatch.
  */
trait WorldOpcodes:
  def name(op: Short): String

  // Client → World
  def SendLoginInfo: Short
  def EnterWorld: Short
  def ApproveName: Short
  def CharacterCreate: Short
  def DeleteCharacter: Short

  // World → Client
  def GuildsList: Short
  def LogServer: Short
  def ApproveWorld: Short
  def SendCharInfo: Short
  def ExpansionInfo: Short
  def ZoneServerInfo: Short
  def MOTD: Short
  def SetChatServer: Short
  def ZoneUnavail: Short
