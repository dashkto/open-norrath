package opennorrath.network

/** Shared trait for zone server opcodes — implemented by Mac and Titanium.
  * Contains all opcodes used by ZoneClient's outgoing methods and zone entry handshake,
  * allowing protocol-agnostic dispatch without `if Game.macMode` branching.
  */
trait ZoneOpcodes:
  def name(op: Short): String

  // --- Zone Entry Handshake ---
  def ZoneEntry: Short
  def DataRate: Short         // Mac only — Titanium returns 0
  def SetServerFilter: Short
  def ReqNewZone: Short
  def ReqClientSpawn: Short
  def SendExpZonein: Short

  // --- Movement ---
  def ClientUpdate: Short

  // --- Targeting ---
  def TargetMouse: Short
  def Consider: Short

  // --- Chat ---
  def ChannelMessage: Short

  // --- Combat ---
  def AutoAttack: Short

  // --- Appearance ---
  def SpawnAppearance: Short
  def FaceChange: Short

  // --- Social ---
  def WhoAllRequest: Short

  // --- Zone Movement / Logout ---
  def Camp: Short
  def Logout: Short
  def ZoneChange: Short
  def Save: Short
  def Jump: Short
  def Consume: Short

  // --- Spells ---
  def MemorizeSpell: Short
  def CastSpell: Short

  // --- Items ---
  def MoveItem: Short

  // --- Merchant ---
  def ShopRequest: Short
  def ShopPlayerBuy: Short
  def ShopEnd: Short

  // --- Group ---
  def GroupDisband: Short
  def GroupInvite: Short
  def GroupFollow: Short
  def GroupCancelInvite: Short

  // --- Loot ---
  def LootRequest: Short
  def LootItem: Short
  def EndLootRequest: Short
