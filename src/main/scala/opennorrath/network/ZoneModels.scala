package opennorrath.network

/** Color tint for a single equipment slot (BGRA order on wire). */
case class TintColor(red: Int, green: Int, blue: Int, useTint: Boolean)

/** Equipment tints for all 9 visible slots.
  * Slots: 0=Head, 1=Chest, 2=Arms, 3=Wrist, 4=Hands, 5=Legs, 6=Feet, 7=Primary, 8=Secondary
  */
case class TintProfile(slots: Array[TintColor]):
  def head: TintColor = slots(0)
  def chest: TintColor = slots(1)
  def arms: TintColor = slots(2)
  def wrist: TintColor = slots(3)
  def hands: TintColor = slots(4)
  def legs: TintColor = slots(5)
  def feet: TintColor = slots(6)
  def primary: TintColor = slots(7)
  def secondary: TintColor = slots(8)

object TintProfile:
  val Empty: TintProfile = TintProfile(Array.fill(9)(TintColor(0, 0, 0, false)))

// =============================================================================
// Spawn Data — used by rendering system for entity display
// =============================================================================

/** Full spawn data from OP_NewSpawn / OP_ZoneSpawns.
  * Mac Spawn_Struct is 224 bytes (packed).
  *
  * This is the primary model the rendering system uses to display entities.
  */
case class SpawnData(
  spawnId: Int,           // Unique entity ID in zone
  name: String,           // Display name (up to 63 chars)
  lastName: String,       // Surname / guild tag

  // Position
  y: Float,               // EQ Y (server coords)
  x: Float,               // EQ X
  z: Float,               // EQ Z
  heading: Int,           // 0-255 (256 = 360 degrees)

  // Velocity (for interpolation)
  deltaY: Float,
  deltaX: Float,
  deltaZ: Float,
  deltaHeading: Int,

  // Identity
  race: Int,              // Race ID (determines model: 1=Human, 2=Barbarian, etc.)
  classId: Int,           // Class ID (1=Warrior, etc.)
  gender: Int,            // 0=Male, 1=Female, 2=Neutral
  level: Int,
  bodytype: Int,          // Body type for targeting rules
  deity: Int,

  // Type
  npcType: Int,           // 0=Player, 1=NPC, 2=Player Corpse, 3=Monster Corpse, 10=Self
  petOwnerId: Int,        // Spawn ID of pet owner (0 if not pet)

  // Appearance — used by rendering for model selection
  face: Int,
  hairColor: Int,
  beardColor: Int,
  eyeColor1: Int,
  eyeColor2: Int,
  hairStyle: Int,
  beard: Int,
  bodyTexture: Int,       // Armor texture variant
  helm: Int,              // Helm model

  // Equipment models — rendering needs these for visible gear
  equipment: Array[Int],  // 9 slots: helm/chest/arm/bracer/hand/leg/boot/melee1/melee2
  equipColors: TintProfile,

  // Movement
  size: Float,            // Model scale
  walkSpeed: Float,
  runSpeed: Float,
  animation: Int,         // Current animation state

  // Flags
  light: Int,             // Light emission radius
  flyMode: Int,           // 0=ground, 1=flying, 2=levitating
  isInvis: Boolean,
  isSneaking: Boolean,
  isPvp: Boolean,
  isAfk: Boolean,
  isLd: Boolean,
  isGm: Boolean,
  anon: Int,              // 0=normal, 1=anon, 2=RP
  guildId: Int,
  guildRank: Int,
  standState: Int,        // For sit/stand animation
)

object SpawnData:
  /** NPC type constants */
  val TypePlayer = 0
  val TypeNPC = 1
  val TypePlayerCorpse = 2
  val TypeMonsterCorpse = 3
  val TypeSelf = 10

// =============================================================================
// Position Update — used by rendering for entity movement
// =============================================================================

/** Mob position update from OP_MobUpdate (SpawnPositionUpdate_Struct, 15 bytes).
  * Compact format for frequent position broadcasts.
  */
case class MobPositionUpdate(
  spawnId: Int,
  y: Float,               // EQ server coordinates
  x: Float,
  z: Float,
  heading: Int,           // 0-255
  deltaHeading: Int,
  animType: Int,
  // Packed velocity (11:11:10 bit field)
  deltaY: Float,
  deltaX: Float,
  deltaZ: Float,
)

/** Player's own position update sent via OP_ClientUpdate.
  * This is what we send to the server each tick.
  */
case class PlayerPosition(
  spawnId: Int,
  y: Float,
  x: Float,
  z: Float,
  heading: Float,         // Floating point heading for client
  deltaY: Float,
  deltaX: Float,
  deltaZ: Float,
  deltaHeading: Float,
  animation: Int,
)

// =============================================================================
// Zone Metadata — used by rendering for fog, sky, clip, safe point
// =============================================================================

/** Zone metadata from OP_NewZone (Mac NewZone_Struct, 572 bytes). */
case class NewZoneInfo(
  charName: String,
  zoneShortName: String,  // e.g., "qeynos"
  zoneLongName: String,   // e.g., "South Qeynos"
  zoneType: Int,

  // Fog — 4 layers (weather variants)
  fogRed: Array[Int],     // 4 values, 0-255
  fogGreen: Array[Int],
  fogBlue: Array[Int],
  fogMinClip: Array[Float],
  fogMaxClip: Array[Float],

  // Physics
  gravity: Float,

  // Sky
  sky: Int,               // Sky type index
  timeType: Int,

  // Weather
  rainChance: Array[Int],
  rainDuration: Array[Int],
  snowChance: Array[Int],
  snowDuration: Array[Int],

  // View
  minClip: Float,
  maxClip: Float,

  // Safe point — where to respawn
  safeY: Float,
  safeX: Float,
  safeZ: Float,

  // Bounds
  maxZ: Float,
  underworld: Float,      // Min Z before death

  // Multiplier
  expMultiplier: Float,
)

// =============================================================================
// Player Profile — massive struct, used by UI for character sheet
// =============================================================================

/** Condensed player profile from OP_PlayerProfile (8460 bytes).
  * We extract the fields that the UI and game logic need.
  */
case class PlayerProfileData(
  name: String,
  lastName: String,
  race: Int,
  classId: Int,
  gender: Int,
  level: Int,
  exp: Int,
  points: Int,            // Unspent practice points
  mana: Int,
  curHp: Int,

  // Stats
  str: Int,
  sta: Int,
  cha: Int,
  dex: Int,
  int_ : Int,
  agi: Int,
  wis: Int,

  // Position in zone
  y: Float,
  x: Float,
  z: Float,
  heading: Float,
  zoneId: Int,

  // Money
  platinum: Int,
  gold: Int,
  silver: Int,
  copper: Int,
  platinumBank: Int,
  goldBank: Int,
  silverBank: Int,
  copperBank: Int,

  // Appearance
  face: Int,
  hairColor: Int,
  beardColor: Int,
  eyeColor1: Int,
  eyeColor2: Int,
  hairStyle: Int,
  beard: Int,
  deity: Int,

  // Guild
  guildId: Int,
  guildRank: Int,

  // Spell book (256 slots, -1 = empty)
  spellBook: Array[Int],

  // Spells memorized (8 gem slots)
  memSpells: Array[Int],

  // Skills (first 75 are tradeable skills)
  skills: Array[Int],

  // Buffs
  buffs: Array[SpellBuff],

  // Hunger / thirst
  hungerLevel: Int,
  thirstLevel: Int,

  // AA
  aaExp: Int,
  aaPoints: Int,
  aaPercentage: Int,

  // Expansion
  expansions: Int,

  // Bind points
  bindZones: Array[Int],
  bindY: Array[Float],
  bindX: Array[Float],
  bindZ: Array[Float],

  // Group members
  groupMembers: Array[String],
)

/** Active spell buff on the player. */
case class SpellBuff(
  buffType: Int,          // 0=invisible, 1=permanent, 2=timed
  level: Int,             // Caster level
  bardModifier: Int,      // Instrument modifier
  spellId: Int,
  duration: Int,          // Ticks remaining
  counters: Int,          // Rune/disease/poison counters
)

// =============================================================================
// Combat / Damage — used by UI for combat log and floating text
// =============================================================================

/** Damage message from OP_Damage. */
case class DamageInfo(
  targetId: Int,          // Entity being hit
  sourceId: Int,          // Entity dealing damage
  damageType: Int,        // Type of damage (skill type)
  spellId: Int,           // Spell ID if spell damage (0xFFFF if melee)
  damage: Int,            // Damage amount (0 = miss)
  force: Float,           // Knockback force
  pushHeading: Float,     // Knockback direction
  pushUpAngle: Float,     // Knockback vertical angle
)

/** Death event from OP_Death. */
case class DeathInfo(
  spawnId: Int,           // Who died
  killerId: Int,          // Who killed them
  corpseId: Int,          // Corpse spawn ID
  spawnLevel: Int,
  spellId: Int,           // Killing spell (-1 if melee)
  attackSkill: Int,
  damage: Int,
  isPC: Boolean,          // Was it a player?
)

/** Consider result from OP_Consider. */
case class ConsiderResult(
  playerId: Int,          // Who considered
  targetId: Int,          // Who was considered
  faction: Int,           // Faction standing
  level: Int,             // Target level
  curHp: Int,
  maxHp: Int,
  pvpCon: Boolean,        // PvP flagged?
)

/** Spell action from OP_Action — triggers spell visual effects. */
case class SpellAction(
  targetId: Int,
  sourceId: Int,
  level: Int,
  instrumentMod: Int,
  force: Float,
  pushHeading: Float,
  pushUpAngle: Float,
  actionType: Int,        // 231 for spells
  spellId: Int,
  tapAmount: Int,         // For lifetap spells
)

// =============================================================================
// HP / Mana / Exp Updates — used by UI for bars
// =============================================================================

/** HP update from OP_HPUpdate (SpawnHPUpdate_Struct, 12 bytes). */
case class HPUpdate(
  spawnId: Int,
  curHp: Int,
  maxHp: Int,
)

/** Mana update from OP_ManaChange. */
case class ManaChange(
  spawnId: Int,
  curMana: Int,
)

/** Experience update from OP_ExpUpdate. */
case class ExpChange(
  exp: Int,               // Current total experience
)

/** Level update from OP_LevelUpdate. */
case class LevelChange(
  level: Int,
  oldLevel: Int,
  exp: Int,
)

/** Skill update from OP_SkillUpdate. */
case class SkillChange(
  skillId: Int,
  value: Int,
)

// =============================================================================
// Chat — used by UI for chat window
// =============================================================================

/** Chat message from OP_ChannelMessage. */
case class ChatMessage(
  sender: String,
  target: String,         // For tells
  language: Int,
  channel: Int,           // 0=guild, 2=group, 3=shout, 4=auction, 5=ooc, 7=tell, 8=say
  languageSkill: Int,
  message: String,
)

object ChatMessage:
  // Channel constants
  val Guild = 0
  val Group = 2
  val Shout = 3
  val Auction = 4
  val OOC = 5
  val Broadcast = 6
  val Tell = 7
  val Say = 8
  val GMSay = 11

// =============================================================================
// Spawn Appearance — used by rendering for state changes
// =============================================================================

/** Appearance change from OP_SpawnAppearance.
  * Controls state like sit/stand, animation, hp display, etc.
  */
case class SpawnAppearanceChange(
  spawnId: Int,
  appearanceType: Int,    // What is changing
  parameter: Int,         // New value
)

object SpawnAppearanceChange:
  // Appearance type constants
  val Die = 0
  val WhoLevel = 1
  val MaxHP = 2          // Sends 100 for 100%
  val Invis = 3
  val PVP = 4
  val Light = 5
  val Animation = 14
  // Animation parameter values
  val AnimStand = 100
  val AnimLoot = 105
  val AnimSit = 110
  val AnimCrouch = 111
  val AnimDead = 120
  val Sneak = 15
  val SpawnID = 16
  val HPPercent = 17
  val LinkDead = 18
  val FlyMode = 19       // 0=ground, 1=flying, 2=levitate
  val GM = 20
  val Anon = 21
  val GuildID = 22
  val GuildRank = 23
  val AFK = 24
  val Pet = 25
  val Split = 28
  val Size = 29
  val NPCName = 31

// =============================================================================
// Equipment Visual — used by rendering for gear display
// =============================================================================

/** Equipment visual change from OP_WearChange. */
case class WearChangeInfo(
  spawnId: Int,
  wearSlot: Int,          // 0-8: head/chest/arms/wrist/hands/legs/feet/primary/secondary
  material: Int,          // Equipment model ID
  color: TintColor,
)

/** Animation trigger from OP_Animation. */
case class AnimationInfo(
  spawnId: Int,
  targetId: Int,
  action: Int,            // Animation action type
  value: Int,
)

// =============================================================================
// Environment — used by rendering for atmosphere
// =============================================================================

/** Game time from OP_TimeOfDay. */
case class GameTime(
  hour: Int,              // 0-23
  minute: Int,            // 0-59
  day: Int,
  month: Int,
  year: Int,
)

/** Weather from OP_Weather. */
case class WeatherInfo(
  weatherType: Int,       // 0=none, 1=rain, 2=snow
  intensity: Int,         // 0-3
)

// =============================================================================
// Zone Objects — used by rendering for doors and items
// =============================================================================

/** Door/object in zone from OP_SpawnDoor (Door_Struct, 44 bytes). */
case class DoorData(
  name: String,           // Filename/model name
  y: Float,               // EQ coordinates
  x: Float,
  z: Float,
  heading: Float,
  incline: Int,
  size: Int,
  doorId: Int,            // Unique door ID
  openType: Int,          // How it opens
  isOpen: Boolean,
  inverted: Boolean,
  parameter: Int,         // e.g., destination zone ID
)

/** Ground item from OP_GroundSpawn (Object_Struct, 224 bytes). */
case class GroundItemData(
  itemId: Int,
  dropId: Int,            // Click target ID
  zoneId: Int,
  charges: Int,
  maxCharges: Int,
  heading: Float,
  y: Float,               // EQ coordinates
  x: Float,
  z: Float,
  objectName: String,     // Model name (e.g., "IT63" for a weapon)
  objectType: Int,        // 1=generic, 2=armor, 0x19=weapon, 0xff=tradeskill
)

/** Zone line / teleport point from OP_SendZonepoints. */
case class ZonePointData(
  iterator: Int,          // Zone point index
  y: Float,
  x: Float,
  z: Float,
  heading: Float,
  targetZoneId: Int,      // Destination zone
)

// =============================================================================
// Zone Change — used for teleportation
// =============================================================================

/** Zone change request from OP_RequestClientZoneChange. */
case class ZoneChangeRequest(
  zoneId: Int,
  y: Float,
  x: Float,
  z: Float,
  heading: Float,
)

/** Zone change confirmation from OP_ZoneChange. */
case class ZoneChangeResult(
  charName: String,
  zoneId: Int,
  zoneReason: Int,
  success: Int,           // 0=pending, 1=success, negative=error
)

// =============================================================================
// Inventory Items — decoded from OP_CharInventory
// =============================================================================

/** An item from the player's inventory.
  *
  * Decoded from Item_Struct (360 bytes) in mac_structs.h.
  * Only the fields needed for UI display are extracted.
  */
case class InventoryItem(
  name: String,
  lore: String,
  equipSlot: Int,         // Current slot: 0-21=equipment, 22-29=general, 250+=bag contents
  itemClass: Int,         // 0=common, 1=container, 2=book
  id: Int,                // Item ID
  icon: Int,              // Icon number
  slots: Int,             // Bitmask of valid equipment slots (bit N = slot N allowed)
  weight: Int,            // Weight (tenths of a unit)
  noRent: Boolean,
  noDrop: Boolean,
  magic: Boolean,
  // Common item stats (itemClass == 0)
  aStr: Int, aSta: Int, aCha: Int, aDex: Int, aInt: Int, aAgi: Int, aWis: Int,
  mr: Int, fr: Int, cr: Int, dr: Int, pr: Int,
  ac: Int,
  hp: Int,
  mana: Int,
  damage: Int,
  delay: Int,
  charges: Int,
  stackable: Boolean,
  idFileNum: Int = 0,      // IT number from IDFile (e.g., "IT27" → 27) — used for weapon model lookup
):
  def stackCount: Int = if stackable && charges > 1 then charges else 0
  /** Check if this item can be placed in the given equipment slot (0-21). */
  def canEquipIn(slotId: Int): Boolean =
    if slotId < 0 || slotId > 21 then true // general/bag slots always allowed
    else (slots & (1 << slotId)) != 0

object InventoryItem:
  // Equipment slot constants (Mac client slot IDs)
  val Charm     = 0
  val EarL      = 1
  val Head      = 2
  val Face      = 3
  val EarR      = 4
  val Neck      = 5
  val Shoulders = 6
  val Arms      = 7
  val Back      = 8
  val WristL    = 9
  val WristR    = 10
  val Range     = 11
  val Hands     = 12
  val Primary   = 13
  val Secondary = 14
  val RingL     = 15
  val RingR     = 16
  val Chest     = 17
  val Legs      = 18
  val Feet      = 19
  val Waist     = 20
  val Ammo      = 21

  /** All equipment slot IDs in display order, with labels. */
  val equipmentSlots: Vector[(Int, String)] = Vector(
    Charm     -> "Charm",
    EarL      -> "Ear (L)",
    Head      -> "Head",
    Face      -> "Face",
    EarR      -> "Ear (R)",
    Neck      -> "Neck",
    Shoulders -> "Shoulders",
    Arms      -> "Arms",
    Back      -> "Back",
    WristL    -> "Wrist (L)",
    WristR    -> "Wrist (R)",
    Range     -> "Range",
    Hands     -> "Hands",
    Primary   -> "Primary",
    Secondary -> "Secondary",
    RingL     -> "Ring (L)",
    RingR     -> "Ring (R)",
    Chest     -> "Chest",
    Legs      -> "Legs",
    Feet      -> "Feet",
    Waist     -> "Waist",
    Ammo      -> "Ammo",
  )
