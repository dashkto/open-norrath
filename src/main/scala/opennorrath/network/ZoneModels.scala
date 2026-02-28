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
  var duration: Int,      // Ticks remaining (ticked down client-side, 1 tick = 6 sec)
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
  damage: Int,            // Damage amount (0 = miss, <1 ignored by UI)
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

/** Consider result from OP_Consider.
  *
  * The `conLevel` field is NOT the target's actual level — it's a con color code
  * from GetLevelCon(): Green=2, Blue=4, Red=13, Yellow=15, LightBlue=18, White=20.
  *
  * The `faction` field is the faction standing (1=Ally, 2=Warmly, 3=Kindly,
  * 4=Amiably, 5=Indifferent, 6=Apprehensive, 7=Dubious, 8=Threatening, 9=Scowls).
  * NOTE: the server swaps Apprehensive<->Scowls and Dubious<->Threatening on the wire,
  * so the values arrive pre-swapped and can be used directly.
  */
case class ConsiderResult(
  playerId: Int,          // Who considered
  targetId: Int,          // Who was considered
  faction: Int,           // Faction standing (wire-swapped, use directly)
  conLevel: Int,          // Con color code (NOT actual level)
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
  buffUnknown: Int,       // 1 = cast begin, 4 = spell landed/success
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

/** Mana update from OP_ManaChange or OP_ManaUpdate.
  * For OP_ManaChange (self-only): spawnId is 0, curMana is the new mana value.
  * For OP_ManaUpdate: spawnId is the entity, curMana is the current mana.
  */
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

/** Stamina update from OP_Stamina. Server sends every ~46 seconds as hunger/thirst tick down. */
case class StaminaInfo(
  food: Int,      // Hunger level (0–32000, auto-eat threshold = 3000, famished = 0)
  water: Int,     // Thirst level (0–32000, auto-drink threshold = 3000, famished = 0)
  fatigue: Int,   // Fatigue level (0–100)
)

/** Begin-cast notification from OP_BeginCast (8 bytes).
  * Sent when any entity starts casting a spell — used for cast bar / animation.
  */
case class BeginCast(
  casterId: Int,   // Spawn ID of caster
  spellId: Int,    // Spell being cast
  castTime: Int,   // Cast time in milliseconds
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

/** MT_* message type codes from eq_constants.h. Used by OP_SpecialMesg and OP_FormattedMessage. */
enum MessageType(val code: Int):
  // Chat channels
  case Say              extends MessageType(256)
  case Tell             extends MessageType(257)
  case Group            extends MessageType(258)
  case Guild            extends MessageType(259)
  case OOC              extends MessageType(260)
  case Auction          extends MessageType(261)
  case Shout            extends MessageType(262)
  case Emote            extends MessageType(263)
  // Combat & spells
  case Spells           extends MessageType(264)
  case YouHitOther      extends MessageType(265)
  case OtherHitsYou     extends MessageType(266)
  case YouMissOther     extends MessageType(267)
  case OtherMissesYou   extends MessageType(268)
  case Broadcasts       extends MessageType(269)
  case Skills           extends MessageType(270)
  case Disciplines      extends MessageType(271)
  // 272 = unused
  case DefaultText      extends MessageType(273)
  // 274 = unused
  case MerchantOffer    extends MessageType(275)
  case MerchantBuySell  extends MessageType(276)
  case YourDeath        extends MessageType(277)
  case OtherDeath       extends MessageType(278)
  case OtherHits        extends MessageType(279)
  case OtherMisses      extends MessageType(280)
  case Who              extends MessageType(281)
  case YellForHelp      extends MessageType(282)
  case NonMelee         extends MessageType(283)
  case WornOff          extends MessageType(284)
  case MoneySplit        extends MessageType(285)
  case LootMessages     extends MessageType(286)
  case DiceRoll         extends MessageType(287)
  case OtherSpells      extends MessageType(288)
  case SpellFailure     extends MessageType(289)
  // Custom chat channels
  case Chat             extends MessageType(290)
  case Channel1         extends MessageType(291)
  case Channel2         extends MessageType(292)
  case Channel3         extends MessageType(293)
  case Channel4         extends MessageType(294)
  case Channel5         extends MessageType(295)
  case Channel6         extends MessageType(296)
  case Channel7         extends MessageType(297)
  case Channel8         extends MessageType(298)
  case Channel9         extends MessageType(299)
  case Channel10        extends MessageType(300)
  // Crits & special combat
  case CritMelee        extends MessageType(301)
  case SpellCrits       extends MessageType(302)
  case TooFarAway       extends MessageType(303)
  case NPCRampage       extends MessageType(304)
  case NPCFlurry        extends MessageType(305)
  case NPCEnrage        extends MessageType(306)
  // Echo channels — your own messages echoed back
  case SayEcho          extends MessageType(307)
  case TellEcho         extends MessageType(308)
  case GroupEcho        extends MessageType(309)
  case GuildEcho        extends MessageType(310)
  case OOCEcho          extends MessageType(311)
  case AuctionEcho      extends MessageType(312)
  case ShoutEcho        extends MessageType(313)
  case EmoteEcho        extends MessageType(314)
  case Chat1Echo        extends MessageType(315)
  case Chat2Echo        extends MessageType(316)
  case Chat3Echo        extends MessageType(317)
  case Chat4Echo        extends MessageType(318)
  case Chat5Echo        extends MessageType(319)
  case Chat6Echo        extends MessageType(320)
  case Chat7Echo        extends MessageType(321)
  case Chat8Echo        extends MessageType(322)
  case Chat9Echo        extends MessageType(323)
  case Chat10Echo       extends MessageType(324)
  // DoTs, pets, misc
  case DoTDamage        extends MessageType(325)
  case ItemLink         extends MessageType(326)
  case RaidSay          extends MessageType(327)
  case MyPet            extends MessageType(328)
  case DamageShield     extends MessageType(329)
  case Leadership       extends MessageType(330)
  case PetFlurry        extends MessageType(331)
  case PetCrit          extends MessageType(332)
  case FocusEffect      extends MessageType(333)
  case Experience       extends MessageType(334)
  case System           extends MessageType(335)
  case PetSpell         extends MessageType(336)
  case PetResponse      extends MessageType(337)
  case ItemSpeech       extends MessageType(338)
  case StrikeThrough    extends MessageType(339)
  case Stun             extends MessageType(340)

object MessageType:
  private val byCode: Map[Int, MessageType] = values.map(v => v.code -> v).toMap
  def fromCode(code: Int): MessageType = byCode.getOrElse(code, DefaultText)

/** System message from OP_SpecialMesg. Server-generated text with msg_type color coding. */
case class SpecialMessage(msgType: Int, targetSpawnId: Int, sayer: String, message: String)

/** String-ID message from OP_FormattedMessage. Resolved via eqstr_us.txt client string table. */
case class FormattedMessage(stringId: Int, msgType: Int, arguments: Vector[String])

/** Emote text from OP_Emote. Broadcast contains sender name prepended to message. */
case class EmoteMessage(message: String)

/** Loot response from OP_MoneyOnCorpse (20 bytes).
  * response: 0=someone else looting, 1=success, 2=not allowed.
  * Money fields are only meaningful when response == 1.
  */
case class LootResponse(response: Int, platinum: Int, gold: Int, silver: Int, copper: Int)

// =============================================================================
// Spawn Appearance — used by rendering for state changes
// =============================================================================

/** Face/hair/beard change from OP_FaceChange.
  * 7 bytes, all uint8. No spawn ID — the server knows the sender, and broadcasts
  * the same struct to other clients in the zone.
  */
case class FaceChangeData(
  hairColor: Int,
  beardColor: Int,
  eyeColor1: Int,
  eyeColor2: Int,
  hairStyle: Int,
  beard: Int,     // Face overlay / woad (barbarian only)
  face: Int,
)

// =============================================================================

/** Appearance change from OP_SpawnAppearance.
  * Controls state like sit/stand, animation, hp display, etc.
  */
case class SpawnAppearanceChange(
  spawnId: Int,
  appearanceType: Int,    // What is changing (see AppearanceType)
  parameter: Int,         // New value
)

/** Appearance type codes from AT_* in eq_constants.h. */
enum AppearanceType(val code: Int):
  case Die         extends AppearanceType(0)   // Keel over and zone to bind
  case WhoLevel    extends AppearanceType(1)   // Level shown on /who
  case MaxHP       extends AppearanceType(2)   // Max HP value (sends 100 for 100%)
  case Invis       extends AppearanceType(3)   // 0=visible, 1=invisible
  case PVP         extends AppearanceType(4)   // 0=blue, 1=pvp (red)
  case Light       extends AppearanceType(5)   // Light type (lightstone, shiny shield)
  case Animation   extends AppearanceType(14)  // Stand/sit/loot state (see AnimState)
  case Sneak       extends AppearanceType(15)  // 0=normal, 1=sneaking
  case SpawnID     extends AppearanceType(16)  // Server assigns player spawn ID
  case HPPercent   extends AppearanceType(17)  // HP percentage update
  case LinkDead    extends AppearanceType(18)  // 0=normal, 1=linkdead
  case FlyMode     extends AppearanceType(19)  // 0=ground, 1=flying, 2=levitate
  case GM          extends AppearanceType(20)  // 0=normal, 1=GM
  case Anon        extends AppearanceType(21)  // 0=normal, 1=anon, 2=roleplay
  case GuildID     extends AppearanceType(22)  // Guild ID
  case GuildRank   extends AppearanceType(23)  // 0=member, 1=officer, 2=leader
  case AFK         extends AppearanceType(24)  // 0=normal, 1=afk
  case Pet         extends AppearanceType(25)  // Owner entity ID, 0=charm break
  case SummonedPC  extends AppearanceType(27)  // Summoned PC flag
  case Split       extends AppearanceType(28)  // 0=normal, 1=autosplit on
  case Size        extends AppearanceType(29)  // Spawn size
  case NPC         extends AppearanceType(30)  // Make entity an NPC
  case NPCName     extends AppearanceType(31)  // 0=normal name color, 1=NPC name color
  case DamageState extends AppearanceType(44)  // Destructible object damage state (0-4)

object AppearanceType:
  private val byCode: Map[Int, AppearanceType] = values.map(v => v.code -> v).toMap
  def fromCode(code: Int): Option[AppearanceType] = byCode.get(code)

/** Animation state values for AppearanceType.Animation parameter.
  * From AnimTypePositionEnum in eq_constants.h.
  */
enum AnimState(val code: Int):
  case Stand  extends AnimState(100)
  case Freeze extends AnimState(102)  // Stunned / rooted
  case Loot   extends AnimState(105)
  case Sit    extends AnimState(110)
  case Crouch extends AnimState(111)
  case Feign  extends AnimState(115)  // Feign death
  case Dead   extends AnimState(120)  // Corpse

object AnimState:
  private val byCode: Map[Int, AnimState] = values.map(v => v.code -> v).toMap
  def fromCode(code: Int): Option[AnimState] = byCode.get(code)

// Back-compat: keep old constant access on the companion so existing code still compiles.
// TODO: migrate callers to use AppearanceType / AnimState enums directly.
object SpawnAppearanceChange:
  val Die = AppearanceType.Die.code
  val WhoLevel = AppearanceType.WhoLevel.code
  val MaxHP = AppearanceType.MaxHP.code
  val Invis = AppearanceType.Invis.code
  val PVP = AppearanceType.PVP.code
  val Light = AppearanceType.Light.code
  val Animation = AppearanceType.Animation.code
  val AnimStand = AnimState.Stand.code
  val AnimLoot = AnimState.Loot.code
  val AnimSit = AnimState.Sit.code
  val AnimCrouch = AnimState.Crouch.code
  val AnimDead = AnimState.Dead.code
  val Sneak = AppearanceType.Sneak.code
  val SpawnID = AppearanceType.SpawnID.code
  val HPPercent = AppearanceType.HPPercent.code
  val LinkDead = AppearanceType.LinkDead.code
  val FlyMode = AppearanceType.FlyMode.code
  val GM = AppearanceType.GM.code
  val Anon = AppearanceType.Anon.code
  val GuildID = AppearanceType.GuildID.code
  val GuildRank = AppearanceType.GuildRank.code
  val AFK = AppearanceType.AFK.code
  val Pet = AppearanceType.Pet.code
  val Split = AppearanceType.Split.code
  val Size = AppearanceType.Size.code
  val NPCName = AppearanceType.NPCName.code

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

/** Game time from OP_TimeOfDay.
  * EQ time runs at 1 EQ minute = 3 real seconds (20x speed).
  */
case class GameTime(
  hour: Int,              // 1-24 (1=1am, 24=12am midnight)
  minute: Int,            // 0-59
  day: Int,               // 1-28
  month: Int,             // 1-12
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

// =============================================================================
// WhoAll — decoded from OP_WhoAllResponse
// =============================================================================

/** A single player entry from the WhoAll response.
  * The server sends variable-length records with null-terminated strings
  * packed inline (not at fixed offsets). See WhoAllPlayer in eq_packet_structs.h.
  */
case class WhoAllPlayerEntry(
  name: String,
  guild: String,          // Empty if no guild or hidden (anon)
  level: Int,
  classId: Int,
  race: Int,
  zone: Int,              // Zone ID (0 if hidden)
  account: String,        // Empty for non-GMs
  formatString: Int,      // Controls display format (anon, roleplay, GM, etc.)
  rankString: Int,        // GM rank string ID (0xFFFF = none)
  zoneString: Int,        // Zone string ID (0xFFFF = hidden)
)

/** Full WhoAll response from the server. */
case class WhoAllResponse(
  playerCount: Int,
  players: Vector[WhoAllPlayerEntry],
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
// Merchant — decoded from OP_ShopRequest / OP_ShopInventoryPacket
// =============================================================================

/** Server response to OP_ShopRequest (Merchant_Click_Struct, 12 bytes). */
case class MerchantOpen(
  merchantId: Int,        // NPC spawn ID
  rate: Float,            // Price modifier from server (faction/charisma adjusted)
)

// =============================================================================
// Inventory Items — decoded from OP_CharInventory
// =============================================================================

/** Item type / skill codes from ItemTypes enum in item_data.h.
  * Determines what the item "is" — weapon type, consumable, instrument, etc.
  */
enum ItemType(val code: Int, val label: String):
  case OneHandSlash          extends ItemType(0,  "1H Slashing")
  case TwoHandSlash          extends ItemType(1,  "2H Slashing")
  case OneHandPiercing       extends ItemType(2,  "1H Piercing")
  case OneHandBlunt          extends ItemType(3,  "1H Blunt")
  case TwoHandBlunt          extends ItemType(4,  "2H Blunt")
  case Bow                   extends ItemType(5,  "Bow")
  case Unknown1              extends ItemType(6,  "Unknown")
  case LargeThrowing         extends ItemType(7,  "Large Throwing")
  case Shield                extends ItemType(8,  "Shield")
  case Scroll                extends ItemType(9,  "Scroll")
  case Armor                 extends ItemType(10, "Armor")
  case Misc                  extends ItemType(11, "Misc")           // Catch-all for random items
  case LockPick              extends ItemType(12, "Lock Pick")
  case Unknown2              extends ItemType(13, "Unknown")
  case Food                  extends ItemType(14, "Food")
  case Drink                 extends ItemType(15, "Drink")
  case Light                 extends ItemType(16, "Light")
  case Combinable            extends ItemType(17, "Combinable")     // Not all stackable items are this type
  case Bandage               extends ItemType(18, "Bandage")
  case SmallThrowing         extends ItemType(19, "Small Throwing")
  case Spell                 extends ItemType(20, "Spell")          // Spells and tomes
  case Potion                extends ItemType(21, "Potion")
  case FletchedArrows        extends ItemType(22, "Fletched Arrows")
  case WindInstrument        extends ItemType(23, "Wind Instrument")
  case StringedInstrument    extends ItemType(24, "Stringed Instrument")
  case BrassInstrument       extends ItemType(25, "Brass Instrument")
  case PercussionInstrument  extends ItemType(26, "Percussion Instrument")
  case Arrow                 extends ItemType(27, "Arrow")
  case Unknown4              extends ItemType(28, "Unknown")
  case Jewelry               extends ItemType(29, "Jewelry")
  case Skull                 extends ItemType(30, "Skull")
  case Book                  extends ItemType(31, "Book")           // Skill-up tomes/books
  case Note                  extends ItemType(32, "Note")
  case Key                   extends ItemType(33, "Key")
  case Coin                  extends ItemType(34, "Coin")
  case TwoHandPiercing       extends ItemType(35, "2H Piercing")
  case FishingPole           extends ItemType(36, "Fishing Pole")
  case FishingBait           extends ItemType(37, "Fishing Bait")
  case Alcohol               extends ItemType(38, "Alcohol")
  case Key2                  extends ItemType(39, "Key")            // Questable keys / satchels
  case Compass               extends ItemType(40, "Compass")
  case Unknown5              extends ItemType(41, "Unknown")
  case Poison                extends ItemType(42, "Poison")
  case Unknown6              extends ItemType(43, "Unknown")
  case Unknown7              extends ItemType(44, "Unknown")
  case Martial               extends ItemType(45, "Martial")

object ItemType:
  private val byCode: Map[Int, ItemType] = values.map(v => v.code -> v).toMap
  def fromCode(code: Int): Option[ItemType] = byCode.get(code)

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
  itemType: ItemType = ItemType.OneHandSlash,  // EQ item type (from ItemType field at offset 253)
  price: Int = 0,          // Item price in copper (offset 192 in Item_Struct)
  idFileNum: Int = 0,      // IT number from IDFile (e.g., "IT27" → 27) — used for weapon model lookup
  scrollSpellId: Int = 0,  // Spell ID for scroll items (Effect1 at offset 266 in Item_Struct)
  // Container fields (itemClass == 1 only; 0 for all other items)
  bagSlots: Int = 0,       // number of slots this bag has (4, 6, 8, or 10)
  bagSize: Int = 0,        // max item size the bag accepts (0=tiny..4=giant)
  bagWR: Int = 0,          // weight reduction % (0, 10, 25, etc.)
):
  def stackCount: Int = if stackable && charges > 1 then charges else 0
  /** True if this item is a container with usable bag slots. */
  def isBag: Boolean = itemClass == 1 && bagSlots > 0
  /** Check if this item can be placed in the given equipment slot (0-21). */
  def canEquipIn(slotId: Int): Boolean =
    if slotId < 0 || slotId > 21 then true // general/bag slots always allowed
    else (slots & (1 << slotId)) != 0

object InventoryItem:
  // Equipment slot constants (Mac client slot IDs).
  // In the Mac client, slot 0 is the cursor — there is no separate charm slot.
  val Cursor    = 0
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
