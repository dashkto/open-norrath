package opennorrath.ui

/** EQ class IDs as received on the Mac wire protocol.
  * IMPORTANT: The Mac patch (mac.cpp) remaps class IDs before sending to the client:
  *   - Player classes 1-15: unchanged
  *   - GM variants: server 20-34 → wire 17-31 (class - 3)
  *   - Banker: server 40 → wire 16
  *   - Merchant: server 41 → wire 32
  * These codes reflect what we actually receive, NOT the server-internal values.
  */
enum EqClass(val code: Int, val displayName: String, val abbrev: String):
  case Warrior      extends EqClass(1,  "Warrior",      "WAR")
  case Cleric       extends EqClass(2,  "Cleric",       "CLR")
  case Paladin      extends EqClass(3,  "Paladin",      "PAL")
  case Ranger       extends EqClass(4,  "Ranger",       "RNG")
  case ShadowKnight extends EqClass(5,  "Shadow Knight", "SHD")
  case Druid        extends EqClass(6,  "Druid",        "DRU")
  case Monk         extends EqClass(7,  "Monk",         "MNK")
  case Bard         extends EqClass(8,  "Bard",         "BRD")
  case Rogue        extends EqClass(9,  "Rogue",        "ROG")
  case Shaman       extends EqClass(10, "Shaman",       "SHM")
  case Necromancer  extends EqClass(11, "Necromancer",   "NEC")
  case Wizard       extends EqClass(12, "Wizard",       "WIZ")
  case Magician     extends EqClass(13, "Magician",     "MAG")
  case Enchanter    extends EqClass(14, "Enchanter",    "ENC")
  case Beastlord    extends EqClass(15, "Beastlord",    "BST")
  // Special NPC classes (remapped by Mac patch)
  case Banker       extends EqClass(16, "Banker",          "BNK")  // server 40 → wire 16
  // GM variants (server 20-34 → wire 17-31, offset by -3)
  case WarriorGM      extends EqClass(17, "Warrior",      "WAR")
  case ClericGM       extends EqClass(18, "Cleric",       "CLR")
  case PaladinGM      extends EqClass(19, "Paladin",      "PAL")
  case RangerGM       extends EqClass(20, "Ranger",       "RNG")
  case ShadowKnightGM extends EqClass(21, "Shadow Knight", "SHD")
  case DruidGM        extends EqClass(22, "Druid",        "DRU")
  case MonkGM         extends EqClass(23, "Monk",         "MNK")
  case BardGM         extends EqClass(24, "Bard",         "BRD")
  case RogueGM        extends EqClass(25, "Rogue",        "ROG")
  case ShamanGM       extends EqClass(26, "Shaman",       "SHM")
  case NecromancerGM  extends EqClass(27, "Necromancer",   "NEC")
  case WizardGM       extends EqClass(28, "Wizard",       "WIZ")
  case MagicianGM     extends EqClass(29, "Magician",     "MAG")
  case EnchanterGM    extends EqClass(30, "Enchanter",    "ENC")
  case BeastlordGM    extends EqClass(31, "Beastlord",    "BST")
  // Merchant (server 41 → wire 32)
  case Merchant        extends EqClass(32, "Merchant",        "MRC")
  case DiscordMerchant extends EqClass(59, "Discord Merchant", "DMC")  // TODO: verify wire value
  case Corpse          extends EqClass(62, "Corpse",          "CRP")   // TODO: verify wire value

object EqClass:
  private val byCode: Map[Int, EqClass] = values.map(v => v.code -> v).toMap
  def fromCode(code: Int): Option[EqClass] = byCode.get(code)

/** Shared EverQuest class/race name lookups used across multiple screens. */
object EqData:

  def className(id: Int): String =
    EqClass.fromCode(id).map(_.displayName).getOrElse(s"Class($id)")

  def raceName(id: Int): String = id match
    case 1   => "Human"
    case 2   => "Barbarian"
    case 3   => "Erudite"
    case 4   => "Wood Elf"
    case 5   => "High Elf"
    case 6   => "Dark Elf"
    case 7   => "Half Elf"
    case 8   => "Dwarf"
    case 9   => "Troll"
    case 10  => "Ogre"
    case 11  => "Halfling"
    case 12  => "Gnome"
    case 128 => "Iksar"
    case 130 => "Vah Shir"
    case _   => s"Race($id)"

  /** Default model size by race ID. The server sends size=0 for player characters
    * (setting a non-zero size breaks movement on the server side), so the client must
    * supply the default. Values from EQEmu's GetRaceGenderDefaultHeight table.
    */
  def raceDefaultSize(race: Int): Float = race match
    case 1   => 6.0f   // Human
    case 2   => 7.0f   // Barbarian
    case 3   => 6.0f   // Erudite
    case 4   => 5.0f   // Wood Elf
    case 5   => 6.0f   // High Elf
    case 6   => 5.0f   // Dark Elf
    case 7   => 5.5f   // Half Elf
    case 8   => 4.0f   // Dwarf
    case 9   => 8.0f   // Troll
    case 10  => 9.0f   // Ogre
    case 11  => 3.5f   // Halfling
    case 12  => 3.0f   // Gnome
    case 128 => 6.0f   // Iksar
    case 130 => 6.0f   // Vah Shir
    case _   => 6.0f   // Default to human size

  /** Short class abbreviation for compact displays. */
  def classAbbrev(id: Int): String =
    EqClass.fromCode(id).map(_.abbrev).getOrElse("???")

  /** Whether this class uses mana. Non-caster player classes and special NPC classes don't. */
  def usesMana(classId: Int): Boolean = EqClass.fromCode(classId) match
    case Some(EqClass.Warrior | EqClass.Monk | EqClass.Bard | EqClass.Rogue |
              EqClass.WarriorGM | EqClass.MonkGM | EqClass.BardGM | EqClass.RogueGM |
              EqClass.Banker | EqClass.Merchant | EqClass.DiscordMerchant | EqClass.Corpse) => false
    case Some(_) => true
    case None    => false

  /** Map race ID + gender to 3-char S3D model code (lowercase).
    * Gender: 0=male, 1=female, 2=neutral (uses male code).
    * Returns None for unknown races.
    *
    * Race IDs are the same between Mac and Titanium protocols — both use the
    * canonical EQ race table from races.h. The race→model mapping below is
    * authoritative, derived from EQMacEmu/EQEmu races.h cross-referenced with
    * actual NPC spawn data (e.g. race 36 = giant rat, NOT minotaur).
    */
  def raceModelCode(race: Int, gender: Int): Option[String] =
    val code = race match
      // Playable races (1-12, 128, 130)
      case 1   => if gender == 1 then "huf" else "hum"  // Human
      case 2   => if gender == 1 then "baf" else "bam"  // Barbarian
      case 3   => if gender == 1 then "erf" else "erm"  // Erudite
      case 4   => if gender == 1 then "elf" else "elm"  // Wood Elf
      case 5   => if gender == 1 then "hif" else "him"  // High Elf
      case 6   => if gender == 1 then "daf" else "dam"  // Dark Elf
      case 7   => if gender == 1 then "haf" else "ham"  // Half Elf
      case 8   => if gender == 1 then "dwf" else "dwm"  // Dwarf
      case 9   => if gender == 1 then "trf" else "trm"  // Troll
      case 10  => if gender == 1 then "ogf" else "ogm"  // Ogre
      case 11  => if gender == 1 then "hof" else "hom"  // Halfling
      case 12  => if gender == 1 then "gnf" else "gnm"  // Gnome
      case 128 => if gender == 1 then "ikf" else "ikm"  // Iksar
      case 130 => if gender == 1 then "kef" else "kem"  // Vah Shir
      // NPC races — IDs from EQMacEmu/EQEmu races.h
      case 14  => "wer"  // Werewolf
      case 15  => if gender == 1 then "brf" else "brm"  // Brownie
      case 17  => "gol"  // Golem
      case 21  => "eye"  // Evil Eye
      case 22  => "bet"  // Beetle
      case 24  => "fis"  // Fish
      case 26  => "fro"  // Froglok
      case 27  => "frg"  // Froglok Ghoul
      case 28  => "fun"  // Fungusman
      case 29  => "gar"  // Gargoyle
      case 30  => "gal"  // Gasbag
      case 32  => "gho"  // Ghost
      case 33  => "gho"  // Ghoul (shares ghost model)
      case 34  => "bat"  // Giant Bat
      case 35  => "eel"  // Giant Eel
      case 36  => "rat"  // Giant Rat
      case 37  => "sna"  // Giant Snake
      case 38  => "spi"  // Giant Spider
      case 39  => "gno"  // Gnoll
      case 40  => "gob"  // Goblin
      case 41  => "gor"  // Gorilla
      case 42  => "wol"  // Wolf
      case 43  => "bea"  // Bear
      case 44  => if gender == 1 then "huf" else "hum"  // Freeport Guard (human)
      case 46  => "imp"  // Imp
      case 47  => "gri"  // Griffin
      case 49  => "dra"  // Lava Dragon
      case 50  => "lio"  // Lion
      case 51  => "lim"  // Lizard Man
      case 52  => "mim"  // Mimic
      case 53  => "min"  // Minotaur
      case 54  => "orc"  // Orc
      case 55  => if gender == 1 then "huf" else "hum"  // Human Beggar
      case 56  => "pix"  // Pixie
      case 58  => "sol"  // Solusek Ro
      case 59  => "gob"  // Bloodgill (goblin variant)
      case 60  => "ske"  // Skeleton
      case 61  => "sha"  // Shark
      case 62  => "tun"  // Tunare
      case 63  => "tig"  // Tiger
      case 65  => "vam"  // Vampire
      case 66  => "ral"  // Statue of Rallos Zek
      case 67  => if gender == 1 then "huf" else "hum"  // Highpass Citizen (human)
      case 68  => "ten"  // Tentacle Terror
      case 69  => "wil"  // Will-o-Wisp
      case 70  => "zom"  // Zombie
      case 71  => if gender == 1 then "huf" else "hum"  // Qeynos Citizen (human)
      case 74  => "pir"  // Piranha
      case 75  => "ele"  // Elemental
      case 77  => if gender == 1 then "daf" else "dam"  // Neriak Citizen (dark elf)
      case 78  => if gender == 1 then "erf" else "erm"  // Erudite Citizen
      case 79  => "bix"  // Bixie
      case 81  => if gender == 1 then "hof" else "hom"  // Rivervale Citizen (halfling)
      case 82  => "sca"  // Scarecrow
      case 85  => "spe"  // Spectre
      case 88  => if gender == 1 then "gnf" else "gnm"  // Clockwork Gnome
      case 89  => "dra"  // Drake
      case 90  => if gender == 1 then "baf" else "bam"  // Halas Citizen (barbarian)
      case 91  => "ali"  // Alligator
      case 92  => if gender == 1 then "trf" else "trm"  // Grobb Citizen (troll)
      case 93  => if gender == 1 then "ogf" else "ogm"  // Oggok Citizen (ogre)
      case 94  => if gender == 1 then "dwf" else "dwm"  // Kaladim Citizen (dwarf)
      case 101 => "efr"  // Efreeti
      case 106 => if gender == 1 then "huf" else "hum"  // Felguard (human guard)
      case 108 => "eye"  // Eye of Zomm
      case 109 => "was"  // Wasp
      case 111 => "har"  // Harpy
      case 112 => if gender == 1 then "elf" else "elm"  // Fayguard (wood elf)
      case 113 => "dri"  // Drixie
      case 120 => "wol"  // Wolf Elemental (wolf model)
      case 129 => "sco"  // Scorpion
      case 131 => "sar"  // Sarnak
      case 133 => "dvm"  // Drolvarg
      case 137 => "gob"  // Kunark Goblin
      case 139 => if gender == 1 then "ikf" else "ikm"  // Iksar Citizen
      case 145 => "goo"  // Goo
      case 156 => "rtm"  // Ratman
      case 161 => "iks"  // Iksar Skeleton
      case _   => null
    if code != null then Some(code) else None

  /** Map zone ID to display name. Covers all classic through PoP zones. */
  def zoneName(id: Int): String = zoneNames.getOrElse(id, s"Zone($id)")

  private val zoneNames: Map[Int, String] = Map(
    1 -> "South Qeynos", 2 -> "North Qeynos", 3 -> "Surefall Glade", 4 -> "Qeynos Hills",
    5 -> "Highpass Hold", 6 -> "High Keep", 8 -> "North Freeport", 9 -> "West Freeport",
    10 -> "East Freeport", 11 -> "Runnyeye", 12 -> "West Karana", 13 -> "North Karana",
    14 -> "South Karana", 15 -> "East Karana", 16 -> "Gorge of King Xorbb",
    17 -> "Blackburrow", 18 -> "Splitpaw", 19 -> "Rivervale", 20 -> "Kithicor Forest",
    21 -> "West Commonlands", 22 -> "East Commonlands", 23 -> "Erudin Palace", 24 -> "Erudin",
    25 -> "Nektulos Forest", 26 -> "Sunset Home", 27 -> "Lavastorm", 28 -> "Nektropos",
    29 -> "Halas", 30 -> "Everfrost", 31 -> "Solusek's Eye", 32 -> "Nagafen's Lair",
    33 -> "Misty Thicket", 34 -> "North Ro", 35 -> "South Ro", 36 -> "Befallen",
    37 -> "Oasis of Marr", 38 -> "Toxxulia Forest", 39 -> "The Hole",
    40 -> "Neriak Foreign Quarter", 41 -> "Neriak Commons", 42 -> "Neriak 3rd Gate",
    43 -> "Neriak Palace", 44 -> "Najena", 45 -> "Qeynos Aqueducts", 46 -> "Innothule Swamp",
    47 -> "The Feerrott", 48 -> "Cazic-Thule", 49 -> "Oggok", 50 -> "Rathe Mountains",
    51 -> "Lake Rathetear", 52 -> "Grobb", 53 -> "Aviak Village", 54 -> "Greater Faydark",
    55 -> "Ak'Anon", 56 -> "Steamfont", 57 -> "Lesser Faydark", 58 -> "Crushbone",
    59 -> "Castle Mistmoore", 60 -> "South Kaladim", 61 -> "North Felwithe",
    62 -> "South Felwithe", 63 -> "Estate of Unrest", 64 -> "Kedge Keep",
    65 -> "Guk", 66 -> "Old Guk", 67 -> "North Kaladim", 68 -> "Butcherblock",
    69 -> "Ocean of Tears", 70 -> "Dagnor's Cauldron", 71 -> "Plane of Sky",
    72 -> "Plane of Fear", 73 -> "Permafrost", 74 -> "Kerra Isle", 75 -> "Paineel",
    76 -> "Plane of Hate", 77 -> "The Arena", 78 -> "Field of Bone", 79 -> "Warsliks Woods",
    80 -> "Temple of Solusek Ro", 81 -> "Droga", 82 -> "Cabilis West",
    83 -> "Swamp of No Hope", 84 -> "Firiona Vie", 85 -> "Lake of Ill Omen",
    86 -> "Dreadlands", 87 -> "Burning Wood", 88 -> "Kaesora", 89 -> "Sebilis",
    90 -> "City of Mist", 91 -> "Skyfire", 92 -> "Frontier Mountains",
    93 -> "Overthere", 94 -> "Emerald Jungle", 95 -> "Trakanon's Teeth",
    96 -> "Timorous Deep", 97 -> "Kurn's Tower", 98 -> "Erud's Crossing",
    100 -> "Stonebrunt", 101 -> "The Warrens", 102 -> "Karnor's Castle",
    103 -> "Chardok", 104 -> "Crypt of Dalnir", 105 -> "Howling Stones",
    106 -> "Cabilis East", 107 -> "Nurga", 108 -> "Veeshan's Peak", 109 -> "Veksar",
    110 -> "Iceclad Ocean", 111 -> "Tower of Frozen Shadow", 112 -> "Velketor's Labyrinth",
    113 -> "Kael Drakkel", 114 -> "Skyshrine", 115 -> "Thurgadin", 116 -> "Eastern Wastes",
    117 -> "Cobaltscar", 118 -> "Great Divide", 119 -> "Wakening Land",
    120 -> "Western Wastes", 121 -> "Crystal Caverns", 123 -> "Dragon Necropolis",
    124 -> "Temple of Veeshan", 125 -> "Siren's Grotto", 126 -> "Plane of Mischief",
    127 -> "Plane of Growth", 128 -> "Sleeper's Tomb", 129 -> "Icewell Keep",
    130 -> "Marauders Mire", 150 -> "Shadow Haven", 151 -> "The Bazaar", 152 -> "Nexus",
    153 -> "Echo Caverns", 154 -> "Acrylia Caverns", 155 -> "Shar Vahl",
    156 -> "Paludal Caverns", 157 -> "Fungus Grove", 158 -> "Vex Thal",
    159 -> "Sanctus Seru", 160 -> "Katta Castellum", 161 -> "Netherbian Lair",
    162 -> "Ssraeshza Temple", 163 -> "Grieg's End", 164 -> "The Deep",
    165 -> "Shadeweaver's Thicket", 166 -> "Hollowshade Moor", 167 -> "Grimling Forest",
    168 -> "Marus Seru", 169 -> "Mons Letalis", 170 -> "Twilight",
    171 -> "The Grey", 172 -> "Tenebrous Mountains", 173 -> "Maiden's Eye",
    174 -> "Dawnshroud Peaks", 175 -> "Scarlet Desert", 176 -> "Umbral Plains",
    179 -> "Akheva Ruins", 181 -> "Jaggedpine Forest",
    200 -> "Crypt of Decay", 201 -> "Plane of Justice", 202 -> "Plane of Knowledge",
    203 -> "Plane of Tranquility", 204 -> "Plane of Nightmares", 205 -> "Plane of Disease",
    206 -> "Plane of Innovation", 207 -> "Plane of Pain", 208 -> "Plane of Valor",
    209 -> "Bastion of Thunder", 210 -> "Plane of Storms", 211 -> "Halls of Honor",
    212 -> "Tower of Solusek Ro", 213 -> "Plane of War", 214 -> "Fortress of Zek",
    215 -> "Plane of Air", 216 -> "Plane of Water", 217 -> "Plane of Fire",
    218 -> "Plane of Earth", 219 -> "Plane of Time", 220 -> "Temple of Marr",
    221 -> "Lair of Terris Thule", 222 -> "Plane of Earth B", 223 -> "Plane of Time B",
  )

  private val skillNames: Map[Int, String] = Map(
    0 -> "1H Blunt", 1 -> "1H Slashing", 2 -> "2H Blunt", 3 -> "2H Slashing",
    4 -> "Abjuration", 5 -> "Alteration", 6 -> "Apply Poison", 7 -> "Archery",
    8 -> "Backstab", 9 -> "Bind Wound", 10 -> "Bash", 11 -> "Block",
    12 -> "Brass Instruments", 13 -> "Channeling", 14 -> "Conjuration",
    15 -> "Defense", 16 -> "Disarm", 17 -> "Disarm Traps", 18 -> "Divination",
    19 -> "Dodge", 20 -> "Double Attack", 21 -> "Dragon Punch", 22 -> "Dual Wield",
    23 -> "Eagle Strike", 24 -> "Evocation", 25 -> "Feign Death",
    26 -> "Flying Kick", 27 -> "Forage", 28 -> "Hand to Hand", 29 -> "Hide",
    30 -> "Kick", 31 -> "Meditate", 32 -> "Mend", 33 -> "Offense",
    34 -> "Parry", 35 -> "Pick Lock", 36 -> "Piercing",
    37 -> "Riposte", 38 -> "Round Kick", 39 -> "Safe Fall",
    40 -> "Sense Heading", 41 -> "Singing", 42 -> "Sneak",
    43 -> "Specialize Abjure", 44 -> "Specialize Alteration",
    45 -> "Specialize Conjuration", 46 -> "Specialize Divination",
    47 -> "Specialize Evocation", 48 -> "Pick Pockets",
    49 -> "Stringed Instruments", 50 -> "Swimming", 51 -> "Throwing",
    52 -> "Tiger Claw", 53 -> "Tracking", 54 -> "Wind Instruments",
    55 -> "Fishing", 56 -> "Make Poison", 57 -> "Tinkering",
    58 -> "Research", 59 -> "Alchemy", 60 -> "Baking",
    61 -> "Tailoring", 62 -> "Sense Traps", 63 -> "Blacksmithing",
    64 -> "Fletching", 65 -> "Brewing", 66 -> "Alcohol Tolerance",
    67 -> "Begging", 68 -> "Jewelry Making", 69 -> "Pottery",
    70 -> "Percussion Instruments", 71 -> "Intimidation",
    72 -> "Berserking", 73 -> "Taunt",
    74 -> "Frenzy",
  )

  def skillName(id: Int): String = skillNames.getOrElse(id, s"Skill($id)")

  /** Base racial resistances: (MR, FR, CR, DR, PR) */
  def raceBaseResists(race: Int): (Int, Int, Int, Int, Int) = race match
    case 1   => (25, 25, 25, 25, 25)  // Human
    case 2   => (25, 25, 35, 25, 25)  // Barbarian
    case 3   => (30, 25, 25, 30, 25)  // Erudite
    case 4   => (25, 25, 25, 25, 25)  // Wood Elf
    case 5   => (25, 25, 25, 25, 25)  // High Elf
    case 6   => (25, 25, 25, 25, 25)  // Dark Elf
    case 7   => (25, 25, 25, 25, 25)  // Half Elf
    case 8   => (30, 25, 25, 25, 30)  // Dwarf
    case 9   => (25, 30, 25, 25, 35)  // Troll
    case 10  => (25, 30, 25, 25, 35)  // Ogre
    case 11  => (25, 25, 25, 25, 30)  // Halfling
    case 12  => (25, 30, 25, 25, 25)  // Gnome
    case 128 => (25, 25, 25, 25, 25)  // Iksar
    case 130 => (25, 25, 25, 25, 25)  // Vah Shir
    case _   => (25, 25, 25, 25, 25)
