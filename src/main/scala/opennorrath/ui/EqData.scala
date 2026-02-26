package opennorrath.ui

/** Shared EverQuest class/race name lookups used across multiple screens. */
object EqData:

  def className(id: Int): String = id match
    case 1  => "Warrior"
    case 2  => "Cleric"
    case 3  => "Paladin"
    case 4  => "Ranger"
    case 5  => "Shadow Knight"
    case 6  => "Druid"
    case 7  => "Monk"
    case 8  => "Bard"
    case 9  => "Rogue"
    case 10 => "Shaman"
    case 11 => "Necromancer"
    case 12 => "Wizard"
    case 13 => "Magician"
    case 14 => "Enchanter"
    case 15 => "Beastlord"
    case _  => s"Class($id)"

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

  /** Short class abbreviation for compact displays. */
  def classAbbrev(id: Int): String = id match
    case 1  => "WAR"
    case 2  => "CLR"
    case 3  => "PAL"
    case 4  => "RNG"
    case 5  => "SHD"
    case 6  => "DRU"
    case 7  => "MNK"
    case 8  => "BRD"
    case 9  => "ROG"
    case 10 => "SHM"
    case 11 => "NEC"
    case 12 => "WIZ"
    case 13 => "MAG"
    case 14 => "ENC"
    case 15 => "BST"
    case _  => "???"

  /** Whether this class uses mana. */
  def usesMana(classId: Int): Boolean = classId match
    case 1 | 7 | 8 | 9 => false // WAR, MNK, BRD, ROG
    case _              => true

  /** Map race ID + gender to 3-char S3D model code (lowercase).
    * Gender: 0=male, 1=female, 2=neutral (uses male code).
    * Returns None for unknown races.
    */
  def raceModelCode(race: Int, gender: Int): Option[String] =
    val code = race match
      // Playable races
      case 1   => if gender == 1 then "huf" else "hum"  // Human
      case 2   => if gender == 1 then "baf" else "bam"  // Barbarian
      case 3   => if gender == 1 then "erf" else "erm"  // Erudite
      case 4   => if gender == 1 then "elf" else "elm"  // Wood Elf
      case 5   => if gender == 1 then "ehf" else "ehm"  // High Elf
      case 6   => if gender == 1 then "def" else "dem"  // Dark Elf
      case 7   => if gender == 1 then "haf" else "ham"  // Half Elf
      case 8   => if gender == 1 then "dwf" else "dwm"  // Dwarf
      case 9   => if gender == 1 then "trf" else "trm"  // Troll
      case 10  => if gender == 1 then "ogf" else "ogm"  // Ogre
      case 11  => if gender == 1 then "hof" else "hom"  // Halfling
      case 12  => if gender == 1 then "gnf" else "gnm"  // Gnome
      case 128 => if gender == 1 then "ikf" else "ikm"  // Iksar
      case 130 => if gender == 1 then "kef" else "kem"  // Vah Shir
      // Common NPC races (gender-neutral)
      case 14  => "wer"  // Werewolf
      case 15  => "ske"  // Skeleton (tall)
      case 19  => "ten"  // Tentacle Terror
      case 21  => "gua"  // Guard
      case 26  => "bat"  // Bat
      case 27  => "eel"  // Eel (Ghoul variant)
      case 28  => "rat"  // Rat
      case 29  => "sna"  // Snake
      case 33  => "spi"  // Spider
      case 36  => "min"  // Minotaur
      case 38  => "ali"  // Alligator
      case 42  => "orc"  // Orc
      case 43  => "ske"  // Skeleton
      case 44  => "bro"  // Brownie
      case 45  => "dri"  // Drixie
      case 46  => "wol"  // Wolf
      case 47  => "bea"  // Bear
      case 49  => "fae"  // Fairy
      case 50  => "fun"  // Fungus Man
      case 51  => "gar"  // Gargoyle
      case 52  => "gal"  // Gasbag
      case 55  => "gno"  // Gnoll
      case 56  => "gob"  // Goblin
      case 60  => "gor"  // Gorilla
      case 62  => "cor"  // Cornsnake
      case 63  => "pix"  // Pixie
      case 67  => "imp"  // Imp
      case 69  => "bix"  // Bixie
      case 71  => "sca"  // Scarecrow
      case 72  => "dra"  // Drake
      case 73  => "dra"  // Drake (variant)
      case 75  => "wil"  // Will-o-wisp
      case 77  => "ele"  // Elemental
      case 85  => "spe"  // Spectre
      case 88  => "ban"  // Banshee
      case 89  => "lic"  // Basilisk
      case 93  => "gho"  // Ghost
      case 94  => "gho"  // Ghoul
      case 120 => "lio"  // Lion
      case 145 => "eye"  // Eye of Zomm
      case _   => null
    if code != null then Some(code) else None

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
