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
