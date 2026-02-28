package opennorrath.ui

import java.io.{File, RandomAccessFile}
import scala.io.Source

/** Loads spell names, icons, stats, and effect names from EQ data files.
  *
  * Spell data fields from spells_us.txt (^-delimited, Mac client 217-field format):
  *   Field 0 = spell ID, Field 1 = name, Field 13 = cast time (ms),
  *   Field 19 = mana cost, Field 83 = goodEffect (1=beneficial),
  *   Field 85 = resistType (0=none, 1=magic, 2=fire, 3=cold, 4=poison, 5=disease),
  *   Field 144 = spellanim index.
  */
object SpellData:

  // Resist type constants (from field 85 of spells_us.txt)
  val ResistNone    = 0
  val ResistMagic   = 1
  val ResistFire    = 2
  val ResistCold    = 3
  val ResistPoison  = 4
  val ResistDisease = 5

  private var names: Map[Int, String] = Map.empty
  private var spellAnims: Map[Int, Int] = Map.empty
  private var effectNames: Map[Int, String] = Map.empty
  private var manaCosts: Map[Int, Int] = Map.empty
  private var castTimes: Map[Int, Int] = Map.empty
  private var resistTypes: Map[Int, Int] = Map.empty
  private var goodEffects: Map[Int, Boolean] = Map.empty
  private var buffDurFormulas: Map[Int, Int] = Map.empty  // field 17
  private var buffDurValues: Map[Int, Int] = Map.empty    // field 18

  def init(basePath: String): Unit =
    loadSpells(basePath)
    loadEffectNames(basePath)

  private def loadSpells(basePath: String): Unit =
    val file = File(s"$basePath/spells_us.txt")
    if !file.exists() then
      println(s"[SpellData] spells_us.txt not found at ${file.getPath}")
      return
    val src = Source.fromFile(file, "ISO-8859-1")
    try
      for line <- src.getLines() do
        val fields = line.split("\\^", -1)
        if fields.length >= 2 then
          val id = fields(0).toIntOption
          val name = fields(1)
          if id.isDefined && name.nonEmpty then
            names += (id.get -> name)
            // Field 19 = mana cost
            if fields.length > 19 then
              fields(19).toIntOption.foreach { mana =>
                if mana > 0 then manaCosts += (id.get -> mana)
              }
            // Field 13 = cast time in milliseconds
            if fields.length > 13 then
              fields(13).toIntOption.foreach { ct =>
                if ct > 0 then castTimes += (id.get -> ct)
              }
            // Field 17 = buffdurationformula, Field 18 = buffduration (ticks)
            if fields.length > 18 then
              fields(17).toIntOption.foreach { formula =>
                buffDurFormulas += (id.get -> formula)
              }
              fields(18).toIntOption.foreach { dur =>
                buffDurValues += (id.get -> dur)
              }
            // Field 83 = goodEffect: 0=detrimental, 1=beneficial, 2=beneficial (group),
            // 3=beneficial (self-triggered, e.g. AA heal procs)
            if fields.length > 83 then
              fields(83).toIntOption.foreach { ge =>
                goodEffects += (id.get -> (ge > 0))
              }
            // Field 85 = resistType: 0=none, 1=magic, 2=fire, 3=cold, 4=poison, 5=disease
            if fields.length > 85 then
              fields(85).toIntOption.foreach { rt =>
                resistTypes += (id.get -> rt)
              }
            // Field 144 = spellanim (index into spellsnew.eff)
            if fields.length > 144 then
              fields(144).toIntOption.foreach { anim =>
                if anim > 0 then spellAnims += (id.get -> anim)
              }
      println(s"[SpellData] Loaded ${names.size} spell names, ${spellAnims.size} spell anims")
    finally src.close()

  /** Parse spellsnew.eff: 268-byte records, first 64 bytes = null-terminated effect name. */
  private def loadEffectNames(basePath: String): Unit =
    val file = File(s"$basePath/spellsnew.eff")
    if !file.exists() then
      println(s"[SpellData] spellsnew.eff not found")
      return
    val RecordSize = 268
    val NameFieldSize = 64
    val raf = RandomAccessFile(file, "r")
    try
      val totalRecords = (raf.length() / RecordSize).toInt
      val nameBytes = new Array[Byte](NameFieldSize)
      for i <- 0 until totalRecords do
        raf.seek(i.toLong * RecordSize)
        raf.readFully(nameBytes)
        val end = nameBytes.indexOf(0.toByte)
        val name = new String(nameBytes, 0, if end >= 0 then end else NameFieldSize, "ASCII").trim
        if name.nonEmpty && name != "None" then
          effectNames += (i -> name)
      println(s"[SpellData] Loaded ${effectNames.size} effect names from spellsnew.eff ($totalRecords records)")
    finally raf.close()

  def spellName(id: Int): String =
    names.getOrElse(id, s"Spell#$id")

  /** Mana cost for the spell. */
  def spellMana(id: Int): Int =
    manaCosts.getOrElse(id, 0)

  /** Cast time in milliseconds. */
  def spellCastTime(id: Int): Int =
    castTimes.getOrElse(id, 0)

  def spellAnim(spellId: Int): Int =
    spellAnims.getOrElse(spellId, 0)

  def effectName(animIndex: Int): String =
    effectNames.getOrElse(animIndex, "")

  /** Resist type for the spell (0=none, 1=magic, 2=fire, 3=cold, 4=poison, 5=disease). */
  def resistType(id: Int): Int =
    resistTypes.getOrElse(id, 0)

  /** True if the spell is beneficial (heal, buff, etc.). */
  def isBeneficial(id: Int): Boolean =
    goodEffects.getOrElse(id, false)

  /** Calculate buff duration in ticks for a spell at a given caster level.
    * Mirrors server-side CalcBuffDuration_formula (spells.cpp:2435).
    * Returns 0 for non-buff spells. Each tick = 6 seconds.
    * Does NOT include the server's +1 extraTick (that's added server-side only).
    */
  def calcBuffDuration(spellId: Int, casterLevel: Int): Int =
    val formula = buffDurFormulas.getOrElse(spellId, 0)
    val duration = buffDurValues.getOrElse(spellId, 0)
    calcBuffDurationFormula(casterLevel, formula, duration)

  private def calcBuffDurationFormula(level: Int, formula: Int, duration: Int): Int =
    if formula >= 200 then return formula
    formula match
      case 0  => 0 // not a buff
      case 1  =>
        val i = level / 2
        if i < duration then (if i < 1 then 1 else i) else duration
      case 2  =>
        val i = if level <= 1 then 6 else level / 2 + 5
        if i < duration then (if i < 1 then 1 else i) else duration
      case 3  =>
        val i = level * 30
        if i < duration then (if i < 1 then 1 else i) else duration
      case 4  =>
        val i = 50
        if duration != 0 then (if i < duration then i else duration) else i
      case 5  =>
        val i = 2
        if duration != 0 then (if i < duration then i else duration) else i
      case 6  =>
        val i = level / 2 + 2
        if duration != 0 then (if i < duration then i else duration) else i
      case 7  =>
        val i = level
        if duration != 0 then (if i < duration then i else duration) else i
      case 8  =>
        val i = level + 10
        if i < duration then (if i < 1 then 1 else i) else duration
      case 9  =>
        val i = level * 2 + 10
        if i < duration then (if i < 1 then 1 else i) else duration
      case 10 =>
        val i = level * 3 + 10
        if i < duration then (if i < 1 then 1 else i) else duration
      case 11 =>
        val i = level * 30 + 90
        if i < duration then (if i < 1 then 1 else i) else duration
      case 12 =>
        val i = if level / 4 > 0 then level / 4 else 1
        if duration != 0 then (if i < duration then i else duration) else i
      case 50 => 0xFFFE // permanent
      case _  => 0
