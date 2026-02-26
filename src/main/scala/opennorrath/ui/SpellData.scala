package opennorrath.ui

import java.io.{File, RandomAccessFile}
import scala.io.Source

/** Loads spell names, spellanim indices, and effect names from EQ data files. */
object SpellData:

  private var names: Map[Int, String] = Map.empty
  private var spellAnims: Map[Int, Int] = Map.empty
  private var effectNames: Map[Int, String] = Map.empty

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

  def spellAnim(spellId: Int): Int =
    spellAnims.getOrElse(spellId, 0)

  def effectName(animIndex: Int): String =
    effectNames.getOrElse(animIndex, "")
