package opennorrath.ui

import scala.io.Source

/** Loads spell names from the EQ spells_us.txt file. */
object SpellData:

  private var names: Map[Int, String] = Map.empty

  def init(basePath: String): Unit =
    val file = java.io.File(s"$basePath/spells_us.txt")
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
      println(s"[SpellData] Loaded ${names.size} spell names")
    finally src.close()

  def spellName(id: Int): String =
    names.getOrElse(id, s"Spell#$id")
