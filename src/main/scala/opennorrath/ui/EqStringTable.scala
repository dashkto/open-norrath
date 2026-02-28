package opennorrath.ui

import java.io.File
import scala.io.Source

/** Loads EQ client string table (eqstr_us.txt) for resolving OP_FormattedMessage string IDs.
  * Format: header line "EQST0002", then "id text" per line where id is an integer.
  * Strings may contain %1..%9 placeholders for substitution.
  */
object EqStringTable:

  private var strings: Map[Int, String] = Map.empty

  def init(basePath: String): Unit =
    val file = File(s"$basePath/eqstr_us.txt")
    if !file.exists() then
      println(s"[EqStringTable] eqstr_us.txt not found at ${file.getPath}")
      return
    val src = Source.fromFile(file, "ISO-8859-1")
    try
      for line <- src.getLines() do
        // Skip header lines (EQST0002, etc.)
        val spaceIdx = line.indexOf(' ')
        if spaceIdx > 0 then
          val idStr = line.substring(0, spaceIdx)
          idStr.toIntOption.foreach { id =>
            val text = line.substring(spaceIdx + 1)
            if text.nonEmpty then strings += (id -> text)
          }
      println(s"[EqStringTable] Loaded ${strings.size} strings")
    finally src.close()

  def get(id: Int): Option[String] = strings.get(id)

  /** Resolve a string ID and substitute %1..%9 with the provided arguments. */
  def format(id: Int, args: Seq[String]): Option[String] =
    strings.get(id).map { template =>
      var result = template
      for i <- args.indices do
        result = result.replace(s"%${i + 1}", args(i))
      result
    }
