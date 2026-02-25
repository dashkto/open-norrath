package opennorrath.archive

case class PfsEntry(name: String, data: Array[Byte]):
  def extension: String =
    val dot = name.lastIndexOf('.')
    if dot >= 0 then name.substring(dot + 1).toLowerCase else ""

  override def toString: String =
    s"PfsEntry($name, ${data.length} bytes)"
