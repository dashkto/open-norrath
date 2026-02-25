package opennorrath.ui

/** Central color definitions for the entire UI. Colors are (r, g, b, a) tuples. */
object Colors:

  // Theme
  val title     = hex("FFE6B3")
  val text      = hex("D9C79E")
  val textDim   = hex("808080", 0.7f)
  val gold      = hex("FFD700")
  val highlight = hex("FFD700", 0.25f)
  val overlay   = hex("000000", 0.55f)

  def withAlpha(c: (Float, Float, Float, Float), a: Float): (Float, Float, Float, Float) =
    (c._1, c._2, c._3, a)

  def hex(color: String, alpha: Float = 1f): (Float, Float, Float, Float) =
    val r = Integer.parseInt(color.substring(0, 2), 16) / 255f
    val g = Integer.parseInt(color.substring(2, 4), 16) / 255f
    val b = Integer.parseInt(color.substring(4, 6), 16) / 255f
    (r, g, b, alpha)
