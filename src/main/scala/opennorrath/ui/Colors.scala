package opennorrath.ui

/** Central color definitions for the entire UI. Colors are (r, g, b, a) tuples.
  *
  * Based on the "Giant Goldfish" palette:
  * https://www.colourlovers.com/palette/92095/Giant_Goldfish
  */
object Colors:

  // Helpers (must be defined before vals that use them)
  private val black = (0f, 0f, 0f, 1f)

  def hex(color: String, alpha: Float = 1f): (Float, Float, Float, Float) =
    val r = Integer.parseInt(color.substring(0, 2), 16) / 255f
    val g = Integer.parseInt(color.substring(2, 4), 16) / 255f
    val b = Integer.parseInt(color.substring(4, 6), 16) / 255f
    (r, g, b, alpha)

  def withAlpha(c: (Float, Float, Float, Float), a: Float): (Float, Float, Float, Float) =
    (c._1, c._2, c._3, a)

  def tint(from: (Float, Float, Float, Float), to: (Float, Float, Float, Float), t: Float): (Float, Float, Float, Float) =
    (from._1 + (to._1 - from._1) * t,
     from._2 + (to._2 - from._2) * t,
     from._3 + (to._3 - from._3) * t,
     from._4 + (to._4 - from._4) * t)

  // Giant Goldfish palette
  val primary    = hex("F38630") // Orange
  val primary2   = hex("FA6900") // Darker orange
  val secondary  = hex("69D2E7") // Cyan
  val secondary2 = hex("A7DBD8") // Muted cyan
  val cream      = hex("E0E4CC") // Light container

  // Derived
  val background    = tint(cream, black, 0.8f)
  val darkContainer = tint(cream, black, 0.6f)

  // Theme (mapped from palette)
  val title   = cream
  val text    = tint(cream, black, 0.2f)
  val textDim = tint(cream, black, 0.5f)
  val gold    = hex("E8B84B")
  val highlight = withAlpha(primary, 0.25f)
  val overlay   = hex("000000", 0.55f)

  // Status
  val danger  = hex("E04040")
  val heal    = hex("7EC876")
  val violet  = hex("9B6EB7")
  val slate   = hex("6B7D8A")
  val coral   = hex("E86850")
  val error   = danger
  val success = heal

  // Input fields
  val inputBg     = tint(cream, black, 0.85f)
  val inputBorder = darkContainer
  val inputActive = primary
