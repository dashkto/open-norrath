package opennorrath.ui

/** Global layout constants: standard padding, menu spacing, and margins. */
object Spacing:
  val pad = 8f
  def pad(multiply: Float): Float = pad * multiply

  val menuItemSpacing = 55f
  val menuPad = pad

  // ImGui style
  val rounding = 1f
  val framePadX = 4f
  val framePadY = 1f
  val itemSpacingX = 8f
  val itemSpacingY = 6f

  // Bars (HP, mana, etc.)
  val barHeight = 18f
  val barHeightSmall = 14f

  // Buttons
  val buttonHeight = 28f

  // Font scales
  val fontScaleSmall = 0.85f
  val fontScaleMedium = 0.9f

  // Inventory slots
  val slotSize = 52f
  val slotBoxHeight = 32f
  val slotGap = 4f
  val slotPad = 4f
  val columnGap = 8f

  // Panel widths
  val panelWidthNarrow = 200f

  val phi = 1.618f
  val pi: Float = Math.PI.toFloat
