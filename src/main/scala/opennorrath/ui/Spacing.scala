package opennorrath.ui

/** Global layout constants: standard padding, menu spacing, and margins. */
object Spacing:
  val pad = 8f
  def pad(multiply: Float): Float = pad * multiply

  val menuItemSpacing = 55f
  val menuPad = pad

  val phi = 1.618f
  val pi: Float = Math.PI.toFloat
