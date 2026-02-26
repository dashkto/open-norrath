package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

/** Base for moveable, resizable game UI panels rendered via ImGui.
  *
  * Subclasses implement `renderContent()` to draw their panel interior.
  * The panel handles window framing, visibility toggling, and consistent styling.
  */
trait Panel:

  /** Window title shown in the title bar. Must be unique across all panels. */
  def title: String

  /** Initial position and size (applied only once via ImGuiCond.FirstUseEver). */
  def defaultX: Float
  def defaultY: Float
  def defaultWidth: Float
  def defaultHeight: Float

  /** Minimum size constraint. Override to change. Set to 0 to disable. */
  def minWidth: Float = 0f
  def minHeight: Float = 0f

  /** Extra ImGui window flags. Override to add NoResize, NoScrollbar, etc. */
  def extraFlags: Int = 0

  /** Font scale for the entire window including title bar. Override to shrink. */
  def fontScale: Float = 1.0f

  var visible: Boolean = true
  var locked: Boolean = false

  /** Render the panel contents. Called between ImGui.begin/end. */
  protected def renderContent(): Unit

  /** Call each frame from the screen's render method. */
  def render(): Unit =
    if !visible then return

    ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)
    if minWidth > 0f || minHeight > 0f then
      ImGui.setNextWindowSizeConstraints(minWidth, minHeight, Float.MaxValue, Float.MaxValue)

    val flags = extraFlags | (if locked then ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoResize else 0)
    ImGui.begin(title, flags)
    if fontScale != 1.0f then ImGui.setWindowFontScale(fontScale)
    renderContent()

    if ImGui.beginPopupContextWindow("##panelmenu") then
      if locked then
        if ImGui.menuItem("Unlock") then locked = false
      else
        if ImGui.menuItem("Lock") then locked = true
      ImGui.endPopup()

    ImGui.end()

  protected def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)

  /** Draw a horizontal bar (HP/mana style). */
  protected def bar(fraction: Float, color: (Float, Float, Float, Float), height: Float = 18f): Unit =
    val drawList = ImGui.getWindowDrawList()
    val cx = ImGui.getCursorScreenPosX()
    val cy = ImGui.getCursorScreenPosY()
    val availW = ImGui.getContentRegionAvailX()

    // Background
    val (br, bg, bb, ba) = Colors.withAlpha(Colors.background, 0.8f)
    drawList.addRectFilled(cx, cy, cx + availW, cy + height,
      ImGui.colorConvertFloat4ToU32(br, bg, bb, ba), 3f)

    // Fill
    val fillW = availW * fraction.max(0f).min(1f)
    if fillW > 0f then
      val (fr, fg, fb, fa) = color
      drawList.addRectFilled(cx, cy, cx + fillW, cy + height,
        ImGui.colorConvertFloat4ToU32(fr, fg, fb, fa), 3f)

    // Percentage text centered on the bar
    val pct = s"${(fraction * 100).toInt}%"
    val textW = ImGui.calcTextSize(pct).x
    val textH = ImGui.calcTextSize(pct).y
    drawList.addText(cx + (availW - textW) / 2f, cy + (height - textH) / 2f - 1f,
      ImGui.colorConvertFloat4ToU32(1f, 1f, 1f, 1f), pct)

    // Advance cursor past the bar
    ImGui.dummy(availW, height)
