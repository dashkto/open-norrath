package opennorrath.ui

import scala.collection.mutable.ArrayBuffer

import imgui.ImGui
import imgui.`type`.ImString
import imgui.flag.{ImGuiCol, ImGuiInputTextFlags, ImGuiKey, ImGuiWindowFlags}

import opennorrath.network.InventoryItem

/** A segment of text within a chat line — either plain text or a hoverable item link. */
sealed trait TextSegment
case class PlainSegment(text: String) extends TextSegment
case class ItemLinkSegment(itemId: Int, name: String) extends TextSegment

/** A line of text composed of segments, with a base color. */
case class TextLine(segments: Vector[TextSegment], color: (Float, Float, Float, Float) = Colors.text)

object TextLine:
  /** Parse EQ item links (\x12 delimited) from raw message text.
    * Link wire format: 0x12 + 1 hex action_id + 6 decimal item_id + item_name + 0x12
    * Returns segments with links extracted and plain text preserved.
    */
  def parse(text: String, color: (Float, Float, Float, Float) = Colors.text): TextLine =
    val segments = Vector.newBuilder[TextSegment]
    var i = 0
    var plainStart = 0
    while i < text.length do
      if text.charAt(i) == '\u0012' then
        // Flush preceding plain text
        if i > plainStart then
          segments += PlainSegment(text.substring(plainStart, i))
        // Find closing 0x12
        val bodyStart = i + 1
        val closeIdx = text.indexOf('\u0012', bodyStart)
        if closeIdx > bodyStart + 7 then
          // Body: 1 hex action + 6 decimal item_id + item name
          val body = text.substring(bodyStart, closeIdx)
          val itemIdStr = body.substring(1, 7)
          val itemName = body.substring(7)
          val itemId = try itemIdStr.toInt catch case _: NumberFormatException => 0
          segments += ItemLinkSegment(itemId, itemName)
          i = closeIdx + 1
        else
          // Malformed link — skip the 0x12 byte
          i = bodyStart
        plainStart = i
      else
        i += 1
    // Flush remaining plain text
    if plainStart < text.length then
      segments += PlainSegment(text.substring(plainStart))
    TextLine(segments.result(), color)

/** Scrollable text display with an input field at the bottom.
  *
  * Multiple instances can coexist — each needs a unique `id` to distinguish
  * ImGui windows. Call `addLine` to append text; the panel auto-scrolls to bottom.
  *
  * @param id unique identifier (e.g. "Main", "Group", "Tell")
  * @param onSubmit called with the input text when the user presses Enter
  */
class TextPanel(
  val id: String,
  val onSubmit: String => Unit = _ => (),
) extends Panel:

  private var firstFrame = true
  val title = s"$id###textpanel_$id"
  val defaultX = 10f
  val defaultY = 500f
  val defaultWidth = 450f
  val defaultHeight = 200f
  override def fontScale: Float = Spacing.fontScaleSmall
  override def extraFlags: Int = ImGuiWindowFlags.NoScrollbar | ImGuiWindowFlags.NoScrollWithMouse |
    ImGuiWindowFlags.NoNavInputs  // prevent Tab from focusing the input field

  private val maxLines = 200
  private val lines = ArrayBuffer.empty[TextLine]
  private val inputBuf = new ImString(256)
  private var scrollToBottom = false

  /** Optional item lookup for rendering item link tooltips.
    * Set by the screen/hud to provide access to the player's inventory.
    */
  var itemLookup: Int => Option[InventoryItem] = _ => None

  def addLine(text: String, color: (Float, Float, Float, Float) = Colors.text): Unit =
    lines += TextLine.parse(text, color)
    if lines.size > maxLines then lines.remove(0, lines.size - maxLines)
    scrollToBottom = true

  override protected def renderContent(): Unit =
    // Text area fills available space minus input row
    val inputRowHeight = ImGui.getFrameHeight() + ImGui.getStyle().getItemSpacingY()
    ImGui.beginChild("##scrollarea", 0f, -inputRowHeight, false, ImGuiWindowFlags.NoBackground)
    for (line, lineIdx) <- lines.zipWithIndex do
      val hasLinks = line.segments.exists(_.isInstanceOf[ItemLinkSegment])
      if hasLinks then
        renderSegmentedLine(line, lineIdx)
      else
        // Fast path: plain text only, use textWrapped
        val text = line.segments.map { case PlainSegment(t) => t; case _ => "" }.mkString
        pushColor(ImGuiCol.Text, line.color)
        ImGui.textWrapped(text)
        ImGui.popStyleColor()
    if scrollToBottom then
      ImGui.setScrollHereY(1.0f)
      scrollToBottom = false
    ImGui.endChild()

    // Focus input on Enter when not already typing
    val wantFocus = !ImGui.getIO().getWantCaptureKeyboard() &&
      (ImGui.isKeyPressed(ImGuiKey.Enter) || ImGui.isKeyPressed(ImGuiKey.KeypadEnter))

    // Input field
    ImGui.pushItemWidth(-1f)
    if wantFocus && !firstFrame then ImGui.setKeyboardFocusHere()
    val flags = ImGuiInputTextFlags.EnterReturnsTrue
    if ImGui.inputText("##input", inputBuf, flags) then
      val text = inputBuf.get().trim
      if text.nonEmpty then
        onSubmit(text)
      inputBuf.set("")
    ImGui.popItemWidth()
    if firstFrame then
      ImGui.setWindowFocus(null)
      firstFrame = false

  // --- Segmented line rendering (for lines with item links) ---

  // Mutable cursor position shared between renderSegmentedLine and drawPlainText.
  // Only used during rendering — no concurrency concern.
  private var penX = 0f
  private var penY = 0f

  /** Render a line containing item links as inline segments with hover tooltips. */
  private def renderSegmentedLine(line: TextLine, lineIdx: Int): Unit =
    val drawList = ImGui.getWindowDrawList()
    val lineH = ImGui.getTextLineHeight()
    val wrapWidth = ImGui.getContentRegionAvailX()
    penX = ImGui.getCursorScreenPosX()
    penY = ImGui.getCursorScreenPosY()
    val startX = penX

    var segIdx = 0
    for seg <- line.segments do
      seg match
        case PlainSegment(text) =>
          val (cr, cg, cb, ca) = line.color
          val col = ImGui.colorConvertFloat4ToU32(cr, cg, cb, ca)
          drawPlainText(drawList, text, col, startX, wrapWidth, lineH)

        case ItemLinkSegment(itemId, name) =>
          val textW = ImGui.calcTextSize(name).x
          // Wrap if this link doesn't fit on the current line
          if penX - startX + textW > wrapWidth && penX > startX then
            penX = startX
            penY += lineH
          // Draw colored item name via draw list
          val (lr, lg, lb, la) = Colors.secondary
          drawList.addText(penX, penY, ImGui.colorConvertFloat4ToU32(lr, lg, lb, la), name)
          // Invisible button for hover detection
          ImGui.setCursorScreenPos(penX, penY)
          ImGui.invisibleButton(s"##link${lineIdx}_$segIdx", textW, lineH)
          if ImGui.isItemHovered() then
            renderItemTooltip(itemId, name)
          penX += textW
      segIdx += 1

    // Advance ImGui cursor past the rendered content
    ImGui.setCursorScreenPos(startX, penY + lineH)
    ImGui.dummy(0f, 0f)

  /** Draw plain text word-by-word with wrapping, advancing penX/penY. */
  private def drawPlainText(drawList: imgui.ImDrawList, text: String, color: Int,
                            startX: Float, wrapWidth: Float, lineH: Float): Unit =
    var i = 0
    while i < text.length do
      // Find next word boundary (include trailing space with the word)
      var j = i
      while j < text.length && text.charAt(j) != ' ' do j += 1
      if j < text.length then j += 1

      val word = text.substring(i, j)
      val wordW = ImGui.calcTextSize(word).x

      // Wrap if this word doesn't fit
      if penX - startX + wordW > wrapWidth && penX > startX then
        penX = startX
        penY += lineH

      drawList.addText(penX, penY, color, word)
      penX += wordW
      i = j

  // --- Item tooltip ---

  /** Show item tooltip on hover — uses inventory lookup if available, else just the name. */
  private def renderItemTooltip(itemId: Int, name: String): Unit =
    ImGui.beginTooltip()
    ImGui.setWindowFontScale(0.85f)
    itemLookup(itemId) match
      case Some(item) =>
        if ItemIcons.render(item.icon, 40f) then ImGui.sameLine()
        val nameColor = if item.magic then Colors.secondary else Colors.text
        pushColor(ImGuiCol.Text, nameColor)
        ImGui.text(item.name)
        ImGui.popStyleColor()
        val tags = Vector.newBuilder[String]
        if item.magic then tags += "MAGIC ITEM"
        if item.noDrop then tags += "NO DROP"
        if item.noRent then tags += "NO RENT"
        val tagStr = tags.result()
        if tagStr.nonEmpty then
          pushColor(ImGuiCol.Text, Colors.textDim)
          ImGui.text(tagStr.mkString("  "))
          ImGui.popStyleColor()
        ImGui.separator()
        if item.ac != 0 then tooltipStat("AC", item.ac)
        if item.hp != 0 then tooltipStat("HP", item.hp)
        if item.mana != 0 then tooltipStat("Mana", item.mana)
        if item.damage != 0 then tooltipStat("Dmg", item.damage)
        if item.delay != 0 then tooltipStat("Delay", item.delay)
        if item.aStr != 0 then tooltipStat("STR", item.aStr)
        if item.aSta != 0 then tooltipStat("STA", item.aSta)
        if item.aAgi != 0 then tooltipStat("AGI", item.aAgi)
        if item.aDex != 0 then tooltipStat("DEX", item.aDex)
        if item.aWis != 0 then tooltipStat("WIS", item.aWis)
        if item.aInt != 0 then tooltipStat("INT", item.aInt)
        if item.aCha != 0 then tooltipStat("CHA", item.aCha)
        val w = item.weight
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text(s"Wt: ${w / 10}.${w % 10}")
        ImGui.popStyleColor()
      case None =>
        pushColor(ImGuiCol.Text, Colors.secondary)
        ImGui.text(name)
        ImGui.popStyleColor()
    ImGui.endTooltip()

  private def tooltipStat(label: String, value: Int): Unit =
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"  $label: $value")
    ImGui.popStyleColor()
