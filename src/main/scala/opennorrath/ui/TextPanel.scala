package opennorrath.ui

import scala.collection.mutable.ArrayBuffer

import imgui.ImGui
import imgui.`type`.ImString
import imgui.flag.{ImGuiCol, ImGuiInputTextFlags, ImGuiKey, ImGuiWindowFlags}

/** A colored line of text in a TextPanel. */
case class TextLine(text: String, color: (Float, Float, Float, Float) = Colors.text)

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
  override def fontScale: Float = 0.85f
  override def extraFlags: Int = ImGuiWindowFlags.NoScrollbar | ImGuiWindowFlags.NoScrollWithMouse

  private val maxLines = 200
  private val lines = ArrayBuffer.empty[TextLine]
  private val inputBuf = new ImString(256)
  private var scrollToBottom = false

  def addLine(text: String, color: (Float, Float, Float, Float) = Colors.text): Unit =
    lines += TextLine(text, color)
    if lines.size > maxLines then lines.remove(0, lines.size - maxLines)
    scrollToBottom = true

  override protected def renderContent(): Unit =
    // Text area fills available space minus input row
    val inputRowHeight = ImGui.getFrameHeight() + ImGui.getStyle().getItemSpacingY()
    ImGui.beginChild("##scrollarea", 0f, -inputRowHeight, false, ImGuiWindowFlags.NoBackground)
    for line <- lines do
      pushColor(ImGuiCol.Text, line.color)
      ImGui.textWrapped(line.text)
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
