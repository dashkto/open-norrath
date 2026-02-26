package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import opennorrath.state.PlayerCharacter

/** Displays all known spells from the player's spell book. */
class SpellBookPanel(player: PlayerCharacter) extends Panel:

  val title = "Spell Book"
  val defaultX = 400f
  val defaultY = 80f
  val defaultWidth = 250f
  val defaultHeight = 400f
  override def fontScale: Float = Spacing.fontScaleSmall

  visible = false
  private val pOpen = new ImBoolean(true)

  def toggle(): Unit =
    visible = !visible
    if visible then pOpen.set(true)

  override def render(): Unit =
    if !visible then return

    ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSizeConstraints(200f, 200f, Float.MaxValue, Float.MaxValue)

    val flags = extraFlags | (if locked then ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoResize else 0)
    pOpen.set(true)
    ImGui.begin(title, pOpen, flags)
    if !pOpen.get() then
      visible = false
      ImGui.end()
      return
    if fontScale != 1.0f then ImGui.setWindowFontScale(fontScale)
    renderContent()

    if ImGui.beginPopupContextWindow("##spellbookmenu") then
      if locked then
        if ImGui.menuItem("Unlock") then locked = false
      else
        if ImGui.menuItem("Lock") then locked = true
      ImGui.endPopup()

    ImGui.end()

  override protected def renderContent(): Unit =
    if player.spellBook.isEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text("No spells known")
      ImGui.popStyleColor()
      return

    val sorted = player.spellBook.map(id => (id, SpellData.spellName(id))).sortBy(_._2)
    for (id, name) <- sorted do
      pushColor(ImGuiCol.Text, Colors.secondary)
      ImGui.text(name)
      ImGui.popStyleColor()
