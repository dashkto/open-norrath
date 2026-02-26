package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.state.PlayerCharacter

/** Displays active buffs on the player character. */
class BuffPanel(player: PlayerCharacter) extends Panel:

  val title = "Buffs"
  val defaultX = 800f
  val defaultY = 10f
  val defaultWidth = 160f
  val defaultHeight = 300f
  override def fontScale: Float = Spacing.fontScaleSmall
  override def extraFlags: Int = ImGuiWindowFlags.NoScrollbar

  opacity = 0.5f

  override protected def renderContent(): Unit =
    if player.buffs.isEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text("No active buffs")
      ImGui.popStyleColor()
      return

    val sorted = player.buffs.toVector.sortBy(_._1)
    for (slot, buff) <- sorted do
      val name = SpellData.spellName(buff.spellId)
      pushColor(ImGuiCol.Text, Colors.secondary)
      ImGui.text(name)
      ImGui.popStyleColor()

      // Duration in ticks (6 sec each)
      if buff.duration > 0 then
        val totalSec = buff.duration * 6
        val min = totalSec / 60
        val sec = totalSec % 60
        ImGui.sameLine()
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text(f"$min%d:$sec%02d")
        ImGui.popStyleColor()
