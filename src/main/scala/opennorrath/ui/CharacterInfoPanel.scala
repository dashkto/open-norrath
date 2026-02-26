package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.PlayerState

/** Compact HUD panel: name + level/class on one line, then HP and mana bars. */
class CharacterInfoPanel(player: PlayerState) extends Panel:

  val title = "##charinfo"
  val defaultX = 10f
  val defaultY = 10f
  val defaultWidth = 200f
  val defaultHeight = 90f
  override def extraFlags: Int =
    ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoScrollbar

  override protected def renderContent(): Unit =
    // "Name  Lv50 CLR"
    ImGui.pushFont(Fonts.defaultBold)
    pushColor(ImGuiCol.Text, Colors.gold)
    ImGui.text(player.name)
    ImGui.popStyleColor()
    ImGui.popFont()
    ImGui.sameLine()
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(s"Lv${player.level} ${EqData.classAbbrev(player.classId)}")
    ImGui.popStyleColor()

    // HP bar
    bar(player.hpPercent, Colors.danger, 14f)

    // Mana bar
    if EqData.usesMana(player.classId) then
      bar(player.manaPercent, Colors.secondary, 14f)
