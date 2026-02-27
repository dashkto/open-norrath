package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.state.ZoneCharacter

/** Displays the currently targeted spawn's name, level, class, and HP. */
class TargetPanel extends Panel:

  val title = "##targetinfo"
  val defaultX = 220f
  val defaultY = 10f
  val defaultWidth = Spacing.panelWidthNarrow
  val defaultHeight = 60f
  override def extraFlags: Int =
    ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoScrollbar

  var target: Option[ZoneCharacter] = None

  private def isAutoAttacking: Boolean =
    Game.player.exists(_.autoAttacking)

  override def render(): Unit =
    if isAutoAttacking && target.isDefined then
      ImGui.pushStyleColor(ImGuiCol.Border, 0.9f, 0.2f, 0.2f, 1.0f)
      ImGui.pushStyleVar(imgui.flag.ImGuiStyleVar.WindowBorderSize, 2.0f)
      super.render()
      ImGui.popStyleVar()
      ImGui.popStyleColor()
    else
      super.render()

  override protected def renderContent(): Unit =
    target match
      case None =>
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text("No target")
        ImGui.popStyleColor()
      case Some(zc) =>
        ImGui.pushFont(Fonts.defaultBold)
        pushColor(ImGuiCol.Text, Colors.gold)
        ImGui.text(zc.displayName)
        ImGui.popStyleColor()
        ImGui.popFont()
        ImGui.sameLine()
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text(s"(${zc.level})")
        ImGui.popStyleColor()

        bar(zc.hpFraction, Colors.danger, Spacing.barHeightSmall)
