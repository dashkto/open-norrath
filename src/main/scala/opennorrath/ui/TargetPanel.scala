package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

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

        bar(zc.hpFraction, Colors.danger, Spacing.barHeightSmall)
