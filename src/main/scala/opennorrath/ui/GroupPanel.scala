package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.state.ZoneCharacter

/** Displays party/group members with name, level, class, and HP bar.
  * Members are resolved by name from the zone's character map.
  */
class GroupPanel(characters: scala.collection.Map[Int, ZoneCharacter]) extends Panel:

  val title = "Group"
  val defaultX = 10f
  val defaultY = 110f
  val defaultWidth = Spacing.panelWidthNarrow
  val defaultHeight = 200f
  override def extraFlags: Int =
    ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoScrollbar

  var members: Vector[String] = Vector.empty
  var leader: String = ""
  opacity = 0.5f

  override def render(): Unit =
    if members.isEmpty then return
    super.render()

  override protected def renderContent(): Unit =
    for name <- members do
      val zc = findByName(name)
      // Name (bold, gold for leader)
      ImGui.pushFont(Fonts.defaultBold)
      val nameColor = if name == leader then Colors.gold else Colors.secondary
      pushColor(ImGuiCol.Text, nameColor)
      ImGui.text(name)
      ImGui.popStyleColor()
      ImGui.popFont()

      zc match
        case Some(c) =>
          // "Lv50 CLR" on same line
          ImGui.sameLine()
          pushColor(ImGuiCol.Text, Colors.textDim)
          ImGui.text(s"Lv${c.level} ${EqData.classAbbrev(c.classId)}")
          ImGui.popStyleColor()
          // HP bar
          bar(c.hpFraction, Colors.danger, Spacing.barHeightSmall)
        case None =>
          ImGui.sameLine()
          pushColor(ImGuiCol.Text, Colors.textDim)
          ImGui.text("(not in zone)")
          ImGui.popStyleColor()

      ImGui.spacing()

  private def findByName(name: String): Option[ZoneCharacter] =
    characters.values.find(_.name.equalsIgnoreCase(name))
