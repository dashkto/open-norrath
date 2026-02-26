package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.network.SpawnData

/** Displays the currently targeted spawn's name, level, class, and HP. */
class TargetPanel extends Panel:

  val title = "##targetinfo"
  val defaultX = 220f
  val defaultY = 10f
  val defaultWidth = 200f
  val defaultHeight = 60f
  override def extraFlags: Int =
    ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoScrollbar

  var target: Option[SpawnData] = None
  private val spawnHp = scala.collection.mutable.Map.empty[Int, (Int, Int)] // spawnId -> (cur, max)

  def updateHp(spawnId: Int, curHp: Int, maxHp: Int): Unit =
    spawnHp(spawnId) = (curHp, maxHp)

  override protected def renderContent(): Unit =
    target match
      case None => ()
      case Some(s) =>
        val cleanedName = s.name.replaceAll("\\d+$", "").replace('_', ' ').trim
        ImGui.pushFont(Fonts.defaultBold)
        pushColor(ImGuiCol.Text, Colors.gold)
        ImGui.text(cleanedName)
        ImGui.popStyleColor()
        ImGui.popFont()
        ImGui.sameLine()
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text(s"Lv${s.level} ${EqData.classAbbrev(s.classId)}")
        ImGui.popStyleColor()

        // HP bar
        val hpFraction = spawnHp.get(s.spawnId) match
          case Some((cur, max)) if max > 0 => cur.toFloat / max.toFloat
          case _ => 1f // assume full if unknown
        bar(hpFraction, Colors.danger, 14f)

  override def render(): Unit =
    if target.isEmpty then return
    super.render()
