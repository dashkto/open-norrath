package opennorrath.ui

import imgui.ImGui
import imgui.flag.ImGuiCol

import opennorrath.network.InventoryItem
import opennorrath.state.PlayerCharacter

/** Standalone stats panel — HP, Mana, AC, Exp, Attributes, Resists.
  * Extracted from InventoryPanel and given its own toggle (default: O key).
  */
class StatsPanel(player: Option[PlayerCharacter] = None) extends Panel:

  val title = "Stats"
  val defaultX = 1010f
  val defaultY = 50f
  val defaultWidth = 145f
  val defaultHeight = 580f
  override def fontScale: Float = Spacing.fontScaleMedium

  visible = false

  def toggle(): Unit =
    visible = !visible

  private def itemsBySlot: Map[Int, InventoryItem] =
    player.map(_.inventory.items).getOrElse(Map.empty)

  override protected def renderContent(): Unit =
    player.foreach(renderStats)

  private def renderStats(pc: PlayerCharacter): Unit =
    val items = itemsBySlot
    sectionHeader("Stats")

    // HP / Mana
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"HP: ${pc.currentHp} / ${pc.maxHp}")
    ImGui.popStyleColor()
    if EqData.usesMana(pc.classId) then
      pushColor(ImGuiCol.Text, Colors.text)
      ImGui.text(s"Mana: ${pc.currentMana} / ${pc.maxMana}")
      ImGui.popStyleColor()

    // AC from equipped items (slots 0-21)
    val equipItems = items.filter((slot, _) => slot >= 0 && slot <= 21).values.toVector
    val totalAc = equipItems.map(_.ac).sum
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"AC: $totalAc")
    ImGui.popStyleColor()

    // Experience — displayed as percentage within current level (0-330 scale from server)
    val expPct = pc.exp * 100f / 330f
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(f"Exp: $expPct%.1f%%")
    ImGui.popStyleColor()

    ImGui.spacing()

    // Attributes
    statLine("STR", pc.str)
    statLine("STA", pc.sta)
    statLine("AGI", pc.agi)
    statLine("DEX", pc.dex)
    statLine("WIS", pc.wis)
    statLine("INT", pc.int)
    statLine("CHA", pc.cha)

    // Resistances
    ImGui.spacing()
    ImGui.separator()
    sectionHeader("Resists")

    val (baseMr, baseFr, baseCr, baseDr, basePr) = EqData.raceBaseResists(pc.race)
    val gearMr = equipItems.map(_.mr).sum
    val gearFr = equipItems.map(_.fr).sum
    val gearCr = equipItems.map(_.cr).sum
    val gearDr = equipItems.map(_.dr).sum
    val gearPr = equipItems.map(_.pr).sum

    statLine("MR", baseMr + gearMr)
    statLine("FR", baseFr + gearFr)
    statLine("CR", baseCr + gearCr)
    statLine("DR", baseDr + gearDr)
    statLine("PR", basePr + gearPr)

  private val StatValueCol = 40f

  private def statLine(label: String, value: Int): Unit =
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(label)
    ImGui.popStyleColor()
    ImGui.sameLine(StatValueCol)
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(value.toString)
    ImGui.popStyleColor()
