package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.network.{InventoryItem, LootResponse}

/** Loot window shown when looting a corpse.
  * Displays money and items from the corpse. Click an item to loot it.
  * Closing the panel sends OP_EndLootRequest to the server.
  */
class LootPanel extends Panel:

  val title = "Looting"
  val defaultX = 400f
  val defaultY = 200f
  val defaultWidth = 260f
  val defaultHeight = 300f
  override def fontScale: Float = Spacing.fontScaleMedium

  visible = false
  private val pOpen = new ImBoolean(true)

  /** Corpse entity ID we're currently looting. */
  var corpseId: Int = 0

  /** Money from the corpse (auto-looted by server, displayed for feedback). */
  private var money: Option[LootResponse] = None

  /** Items available on the corpse, keyed by loot slot index. */
  private var lootItems: Vector[InventoryItem] = Vector.empty

  /** Called when OP_MoneyOnCorpse arrives with a successful response. */
  def open(id: Int, resp: LootResponse): Unit =
    corpseId = id
    money = if resp.platinum > 0 || resp.gold > 0 || resp.silver > 0 || resp.copper > 0
      then Some(resp) else None
    lootItems = Vector.empty
    visible = true
    pOpen.set(true)

  /** Called when OP_LootItemPacket arrives — add an item to the loot list. */
  def addItem(item: InventoryItem): Unit =
    lootItems = lootItems :+ item

  /** Called when the player clicks an item to loot it. */
  private def lootItem(item: InventoryItem): Unit =
    Game.zoneSession.foreach(_.client.lootItem(corpseId, item.equipSlot))
    lootItems = lootItems.filterNot(_.equipSlot == item.equipSlot)

  /** Close the loot window and notify the server. */
  def close(): Unit =
    if visible then
      Game.zoneSession.foreach(_.client.endLoot(corpseId))
      visible = false
      corpseId = 0
      money = None
      lootItems = Vector.empty

  override def render(): Unit =
    if !visible then return

    ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)

    val flags = extraFlags | (if locked then ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoResize else 0)
    pOpen.set(true)
    ImGui.begin(title, pOpen, flags)
    if !pOpen.get() then
      ImGui.end()
      close()
      return
    if fontScale != 1.0f then ImGui.setWindowFontScale(fontScale)
    renderContent()
    ImGui.end()

  override protected def renderContent(): Unit =
    // Money row (if any was on the corpse)
    money.foreach { m =>
      pushColor(ImGuiCol.Text, Colors.gold)
      val parts = Vector.newBuilder[String]
      if m.platinum > 0 then parts += s"${m.platinum} PP"
      if m.gold > 0 then parts += s"${m.gold} GP"
      if m.silver > 0 then parts += s"${m.silver} SP"
      if m.copper > 0 then parts += s"${m.copper} CP"
      ImGui.text(s"Money: ${parts.result().mkString("  ")}")
      ImGui.popStyleColor()
      ImGui.separator()
    }

    // Item list
    if lootItems.isEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text("  Empty")
      ImGui.popStyleColor()
    else
      for item <- lootItems do
        renderLootRow(item)

  private val IconSize = 32f

  private def renderLootRow(item: InventoryItem): Unit =
    val cx = ImGui.getCursorScreenPosX()
    val cy = ImGui.getCursorScreenPosY()

    // Icon
    ImGui.setCursorScreenPos(cx, cy)
    ItemIcons.render(item.icon, IconSize)

    // Invisible button over icon for hover tooltip + right-click loot
    ImGui.setCursorScreenPos(cx, cy)
    ImGui.invisibleButton(s"##looticon${item.equipSlot}", IconSize, IconSize)
    if ImGui.isItemHovered() then
      renderTooltip(item)
      if ImGui.isItemClicked(1) then lootItem(item)

    // Item name as a clickable button to loot (left or right click)
    ImGui.sameLine(0f, Spacing.slotGap)
    val nameColor = if item.magic then Colors.secondary else Colors.text
    pushColor(ImGuiCol.Text, nameColor)
    if ImGui.selectable(s"${item.name}##loot${item.equipSlot}") then
      lootItem(item)
    if ImGui.isItemClicked(1) then lootItem(item)
    ImGui.popStyleColor()

  // ---------------------------------------------------------------------------
  // Tooltip — reuses the same pattern as InventoryPanel
  // ---------------------------------------------------------------------------

  private def renderTooltip(item: InventoryItem): Unit =
    ImGui.beginTooltip()
    ImGui.setWindowFontScale(0.85f)

    if ItemIcons.render(item.icon, 40f) then ImGui.sameLine()
    val nameColor = if item.magic then Colors.secondary else Colors.text
    pushColor(ImGuiCol.Text, nameColor)
    ImGui.text(item.name)
    ImGui.popStyleColor()

    // Tags
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

    // Stats (common items only)
    if item.itemClass == 0 then
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
      if item.mr != 0 then tooltipStat("SvMagic", item.mr)
      if item.fr != 0 then tooltipStat("SvFire", item.fr)
      if item.cr != 0 then tooltipStat("SvCold", item.cr)
      if item.dr != 0 then tooltipStat("SvDisease", item.dr)
      if item.pr != 0 then tooltipStat("SvPoison", item.pr)

    // Weight
    val w = item.weight
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(s"Wt: ${w / 10}.${w % 10}")
    ImGui.popStyleColor()

    ImGui.endTooltip()

  private def tooltipStat(label: String, value: Int, suffix: String = ""): Unit =
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"  $label: $value$suffix")
    ImGui.popStyleColor()
