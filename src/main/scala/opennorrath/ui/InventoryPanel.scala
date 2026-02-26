package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.network.{InventoryItem, ZoneEvent}

/** Inventory panel toggled with the "i" key. Shows equipment slots and general inventory.
  * Registers as a ZoneClient listener to receive InventoryLoaded events.
  */
class InventoryPanel extends Panel:

  val title = "Inventory"
  val defaultX = 400f
  val defaultY = 50f
  val defaultWidth = 280f
  val defaultHeight = 400f
  override def fontScale: Float = 0.9f

  visible = false
  private val pOpen = new ImBoolean(true)

  // Item data keyed by equipSlot
  private var equipmentBySlot = Map.empty[Int, InventoryItem]
  private var generalItems = Vector.empty[InventoryItem]

  val listener: ZoneEvent => Unit = {
    case ZoneEvent.InventoryLoaded(items) =>
      equipmentBySlot = items.filter(i => i.equipSlot >= 0 && i.equipSlot <= 21).map(i => i.equipSlot -> i).toMap
      generalItems = items.filter(i => i.equipSlot >= 22 && i.equipSlot <= 29)
    case _ => ()
  }

  def toggle(): Unit =
    visible = !visible
    if visible then pOpen.set(true)

  override def render(): Unit =
    if !visible then return

    ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)
    if minWidth > 0f || minHeight > 0f then
      ImGui.setNextWindowSizeConstraints(minWidth, minHeight, Float.MaxValue, Float.MaxValue)

    val flags = extraFlags | (if locked then ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoResize else 0)
    pOpen.set(true)
    ImGui.begin(title, pOpen, flags)
    if !pOpen.get() then
      visible = false
      ImGui.end()
      return
    if fontScale != 1.0f then ImGui.setWindowFontScale(fontScale)
    renderContent()

    if ImGui.beginPopupContextWindow("##inventorymenu") then
      if locked then
        if ImGui.menuItem("Unlock") then locked = false
      else
        if ImGui.menuItem("Lock") then locked = true
      ImGui.endPopup()

    ImGui.end()

  override protected def renderContent(): Unit =
    // Equipment section
    ImGui.pushFont(Fonts.defaultBold)
    pushColor(ImGuiCol.Text, Colors.gold)
    ImGui.text("Equipment")
    ImGui.popStyleColor()
    ImGui.popFont()
    ImGui.separator()

    for (slotId, slotName) <- InventoryItem.equipmentSlots do
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text(s"  $slotName:")
      ImGui.popStyleColor()
      ImGui.sameLine()
      equipmentBySlot.get(slotId) match
        case Some(item) =>
          if ItemIcons.render(item.icon, 20f) then ImGui.sameLine()
          val color = if item.magic then Colors.secondary else Colors.text
          pushColor(ImGuiCol.Text, color)
          ImGui.text(item.name)
          ImGui.popStyleColor()
          if ImGui.isItemHovered() then renderTooltip(item)
        case None =>
          pushColor(ImGuiCol.Text, Colors.withAlpha(Colors.textDim, 0.5f))
          ImGui.text("Empty")
          ImGui.popStyleColor()

    // General inventory section
    ImGui.spacing()
    ImGui.pushFont(Fonts.defaultBold)
    pushColor(ImGuiCol.Text, Colors.gold)
    ImGui.text("General")
    ImGui.popStyleColor()
    ImGui.popFont()
    ImGui.separator()

    if generalItems.isEmpty then
      pushColor(ImGuiCol.Text, Colors.withAlpha(Colors.textDim, 0.5f))
      ImGui.text("  (empty)")
      ImGui.popStyleColor()
    else
      for item <- generalItems do
        val slot = item.equipSlot - 22 + 1 // 1-based display
        val color = if item.magic then Colors.secondary else Colors.text
        pushColor(ImGuiCol.Text, Colors.textDim)
        ImGui.text(s"  Slot $slot:")
        ImGui.popStyleColor()
        ImGui.sameLine()
        if ItemIcons.render(item.icon, 20f) then ImGui.sameLine()
        pushColor(ImGuiCol.Text, color)
        ImGui.text(item.name)
        ImGui.popStyleColor()
        if ImGui.isItemHovered() then renderTooltip(item)

  private def renderTooltip(item: InventoryItem): Unit =
    ImGui.beginTooltip()
    ImGui.setWindowFontScale(0.85f)

    // Icon + item name
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
      if item.ac != 0 then statLine("AC", item.ac)
      if item.hp != 0 then statLine("HP", item.hp)
      if item.mana != 0 then statLine("Mana", item.mana)
      if item.damage != 0 then statLine("Dmg", item.damage)
      if item.delay != 0 then statLine("Delay", item.delay)

    // Lore
    if item.lore.nonEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.textWrapped(item.lore)
      ImGui.popStyleColor()

    // Weight
    val w = item.weight
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(s"Wt: ${w / 10}.${w % 10}")
    ImGui.popStyleColor()

    ImGui.endTooltip()

  private def statLine(label: String, value: Int): Unit =
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"  $label: $value")
    ImGui.popStyleColor()
