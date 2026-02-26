package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiDragDropFlags, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.network.{InventoryItem, ZoneEvent}

/** Inventory panel toggled with the "i" key. Shows equipment slots and general inventory.
  * Registers as a ZoneClient listener to receive InventoryLoaded events.
  */
class InventoryPanel extends Panel:

  val title = "Inventory"
  val defaultX = 400f
  val defaultY = 50f
  val defaultWidth = 300f
  val defaultHeight = 500f
  override def fontScale: Float = 0.9f

  visible = false
  private val pOpen = new ImBoolean(true)

  // All items keyed by slot ID
  private var itemsBySlot = Map.empty[Int, InventoryItem]

  val listener: ZoneEvent => Unit = {
    case ZoneEvent.InventoryLoaded(items) =>
      itemsBySlot = items.filter(i => i.equipSlot >= 0 && i.equipSlot <= 29).map(i => i.equipSlot -> i).toMap
    case ZoneEvent.InventoryItemUpdated(item) =>
      if item.equipSlot >= 0 && item.equipSlot <= 29 then
        itemsBySlot = itemsBySlot + (item.equipSlot -> item)
    case ZoneEvent.InventoryMoved(from, to) =>
      swapSlots(from, to)
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

  // Slot pairs for two-column equipment layout
  private val slotRows: Vector[(Int, String, Int, String)] = Vector(
    (InventoryItem.Charm,     "Charm",      InventoryItem.Ammo,      "Ammo"),
    (InventoryItem.EarL,      "Ear (L)",    InventoryItem.EarR,      "Ear (R)"),
    (InventoryItem.Head,      "Head",       InventoryItem.Face,      "Face"),
    (InventoryItem.Neck,      "Neck",       InventoryItem.Shoulders, "Shoulders"),
    (InventoryItem.Back,      "Back",       InventoryItem.Arms,      "Arms"),
    (InventoryItem.WristL,    "Wrist (L)",  InventoryItem.WristR,    "Wrist (R)"),
    (InventoryItem.Range,     "Range",      InventoryItem.Hands,     "Hands"),
    (InventoryItem.RingL,     "Ring (L)",   InventoryItem.RingR,     "Ring (R)"),
    (InventoryItem.Primary,   "Primary",    InventoryItem.Secondary, "Secondary"),
    (InventoryItem.Chest,     "Chest",      InventoryItem.Waist,     "Waist"),
    (InventoryItem.Legs,      "Legs",       InventoryItem.Feet,      "Feet"),
  )

  private val SlotBoxH = 40f
  private val SlotPad = 4f

  override protected def renderContent(): Unit =
    val availW = ImGui.getContentRegionAvailX()
    val slotW = (availW - SlotPad) / 2f

    // Equipment slots
    for (leftId, leftName, rightId, rightName) <- slotRows do
      renderSlot(leftId, leftName, slotW)
      ImGui.sameLine(0f, SlotPad)
      renderSlot(rightId, rightName, slotW)

    // General inventory section
    ImGui.spacing()
    ImGui.separator()
    ImGui.pushFont(Fonts.defaultBold)
    pushColor(ImGuiCol.Text, Colors.gold)
    ImGui.text("General")
    ImGui.popStyleColor()
    ImGui.popFont()

    val genSlotW = (availW - SlotPad) / 2f
    for row <- 0 until 4 do
      val leftIdx = row
      val rightIdx = row + 4
      val leftSlotId = 22 + leftIdx
      val rightSlotId = 22 + rightIdx
      renderSlot(leftSlotId, s"Slot ${leftIdx + 1}", genSlotW)
      ImGui.sameLine(0f, SlotPad)
      renderSlot(rightSlotId, s"Slot ${rightIdx + 1}", genSlotW)

  private def renderSlot(slotId: Int, label: String, width: Float): Unit =
    val item = itemsBySlot.get(slotId)
    val drawList = ImGui.getWindowDrawList()
    val cx = ImGui.getCursorScreenPosX()
    val cy = ImGui.getCursorScreenPosY()

    // Background
    val bgColor = if item.isDefined then Colors.withAlpha(Colors.darkContainer, 0.6f) else Colors.withAlpha(Colors.background, 0.5f)
    val (br, bg, bb, ba) = bgColor
    drawList.addRectFilled(cx, cy, cx + width, cy + SlotBoxH,
      ImGui.colorConvertFloat4ToU32(br, bg, bb, ba), 3f)

    // Border
    val (borR, borG, borB, borA) = Colors.withAlpha(Colors.darkContainer, 0.8f)
    drawList.addRect(cx, cy, cx + width, cy + SlotBoxH,
      ImGui.colorConvertFloat4ToU32(borR, borG, borB, borA), 3f)

    item match
      case Some(it) =>
        val iconSize = SlotBoxH - 6f
        val iconX = cx + 3f
        val iconY = cy + 3f
        ImGui.setCursorScreenPos(iconX, iconY)
        val hasIcon = ItemIcons.render(it.icon, iconSize)
        val textX = if hasIcon then iconX + iconSize + 4f else cx + 4f
        val textH = ImGui.calcTextSize(it.name).y
        val (tr, tg, tb, ta) = if it.magic then Colors.secondary else Colors.text
        drawList.addText(textX, cy + (SlotBoxH - textH) / 2f,
          ImGui.colorConvertFloat4ToU32(tr, tg, tb, ta), it.name)
      case None =>
        val textW = ImGui.calcTextSize(label).x
        val textH = ImGui.calcTextSize(label).y
        val (tr, tg, tb, ta) = Colors.withAlpha(Colors.textDim, 0.5f)
        drawList.addText(cx + (width - textW) / 2f, cy + (SlotBoxH - textH) / 2f,
          ImGui.colorConvertFloat4ToU32(tr, tg, tb, ta), label)

    // Invisible button for interaction
    ImGui.setCursorScreenPos(cx, cy)
    ImGui.invisibleButton(s"##slot$slotId", width, SlotBoxH)
    if item.isDefined && ImGui.isItemHovered() then renderTooltip(item.get)

    // Drag source — occupied slots can be dragged
    if item.isDefined && ImGui.beginDragDropSource(ImGuiDragDropFlags.None) then
      ImGui.setDragDropPayload("INV_SLOT", Integer.valueOf(slotId))
      // Preview tooltip
      val it = item.get
      ItemIcons.render(it.icon, 32f)
      ImGui.sameLine()
      ImGui.text(it.name)
      ImGui.endDragDropSource()

    // Drop target — validate slot restrictions before accepting
    if ImGui.beginDragDropTarget() then
      val payload = ImGui.acceptDragDropPayload("INV_SLOT", classOf[Integer])
      if payload != null then
        val sourceSlot = payload.intValue()
        if sourceSlot != slotId then
          val sourceItem = itemsBySlot.get(sourceSlot)
          val destItem = itemsBySlot.get(slotId)
          // Check both items can go in their destination slots
          val srcAllowed = sourceItem.forall(_.canEquipIn(slotId))
          val dstAllowed = destItem.forall(_.canEquipIn(sourceSlot))
          if srcAllowed && dstAllowed then
            swapSlots(sourceSlot, slotId)
            Game.zoneSession.foreach(_.client.sendMoveItem(sourceSlot, slotId))
      ImGui.endDragDropTarget()

  private def swapSlots(from: Int, to: Int): Unit =
    if to == -1 || to == 0xFFFFFFFF then
      // Server delete — remove item from slot
      itemsBySlot = itemsBySlot - from
    else
      val fromItem = itemsBySlot.get(from)
      val toItem = itemsBySlot.get(to)
      var updated = itemsBySlot - from - to
      fromItem.foreach(it => updated = updated + (to -> it.copy(equipSlot = to)))
      toItem.foreach(it => updated = updated + (from -> it.copy(equipSlot = from)))
      itemsBySlot = updated

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

    // Equippable slots
    val slotNames = slotsToNames(item.slots)
    if slotNames.nonEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text(s"Slot: ${slotNames.mkString(" ")}")
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

  private val SlotNamesByBit: Vector[(Int, String)] = Vector(
    0 -> "CHARM", 1 -> "EAR", 2 -> "HEAD", 3 -> "FACE", 4 -> "EAR",
    5 -> "NECK", 6 -> "SHOULDERS", 7 -> "ARMS", 8 -> "BACK",
    9 -> "WRIST", 10 -> "WRIST", 11 -> "RANGE", 12 -> "HANDS",
    13 -> "PRIMARY", 14 -> "SECONDARY", 15 -> "FINGERS", 16 -> "FINGERS",
    17 -> "CHEST", 18 -> "LEGS", 19 -> "FEET", 20 -> "WAIST", 21 -> "AMMO",
  )

  private def slotsToNames(slots: Int): Vector[String] =
    val names = Vector.newBuilder[String]
    val seen = scala.collection.mutable.Set.empty[String]
    for (bit, name) <- SlotNamesByBit do
      if (slots & (1 << bit)) != 0 && seen.add(name) then
        names += name
    names.result()

  private def statLine(label: String, value: Int): Unit =
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"  $label: $value")
    ImGui.popStyleColor()
