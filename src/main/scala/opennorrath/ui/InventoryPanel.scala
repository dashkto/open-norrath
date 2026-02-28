package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiDragDropFlags, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.network.{InventoryItem, ItemType}
import opennorrath.state.PlayerCharacter

/** Inventory panel toggled with the "i" key.
  * Three-column layout: equipment (left), general inventory (middle), bags (right).
  * Stats are displayed in the separate StatsPanel (toggled with "o" key).
  */
class InventoryPanel(player: Option[PlayerCharacter] = None) extends Panel:

  val title = "Inventory"
  val defaultX = 200f
  val defaultY = 50f
  val defaultWidth = 880f
  val defaultHeight = 580f
  override def fontScale: Float = Spacing.fontScaleMedium

  visible = false
  private val pOpen = new ImBoolean(true)

  /** Callback to scribe a spell scroll. Set by ZoneHud.
    * Parameters: (inventorySlot, scrollSpellId).
    */
  var onScribeSpell: (Int, Int) => Unit = (_, _) => ()

  private def itemsBySlot: Map[Int, InventoryItem] =
    player.map(_.inventory.items).getOrElse(Map.empty)

  def toggle(): Unit =
    visible = !visible
    if visible then pOpen.set(true)

  override def render(): Unit =
    if !visible then return

    ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)
    ImGui.setNextWindowSizeConstraints(640f, 400f, Float.MaxValue, Float.MaxValue)

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

  // Equipment slots grouped into rows
  private val equipRows: Vector[Vector[(Int, String)]] = Vector(
    // Head & face
    Vector(
      (InventoryItem.Charm, "Charm"), (InventoryItem.EarL, "Ear L"),
      (InventoryItem.Head, "Head"),   (InventoryItem.Face, "Face"),
      (InventoryItem.EarR, "Ear R"),  (InventoryItem.Neck, "Neck"),
    ),
    // Upper body
    Vector(
      (InventoryItem.Shoulders, "Shld"), (InventoryItem.Arms, "Arms"),
      (InventoryItem.Back, "Back"),      (InventoryItem.WristL, "Wrs L"),
      (InventoryItem.WristR, "Wrs R"),   (InventoryItem.Hands, "Hands"),
    ),
    // Lower body
    Vector(
      (InventoryItem.Chest, "Chest"), (InventoryItem.Waist, "Waist"),
      (InventoryItem.Legs, "Legs"),   (InventoryItem.Feet, "Feet"),
    ),
    // Weapons & accessories
    Vector(
      (InventoryItem.Primary, "Pri"),   (InventoryItem.Secondary, "Sec"),
      (InventoryItem.Range, "Range"),   (InventoryItem.RingL, "Ring L"),
      (InventoryItem.RingR, "Ring R"),  (InventoryItem.Ammo, "Ammo"),
    ),
  )

  private val SlotBoxH = Spacing.slotBoxHeight
  private val SlotSize = Spacing.slotSize
  private val SlotGap = Spacing.slotGap
  private val SlotPad = Spacing.slotPad
  private val ColPad = Spacing.columnGap

  override protected def renderContent(): Unit =
    val availH = ImGui.getContentRegionAvailY()
    val equipW = SlotSize * MaxSlotsPerRow + SlotGap * (MaxSlotsPerRow - 1)
    val invCols = 2
    val invW = SlotSize * invCols + SlotGap * (invCols - 1)
    // Bag column: fits BagRowCols slots per row + gaps
    val bagW = SlotSize * BagRowCols + SlotGap * (BagRowCols - 1)
    val childFlags = ImGuiWindowFlags.NoBackground
    val moneyRowH = ImGui.getTextLineHeightWithSpacing() + Spacing.slotGap * 2

    // Left column — equipment grid (fixed size)
    ImGui.beginChild("##equip", equipW, availH - moneyRowH, false, childFlags)
    sectionHeader("Equipment")
    for row <- equipRows do
      renderSlotRow(row)
    ImGui.endChild()

    ImGui.sameLine(0f, ColPad)

    // Middle column — general inventory (2x4 grid)
    ImGui.beginChild("##general", invW, availH - moneyRowH, false, childFlags)
    sectionHeader("General")
    for row <- 0 until 4 do
      for col <- 0 until invCols do
        val i = row * invCols + col
        renderSquareSlot(22 + i, s"Slot ${i + 1}", SlotSize)
        if col < invCols - 1 then ImGui.sameLine(0f, SlotGap)
      ImGui.spacing()
    ImGui.endChild()

    ImGui.sameLine(0f, ColPad)

    // Right column — bag contents (scrollable)
    ImGui.beginChild("##bags", bagW, availH - moneyRowH, false, childFlags)
    renderBagsColumn()
    ImGui.endChild()

    // Money row across the bottom
    player.foreach(renderMoneyRow)

  // ---------------------------------------------------------------------------
  // Bag column
  // ---------------------------------------------------------------------------

  /** Max slots displayed per visual row in a bag. */
  private val BagRowCols = 5

  private def renderBagsColumn(): Unit =
    sectionHeader("Bags")
    val items = itemsBySlot
    for generalSlot <- 22 until 30 do
      items.get(generalSlot) match
        case Some(bag) if bag.isBag =>
          renderBagRow(generalSlot, bag)
        case _ => () // skip empty general slots and non-bag items

  /** Render one bag: icon + name header, then a grid of content slots. */
  private def renderBagRow(generalSlot: Int, bag: InventoryItem): Unit =
    // Bag icon + name as a compact header with hover tooltip
    val drawList = ImGui.getWindowDrawList()
    val hx = ImGui.getCursorScreenPosX()
    val hy = ImGui.getCursorScreenPosY()
    val iconSize = SlotBoxH - 6f
    ImGui.setCursorScreenPos(hx, hy)
    ItemIcons.render(bag.icon, iconSize)
    ImGui.setCursorScreenPos(hx, hy)
    ImGui.invisibleButton(s"##bag$generalSlot", iconSize, iconSize)
    if ImGui.isItemHovered() then renderTooltip(bag)
    ImGui.sameLine(0f, SlotGap)
    val nameY = hy + (iconSize - ImGui.calcTextSize(bag.name).y) / 2f
    val (nr, ng, nb, na) = Colors.gold
    drawList.addText(hx + iconSize + SlotGap, nameY,
      ImGui.colorConvertFloat4ToU32(nr, ng, nb, na), bag.name)
    // Advance cursor past the header line
    ImGui.setCursorScreenPos(hx, hy + iconSize + SlotGap)

    // Bag content slots: 250 + (generalSlot - 22) * 10 + index
    val baseSlot = 250 + (generalSlot - 22) * 10
    for i <- 0 until bag.bagSlots do
      val contentSlot = baseSlot + i
      renderSquareSlot(contentSlot, (i + 1).toString, SlotSize)
      val isLastInRow = (i + 1) % BagRowCols == 0
      val isLast = i == bag.bagSlots - 1
      if !isLastInRow && !isLast then
        ImGui.sameLine(0f, SlotGap)
    ImGui.spacing()
    ImGui.spacing()

  // ---------------------------------------------------------------------------
  // Money row
  // ---------------------------------------------------------------------------

  private def renderMoneyRow(pc: PlayerCharacter): Unit =
    ImGui.spacing()
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(s"PP: ${pc.platinum}   GP: ${pc.gold}   SP: ${pc.silver}   CP: ${pc.copper}")
    ImGui.popStyleColor()

  // ---------------------------------------------------------------------------
  // Shared rendering helpers
  // ---------------------------------------------------------------------------

  private val MaxSlotsPerRow = equipRows.map(_.size).max

  private def renderSlotRow(slots: Vector[(Int, String)]): Unit =
    for i <- slots.indices do
      val (slotId, label) = slots(i)
      renderSquareSlot(slotId, label, SlotSize)
      if i < slots.size - 1 then ImGui.sameLine(0f, SlotGap)
    ImGui.spacing()

  private def renderSquareSlot(slotId: Int, label: String, size: Float): Unit =
    val item = itemsBySlot.get(slotId)
    val drawList = ImGui.getWindowDrawList()
    val cx = ImGui.getCursorScreenPosX()
    val cy = ImGui.getCursorScreenPosY()

    // Background
    val bgColor = if item.isDefined then Colors.withAlpha(Colors.darkContainer, 0.6f) else Colors.withAlpha(Colors.background, 0.5f)
    val (br, bg, bb, ba) = bgColor
    drawList.addRectFilled(cx, cy, cx + size, cy + size,
      ImGui.colorConvertFloat4ToU32(br, bg, bb, ba), Spacing.rounding)

    // Border
    val (borR, borG, borB, borA) = Colors.withAlpha(Colors.darkContainer, 0.8f)
    drawList.addRect(cx, cy, cx + size, cy + size,
      ImGui.colorConvertFloat4ToU32(borR, borG, borB, borA), Spacing.rounding)

    item match
      case Some(it) =>
        val iconSize = size - 6f
        ImGui.setCursorScreenPos(cx + 3f, cy + 3f)
        ItemIcons.render(it.icon, iconSize)
        // Stack count overlay (bottom-right corner)
        if it.stackCount > 0 then
          val countStr = it.stackCount.toString
          val countW = ImGui.calcTextSize(countStr).x
          val countH = ImGui.calcTextSize(countStr).y
          val (cr, cg, cb, ca) = Colors.cream
          drawList.addText(cx + size - countW - 3f, cy + size - countH - 2f,
            ImGui.colorConvertFloat4ToU32(cr, cg, cb, ca), countStr)
      case None =>
        val textW = ImGui.calcTextSize(label).x
        val textH = ImGui.calcTextSize(label).y
        val (tr, tg, tb, ta) = Colors.withAlpha(Colors.textDim, 0.5f)
        drawList.addText(cx + (size - textW) / 2f, cy + (size - textH) / 2f,
          ImGui.colorConvertFloat4ToU32(tr, tg, tb, ta), label)

    // Invisible button for interaction
    ImGui.setCursorScreenPos(cx, cy)
    ImGui.invisibleButton(s"##slot$slotId", size, size)
    if item.isDefined && ImGui.isItemHovered() then renderTooltip(item.get)

    // Drag source
    if item.isDefined && ImGui.beginDragDropSource(ImGuiDragDropFlags.None) then
      ImGui.setDragDropPayload("INV_SLOT", Integer.valueOf(slotId))
      val it = item.get
      ItemIcons.render(it.icon, 32f)
      ImGui.sameLine()
      ImGui.text(it.name)
      ImGui.endDragDropSource()

    // Drop target
    if ImGui.beginDragDropTarget() then
      val payload = ImGui.acceptDragDropPayload("INV_SLOT", classOf[Integer])
      if payload != null then
        val sourceSlot = payload.intValue()
        if sourceSlot != slotId then
          val sourceItem = itemsBySlot.get(sourceSlot)
          val destItem = itemsBySlot.get(slotId)
          val srcAllowed = sourceItem.forall(_.canEquipIn(slotId))
          val dstAllowed = destItem.forall(_.canEquipIn(sourceSlot))
          if srcAllowed && dstAllowed then
            Game.zoneSession.foreach(_.client.sendMoveItem(sourceSlot, slotId))
      ImGui.endDragDropTarget()

    // Right-click context menu for spell scrolls
    item.foreach { it =>
      if it.itemType == ItemType.Spell && it.scrollSpellId > 0 then
        if ImGui.beginPopupContextItem(s"##ctx$slotId") then
          val spellName = SpellData.spellName(it.scrollSpellId)
          if ImGui.menuItem(s"Scribe: $spellName") then
            onScribeSpell(slotId, it.scrollSpellId)
          ImGui.endPopup()
    }

  // ---------------------------------------------------------------------------
  // Tooltip
  // ---------------------------------------------------------------------------

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

    // Item type (skip containers/books and generic "Unknown" types)
    if item.itemClass == 0 && !item.itemType.label.startsWith("Unknown") then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text(s"Type: ${item.itemType.label}")
      ImGui.popStyleColor()

    ImGui.separator()

    // Stats (common items only)
    if item.itemClass == 0 then
      if item.ac != 0 then tooltipStat("AC", item.ac)
      if item.hp != 0 then tooltipStat("HP", item.hp)
      if item.mana != 0 then tooltipStat("Mana", item.mana)
      if item.damage != 0 then tooltipStat("Dmg", item.damage)
      if item.delay != 0 then tooltipStat("Delay", item.delay)
      // Attribute bonuses
      if item.aStr != 0 then tooltipStat("STR", item.aStr)
      if item.aSta != 0 then tooltipStat("STA", item.aSta)
      if item.aAgi != 0 then tooltipStat("AGI", item.aAgi)
      if item.aDex != 0 then tooltipStat("DEX", item.aDex)
      if item.aWis != 0 then tooltipStat("WIS", item.aWis)
      if item.aInt != 0 then tooltipStat("INT", item.aInt)
      if item.aCha != 0 then tooltipStat("CHA", item.aCha)
      // Resist bonuses
      if item.mr != 0 then tooltipStat("SvMagic", item.mr)
      if item.fr != 0 then tooltipStat("SvFire", item.fr)
      if item.cr != 0 then tooltipStat("SvCold", item.cr)
      if item.dr != 0 then tooltipStat("SvDisease", item.dr)
      if item.pr != 0 then tooltipStat("SvPoison", item.pr)

    // Container info (bags)
    if item.isBag then
      tooltipStat("Slots", item.bagSlots)
      val sizeName = item.bagSize match
        case 0 => "Tiny"
        case 1 => "Small"
        case 2 => "Medium"
        case 3 => "Large"
        case _ => "Giant"
      pushColor(ImGuiCol.Text, Colors.text)
      ImGui.text(s"  Size: $sizeName")
      ImGui.popStyleColor()
      if item.bagWR > 0 then tooltipStat("WR", item.bagWR, "%")

    // Charges / stack count
    if item.stackable && item.charges > 1 then
      tooltipStat("Count", item.charges)
    else if !item.stackable && item.charges > 0 then
      tooltipStat("Charges", item.charges)

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

  private def tooltipStat(label: String, value: Int, suffix: String = ""): Unit =
    pushColor(ImGuiCol.Text, Colors.text)
    ImGui.text(s"  $label: $value$suffix")
    ImGui.popStyleColor()
