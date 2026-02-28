package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import opennorrath.Game
import opennorrath.network.{InventoryItem, MerchantOpen}

/** Merchant buy panel shown when right-clicking a merchant NPC.
  * Displays the merchant's inventory as a scrollable list of items with prices.
  * Clicking an item sends OP_ShopPlayerBuy to the server.
  * Closing the panel sends OP_ShopEnd.
  */
class MerchantPanel extends Panel:

    val title = "Merchant"
    val defaultX = 300f
    val defaultY = 150f
    val defaultWidth = 320f
    val defaultHeight = 400f
    override def fontScale: Float = Spacing.fontScaleMedium

    visible = false
    private val pOpen = new ImBoolean(true)

    /** Merchant NPC spawn ID. */
    private var merchantId: Int = 0

    /** Price modifier from the server (faction/charisma adjusted). */
    private var rate: Float = 1.0f

    /** Merchant name shown in the title bar. */
    private var merchantName: String = "Merchant"

    /** Items available for purchase. */
    private var items: Vector[InventoryItem] = Vector.empty

    /** Open the merchant panel with the given data. */
    def open(open: MerchantOpen, shopItems: Vector[InventoryItem], name: String): Unit =
        merchantId = open.merchantId
        rate = open.rate
        merchantName = name
        items = shopItems
        visible = true
        pOpen.set(true)

    /** Add or update an item in the merchant's stock (from MerchantItemPacket). */
    def addItem(item: InventoryItem): Unit =
        // Replace existing item in the same slot, or append
        val idx = items.indexWhere(_.equipSlot == item.equipSlot)
        if idx >= 0 then items = items.updated(idx, item)
        else items = items :+ item

    /** Close the merchant panel and notify the server. */
    def close(): Unit =
        if visible then
            Game.zoneSession.foreach(_.client.closeMerchant())
            visible = false
            merchantId = 0
            items = Vector.empty

    override def render(): Unit =
        if !visible then return

        ImGui.setNextWindowPos(defaultX, defaultY, ImGuiCond.FirstUseEver)
        ImGui.setNextWindowSize(defaultWidth, defaultHeight, ImGuiCond.FirstUseEver)
        ImGui.setNextWindowSizeConstraints(260f, 200f, Float.MaxValue, Float.MaxValue)

        val flags = extraFlags |
            (if locked then ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoResize else 0)
        pOpen.set(true)
        ImGui.begin(s"$merchantName###merchant", pOpen, flags)
        if !pOpen.get() then
            ImGui.end()
            close()
            return
        if fontScale != 1.0f then ImGui.setWindowFontScale(fontScale)
        renderContent()
        ImGui.end()

    override protected def renderContent(): Unit =
        // Price modifier header — shows the merchant's rate adjustment (faction/charisma)
        pushColor(ImGuiCol.Text, Colors.textDim)
        val pct = (rate * 100).toInt
        ImGui.text(s"Price modifier: $pct%%")
        ImGui.popStyleColor()
        ImGui.separator()

        if items.isEmpty then
            pushColor(ImGuiCol.Text, Colors.textDim)
            ImGui.text("  This merchant has nothing for sale.")
            ImGui.popStyleColor()
        else
            for item <- items do
                renderItemRow(item)

    private val IconSize = 32f

    private def renderItemRow(item: InventoryItem): Unit =
        val cx = ImGui.getCursorScreenPosX()
        val cy = ImGui.getCursorScreenPosY()

        // Icon
        ImGui.setCursorScreenPos(cx, cy)
        ItemIcons.render(item.icon, IconSize)

        // Invisible button over icon for hover tooltip
        ImGui.setCursorScreenPos(cx, cy)
        ImGui.invisibleButton(s"##shopicon${item.equipSlot}", IconSize, IconSize)
        if ImGui.isItemHovered() then renderTooltip(item)

        // Item name + price as a clickable selectable to buy
        ImGui.sameLine(0f, Spacing.slotGap)
        val nameColor = if item.magic then Colors.secondary else Colors.text
        pushColor(ImGuiCol.Text, nameColor)
        val priceStr = formatPrice(adjustPrice(item.price))
        if ImGui.selectable(s"${item.name}  $priceStr##shop${item.equipSlot}") then
            buyItem(item)
        ImGui.popStyleColor()

    /** Apply the merchant's rate modifier to the item's base price. */
    private def adjustPrice(basePriceCopper: Int): Int =
        (basePriceCopper.toLong * rate.toDouble).toInt.max(0)

    /** Convert copper to a human-readable price string (PP/GP/SP/CP). */
    private def formatPrice(copper: Int): String =
        if copper <= 0 then return "Free"
        val pp = copper / 1000
        val gp = (copper % 1000) / 100
        val sp = (copper % 100) / 10
        val cp = copper % 10
        val parts = Vector.newBuilder[String]
        if pp > 0 then parts += s"${pp}pp"
        if gp > 0 then parts += s"${gp}gp"
        if sp > 0 then parts += s"${sp}sp"
        if cp > 0 then parts += s"${cp}cp"
        val result = parts.result()
        if result.isEmpty then "Free" else result.mkString(" ")

    private def buyItem(item: InventoryItem): Unit =
        Game.zoneSession.foreach(_.client.buyFromMerchant(item.equipSlot, 1))

    // ---------------------------------------------------------------------------
    // Tooltip — same pattern as LootPanel / InventoryPanel
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

        // Price
        ImGui.separator()
        val priceStr = formatPrice(adjustPrice(item.price))
        pushColor(ImGuiCol.Text, Colors.gold)
        ImGui.text(s"  Price: $priceStr")
        ImGui.popStyleColor()

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
