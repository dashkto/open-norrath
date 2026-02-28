package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiDragDropFlags, ImGuiWindowFlags}

import opennorrath.state.PlayerCharacter

/** Vertical list of 8 spell gem slots.
  *
  * Players drag spells from the SpellBookPanel onto gem slots to memorize them.
  * Left-click a memorized spell name to cast it. Hover for tooltip.
  */
class SpellBarPanel(player: PlayerCharacter) extends Panel:

    val title = "Spell Bar"
    val defaultX = 10f
    val defaultY = 200f
    val defaultWidth = 160f
    val defaultHeight = 0f // auto-size vertically
    override def fontScale: Float = Spacing.fontScaleSmall
    override def extraFlags: Int = ImGuiWindowFlags.NoScrollbar |
        ImGuiWindowFlags.AlwaysAutoResize

    /** Callback to cast a spell. Set by ZoneHud. Parameters: (gemSlot, spellId). */
    var onCastSpell: (Int, Int) => Unit = (_, _) => ()

    /** Callback to memorize a spell into a gem slot. Set by ZoneHud. Parameters: (gemSlot, spellId). */
    var onMemorizeSpell: (Int, Int) => Unit = (_, _) => ()

    /** Callback to unmemorize a spell from a gem slot. Set by ZoneHud. Parameters: (gemSlot, spellId). */
    var onForgetSpell: (Int, Int) => Unit = (_, _) => ()

    private val GemSlots = 8

    override protected def renderContent(): Unit =
        for i <- 0 until GemSlots do
            val spellId = player.memSpells(i)
            // Empty slots are -1 (server sends 0xFFFF as signed short, which decodes to -1)
            val hasMem = spellId > 0
            val label = if hasMem then s"${i + 1}. ${SpellData.spellName(spellId)}"
                         else s"${i + 1}. -empty-"

            ImGui.pushID(s"gem$i")

            // Color: memorized spells in secondary, empty in dim
            val color = if hasMem then Colors.secondary else Colors.textDim
            pushColor(ImGuiCol.Text, color)
            ImGui.selectable(label)
            ImGui.popStyleColor()

            // Tooltip on hover for memorized spells
            if hasMem && ImGui.isItemHovered() then SpellTooltip.render(spellId)

            // Left-click to cast
            if hasMem && ImGui.isItemClicked(0) then
                onCastSpell(i, spellId)

            // Right-click to unmemorize
            if hasMem && ImGui.isItemClicked(1) then
                player.memSpells(i) = -1
                onForgetSpell(i, spellId)

            // Drag source — allow re-dragging memorized spells to other gem slots
            if hasMem && ImGui.beginDragDropSource(ImGuiDragDropFlags.None) then
                ImGui.setDragDropPayload("SPELL_ID", Integer.valueOf(spellId))
                ImGui.text(SpellData.spellName(spellId))
                ImGui.endDragDropSource()

            // Drop target — accept spells dragged from spell book or other gem slots
            if ImGui.beginDragDropTarget() then
                val payload = ImGui.acceptDragDropPayload("SPELL_ID", classOf[Integer])
                if payload != null then
                    val droppedSpellId = payload.intValue()
                    player.memSpells(i) = droppedSpellId
                    onMemorizeSpell(i, droppedSpellId)
                ImGui.endDragDropTarget()

            ImGui.popID()
