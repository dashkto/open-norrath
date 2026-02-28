package opennorrath.ui

import imgui.ImGui
import imgui.flag.ImGuiCol

/** Shared tooltip renderer for spell icons, used by SpellBookPanel and SpellBarPanel. */
object SpellTooltip:

    def render(spellId: Int): Unit =
        ImGui.beginTooltip()
        ImGui.setWindowFontScale(0.85f)

        val (nr, ng, nb, na) = Colors.secondary
        ImGui.pushStyleColor(ImGuiCol.Text, nr, ng, nb, na)
        ImGui.text(SpellData.spellName(spellId))
        ImGui.popStyleColor()

        ImGui.separator()

        // Mana cost
        val mana = SpellData.spellMana(spellId)
        if mana > 0 then
            val (dr, dg, db, da) = Colors.textDim
            ImGui.pushStyleColor(ImGuiCol.Text, dr, dg, db, da)
            ImGui.text(s"  Mana: $mana")
            ImGui.popStyleColor()

        // Cast time
        val ct = SpellData.spellCastTime(spellId)
        if ct > 0 then
            val (dr, dg, db, da) = Colors.textDim
            ImGui.pushStyleColor(ImGuiCol.Text, dr, dg, db, da)
            val ctSec = ct / 1000f
            ImGui.text(f"  Cast: $ctSec%.1fs")
            ImGui.popStyleColor()

        ImGui.endTooltip()
