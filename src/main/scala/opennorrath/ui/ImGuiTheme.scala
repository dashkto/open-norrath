package opennorrath.ui

import imgui.ImGui
import imgui.flag.ImGuiCol

/** Applies the Giant Goldfish color palette to ImGui style. */
object ImGuiTheme:

  def apply(): Unit =
    val style = ImGui.getStyle()

    // Rounding
    style.setWindowRounding(4f)
    style.setFrameRounding(3f)
    style.setGrabRounding(2f)
    style.setWindowPadding(12f, 12f)
    style.setFramePadding(8f, 6f)
    style.setItemSpacing(8f, 8f)

    // Colors
    color(ImGuiCol.WindowBg, Colors.background)
    color(ImGuiCol.Text, Colors.text)
    color(ImGuiCol.TextDisabled, Colors.textDim)

    // Frames (input fields)
    color(ImGuiCol.FrameBg, Colors.inputBg)
    color(ImGuiCol.FrameBgHovered, Colors.tint(Colors.inputBg, Colors.primary, 0.15f))
    color(ImGuiCol.FrameBgActive, Colors.tint(Colors.inputBg, Colors.primary, 0.25f))

    // Buttons
    color(ImGuiCol.Button, Colors.primary)
    color(ImGuiCol.ButtonHovered, Colors.tint(Colors.primary, Colors.cream, 0.2f))
    color(ImGuiCol.ButtonActive, Colors.primary2)

    // Headers (selectable items)
    color(ImGuiCol.Header, Colors.withAlpha(Colors.primary, 0.25f))
    color(ImGuiCol.HeaderHovered, Colors.withAlpha(Colors.primary, 0.4f))
    color(ImGuiCol.HeaderActive, Colors.withAlpha(Colors.primary, 0.55f))

    // Title bar
    color(ImGuiCol.TitleBg, Colors.background)
    color(ImGuiCol.TitleBgActive, Colors.darkContainer)

    // Separator
    color(ImGuiCol.Separator, Colors.darkContainer)

    // Scrollbar
    color(ImGuiCol.ScrollbarBg, Colors.background)
    color(ImGuiCol.ScrollbarGrab, Colors.darkContainer)
    color(ImGuiCol.ScrollbarGrabHovered, Colors.slate)
    color(ImGuiCol.ScrollbarGrabActive, Colors.primary)

    // Check/Radio
    color(ImGuiCol.CheckMark, Colors.primary)

    // Slider
    color(ImGuiCol.SliderGrab, Colors.primary)
    color(ImGuiCol.SliderGrabActive, Colors.primary2)

    // Border
    color(ImGuiCol.Border, Colors.darkContainer)

  private def color(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.getStyle().setColor(idx, c._1, c._2, c._3, c._4)
