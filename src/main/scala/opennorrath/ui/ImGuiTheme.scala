package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiStyleVar}

/** Applies the Giant Goldfish color palette to ImGui style. */
object ImGuiTheme:

  def apply(): Unit =
    val style = ImGui.getStyle()

    // Rounding & spacing
    style.setWindowRounding(Spacing.rounding)
    style.setFrameRounding(Spacing.rounding)
    style.setGrabRounding(Spacing.rounding)
    style.setWindowPadding(Spacing.pad, Spacing.pad)
    style.setFramePadding(Spacing.framePadX, Spacing.framePadY)
    style.setItemSpacing(Spacing.itemSpacingX, Spacing.itemSpacingY)
    style.setWindowTitleAlign(0.5f, 0.5f)

    // Colors
    color(ImGuiCol.WindowBg, Colors.background)
    color(ImGuiCol.Text, Colors.text)
    color(ImGuiCol.TextDisabled, Colors.textDim)

    // Frames (input fields)
    color(ImGuiCol.FrameBg, Colors.inputBg)
    color(ImGuiCol.FrameBgHovered, Colors.tint(Colors.inputBg, Colors.primary, 0.15f))
    color(ImGuiCol.FrameBgActive, Colors.tint(Colors.inputBg, Colors.primary, 0.25f))

    // Buttons — dark text on orange for contrast
    color(ImGuiCol.Button, Colors.darkContainer)
    color(ImGuiCol.ButtonHovered, Colors.tint(Colors.darkContainer, Colors.cream, 0.15f))
    color(ImGuiCol.ButtonActive, Colors.tint(Colors.darkContainer, Colors.cream, 0.3f))

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

  /** Push primary (orange) button style — dark text on orange. Call `popButtonStyle()` after. */
  def pushPrimaryButton(): Unit =
    color3(ImGuiCol.Button, Colors.primary)
    color3(ImGuiCol.ButtonHovered, Colors.tint(Colors.primary, Colors.cream, 0.2f))
    color3(ImGuiCol.ButtonActive, Colors.primary2)
    color3(ImGuiCol.Text, Colors.buttonText)

  /** Push secondary (cyan) button style. Call `popButtonStyle()` after. */
  def pushSecondaryButton(): Unit =
    color3(ImGuiCol.Button, Colors.secondary)
    color3(ImGuiCol.ButtonHovered, Colors.tint(Colors.secondary, Colors.cream, 0.2f))
    color3(ImGuiCol.ButtonActive, Colors.secondary2)
    color3(ImGuiCol.Text, Colors.buttonText)

  /** Push danger (red) button style. Call `popButtonStyle()` after. */
  def pushDangerButton(): Unit =
    color3(ImGuiCol.Button, Colors.danger)
    color3(ImGuiCol.ButtonHovered, Colors.tint(Colors.danger, Colors.cream, 0.2f))
    color3(ImGuiCol.ButtonActive, Colors.tint(Colors.danger, Colors.cream, 0.4f))
    color3(ImGuiCol.Text, Colors.buttonText)

  /** Pop button style pushed by `pushPrimaryButton` or `pushDangerButton`. */
  def popButtonStyle(): Unit =
    ImGui.popStyleColor(4)

  private def color(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.getStyle().setColor(idx, c._1, c._2, c._3, c._4)

  private def color3(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
