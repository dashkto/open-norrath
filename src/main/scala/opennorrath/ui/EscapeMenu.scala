package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCond, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*

import opennorrath.screen.GameContext

class EscapeMenu(ctx: GameContext):

  var isOpen = false
  private val settingsOpen = new ImBoolean(false)

  def toggle(): Unit = isOpen = !isOpen

  def render(): Unit =
    if !isOpen then return

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val menuW = 260f
    val menuH = 200f

    // Dim overlay
    val drawList = ImGui.getBackgroundDrawList()
    val (or, og, ob, oa) = Colors.overlay
    drawList.addRectFilled(0f, 0f, w, h, ImGui.colorConvertFloat4ToU32(or, og, ob, oa))

    ImGui.setNextWindowPos((w - menuW) / 2f, (h - menuH) / 2f, ImGuiCond.Always)
    ImGui.setNextWindowSize(menuW, menuH, ImGuiCond.Always)
    val flags = ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoMove |
      ImGuiWindowFlags.NoCollapse | ImGuiWindowFlags.NoScrollbar
    ImGui.begin("Menu", flags)

    val btnW = ImGui.getContentRegionAvailX()

    ImGuiTheme.pushPrimaryButton()
    if ImGui.button("Resume", btnW, Spacing.buttonHeight) then
      isOpen = false
    if ImGui.button("Settings", btnW, Spacing.buttonHeight) then
      settingsOpen.set(true)
    ImGuiTheme.popButtonStyle()

    ImGui.spacing()
    ImGui.separator()
    ImGui.spacing()

    ImGuiTheme.pushDangerButton()
    if ImGui.button("Quit", btnW, Spacing.buttonHeight) then
      glfwSetWindowShouldClose(ctx.window, true)
    ImGuiTheme.popButtonStyle()

    ImGui.end()

    if settingsOpen.get() then renderSettings()

  private def renderSettings(): Unit =
    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val settingsW = 400f
    val settingsH = 300f
    ImGui.setNextWindowPos((w - settingsW) / 2f, (h - settingsH) / 2f, ImGuiCond.Appearing)
    ImGui.setNextWindowSize(settingsW, settingsH, ImGuiCond.Appearing)
    val flags = ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoCollapse
    if ImGui.begin("Settings", settingsOpen, flags) then
      // TODO: settings content
      ()
    ImGui.end()
