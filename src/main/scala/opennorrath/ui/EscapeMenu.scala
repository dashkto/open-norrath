package opennorrath.ui

import imgui.ImGui
import imgui.`type`.ImBoolean
import imgui.flag.{ImGuiCond, ImGuiCol, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*

import opennorrath.{GameAction, InputBinding, KeyBind, MouseBind}
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
    val settingsH = 500f
    ImGui.setNextWindowPos((w - settingsW) / 2f, (h - settingsH) / 2f, ImGuiCond.Appearing)
    ImGui.setNextWindowSize(settingsW, settingsH, ImGuiCond.Appearing)
    val flags = ImGuiWindowFlags.NoCollapse
    if ImGui.begin("Settings", settingsOpen, flags) then
      if ImGui.collapsingHeader("Key Bindings", imgui.flag.ImGuiTreeNodeFlags.DefaultOpen) then
        renderKeyBindings()
    ImGui.end()

  private def renderKeyBindings(): Unit =
    val bindings = ctx.input.keyBindings.bindings
    val tableFlags = imgui.flag.ImGuiTableFlags.RowBg | imgui.flag.ImGuiTableFlags.BordersInnerH
    if ImGui.beginTable("##keybindings", 2, tableFlags) then
      ImGui.tableSetupColumn("Action", imgui.flag.ImGuiTableColumnFlags.WidthFixed, 160f)
      ImGui.tableSetupColumn("Key")
      ImGui.tableHeadersRow()
      // Display in a logical order
      for action <- GameAction.values do
        bindings.get(action).foreach { inputs =>
          ImGui.tableNextRow()
          ImGui.tableNextColumn()
          ImGui.text(actionLabel(action))
          ImGui.tableNextColumn()
          ImGui.text(inputs.map(bindingLabel).mkString(", "))
        }
      ImGui.endTable()

  private def actionLabel(action: GameAction): String = action match
    case GameAction.MoveForward    => "Move Forward"
    case GameAction.MoveBackward   => "Move Backward"
    case GameAction.StrafeLeft     => "Strafe Left"
    case GameAction.StrafeRight    => "Strafe Right"
    case GameAction.MoveUp         => "Move Up"
    case GameAction.MoveDown       => "Move Down"
    case GameAction.Jump           => "Jump"
    case GameAction.FreeLook       => "Free Look"
    case GameAction.Target         => "Target"
    case GameAction.TargetSelf     => "Target Self"
    case GameAction.TabTarget      => "Tab Target"
    case GameAction.AutoAttack     => "Auto Attack"
    case GameAction.Consider       => "Consider"
    case GameAction.Sit            => "Sit"
    case GameAction.ToggleInventory => "Inventory"
    case GameAction.ToggleSpellBook => "Spell Book"
    case GameAction.ToggleStats    => "Stats"
    case GameAction.DumpDebug      => "Debug Dump"
    case GameAction.DetachCamera   => "Detach Camera"
    case GameAction.Escape         => "Escape"

  private def bindingLabel(b: InputBinding): String = b match
    case KeyBind(key, shift) =>
      val name = keyName(key)
      if shift then s"Shift+$name" else name
    case MouseBind(button) => button match
      case GLFW_MOUSE_BUTTON_LEFT   => "Left Click"
      case GLFW_MOUSE_BUTTON_RIGHT  => "Right Click"
      case GLFW_MOUSE_BUTTON_MIDDLE => "Middle Click"
      case n                        => s"Mouse $n"

  private def keyName(key: Int): String = key match
    case GLFW_KEY_SPACE         => "Space"
    case GLFW_KEY_APOSTROPHE    => "'"
    case GLFW_KEY_COMMA         => ","
    case GLFW_KEY_MINUS         => "-"
    case GLFW_KEY_PERIOD        => "."
    case GLFW_KEY_SLASH         => "/"
    case GLFW_KEY_SEMICOLON     => ";"
    case GLFW_KEY_EQUAL         => "="
    case GLFW_KEY_LEFT_BRACKET  => "["
    case GLFW_KEY_BACKSLASH     => "\\"
    case GLFW_KEY_RIGHT_BRACKET => "]"
    case GLFW_KEY_GRAVE_ACCENT  => "`"
    case GLFW_KEY_ESCAPE        => "Esc"
    case GLFW_KEY_ENTER         => "Enter"
    case GLFW_KEY_TAB           => "Tab"
    case GLFW_KEY_BACKSPACE     => "Backspace"
    case GLFW_KEY_INSERT        => "Insert"
    case GLFW_KEY_DELETE        => "Delete"
    case GLFW_KEY_RIGHT         => "Right"
    case GLFW_KEY_LEFT          => "Left"
    case GLFW_KEY_DOWN          => "Down"
    case GLFW_KEY_UP            => "Up"
    case GLFW_KEY_PAGE_UP       => "Page Up"
    case GLFW_KEY_PAGE_DOWN     => "Page Down"
    case GLFW_KEY_HOME          => "Home"
    case GLFW_KEY_END           => "End"
    case GLFW_KEY_LEFT_SHIFT    => "L.Shift"
    case GLFW_KEY_LEFT_CONTROL  => "L.Ctrl"
    case GLFW_KEY_LEFT_ALT      => "L.Alt"
    case GLFW_KEY_RIGHT_SHIFT   => "R.Shift"
    case GLFW_KEY_RIGHT_CONTROL => "R.Ctrl"
    case GLFW_KEY_RIGHT_ALT     => "R.Alt"
    case k if k >= GLFW_KEY_A && k <= GLFW_KEY_Z =>
      (k - GLFW_KEY_A + 'A').toChar.toString
    case k if k >= GLFW_KEY_0 && k <= GLFW_KEY_9 =>
      (k - GLFW_KEY_0 + '0').toChar.toString
    case k if k >= GLFW_KEY_F1 && k <= GLFW_KEY_F12 =>
      s"F${k - GLFW_KEY_F1 + 1}"
    case k => s"Key($k)"
