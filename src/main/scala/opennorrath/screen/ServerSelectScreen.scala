package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiKey, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.Game
import opennorrath.network.{LoginClient, LoginEvent, NetworkThread, ServerInfo}
import opennorrath.ui.Colors

class ServerSelectScreen(
  ctx: GameContext,
  loginClient: LoginClient,
  networkThread: NetworkThread,
  servers: Vector[ServerInfo],
) extends Screen:

  private var selectedIndex = 0
  private var statusText = s"${servers.size} server(s) found"
  private var statusColor = Colors.textDim
  private var waiting = false

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

  override def update(dt: Float): Unit =
    // Keyboard navigation
    if !waiting then
      if ImGui.isKeyPressed(ImGuiKey.UpArrow) || ImGui.isKeyPressed(ImGuiKey.W) then
        selectedIndex = (selectedIndex - 1 + servers.size) % servers.size
      if ImGui.isKeyPressed(ImGuiKey.DownArrow) || ImGui.isKeyPressed(ImGuiKey.S) then
        selectedIndex = (selectedIndex + 1) % servers.size

      if ImGui.isKeyPressed(ImGuiKey.Enter) || ImGui.isKeyPressed(ImGuiKey.KeypadEnter) then
        selectCurrent()

    // Escape to go back
    if ImGui.isKeyPressed(ImGuiKey.Escape) then
      Game.setScreen(LoginScreen(ctx))

    // Poll events
    var event = loginClient.pollEvent()
    while event.isDefined do
      event.get match
        case LoginEvent.PlayApproved(key) =>
          statusText = s"Play approved! Key: $key"
          statusColor = Colors.success
          println(s"[ServerSelect] World key received: $key")
        case LoginEvent.LoginComplete =>
          statusText = "Login complete!"
          statusColor = Colors.success
        case LoginEvent.Error(msg) =>
          statusText = msg
          statusColor = Colors.error
          waiting = false
        case _ => ()
      event = loginClient.pollEvent()

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##serverselect", flags)

    // Title
    val titleText = "SELECT SERVER"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, 100f)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text(titleText)
    ImGui.popStyleColor()

    // Server list — fixed-width column, centered
    val listW = 300f
    val listX = (w - listW) / 2f
    val listStartY = 200f
    for (server, i) <- servers.zipWithIndex do
      val selected = i == selectedIndex
      val itemY = listStartY + i * 60f

      // Server name
      ImGui.setCursorPos(listX, itemY)
      if selected then
        pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(server.name, selected, 0, listW, 0f) then
        selectedIndex = i
        selectCurrent()
      if ImGui.isItemHovered() then selectedIndex = i
      if selected then ImGui.popStyleColor()

      // Detail line
      val detail = s"${server.ip}  |  ${server.userCount} players"
      val detailW = ImGui.calcTextSize(detail).x
      ImGui.setCursorPos((w - detailW) / 2f, itemY + 28f)
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text(detail)
      ImGui.popStyleColor()

    // Status
    val statusTextW = ImGui.calcTextSize(statusText).x
    ImGui.setCursorPos((w - statusTextW) / 2f, h - 65f)
    pushColor(ImGuiCol.Text, statusColor)
    ImGui.text(statusText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit =
    networkThread.stop()

  private def selectCurrent(): Unit =
    if servers.nonEmpty && !waiting then
      val server = servers(selectedIndex)
      statusText = s"Connecting to ${server.name}..."
      statusColor = Colors.text
      waiting = true
      loginClient.selectServer(server.ip)

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
