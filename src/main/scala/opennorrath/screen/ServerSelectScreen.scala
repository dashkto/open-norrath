package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiKey, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{Game, WorldSession}
import opennorrath.network.*
import opennorrath.ui.Colors

class ServerSelectScreen(
  ctx: GameContext,
  servers: Vector[ServerInfo],
) extends Screen:

  private def loginSession = Game.loginSession.get

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
      Game.loginSession.foreach(_.stop())
      Game.loginSession = None
      Game.worldSession.foreach(_.stop())
      Game.worldSession = None
      Game.setScreen(LoginScreen(ctx))

    // Poll login events (login session may be gone after connectToWorld)
    Game.loginSession.foreach { session =>
      var event = session.client.pollEvent()
      while event.isDefined do
        event.get match
          case LoginEvent.PlayApproved(key) =>
            statusText = s"Play approved - connecting to world..."
            statusColor = Colors.success
            println(s"[ServerSelect] World key received: $key")
          case LoginEvent.LoginComplete =>
            statusText = "Connecting to world server..."
            statusColor = Colors.success
            connectToWorld()
          case LoginEvent.Error(msg) =>
            statusText = msg
            statusColor = Colors.error
            waiting = false
          case _ => ()
        event = session.client.pollEvent()
    }

    // Poll world events
    pollWorldEvents()

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

  override def dispose(): Unit = ()

  private def selectCurrent(): Unit =
    println(s"[ServerSelect] selectCurrent() called, servers=${servers.size}, waiting=$waiting")
    if servers.nonEmpty && !waiting then
      val server = servers(selectedIndex)
      println(s"[ServerSelect] Selecting server: ${server.name} (${server.ip})")
      statusText = s"Connecting to ${server.name}..."
      statusColor = Colors.text
      waiting = true
      loginSession.client.selectServer(server.ip)

  private def pollWorldEvents(): Unit =
    Game.worldSession match
      case None => return
      case Some(session) =>
        val wc = session.client
        var done = false
        var wEvent = wc.pollEvent()
        while wEvent.isDefined && !done do
          wEvent.get match
            case WorldEvent.CharacterList(chars) =>
              Game.setScreen(CharacterSelectScreen(ctx, chars))
              done = true
            case WorldEvent.Error(msg) =>
              statusText = msg
              statusColor = Colors.error
              waiting = false
            case WorldEvent.StateChanged(s) =>
              println(s"[ServerSelect] World state: $s")
            case _ => ()
          if !done then wEvent = wc.pollEvent()

  private def connectToWorld(): Unit =
    val loginClient = loginSession.client
    // Parse account ID from session ID ("LS#123" → 123)
    val accountId = loginClient.sessionId.stripPrefix("LS#").toIntOption.getOrElse(0)
    val serverIp = servers.lift(selectedIndex).map(_.ip).getOrElse(ctx.settings.login.host)
    val worldPort = ctx.settings.login.worldPort

    println(s"[ServerSelect] Connecting to world $serverIp:$worldPort (account=$accountId, key=${loginClient.worldKey})")

    // Stop login session, start world session
    Game.loginSession.foreach(_.stop())
    Game.loginSession = None

    val wc = WorldClient()
    val wnt = NetworkThread(wc)
    Game.worldSession = Some(WorldSession(wc, wnt))
    wnt.start()
    wnt.send(NetCommand.Connect(serverIp, worldPort))
    wc.connect(accountId, loginClient.worldKey)

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
