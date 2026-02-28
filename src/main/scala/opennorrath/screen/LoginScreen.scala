package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiInputTextFlags, ImGuiKey, ImGuiWindowFlags}
import imgui.`type`.ImString

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{Game, LoginSession}
import opennorrath.network.{LoginClient, LoginEvent, NetCommand, NetworkThread}
import opennorrath.network.titanium.TitaniumNetworkThread
import opennorrath.ui.Colors

class LoginScreen(ctx: GameContext) extends Screen:

  private val loginClient = LoginClient()
  private val networkThread =
    if Game.macMode then NetworkThread(loginClient)
    else TitaniumNetworkThread(loginClient)

  private val username = new ImString(64)
  private val password = new ImString(64)
  private var statusText = "Enter credentials"
  private var statusColor = Colors.textDim
  private var connecting = false
  private var focusUsername = true

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)
    Game.loginSession = Some(LoginSession(loginClient, networkThread))
    networkThread.start()

  override def update(dt: Float): Unit =
    // Escape to go back
    if ImGui.isKeyPressed(ImGuiKey.Escape) then
      Game.loginSession.foreach(_.stop())
      Game.loginSession = None
      Game.setScreen(SplashScreen(ctx, "assets/arena.s3d"))
      return

    // Poll login events
    var event = loginClient.pollEvent()
    while event.isDefined do
      event.get match
        case LoginEvent.Error(msg) =>
          statusText = msg
          statusColor = Colors.error
          connecting = false
        case LoginEvent.DateReceived(date) =>
          statusText = s"Connected ($date)"
          statusColor = Colors.text
        case LoginEvent.LoginSuccess(sid) =>
          statusText = s"Logged in: $sid"
          statusColor = Colors.success
        case LoginEvent.ServerListUpdated(servers) =>
          Game.setScreen(ServerSelectScreen(ctx, servers))
          return
        case LoginEvent.StateChanged(s) =>
          if statusColor != Colors.error && statusColor != Colors.success then
            statusText = s.toString
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
    ImGui.begin("##login", flags)

    // Title
    val titleText = "LOGIN"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, 120f)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text(titleText)
    ImGui.popStyleColor()

    // Center fields
    val fieldW = 400f
    val fieldX = (w - fieldW) / 2f

    // Username
    ImGui.setCursorPos(fieldX, 240f)
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text("Username")
    ImGui.popStyleColor()
    ImGui.setCursorPosX(fieldX)
    ImGui.pushItemWidth(fieldW)
    if focusUsername then
      ImGui.setKeyboardFocusHere()
      focusUsername = false
    ImGui.inputText("##username", username)

    // Password
    ImGui.setCursorPos(fieldX, 340f)
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text("Password")
    ImGui.popStyleColor()
    ImGui.setCursorPosX(fieldX)
    ImGui.inputText("##password", password, ImGuiInputTextFlags.Password)
    ImGui.popItemWidth()

    // Enter to connect (from any field)
    if ImGui.isKeyPressed(ImGuiKey.Enter) && !connecting then
      tryConnect()

    // Connect button
    val buttonW = 120f
    ImGui.setCursorPos((w - buttonW) / 2f, 440f)
    pushColor(ImGuiCol.Button, Colors.primary)
    pushColor(ImGuiCol.ButtonHovered, Colors.primary2)
    pushColor(ImGuiCol.ButtonActive, Colors.hex("C06820"))
    pushColor(ImGuiCol.Text, Colors.hex("1A1A1A"))
    if ImGui.button("Connect", buttonW, 36f) && !connecting then
      tryConnect()
    ImGui.popStyleColor(4)

    // Status
    val statusW = ImGui.calcTextSize(statusText).x
    ImGui.setCursorPos((w - statusW) / 2f, 500f)
    pushColor(ImGuiCol.Text, statusColor)
    ImGui.text(statusText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit = ()

  private def tryConnect(): Unit =
    val user = username.get().trim
    val pass = password.get().trim
    if user.nonEmpty && pass.nonEmpty then
      connecting = true
      statusText = "Connecting..."
      statusColor = Colors.text
      networkThread.send(NetCommand.Connect(ctx.settings.login.host, ctx.settings.login.port))
      loginClient.connect(user, pass)
    else
      statusText = "Enter username and password"
      statusColor = Colors.error

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
