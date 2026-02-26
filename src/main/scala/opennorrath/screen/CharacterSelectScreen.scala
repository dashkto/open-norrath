package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiKey, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.Game
import opennorrath.network.*
import opennorrath.ui.{Colors, EqData}

class CharacterSelectScreen(
  ctx: GameContext,
  characters: Vector[CharacterInfo],
) extends Screen:

  private var selectedIndex = 0
  private var statusText = s"${characters.size} character(s)"
  private var statusColor = Colors.textDim
  private var entering = false
  private var enteredCharName = ""

  private def worldClient: WorldClient = Game.worldSession.get.client

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

  override def update(dt: Float): Unit =
    // Keyboard navigation
    if !entering && characters.nonEmpty then
      if ImGui.isKeyPressed(ImGuiKey.UpArrow) || ImGui.isKeyPressed(ImGuiKey.W) then
        selectedIndex = (selectedIndex - 1 + characters.size) % characters.size
      if ImGui.isKeyPressed(ImGuiKey.DownArrow) || ImGui.isKeyPressed(ImGuiKey.S) then
        selectedIndex = (selectedIndex + 1) % characters.size

      if ImGui.isKeyPressed(ImGuiKey.Enter) || ImGui.isKeyPressed(ImGuiKey.KeypadEnter) then
        enterWorld()

    // Escape to go back — tear down world session
    if ImGui.isKeyPressed(ImGuiKey.Escape) then
      Game.worldSession.foreach(_.stop())
      Game.worldSession = None
      Game.setScreen(LoginScreen(ctx))
      return

    // Poll world events
    var event = worldClient.pollEvent()
    while event.isDefined do
      event.get match
        case WorldEvent.ZoneInfo(addr) =>
          println(s"[CharSelect] Got zone server: ${addr.ip}:${addr.port}")
          Game.setScreen(ZoneLoadingScreen(ctx, addr, enteredCharName))
          return
        case WorldEvent.Error(msg) =>
          statusText = msg
          statusColor = Colors.error
          entering = false
        case WorldEvent.StateChanged(s) =>
          println(s"[CharSelect] World state: $s")
        case _ => ()
      event = worldClient.pollEvent()

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##charselect", flags)

    // Title
    val titleText = "SELECT CHARACTER"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, 100f)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text(titleText)
    ImGui.popStyleColor()

    if characters.isEmpty then
      val noChars = "No characters - create one!"
      val noW = ImGui.calcTextSize(noChars).x
      ImGui.setCursorPos((w - noW) / 2f, h / 2f - 30f)
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text(noChars)
      ImGui.popStyleColor()
    else
      // Character list — fixed-width column, centered
      val listW = 400f
      val listX = (w - listW) / 2f
      val listStartY = 200f
      for (char, i) <- characters.zipWithIndex do
        val selected = i == selectedIndex
        val itemY = listStartY + i * 60f

        // Character name
        ImGui.setCursorPos(listX, itemY)
        if selected then
          pushColor(ImGuiCol.Text, Colors.gold)
        val label = s"${char.name}  -  Level ${char.level} ${EqData.className(char.classId)} ${EqData.raceName(char.race)}"
        if ImGui.selectable(label, selected, 0, listW, 0f) then
          selectedIndex = i
          enterWorld()
        if ImGui.isItemHovered() then selectedIndex = i
        if selected then ImGui.popStyleColor()

    // Create Character button
    val createW = 180f
    ImGui.setCursorPos((w - createW) / 2f, h - 120f)
    pushColor(ImGuiCol.Button, Colors.primary)
    pushColor(ImGuiCol.ButtonHovered, Colors.primary2)
    pushColor(ImGuiCol.ButtonActive, Colors.hex("C06820"))
    pushColor(ImGuiCol.Text, Colors.hex("1A1A1A"))
    if ImGui.button("Create Character", createW, 36f) then
      Game.setScreen(CharacterCreateScreen(ctx))
      ImGui.popStyleColor(4)
      ImGui.end()
      return
    ImGui.popStyleColor(4)

    // Status
    val statusTextW = ImGui.calcTextSize(statusText).x
    ImGui.setCursorPos((w - statusTextW) / 2f, h - 65f)
    pushColor(ImGuiCol.Text, statusColor)
    ImGui.text(statusText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit =
    // World session is owned by Game, not by this screen
    ()

  private def enterWorld(): Unit =
    if characters.nonEmpty && !entering then
      val char = characters(selectedIndex)
      enteredCharName = char.name
      statusText = s"Entering world as ${char.name}..."
      statusColor = Colors.text
      entering = true
      worldClient.enterWorld(char.name)

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
