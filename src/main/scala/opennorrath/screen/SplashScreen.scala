package opennorrath.screen

import scala.compiletime.uninitialized

import imgui.ImGui
import imgui.flag.{ImGuiCond, ImGuiKey, ImGuiWindowFlags}

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{BuildInfo, Game}
import opennorrath.render.Shader
import opennorrath.world.{Camera, ZoneRenderer}
import org.lwjgl.opengl.GL13.{glActiveTexture, GL_TEXTURE0, GL_TEXTURE1}
import org.lwjgl.opengl.GL20.glVertexAttrib3f
import opennorrath.ui.{Colors, Fonts}

class SplashScreen(ctx: GameContext, zonePath: String) extends Screen:

  private var zoneShader: Shader = uninitialized
  private var shadowShader: Shader = uninitialized
  private var zone: ZoneRenderer = uninitialized
  private var camera: Camera = uninitialized
  private var projection3d: Matrix4f = uninitialized
  private var hasZone = false

  private val model = Matrix4f()
  private var selectedIndex = 0
  private val menuItems = Array("Server Select", "Quit")

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.1f, 0.1f, 0.15f, 1f)

    hasZone = java.nio.file.Files.exists(java.nio.file.Path.of(zonePath))
    if hasZone then
      zoneShader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
      shadowShader = Shader.fromResources("/shaders/shadow.vert", "/shaders/shadow.frag")
      zone = ZoneRenderer(zonePath, ctx.settings)
      camera = Camera(
        position = Vector3f(-150f, 60f, -460f),
        yaw = 30f,
        pitch = -10f,
        speed = 0f,
      )
      projection3d = Matrix4f().perspective(
        Math.toRadians(60.0).toFloat,
        ctx.windowWidth.toFloat / ctx.windowHeight.toFloat,
        0.1f,
        10000f,
      )
    else
      println(s"Splash zone not found: $zonePath — showing menu without background")

  override def update(dt: Float): Unit =
    if hasZone then
      camera.yaw += dt * 3f
      camera.updateVectors()

    // Keyboard navigation
    if ImGui.isKeyPressed(ImGuiKey.UpArrow) || ImGui.isKeyPressed(ImGuiKey.W) then
      selectedIndex = (selectedIndex - 1 + menuItems.length) % menuItems.length
    if ImGui.isKeyPressed(ImGuiKey.DownArrow) || ImGui.isKeyPressed(ImGuiKey.S) then
      selectedIndex = (selectedIndex + 1) % menuItems.length

    if ImGui.isKeyPressed(ImGuiKey.Enter) || ImGui.isKeyPressed(ImGuiKey.KeypadEnter) then
      activateMenuItem()

    if ImGui.isKeyPressed(ImGuiKey.Escape) then
      glfwSetWindowShouldClose(ctx.window, true)

  private def activateMenuItem(): Unit =
    menuItems(selectedIndex) match
      case "Server Select" => Game.setScreen(LoginScreen(ctx))
      case "Quit" => glfwSetWindowShouldClose(ctx.window, true)

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    if hasZone then
      // Shadow map pre-pass
      glEnable(GL_DEPTH_TEST)
      glDisable(GL_BLEND)
      zone.updateCharacterVisibility(camera.position)
      zone.shadowMap.bind()
      zone.drawShadowPass(shadowShader)
      zone.shadowMap.unbind(ctx.windowWidth, ctx.windowHeight)

      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      // 3D zone background with lighting
      zoneShader.use()
      zoneShader.setMatrix4f("projection", projection3d)
      zoneShader.setMatrix4f("view", camera.viewMatrix)
      zoneShader.setMatrix4f("model", model)

      // Lighting uniforms
      zoneShader.setBool("enableLighting", true)
      zoneShader.setBool("enableShadows", true)
      val sunDir = ZoneRenderer.SunDir
      zoneShader.setVec3("lightDir", sunDir.x, sunDir.y, sunDir.z)
      zoneShader.setFloat("ambientStrength", 0.35f)
      zoneShader.setMatrix4f("lightSpaceMatrix", zone.shadowMap.lightSpaceMatrix)

      // Bind shadow map to texture unit 1
      glActiveTexture(GL_TEXTURE1)
      glBindTexture(GL_TEXTURE_2D, zone.shadowMap.depthTexture)
      zoneShader.setInt("shadowMap", 1)
      glActiveTexture(GL_TEXTURE0)
      glVertexAttrib3f(4, 0f, 1f, 0f) // default up normal

      zone.draw(zoneShader, dt, camera.viewMatrix)

    // ImGui overlay
    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar | ImGuiWindowFlags.NoBackground

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##splash", flags)

    // Title centered
    ImGui.pushFont(Fonts.title)
    val titleText = "OPEN NORRATH"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, 120f)
    pushColor(imgui.flag.ImGuiCol.Text, Colors.cream)
    ImGui.text(titleText)
    ImGui.popStyleColor()
    ImGui.popFont()

    // Menu items — fixed-width column, centered
    ImGui.pushFont(Fonts.menu)
    val menuW = 240f
    val menuX = (w - menuW) / 2f
    val menuStartY = h / 2f
    for (item, i) <- menuItems.zipWithIndex do
      val selected = i == selectedIndex
      val itemY = menuStartY + i * 50f
      ImGui.setCursorPos(menuX, itemY)
      if selected then
        pushColor(imgui.flag.ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(item, selected, 0, menuW, 0f) then
        selectedIndex = i
        activateMenuItem()
      if ImGui.isItemHovered() then selectedIndex = i
      if selected then ImGui.popStyleColor()
    ImGui.popFont()

    // Version bottom-right
    val versionText = s"v${BuildInfo.version} (${BuildInfo.gitCommit})"
    val versionW = ImGui.calcTextSize(versionText).x
    ImGui.setCursorPos(w - versionW - 12f, h - 28f)
    pushColor(imgui.flag.ImGuiCol.Text, Colors.textDim)
    ImGui.text(versionText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit =
    if hasZone then
      zone.cleanup()
      zoneShader.cleanup()
      shadowShader.cleanup()

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)
