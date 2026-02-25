package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.BufferUtils

import opennorrath.{BitmapFont, BuildInfo, Camera, Game, Shader, ZoneRenderDebug}
import opennorrath.ui.{Colors, Spacing}

class SplashScreen(ctx: GameContext, zonePath: String) extends Screen:

  private var zoneShader: Shader = uninitialized
  private var uiShader: Shader = uninitialized
  private var zone: ZoneRenderDebug = uninitialized
  private var camera: Camera = uninitialized
  private var projection3d: Matrix4f = uninitialized
  private var projectionUi: Matrix4f = uninitialized

  private var titleFont: BitmapFont = uninitialized
  private var menuFont: BitmapFont = uninitialized
  private var versionFont: BitmapFont = uninitialized

  private var quadVao: Int = 0
  private var quadVbo: Int = 0

  private val model = Matrix4f()
  private var selectedIndex = 0
  private val menuItems = Array("Server Select", "Quit")

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.1f, 0.1f, 0.15f, 1f)

    // 3D zone background
    zoneShader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderDebug(zonePath, ctx.settings, "")
    camera = Camera(
      position = Vector3f(-400f, 80f, -200f),
      yaw = 20f,
      pitch = -15f,
      speed = 0f,
    )
    projection3d = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      ctx.windowWidth.toFloat / ctx.windowHeight.toFloat,
      0.1f,
      10000f,
    )

    // UI
    uiShader = Shader.fromResources("/shaders/ui.vert", "/shaders/ui.frag")
    projectionUi = Matrix4f().ortho(0f, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat, 0f, -1f, 1f)

    titleFont = BitmapFont("/fonts/Roboto-Bold.ttf", 72f, 1024)
    menuFont = BitmapFont("/fonts/Roboto-Regular.ttf", 36f)
    versionFont = BitmapFont("/fonts/Roboto-Regular.ttf", 16f)

    initQuadVao()

  override def update(dt: Float): Unit =
    // Slowly orbit camera
    camera.yaw += dt * 3f
    camera.updateVectors()

    // Mouse hover over menu items
    val (mx, my) = ctx.input.mousePos
    val menuStartY = ctx.windowHeight / 2f
    for (item, i) <- menuItems.zipWithIndex do
      val textX = (ctx.windowWidth - menuFont.textWidth(item)) / 2f
      val (x0, y0, x1, y1) = menuFont.textBounds(item, textX, menuStartY + i * Spacing.menuItemSpacing)
      if mx >= x0 && mx <= x1 && my >= y0 && my <= y1 then
        selectedIndex = i

    // Keyboard navigation
    if ctx.input.isKeyPressed(GLFW_KEY_UP) || ctx.input.isKeyPressed(GLFW_KEY_W) then
      selectedIndex = (selectedIndex - 1 + menuItems.length) % menuItems.length
    if ctx.input.isKeyPressed(GLFW_KEY_DOWN) || ctx.input.isKeyPressed(GLFW_KEY_S) then
      selectedIndex = (selectedIndex + 1) % menuItems.length

    // Activate selection
    if ctx.input.isKeyPressed(GLFW_KEY_ENTER) || ctx.input.isMousePressed(GLFW_MOUSE_BUTTON_LEFT) then
      activateMenuItem()

  private def activateMenuItem(): Unit =
    menuItems(selectedIndex) match
      case "Server Select" => Game.setScreen(ZoneScreen(ctx, zonePath))
      case "Quit" => glfwSetWindowShouldClose(ctx.window, true)

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    // 1. Zone background
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    zoneShader.use()
    zoneShader.setMatrix4f("projection", projection3d)
    zoneShader.setMatrix4f("view", camera.viewMatrix)
    zoneShader.setMatrix4f("model", model)
    zone.draw(zoneShader, dt, camera.viewMatrix)

    // 2. Switch to 2D
    glDisable(GL_DEPTH_TEST)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    uiShader.use()
    uiShader.setMatrix4f("projection", projectionUi)

    // 3. Dark overlay
    drawQuad(0f, 0f, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat, Colors.overlay)

    // 4. Title
    val titleText = "OPEN NORRATH"
    val titleW = titleFont.textWidth(titleText)
    setTextColor(Colors.title)
    titleFont.drawText(titleText, (ctx.windowWidth - titleW) / 2f, 160f)

    // 5. Menu
    val menuStartY = ctx.windowHeight / 2f
    for (item, i) <- menuItems.zipWithIndex do
      val selected = i == selectedIndex
      val w = menuFont.textWidth(item)
      val textX = (ctx.windowWidth - w) / 2f
      val textY = menuStartY + i * Spacing.menuItemSpacing
      if selected then
        val (x0, y0, x1, y1) = menuFont.textBounds(item, textX, textY)
        drawQuad(x0 - Spacing.menuPad, y0 - Spacing.menuPad,
          x1 - x0 + Spacing.menuPad * 2, y1 - y0 + Spacing.menuPad * 2, Colors.highlight)
      setTextColor(if selected then Colors.gold else Colors.text)
      menuFont.drawText(item, textX, textY)

    // 6. Version (bottom-right)
    val versionText = s"v${BuildInfo.version} (${BuildInfo.gitCommit})"
    val versionW = versionFont.textWidth(versionText)
    setTextColor(Colors.textDim)
    versionFont.drawText(versionText, ctx.windowWidth - versionW - Spacing.pad, ctx.windowHeight - Spacing.pad * 2)

  override def dispose(): Unit =
    zone.cleanup()
    zoneShader.cleanup()
    uiShader.cleanup()
    titleFont.cleanup()
    menuFont.cleanup()
    versionFont.cleanup()
    glDeleteBuffers(quadVbo)
    glDeleteVertexArrays(quadVao)

  private def setTextColor(c: (Float, Float, Float, Float)): Unit =
    uiShader.setVec4("color", c._1, c._2, c._3, c._4)
    uiShader.setInt("isText", 1)

  private def drawQuad(x: Float, y: Float, w: Float, h: Float, c: (Float, Float, Float, Float)): Unit =
    uiShader.setVec4("color", c._1, c._2, c._3, c._4)
    uiShader.setInt("isText", 0)

    val vertices = Array[Float](
      x, y, 0f, 0f,
      x + w, y, 1f, 0f,
      x + w, y + h, 1f, 1f,
      x, y, 0f, 0f,
      x + w, y + h, 1f, 1f,
      x, y + h, 0f, 1f,
    )
    val buf = BufferUtils.createFloatBuffer(vertices.length)
    buf.put(vertices).flip()
    glBindBuffer(GL_ARRAY_BUFFER, quadVbo)
    glBufferSubData(GL_ARRAY_BUFFER, 0, buf)
    glBindVertexArray(quadVao)
    glDrawArrays(GL_TRIANGLES, 0, 6)
    glBindVertexArray(0)

  private def initQuadVao(): Unit =
    quadVao = glGenVertexArrays()
    quadVbo = glGenBuffers()
    glBindVertexArray(quadVao)
    glBindBuffer(GL_ARRAY_BUFFER, quadVbo)
    glBufferData(GL_ARRAY_BUFFER, 6 * 4 * 4L, GL_DYNAMIC_DRAW)
    glVertexAttribPointer(0, 2, GL_FLOAT, false, 4 * 4, 0)
    glEnableVertexAttribArray(0)
    glVertexAttribPointer(1, 2, GL_FLOAT, false, 4 * 4, 2 * 4L)
    glEnableVertexAttribArray(1)
    glBindVertexArray(0)
