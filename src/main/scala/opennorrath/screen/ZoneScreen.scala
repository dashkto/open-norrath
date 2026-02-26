package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import imgui.ImGui

import opennorrath.{Camera, EqCoords, Game, Shader, ZoneRenderDebug}
import opennorrath.network.{PlayerProfileData, SpawnData}
import opennorrath.ui.{CharacterInfoPanel, EscapeMenu, TextPanel}

class ZoneScreen(ctx: GameContext, zonePath: String, selfSpawn: Option[SpawnData] = None, profile: Option[PlayerProfileData] = None) extends Screen:

  private var shader: Shader = uninitialized
  private var zone: ZoneRenderDebug = uninitialized
  private var camera: Camera = uninitialized
  private var projection: Matrix4f = uninitialized
  private val model = Matrix4f()
  private val charInfoPanel = Game.playerState.map(CharacterInfoPanel(_))
  private val escapeMenu = EscapeMenu(ctx)
  private var chatPanel: TextPanel = null
  private def initChat(): Unit =
    chatPanel = TextPanel("Main", 10f, 500f, onSubmit = text => {
      // TODO: send to zone server
      chatPanel.addLine(s"You: $text")
    })

  private var freeLook = false

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f)

    initChat()
    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderDebug(zonePath, ctx.settings, ctx.settings.debug.animationModel)

    val EyeHeight = 6f
    val (startPos, startYaw) = selfSpawn match
      case Some(s) =>
        val pos = EqCoords.serverToGl(s.y, s.x, s.z)
        pos.y += EyeHeight
        (pos, EqCoords.spawnHeadingToYaw(s.heading))
      case None => profile match
        case Some(pp) =>
          val pos = EqCoords.serverToGl(pp.y, pp.x, pp.z)
          pos.y += EyeHeight
          (pos, EqCoords.profileHeadingToYaw(pp.heading))
        case None =>
          (Vector3f(-150f, 50f, -460f), 30f)
    camera = Camera(
      position = startPos,
      yaw = startYaw,
      pitch = -5f,
      speed = 100f,
    )
    projection = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      ctx.windowWidth.toFloat / ctx.windowHeight.toFloat,
      0.1f,
      10000f,
    )
    println(s"Loading zone: $zonePath, camera: pos=$startPos yaw=$startYaw")

  override def update(dt: Float): Unit =
    if ctx.input.isKeyPressed(GLFW_KEY_ESCAPE) then
      escapeMenu.toggle()

    if escapeMenu.isOpen then return

    val rightHeld = ctx.input.isMouseHeld(GLFW_MOUSE_BUTTON_RIGHT)
    if rightHeld != freeLook then
      freeLook = rightHeld
      if freeLook then
        glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
      else
        glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

    if !ImGui.getIO().getWantCaptureKeyboard() then
      camera.processMovement(ctx.input, dt)
    if freeLook then
      camera.processLook(ctx.input)

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", projection)
    shader.setMatrix4f("view", camera.viewMatrix)
    shader.setMatrix4f("model", model)

    zone.draw(shader, dt, camera.viewMatrix)

    charInfoPanel.foreach(_.render())
    chatPanel.render()
    escapeMenu.render()

  override def dispose(): Unit =
    zone.cleanup()
    shader.cleanup()
