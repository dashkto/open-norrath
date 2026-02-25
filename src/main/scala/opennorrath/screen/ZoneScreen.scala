package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{Camera, Shader, ZoneRenderDebug}

class ZoneScreen(ctx: GameContext, zonePath: String) extends Screen:

  private var shader: Shader = uninitialized
  private var zone: ZoneRenderDebug = uninitialized
  private var camera: Camera = uninitialized
  private var projection: Matrix4f = uninitialized
  private val model = Matrix4f()

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f)

    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderDebug(zonePath, ctx.settings, ctx.settings.debug.animationModel)
    camera = Camera(
      position = Vector3f(-150f, 50f, -460f),
      yaw = 30f,
      pitch = -5f,
      speed = 100f,
    )
    projection = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      ctx.windowWidth.toFloat / ctx.windowHeight.toFloat,
      0.1f,
      10000f,
    )
    println(s"Loading zone: $zonePath")

  override def update(dt: Float): Unit =
    camera.processInput(ctx.input, dt)

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", projection)
    shader.setMatrix4f("view", camera.viewMatrix)
    shader.setMatrix4f("model", model)

    zone.draw(shader, dt, camera.viewMatrix)

  override def dispose(): Unit =
    zone.cleanup()
    shader.cleanup()
