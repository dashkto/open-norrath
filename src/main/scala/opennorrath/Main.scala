package opennorrath

import org.joml.Matrix4f
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.system.MemoryUtil.NULL

object Main:

  private val WindowWidth = 1280
  private val WindowHeight = 720
  private val WindowTitle = "OpenNorrath"

  def main(args: Array[String]): Unit =
    Logging.init()
    val settings = Settings.load()
    val zonePath = if args.nonEmpty then args(0) else "assets/arena.s3d"

    GLFWErrorCallback.createPrint(System.err).set()

    if !glfwInit() then
      throw IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE)

    val window = glfwCreateWindow(WindowWidth, WindowHeight, WindowTitle, NULL, NULL)
    if window == NULL then
      throw RuntimeException("Failed to create the GLFW window")

    // Center the window
    val vidMode = glfwGetVideoMode(glfwGetPrimaryMonitor())
    if vidMode != null then
      glfwSetWindowPos(
        window,
        (vidMode.width() - WindowWidth) / 2,
        (vidMode.height() - WindowHeight) / 2,
      )

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)
    glfwShowWindow(window)

    GL.createCapabilities()
    glEnable(GL_DEPTH_TEST)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f) // sky blue

    println(s"OpenNorrath ${BuildInfo.version} (${BuildInfo.gitCommit}) — OpenGL ${glGetString(GL_VERSION)}")
    println(s"Loading zone: $zonePath")

    // Init resources
    val shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    val zone = ZoneRenderDebug(zonePath, settings, settings.debug.animationModel)
    // Start at arena edge, slightly above floor level, looking toward center
    // Coordinates are swapped from EQ: GL(X, Y, Z) = EQ(X, Z, -Y)
    val camera = Camera(
      position = org.joml.Vector3f(-400f, 50f, -200f),
      yaw = 20f,
      pitch = -10f,
      speed = 100f,
    )

    val projection = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      WindowWidth.toFloat / WindowHeight.toFloat,
      0.1f,
      10000f, // far plane for large zones
    )

    val model = Matrix4f() // identity — zone is in world space
    val input = InputManager(window)

    var lastTime = glfwGetTime()

    while !glfwWindowShouldClose(window) do
      val now = glfwGetTime()
      val deltaTime = (now - lastTime).toFloat
      lastTime = now

      input.update()

      if input.isKeyReleased(GLFW_KEY_ESCAPE) then
        glfwSetWindowShouldClose(window, true)

      camera.processInput(input, deltaTime)

      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      shader.use()
      shader.setMatrix4f("projection", projection)
      val viewMatrix = camera.viewMatrix
      shader.setMatrix4f("view", viewMatrix)
      shader.setMatrix4f("model", model)

      zone.draw(shader, deltaTime, viewMatrix)

      glfwSwapBuffers(window)
      glfwPollEvents()

    // Cleanup
    zone.cleanup()
    shader.cleanup()
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
