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

  private val VertexShaderSource = """
    |#version 330 core
    |layout (location = 0) in vec3 aPos;
    |layout (location = 1) in vec2 aTexCoord;
    |layout (location = 2) in vec3 aColor;
    |out vec2 TexCoord;
    |out vec3 VertColor;
    |uniform mat4 projection;
    |uniform mat4 view;
    |uniform mat4 model;
    |void main() {
    |    gl_Position = projection * view * model * vec4(aPos, 1.0);
    |    TexCoord = aTexCoord;
    |    VertColor = aColor;
    |}
    |""".stripMargin

  private val FragmentShaderSource = """
    |#version 330 core
    |in vec2 TexCoord;
    |in vec3 VertColor;
    |out vec4 FragColor;
    |uniform sampler2D tex0;
    |void main() {
    |    FragColor = texture(tex0, TexCoord) * vec4(VertColor, 1.0);
    |    if (FragColor.a < 0.1) discard;
    |}
    |""".stripMargin

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
    val shader = Shader(VertexShaderSource, FragmentShaderSource)
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

    // Mouse state
    var lastMouseX = WindowWidth / 2.0
    var lastMouseY = WindowHeight / 2.0
    var firstMouse = true

    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)

    glfwSetCursorPosCallback(window, (_, xPos, yPos) =>
      if firstMouse then
        lastMouseX = xPos
        lastMouseY = yPos
        firstMouse = false

      val xOffset = (xPos - lastMouseX).toFloat
      val yOffset = (lastMouseY - yPos).toFloat
      lastMouseX = xPos
      lastMouseY = yPos
      camera.processMouse(xOffset, yOffset)
    )

    glfwSetKeyCallback(window, (win, key, _, action, _) =>
      if key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE then
        glfwSetWindowShouldClose(win, true)
    )

    var lastTime = glfwGetTime()

    while !glfwWindowShouldClose(window) do
      val now = glfwGetTime()
      val deltaTime = (now - lastTime).toFloat
      lastTime = now

      camera.processKeyboard(window, deltaTime)

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
