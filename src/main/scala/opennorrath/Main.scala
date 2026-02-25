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
    |out vec2 TexCoord;
    |uniform mat4 projection;
    |uniform mat4 view;
    |uniform mat4 model;
    |void main() {
    |    gl_Position = projection * view * model * vec4(aPos, 1.0);
    |    TexCoord = aTexCoord;
    |}
    |""".stripMargin

  private val FragmentShaderSource = """
    |#version 330 core
    |in vec2 TexCoord;
    |out vec4 FragColor;
    |uniform sampler2D tex0;
    |void main() {
    |    FragColor = texture(tex0, TexCoord);
    |}
    |""".stripMargin

  // 24 vertices (4 per face) — position (3) + uv (2) = 5 floats each
  private val CubeVertices = Array[Float](
    // front
    -0.5f, -0.5f,  0.5f,  0f, 0f,
     0.5f, -0.5f,  0.5f,  1f, 0f,
     0.5f,  0.5f,  0.5f,  1f, 1f,
    -0.5f,  0.5f,  0.5f,  0f, 1f,
    // back
     0.5f, -0.5f, -0.5f,  0f, 0f,
    -0.5f, -0.5f, -0.5f,  1f, 0f,
    -0.5f,  0.5f, -0.5f,  1f, 1f,
     0.5f,  0.5f, -0.5f,  0f, 1f,
    // left
    -0.5f, -0.5f, -0.5f,  0f, 0f,
    -0.5f, -0.5f,  0.5f,  1f, 0f,
    -0.5f,  0.5f,  0.5f,  1f, 1f,
    -0.5f,  0.5f, -0.5f,  0f, 1f,
    // right
     0.5f, -0.5f,  0.5f,  0f, 0f,
     0.5f, -0.5f, -0.5f,  1f, 0f,
     0.5f,  0.5f, -0.5f,  1f, 1f,
     0.5f,  0.5f,  0.5f,  0f, 1f,
    // top
    -0.5f,  0.5f,  0.5f,  0f, 0f,
     0.5f,  0.5f,  0.5f,  1f, 0f,
     0.5f,  0.5f, -0.5f,  1f, 1f,
    -0.5f,  0.5f, -0.5f,  0f, 1f,
    // bottom
    -0.5f, -0.5f, -0.5f,  0f, 0f,
     0.5f, -0.5f, -0.5f,  1f, 0f,
     0.5f, -0.5f,  0.5f,  1f, 1f,
    -0.5f, -0.5f,  0.5f,  0f, 1f,
  )

  private val CubeIndices = Array[Int](
     0,  1,  2,   2,  3,  0,  // front
     4,  5,  6,   6,  7,  4,  // back
     8,  9, 10,  10, 11,  8,  // left
    12, 13, 14,  14, 15, 12,  // right
    16, 17, 18,  18, 19, 16,  // top
    20, 21, 22,  22, 23, 20,  // bottom
  )

  def main(args: Array[String]): Unit =
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
    glClearColor(0.1f, 0.1f, 0.15f, 1.0f)

    println(s"OpenNorrath running — OpenGL ${glGetString(GL_VERSION)}")

    // Init resources
    val shader = Shader(VertexShaderSource, FragmentShaderSource)
    val textureId = Texture.createCheckerboard()
    val cube = Mesh(CubeVertices, CubeIndices)
    val camera = Camera()

    val projection = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      WindowWidth.toFloat / WindowHeight.toFloat,
      0.1f,
      100f,
    )

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
      val yOffset = (lastMouseY - yPos).toFloat // reversed: y goes bottom-to-top
      lastMouseX = xPos
      lastMouseY = yPos
      camera.processMouse(xOffset, yOffset)
    )

    glfwSetKeyCallback(window, (win, key, _, action, _) =>
      if key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE then
        glfwSetWindowShouldClose(win, true)
    )

    var lastTime = glfwGetTime()
    val model = Matrix4f()

    while !glfwWindowShouldClose(window) do
      val now = glfwGetTime()
      val deltaTime = (now - lastTime).toFloat
      lastTime = now

      camera.processKeyboard(window, deltaTime)

      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      shader.use()
      shader.setMatrix4f("projection", projection)
      shader.setMatrix4f("view", camera.viewMatrix)

      // Slowly rotate the cube
      model.identity().rotateY((now * 0.5).toFloat).rotateX((now * 0.3).toFloat)
      shader.setMatrix4f("model", model)
      shader.setInt("tex0", 0)

      glBindTexture(GL_TEXTURE_2D, textureId)
      cube.draw()

      glfwSwapBuffers(window)
      glfwPollEvents()

    // Cleanup
    cube.cleanup()
    shader.cleanup()
    glDeleteTextures(textureId)
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
