package opennorrath

import imgui.ImGui
import imgui.gl3.ImGuiImplGl3
import imgui.glfw.ImGuiImplGlfw

import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.system.MemoryUtil.NULL

import opennorrath.screen.{GameContext, SplashScreen, ZoneScreen}
import opennorrath.ui.{Fonts, ImGuiTheme}

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

    println(s"OpenNorrath ${BuildInfo.version} (${BuildInfo.gitCommit}) — OpenGL ${glGetString(GL_VERSION)}")

    val input = InputManager(window)

    // Initialize ImGui (after InputManager so callbacks chain)
    ImGui.createContext()
    val imGuiGlfw = ImGuiImplGlfw()
    val imGuiGl3 = ImGuiImplGl3()
    imGuiGlfw.init(window, true)
    Fonts.init()
    imGuiGl3.init("#version 330 core")
    ImGuiTheme.apply()

    val ctx = GameContext(window, input, settings, WindowWidth, WindowHeight, imGuiGlfw, imGuiGl3)

    val initialScreen =
      if args.nonEmpty then ZoneScreen(ctx, zonePath)
      else SplashScreen(ctx, zonePath)
    Game.run(ctx, initialScreen)

    println("Shutting down")
    imGuiGl3.shutdown()
    imGuiGlfw.shutdown()
    ImGui.destroyContext()

    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
