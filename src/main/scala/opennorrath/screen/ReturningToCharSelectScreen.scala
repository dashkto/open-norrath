package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.Game
import opennorrath.network.*
import opennorrath.ui.Colors

/** Waiting screen shown after camping while the world server re-authenticates.
  * Polls for a fresh character list (triggered by OP_SendLoginInfo with zoning=0),
  * then transitions to CharacterSelectScreen.
  */
class ReturningToCharSelectScreen(ctx: GameContext) extends Screen:

  private var statusText = "Returning to character select..."
  private var statusColor = Colors.text
  private var timeoutTimer = 0f
  private val TimeoutSeconds = 10f

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

  override def update(dt: Float): Unit =
    // Escape to bail out to login
    if ImGui.isKeyPressed(imgui.flag.ImGuiKey.Escape) then
      Game.worldSession.foreach(_.stop())
      Game.worldSession = None
      Game.setScreen(LoginScreen(ctx))
      return

    // Timeout — if the server doesn't respond, go back to login
    timeoutTimer += dt
    if timeoutTimer >= TimeoutSeconds then
      println("[ReturningToCharSelect] Timed out waiting for character list")
      Game.worldSession.foreach(_.stop())
      Game.worldSession = None
      Game.setScreen(LoginScreen(ctx))
      return

    // Poll world events for a fresh character list
    Game.worldSession.foreach { session =>
      var event = session.client.pollEvent()
      while event.isDefined do
        event.get match
          case WorldEvent.CharacterList(chars) =>
            Game.setScreen(CharacterSelectScreen(ctx, chars))
            return
          case WorldEvent.ZoneInfo(_) =>
            // Server auto-sends ZoneServerInfo for the previous character after
            // re-authentication — ignore it so the user can pick a different character.
            ()
          case WorldEvent.Error(msg) =>
            statusText = msg
            statusColor = Colors.error
          case _ => ()
        event = session.client.pollEvent()
    }

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar | ImGuiWindowFlags.NoBackground

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##returning", flags)

    val textW = ImGui.calcTextSize(statusText).x
    ImGui.setCursorPos((w - textW) / 2f, h / 2f)
    ImGui.pushStyleColor(ImGuiCol.Text, statusColor._1, statusColor._2, statusColor._3, statusColor._4)
    ImGui.text(statusText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit = ()
