package opennorrath

import imgui.ImGui

import org.lwjgl.glfw.GLFW.*

import opennorrath.network.{LoginClient, NetworkThread, WorldClient, ZoneClient}
import opennorrath.screen.{GameContext, Screen}

/** Shared login connection, owned by Game, outlives any individual screen. */
class LoginSession(val client: LoginClient, val network: NetworkThread):
  def stop(): Unit = network.stop()

/** Shared world connection, owned by Game, outlives any individual screen. */
class WorldSession(val client: WorldClient, val network: NetworkThread):
  def stop(): Unit = network.stop()

/** Shared zone connection, owned by Game, outlives any individual screen. */
class ZoneSession(val client: ZoneClient, val network: NetworkThread):
  def stop(): Unit = network.stop()

object Game:

  private var currentScreen: Option[Screen] = None

  /** Active login session — created during login, survives into server select. */
  var loginSession: Option[LoginSession] = None

  /** Active world session — created during server select, survives screen transitions. */
  var worldSession: Option[WorldSession] = None

  /** Active zone session — created on zone entry, survives screen transitions. */
  var zoneSession: Option[ZoneSession] = None

  /** World connection credentials — saved for zone-to-zone reconnection.
    * When zoning, the client must reconnect to the world server with the same credentials.
    */
  var worldHost: String = ""
  var worldPort: Int = 0
  var worldAccountId: Int = 0
  var worldKey: String = ""

  def run(ctx: GameContext, initialScreen: Screen): Unit =
    setScreen(initialScreen)

    var lastTime = glfwGetTime()

    while !glfwWindowShouldClose(ctx.window) do
      val now = glfwGetTime()
      val dt = (now - lastTime).toFloat
      lastTime = now

      ctx.input.update()

      // Start ImGui frame
      ctx.imGuiGlfw.newFrame()
      ctx.imGuiGl3.newFrame()
      ImGui.newFrame()

      currentScreen.foreach { screen =>
        screen.update(dt)
        screen.render(dt)
      }

      // End ImGui frame and render
      ImGui.render()
      ctx.imGuiGl3.renderDrawData(ImGui.getDrawData())

      glfwSwapBuffers(ctx.window)
      glfwPollEvents()

    currentScreen.foreach(_.dispose())
    currentScreen = None
    zoneSession.foreach(_.stop())
    zoneSession = None
    loginSession.foreach(_.stop())
    loginSession = None
    worldSession.foreach(_.stop())
    worldSession = None

  def setScreen(screen: Screen): Unit =
    currentScreen.foreach(_.dispose())
    currentScreen = Some(screen)
    screen.show()
