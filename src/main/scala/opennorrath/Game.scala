package opennorrath

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.screen.{GameContext, Screen}

object Game:

  private var currentScreen: Option[Screen] = None

  def run(ctx: GameContext, initialScreen: Screen): Unit =
    setScreen(initialScreen)

    var lastTime = glfwGetTime()

    while !glfwWindowShouldClose(ctx.window) do
      val now = glfwGetTime()
      val dt = (now - lastTime).toFloat
      lastTime = now

      ctx.input.update()

      if ctx.input.isKeyReleased(GLFW_KEY_ESCAPE) then
        glfwSetWindowShouldClose(ctx.window, true)

      currentScreen.foreach { screen =>
        screen.update(dt)
        screen.render(dt)
      }

      glfwSwapBuffers(ctx.window)
      glfwPollEvents()

    currentScreen.foreach(_.dispose())
    currentScreen = None

  def setScreen(screen: Screen): Unit =
    currentScreen.foreach(_.dispose())
    currentScreen = Some(screen)
    screen.show()
