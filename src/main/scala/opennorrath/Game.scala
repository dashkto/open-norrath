package opennorrath

import imgui.ImGui

import org.lwjgl.glfw.GLFW.*

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

  def setScreen(screen: Screen): Unit =
    currentScreen.foreach(_.dispose())
    currentScreen = Some(screen)
    screen.show()
