package opennorrath.world

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*

import imgui.ImGui

import opennorrath.{GameAction, InputManager}
import opennorrath.network.{PlayerProfileData, SpawnData}

/** Manages camera state, freelook cursor toggling, and projection. */
class CameraController(window: Long, screenW: Int, screenH: Int):

  private val EyeHeight = 6f

  var camera: Camera = Camera()
  var freeLook = false

  val projection: Matrix4f = Matrix4f().perspective(
    Math.toRadians(60.0).toFloat,
    screenW.toFloat / screenH.toFloat,
    0.1f,
    10000f,
  )

  def viewMatrix: Matrix4f = camera.viewMatrix

  def initFromSpawn(spawn: Option[SpawnData], profile: Option[PlayerProfileData]): Unit =
    val (startPos, startYaw) = spawn match
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
    println(s"Camera: pos=$startPos yaw=$startYaw")

  def update(input: InputManager, dt: Float): Unit =
    val rightHeld = input.isActionHeld(GameAction.FreeLook)
    if rightHeld != freeLook then
      freeLook = rightHeld
      if freeLook then
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
      else
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

    if !ImGui.getIO().getWantCaptureKeyboard() then
      camera.processMovement(input, dt)
    if freeLook then
      camera.processLook(input)
