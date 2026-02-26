package opennorrath.world

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*

import imgui.ImGui

import opennorrath.{GameAction, InputManager}
import opennorrath.network.{PlayerProfileData, SpawnData}
import opennorrath.state.PlayerCharacter

/** Manages camera state, freelook cursor toggling, and projection.
  * Starts attached to the player position; Shift+T detaches for free flight.
  */
class CameraController(window: Long, screenW: Int, screenH: Int):

  private val EyeHeight = 6f
  private val PlayerSpeed = 30f  // GL units/sec (EQ base run speed)

  var camera: Camera = Camera()
  var freeLook = false
  var attached = true
  var playerMoving = false

  /** Optional reference to the player character — updated with position/heading/moving. */
  var player: Option[PlayerCharacter] = None

  /** Player foot position in GL space (eye = playerPos + EyeHeight on Y). */
  val playerPos = Vector3f()

  val projection: Matrix4f = Matrix4f().perspective(
    Math.toRadians(60.0).toFloat,
    screenW.toFloat / screenH.toFloat,
    0.1f,
    10000f,
  )

  def viewMatrix: Matrix4f = camera.viewMatrix

  /** Player heading in degrees (0-360, CCW from east) for server packets. */
  def playerHeadingDeg: Float =
    val h = -camera.yaw % 360f
    if h < 0 then h + 360f else h

  /** Player heading as EQ byte (0-255, CCW from east) for model rotation. */
  def playerHeading: Int =
    val deg = playerHeadingDeg
    ((deg * 256f / 360f).toInt % 256 + 256) % 256

  def initFromSpawn(spawn: Option[SpawnData], profile: Option[PlayerProfileData]): Unit =
    val (footPos, startYaw) = spawn match
      case Some(s) =>
        (EqCoords.serverToGl(s.y, s.x, s.z), EqCoords.spawnHeadingToYaw(s.heading))
      case None => profile match
        case Some(pp) =>
          (EqCoords.serverToGl(pp.y, pp.x, pp.z), EqCoords.profileHeadingToYaw(pp.heading))
        case None =>
          (Vector3f(-150f, 50f, -460f), 30f)
    playerPos.set(footPos)
    val eyePos = Vector3f(footPos).add(0f, EyeHeight, 0f)
    camera = Camera(
      position = eyePos,
      yaw = startYaw,
      pitch = -5f,
      speed = 100f,
    )
    // Sync initial position/heading to PlayerCharacter
    val hdeg = { val h = -startYaw % 360f; if h < 0 then h + 360f else h }
    player.foreach { pc =>
      pc.position.set(footPos)
      pc.headingDeg = hdeg
    }
    println(s"Camera: pos=$eyePos yaw=$startYaw attached=$attached")

  def update(input: InputManager, dt: Float): Unit =
    if !ImGui.getIO().getWantCaptureKeyboard() && input.isActionPressed(GameAction.DetachCamera) then
      attached = !attached
      if attached then camera.position.set(playerPos.x, playerPos.y + EyeHeight, playerPos.z)
      println(s"Camera ${if attached then "attached" else "detached"}")

    val rightHeld = input.isActionHeld(GameAction.FreeLook)
    if rightHeld != freeLook then
      freeLook = rightHeld
      if freeLook then
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
      else
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

    // Process mouse look first so movement uses the current camera direction
    if freeLook then
      camera.processLook(input)

    if attached then
      playerMoving = false
      if !ImGui.getIO().getWantCaptureKeyboard() then
        // Horizontal movement along camera yaw direction
        val yawRad = Math.toRadians(camera.yaw).toFloat
        val frontX = Math.cos(yawRad).toFloat
        val frontZ = Math.sin(yawRad).toFloat
        val velocity = PlayerSpeed * dt
        var dx = 0f; var dz = 0f
        if input.isActionHeld(GameAction.MoveForward) then
          dx += frontX * velocity; dz += frontZ * velocity
        if input.isActionHeld(GameAction.MoveBackward) then
          dx -= frontX * velocity; dz -= frontZ * velocity
        if input.isActionHeld(GameAction.StrafeLeft) then
          dx += frontZ * velocity; dz -= frontX * velocity
        if input.isActionHeld(GameAction.StrafeRight) then
          dx -= frontZ * velocity; dz += frontX * velocity
        if dx != 0f || dz != 0f then
          playerPos.x += dx
          playerPos.z += dz
          playerMoving = true
      camera.position.set(playerPos.x, playerPos.y + EyeHeight, playerPos.z)
      // Sync state to PlayerCharacter
      player.foreach { pc =>
        pc.position.set(playerPos)
        pc.headingDeg = playerHeadingDeg
        pc.moving = playerMoving
      }
    else if !ImGui.getIO().getWantCaptureKeyboard() then
      camera.processMovement(input, dt)
