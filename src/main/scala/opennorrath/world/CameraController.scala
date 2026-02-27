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

  /** Eye height above feet — at the top of the model. */
  private def eyeHeight: Float = player.map(_.modelHeight).filter(_ > 0f).getOrElse(5.5f)
  private def playerSpeed: Float = player.map(_.runSpeed).getOrElse(30f)
  private val MinZoom = 0f       // first person
  private val MaxZoom = 20f      // max third-person distance
  private val ZoomStep = 3f      // distance per scroll notch
  private var zoomDist = 0f      // current distance behind player (0 = first person)

  var camera: Camera = Camera()
  var freeLook = false
  var attached = true
  var playerMoving = false

  /** Optional reference to the player character — updated with position/heading/moving. */
  var player: Option[PlayerCharacter] = None

  /** Player foot position in GL space (eye = playerPos + eyeHeight on Y). */
  val playerPos = Vector3f()
  private val eyeProbe = Vector3f()   // reusable for camera collision raycast
  private val camProbe = Vector3f()

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
    val eyePos = Vector3f(footPos).add(0f, eyeHeight, 0f)
    camera = Camera(
      position = eyePos,
      yaw = startYaw,
      pitch = -5f,
      speed = 100f,
    )
    // Sync initial position/heading to PlayerCharacter (model-origin space)
    val hdeg = { val h = -startYaw % 360f; if h < 0 then h + 360f else h }
    player.foreach { pc =>
      pc.position.set(footPos.x, footPos.y + pc.feetOffset, footPos.z)
      pc.headingDeg = hdeg
    }
    println(s"Camera: pos=$eyePos yaw=$startYaw attached=$attached")

  def update(input: InputManager, dt: Float): Unit =
    if !ImGui.getIO().getWantCaptureKeyboard() && input.isActionPressed(GameAction.DetachCamera) then
      attached = !attached
      if attached then zoomDist = 0f
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

    // Scroll wheel adjusts third-person zoom when attached
    if attached && !ImGui.getIO().getWantCaptureMouse() then
      val scroll = input.scrollY
      if scroll != 0f then
        zoomDist = Math.max(MinZoom, Math.min(MaxZoom, zoomDist - scroll * ZoomStep)).toFloat

    if attached then
      playerMoving = false
      if !ImGui.getIO().getWantCaptureKeyboard() then
        // Horizontal movement along camera yaw direction
        val yawRad = Math.toRadians(camera.yaw).toFloat
        val frontX = Math.cos(yawRad).toFloat
        val frontZ = Math.sin(yawRad).toFloat
        val velocity = playerSpeed * dt
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
        if input.isActionPressed(GameAction.Jump) then
          player.foreach(_.jump())
      // Gravity — delegated to PlayerCharacter (which works in model-origin space)
      player.foreach { pc =>
        pc.position.set(playerPos.x, playerPos.y + pc.feetOffset, playerPos.z)
        pc.applyGravity(dt)
        playerPos.y = pc.position.y - pc.feetOffset  // back to feet
      }

      // Position camera: at eye height above feet, pulled back by zoomDist along camera yaw
      val eyeY = playerPos.y + eyeHeight
      if zoomDist <= 0f then
        camera.position.set(playerPos.x, eyeY, playerPos.z)
      else
        val yawRad2 = Math.toRadians(camera.yaw).toFloat
        val offX = -Math.cos(yawRad2).toFloat * zoomDist
        val offZ = -Math.sin(yawRad2).toFloat * zoomDist
        val desiredX = playerPos.x + offX
        val desiredY = eyeY + zoomDist * 0.3f
        val desiredZ = playerPos.z + offZ
        // Prevent camera from clipping through zone geometry
        val col = player.flatMap(_.collision)
        col match
          case Some(c) =>
            eyeProbe.set(playerPos.x, eyeY, playerPos.z)
            camProbe.set(desiredX, desiredY, desiredZ)
            val hitDist = c.raycast(eyeProbe, camProbe)
            if hitDist > 0f then
              // Pull camera to just before the hit point
              val totalDist = eyeProbe.distance(camProbe)
              val t = (hitDist - 0.5f) / totalDist  // back off slightly
              if t > 0f then
                camera.position.set(
                  playerPos.x + offX * t,
                  eyeY + (desiredY - eyeY) * t,
                  playerPos.z + offZ * t,
                )
              else
                camera.position.set(playerPos.x, eyeY, playerPos.z)
            else
              camera.position.set(desiredX, desiredY, desiredZ)
          case None =>
            camera.position.set(desiredX, desiredY, desiredZ)
      // Sync state to PlayerCharacter (model-origin space)
      player.foreach { pc =>
        pc.position.set(playerPos.x, playerPos.y + pc.feetOffset, playerPos.z)
        pc.headingDeg = playerHeadingDeg
        pc.moving = playerMoving
      }
    else if !ImGui.getIO().getWantCaptureKeyboard() then
      camera.processMovement(input, dt)
