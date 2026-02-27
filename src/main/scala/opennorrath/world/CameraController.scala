package opennorrath.world

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*

import imgui.ImGui

import opennorrath.{GameAction, InputManager}
import opennorrath.network.{PlayerProfileData, SpawnData}
import opennorrath.state.PlayerCharacter

/** Manages camera state, freelook cursor toggling, and projection.
  * Starts attached to the player position; Shift+T detaches for free flight.
  * Player position/heading/moving state lives in PlayerCharacter; this class
  * reads and writes it, then positions the camera eye accordingly.
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

  /** Optional reference to the player character — owns position/heading/moving. */
  var player: Option[PlayerCharacter] = None

  /** Fallback position when no player (debug/test mode). */
  private val fallbackPos = Vector3f()
  private val eyeProbe = Vector3f()   // reusable for camera collision raycast
  private val camProbe = Vector3f()

  val projection: Matrix4f = Matrix4f().perspective(
    Math.toRadians(60.0).toFloat,
    screenW.toFloat / screenH.toFloat,
    0.1f,
    10000f,
  )

  def viewMatrix: Matrix4f = camera.viewMatrix

  def initFromSpawn(spawn: Option[SpawnData], profile: Option[PlayerProfileData]): Unit =
    val (footPos, startYaw) = spawn match
      case Some(s) =>
        (EqCoords.serverToGl(s.y, s.x, s.z), EqCoords.spawnHeadingToYaw(s.heading))
      case None => profile match
        case Some(pp) =>
          (EqCoords.serverToGl(pp.y, pp.x, pp.z), EqCoords.profileHeadingToYaw(pp.heading))
        case None =>
          (Vector3f(-150f, 50f, -460f), 30f)
    fallbackPos.set(footPos)
    player.foreach { pc =>
      pc.position.set(footPos)
      pc.updateHeading(startYaw)
      pc.syncToZoneChar()
    }
    val eyePos = Vector3f(footPos).add(0f, eyeHeight, 0f)
    camera = Camera(
      position = eyePos,
      yaw = startYaw,
      pitch = -5f,
      speed = 100f,
    )
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
      val pos = player.map(_.position).getOrElse(fallbackPos)

      // Sync from ZoneCharacter (model-origin space → feet-level)
      player.foreach(_.syncFromZoneChar())

      var moved = false
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
          player match
            case Some(pc) =>
              moved = pc.tryMove(pos, dx, dz)
            case None =>
              pos.x += dx
              pos.z += dz
              moved = true
        if input.isActionPressed(GameAction.Jump) then
          player.foreach(_.jump())
      // Gravity (feet-level space)
      player.foreach(_.applyGravity(pos, dt))

      // Update player heading and moving state
      player.foreach { pc =>
        pc.updateHeading(camera.yaw)
        pc.moving = moved
      }

      // Position camera: at eye height above feet, pulled back by zoomDist along camera yaw
      val eyeY = pos.y + eyeHeight
      if zoomDist <= 0f then
        camera.position.set(pos.x, eyeY, pos.z)
      else
        val yawRad2 = Math.toRadians(camera.yaw).toFloat
        val offX = -Math.cos(yawRad2).toFloat * zoomDist
        val offZ = -Math.sin(yawRad2).toFloat * zoomDist
        val desiredX = pos.x + offX
        val desiredY = eyeY + zoomDist * 0.3f
        val desiredZ = pos.z + offZ
        // Prevent camera from clipping through zone geometry
        val col = player.flatMap(_.collision)
        col match
          case Some(c) =>
            eyeProbe.set(pos.x, eyeY, pos.z)
            camProbe.set(desiredX, desiredY, desiredZ)
            val hitDist = c.raycast(eyeProbe, camProbe)
            if hitDist > 0f then
              // Pull camera to just before the hit point
              val totalDist = eyeProbe.distance(camProbe)
              val t = (hitDist - 0.5f) / totalDist  // back off slightly
              if t > 0f then
                camera.position.set(
                  pos.x + offX * t,
                  eyeY + (desiredY - eyeY) * t,
                  pos.z + offZ * t,
                )
              else
                camera.position.set(pos.x, eyeY, pos.z)
            else
              camera.position.set(desiredX, desiredY, desiredZ)
          case None =>
            camera.position.set(desiredX, desiredY, desiredZ)

      // Sync state to ZoneCharacter for rendering
      player.foreach(_.syncToZoneChar())
    else if !ImGui.getIO().getWantCaptureKeyboard() then
      camera.processMovement(input, dt)
