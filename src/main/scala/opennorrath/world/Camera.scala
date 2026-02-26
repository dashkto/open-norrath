package opennorrath.world

import opennorrath.{GameAction, InputManager}
import org.joml.{Matrix4f, Vector3f}

class Camera(
    val position: Vector3f = Vector3f(0f, 0f, 3f),
    var yaw: Float = -90f,
    var pitch: Float = 0f,
    var speed: Float = 5f,
    var sensitivity: Float = 0.1f,
):

  private val front = Vector3f()
  private val up = Vector3f(0f, 1f, 0f)
  private val right = Vector3f()
  updateVectors()

  def viewMatrix: Matrix4f =
    val target = Vector3f(position).add(front)
    Matrix4f().lookAt(position, target, up)

  def processInput(input: InputManager, deltaTime: Float): Unit =
    processMovement(input, deltaTime)
    processLook(input)

  def processMovement(input: InputManager, deltaTime: Float): Unit =
    val velocity = speed * deltaTime
    val movement = Vector3f()

    if input.isActionHeld(GameAction.MoveForward) then movement.add(Vector3f(front).mul(velocity))
    if input.isActionHeld(GameAction.MoveBackward) then movement.sub(Vector3f(front).mul(velocity))
    if input.isActionHeld(GameAction.StrafeLeft) then movement.sub(Vector3f(right).mul(velocity))
    if input.isActionHeld(GameAction.StrafeRight) then movement.add(Vector3f(right).mul(velocity))
    if input.isActionHeld(GameAction.MoveUp) then movement.add(Vector3f(up).mul(velocity))
    if input.isActionHeld(GameAction.MoveDown) then movement.sub(Vector3f(up).mul(velocity))

    position.add(movement)

  def processLook(input: InputManager): Unit =
    val (dx, dy) = input.mouseDelta
    if dx != 0f || dy != 0f then
      yaw += dx * sensitivity
      pitch = (pitch + dy * sensitivity).max(-89f).min(89f)
      updateVectors()

  def updateVectors(): Unit =
    val yawRad = Math.toRadians(yaw).toFloat
    val pitchRad = Math.toRadians(pitch).toFloat
    front.set(
      Math.cos(yawRad) * Math.cos(pitchRad),
      Math.sin(pitchRad),
      Math.sin(yawRad) * Math.cos(pitchRad),
    ).normalize()
    front.cross(up, right)
    right.normalize()
