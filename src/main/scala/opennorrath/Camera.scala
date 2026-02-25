package opennorrath

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*

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

  def processKeyboard(window: Long, deltaTime: Float): Unit =
    val velocity = speed * deltaTime
    val movement = Vector3f()

    if glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS then
      movement.add(Vector3f(front).mul(velocity))
    if glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS then
      movement.sub(Vector3f(front).mul(velocity))
    if glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS then
      movement.sub(Vector3f(right).mul(velocity))
    if glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS then
      movement.add(Vector3f(right).mul(velocity))
    if glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_PRESS then
      movement.add(Vector3f(up).mul(velocity))
    if glfwGetKey(window, GLFW_KEY_LEFT_SHIFT) == GLFW_PRESS then
      movement.sub(Vector3f(up).mul(velocity))

    position.add(movement)

  def processMouse(xOffset: Float, yOffset: Float): Unit =
    yaw += xOffset * sensitivity
    pitch += yOffset * sensitivity
    pitch = pitch.max(-89f).min(89f)
    updateVectors()

  private def updateVectors(): Unit =
    val yawRad = Math.toRadians(yaw).toFloat
    val pitchRad = Math.toRadians(pitch).toFloat
    front.set(
      Math.cos(yawRad) * Math.cos(pitchRad),
      Math.sin(pitchRad),
      Math.sin(yawRad) * Math.cos(pitchRad),
    ).normalize()
    front.cross(up, right)
    right.normalize()
