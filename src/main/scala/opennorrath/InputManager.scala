package opennorrath

import org.lwjgl.glfw.GLFW.*

class InputManager(window: Long):

  // Key state — held is persistent, pressed/released are per-frame edges
  private val keysDown = java.util.HashSet[Int]()
  private val keysPressed = java.util.HashSet[Int]()
  private val keysReleased = java.util.HashSet[Int]()
  private val keysPressedStaging = java.util.HashSet[Int]()
  private val keysReleasedStaging = java.util.HashSet[Int]()

  // Mouse state — accumulated deltas between frames
  private var mouseDX: Float = 0f
  private var mouseDY: Float = 0f
  private var mouseStagingDX: Float = 0f
  private var mouseStagingDY: Float = 0f
  private var lastMouseX: Double = 0.0
  private var lastMouseY: Double = 0.0
  private var firstMouse: Boolean = true

  // Mouse absolute position and buttons
  private var _mouseX: Float = 0f
  private var _mouseY: Float = 0f
  private val buttonsDown = java.util.HashSet[Int]()
  private val buttonsPressed = java.util.HashSet[Int]()
  private val buttonsPressedStaging = java.util.HashSet[Int]()

  // Register all GLFW callbacks
  glfwSetKeyCallback(window, (_, key, _, action, _) =>
    if key != GLFW_KEY_UNKNOWN then
      action match
        case GLFW_PRESS =>
          keysDown.add(key)
          keysPressedStaging.add(key)
        case GLFW_RELEASE =>
          keysDown.remove(key)
          keysReleasedStaging.add(key)
        case _ => ()
  )

  glfwSetCursorPosCallback(window, (_, xPos, yPos) =>
    if firstMouse then
      lastMouseX = xPos
      lastMouseY = yPos
      firstMouse = false
    mouseStagingDX += (xPos - lastMouseX).toFloat
    mouseStagingDY += (lastMouseY - yPos).toFloat
    lastMouseX = xPos
    lastMouseY = yPos
    _mouseX = xPos.toFloat
    _mouseY = yPos.toFloat
  )

  glfwSetMouseButtonCallback(window, (_, button, action, _) =>
    action match
      case GLFW_PRESS =>
        buttonsDown.add(button)
        buttonsPressedStaging.add(button)
      case GLFW_RELEASE =>
        buttonsDown.remove(button)
      case _ => ()
  )

  /** Call once per frame before reading input. Promotes staged callback data into current-frame state. */
  def update(): Unit =
    keysPressed.clear()
    keysPressed.addAll(keysPressedStaging)
    keysPressedStaging.clear()

    keysReleased.clear()
    keysReleased.addAll(keysReleasedStaging)
    keysReleasedStaging.clear()

    mouseDX = mouseStagingDX
    mouseDY = mouseStagingDY
    mouseStagingDX = 0f
    mouseStagingDY = 0f

    buttonsPressed.clear()
    buttonsPressed.addAll(buttonsPressedStaging)
    buttonsPressedStaging.clear()

  def isKeyHeld(key: Int): Boolean = keysDown.contains(key)
  def isKeyPressed(key: Int): Boolean = keysPressed.contains(key)
  def isKeyReleased(key: Int): Boolean = keysReleased.contains(key)
  def isMouseHeld(button: Int): Boolean = buttonsDown.contains(button)
  def isMousePressed(button: Int): Boolean = buttonsPressed.contains(button)
  def mousePos: (Float, Float) = (_mouseX, _mouseY)
  def mouseDelta: (Float, Float) = (mouseDX, mouseDY)
