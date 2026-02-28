package opennorrath

import org.lwjgl.glfw.GLFW.*

enum GameAction:
  case MoveForward, MoveBackward, StrafeLeft, StrafeRight, MoveUp, MoveDown, Jump
  case FreeLook, Target
  case TargetSelf, TabTarget
  case AutoAttack, Consider
  case Sit
  case ToggleInventory, ToggleSpellBook, ToggleStats, DumpDebug, DetachCamera, Escape

sealed trait InputBinding
case class KeyBind(key: Int, shift: Boolean = false) extends InputBinding
case class MouseBind(button: Int) extends InputBinding

class KeyBindings(val bindings: Map[GameAction, Seq[InputBinding]] = KeyBindings.defaults)

object KeyBindings:
  val defaults: Map[GameAction, Seq[InputBinding]] = Map(
    GameAction.MoveForward  -> Seq(KeyBind(GLFW_KEY_W)),
    GameAction.MoveBackward -> Seq(KeyBind(GLFW_KEY_S)),
    GameAction.StrafeLeft   -> Seq(KeyBind(GLFW_KEY_A)),
    GameAction.StrafeRight  -> Seq(KeyBind(GLFW_KEY_D)),
    GameAction.MoveUp       -> Seq(KeyBind(GLFW_KEY_SPACE)),
    GameAction.Jump         -> Seq(KeyBind(GLFW_KEY_SPACE)),
    GameAction.MoveDown     -> Seq(KeyBind(GLFW_KEY_LEFT_SHIFT)),
    GameAction.FreeLook     -> Seq(MouseBind(GLFW_MOUSE_BUTTON_RIGHT)),
    GameAction.Target       -> Seq(MouseBind(GLFW_MOUSE_BUTTON_LEFT)),
    GameAction.TargetSelf      -> Seq(KeyBind(GLFW_KEY_F1)),
    GameAction.TabTarget       -> Seq(KeyBind(GLFW_KEY_TAB)),
    GameAction.AutoAttack       -> Seq(KeyBind(GLFW_KEY_GRAVE_ACCENT)),
    GameAction.Consider        -> Seq(KeyBind(GLFW_KEY_C)),
    GameAction.Sit             -> Seq(KeyBind(GLFW_KEY_Q)),
    GameAction.ToggleInventory -> Seq(KeyBind(GLFW_KEY_I)),
    GameAction.ToggleSpellBook -> Seq(KeyBind(GLFW_KEY_B)),
    GameAction.ToggleStats     -> Seq(KeyBind(GLFW_KEY_O)),
    GameAction.DumpDebug    -> Seq(KeyBind(GLFW_KEY_T)),
    GameAction.DetachCamera -> Seq(KeyBind(GLFW_KEY_T, shift = true)),
    GameAction.Escape       -> Seq(KeyBind(GLFW_KEY_ESCAPE)),
  )

class InputManager(window: Long, val keyBindings: KeyBindings = KeyBindings()):

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

  private var scrollStagingY: Float = 0f
  private var _scrollY: Float = 0f

  glfwSetScrollCallback(window, (_, _, yOffset) =>
    scrollStagingY += yOffset.toFloat
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

    _scrollY = scrollStagingY
    scrollStagingY = 0f

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
  def scrollY: Float = _scrollY

  private def shiftHeld: Boolean =
    keysDown.contains(GLFW_KEY_LEFT_SHIFT) || keysDown.contains(GLFW_KEY_RIGHT_SHIFT)

  def isActionHeld(action: GameAction): Boolean =
    keyBindings.bindings.getOrElse(action, Seq.empty).exists {
      case KeyBind(k, shift) => keysDown.contains(k) && (shift == shiftHeld)
      case MouseBind(b) => buttonsDown.contains(b)
    }

  def isActionPressed(action: GameAction): Boolean =
    keyBindings.bindings.getOrElse(action, Seq.empty).exists {
      case KeyBind(k, shift) => keysPressed.contains(k) && (shift == shiftHeld)
      case MouseBind(b) => buttonsPressed.contains(b)
    }
