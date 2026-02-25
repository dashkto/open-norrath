package opennorrath.screen

import opennorrath.{InputManager, Settings}

/** Shared resources available to all screens. */
case class GameContext(
    window: Long,
    input: InputManager,
    settings: Settings,
    windowWidth: Int,
    windowHeight: Int,
)

/** Lifecycle interface for a game screen. */
trait Screen:

  /** Called when this screen becomes the active screen. Create GL resources here. */
  def show(): Unit = ()

  /** Called once per frame. Process input and update game state. */
  def update(dt: Float): Unit = ()

  /** Called once per frame after update. Issue draw calls here. */
  def render(dt: Float): Unit = ()

  /** Called when this screen is replaced or the game exits. Release GL resources. */
  def dispose(): Unit = ()
