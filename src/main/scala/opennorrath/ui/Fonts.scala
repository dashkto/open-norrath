package opennorrath.ui

import scala.compiletime.uninitialized

import imgui.{ImFont, ImFontConfig, ImGui}

/** Loads Roboto at multiple sizes for ImGui. Call `init()` before any ImGui rendering.
  *
  * Note: the default glyph range is ASCII + Latin-1 only. Avoid em dashes,
  * curly quotes, and other extended Unicode in UI strings — they render as '?'.
  */
object Fonts:

  var small: ImFont = uninitialized
  var default: ImFont = uninitialized
  var defaultBold: ImFont = uninitialized
  var menu: ImFont = uninitialized
  var title: ImFont = uninitialized

  private val regular = "/fonts/Roboto-Regular.ttf"
  private val bold = "/fonts/Roboto-Bold.ttf"

  def init(): Unit =
    val atlas = ImGui.getIO().getFonts()
    val cfg = ImFontConfig()
    cfg.setFontDataOwnedByAtlas(false)

    default = atlas.addFontFromMemoryTTF(loadResource(regular), 18f, cfg)
    small = atlas.addFontFromMemoryTTF(loadResource(regular), 13f, cfg)
    defaultBold = atlas.addFontFromMemoryTTF(loadResource(bold), 18f, cfg)
    menu = atlas.addFontFromMemoryTTF(loadResource(bold), 28f, cfg)
    title = atlas.addFontFromMemoryTTF(loadResource(bold), 64f, cfg)

    atlas.build()
    cfg.destroy()

  private def loadResource(path: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(path)
    if stream == null then throw RuntimeException(s"Font resource not found: $path")
    try stream.readAllBytes() finally stream.close()
