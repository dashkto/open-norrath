package opennorrath.ui

import java.nio.file.{Files, Path}

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import org.lwjgl.stb.STBImage.*
import org.lwjgl.system.MemoryStack

import imgui.ImGui

/** Loads and caches EQ item icon atlases from dragitem TGA files.
  *
  * Each dragitem{N}.tga is a 256x256 atlas containing a 6x6 grid of ~40px icons.
  * Item icon numbers map to atlases: sheet = icon/36 + 1, cell = icon % 36.
  *
  * Usage:
  *   ItemIcons.init("assets/EverQuest")  // call once at startup
  *   ItemIcons.render(iconNumber, size)  // call in ImGui context
  */
object ItemIcons:

  private val IconsPerSheet = 36
  private val GridCols = 6
  private val GridRows = 6
  private val AtlasSize = 256f
  private val CellSize = AtlasSize / GridCols // ~42.67 pixels

  // Loaded atlas GL texture IDs, keyed by sheet number (1-based)
  private var atlases = Map.empty[Int, Int]
  private var assetPath = ""
  private var initialized = false

  /** Initialize with the EQ asset directory path. Atlases are loaded lazily on first use. */
  def init(eqAssetsPath: String): Unit =
    assetPath = eqAssetsPath
    initialized = true

  /** Render an item icon at the given size in the current ImGui context.
    * Returns true if the icon was rendered, false if unavailable.
    */
  def render(icon: Int, size: Float = 40f): Boolean =
    if !initialized || icon < 0 then return false
    val sheet = icon / IconsPerSheet + 1
    val cell = icon % IconsPerSheet
    val col = cell / GridRows  // Vertical=true: fills top-to-bottom first
    val row = cell % GridRows

    val texId = getAtlas(sheet)
    if texId == 0 then return false

    val u0 = col * CellSize / AtlasSize
    val v0 = row * CellSize / AtlasSize
    val u1 = (col + 1) * CellSize / AtlasSize
    val v1 = (row + 1) * CellSize / AtlasSize

    ImGui.image(texId, size, size, u0, v0, u1, v1)
    true

  /** Get the GL texture ID for a sheet, loading it lazily. Returns 0 if unavailable. */
  private def getAtlas(sheet: Int): Int =
    atlases.getOrElse(sheet, {
      val texId = loadAtlas(sheet)
      atlases = atlases + (sheet -> texId)
      texId
    })

  private def loadAtlas(sheet: Int): Int =
    val file = Path.of(assetPath, "uifiles", "default", s"dragitem$sheet.tga")
    if !Files.exists(file) then
      println(s"[ItemIcons] Atlas not found: $file")
      return 0

    val data = Files.readAllBytes(file)
    val stack = MemoryStack.stackPush()
    try
      val inputBuf = BufferUtils.createByteBuffer(data.length)
      inputBuf.put(data).flip()

      val w = stack.mallocInt(1)
      val h = stack.mallocInt(1)
      val channels = stack.mallocInt(1)

      stbi_set_flip_vertically_on_load(false) // TGA icons are top-down
      val pixels = stbi_load_from_memory(inputBuf, w, h, channels, 4)
      if pixels == null then
        println(s"[ItemIcons] Failed to load $file: ${stbi_failure_reason()}")
        return 0

      val textureId = glGenTextures()
      glBindTexture(GL_TEXTURE_2D, textureId)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w.get(0), h.get(0), 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels)

      stbi_image_free(pixels)
      println(s"[ItemIcons] Loaded dragitem$sheet.tga (${w.get(0)}x${h.get(0)})")
      textureId
    finally stack.pop()

  /** Release all loaded atlas textures. */
  def cleanup(): Unit =
    for (_, texId) <- atlases if texId != 0 do
      glDeleteTextures(texId)
    atlases = Map.empty
    initialized = false
