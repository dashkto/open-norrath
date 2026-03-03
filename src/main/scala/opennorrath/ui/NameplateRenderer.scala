package opennorrath.ui

import java.awt.RenderingHints
import java.awt.image.BufferedImage

import org.joml.Matrix4f
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*

import opennorrath.render.Shader

/** Renders spawn names as billboarded 3D quads with depth testing.
  * Text is rendered to cached GL textures via Java2D.
  *
  * Batched rendering: all quads are built into one vertex buffer, sorted by
  * texture ID, and drawn with one glBufferSubData + one draw call per unique
  * texture. This avoids the massive overhead of per-nameplate buffer uploads
  * and draw calls (which dominated frame time at 107+ characters).
  */
class NameplateRenderer:

  // Force AWT headless mode to avoid macOS main-thread conflicts with GLFW
  System.setProperty("java.awt.headless", "true")

  // --- Text texture cache ---
  private case class NameTexture(texId: Int, widthPx: Int, heightPx: Int)
  private val cache = scala.collection.mutable.Map.empty[(String, Int), NameTexture]

  private val TextFont = Fonts.awt(Fonts.bold, 24f)
  private val WorldScale = 0.035f // pixels → world units

  // --- Dynamic billboard mesh ---
  private val MaxQuads = 512 // 2 per nameplate (shadow + foreground)
  private val FloatsPerVert = 5 // pos(3) + uv(2)
  private val VertsPerQuad = 4
  private val IndicesPerQuad = 6
  private val vertexData = new Array[Float](MaxQuads * VertsPerQuad * FloatsPerVert)
  private val indexData = new Array[Int](MaxQuads * IndicesPerQuad)
  // Reusable upload buffer sized for all quads (not just one)
  private val uploadBuf = BufferUtils.createFloatBuffer(MaxQuads * VertsPerQuad * FloatsPerVert)

  // Per-quad texture tracking for batched draw: quadTexIds(i) = GL texture for quad i
  private val quadTexIds = new Array[Int](MaxQuads)
  // Sorted draw order: indices into quadTexIds, sorted by texture ID for minimal binds
  private val drawOrder = new Array[Int](MaxQuads)

  // Pre-fill index data (0,1,2, 2,3,0 pattern per quad)
  for i <- 0 until MaxQuads do
    val base = i * 4
    val idx = i * 6
    indexData(idx) = base
    indexData(idx + 1) = base + 1
    indexData(idx + 2) = base + 2
    indexData(idx + 3) = base + 2
    indexData(idx + 4) = base + 3
    indexData(idx + 5) = base

  private var vao = 0
  private var vbo = 0
  private var ebo = 0
  private var initialized = false

  private def ensureInit(): Unit =
    if initialized then return
    vao = glGenVertexArrays()
    vbo = glGenBuffers()
    ebo = glGenBuffers()

    glBindVertexArray(vao)

    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferData(GL_ARRAY_BUFFER, vertexData.length.toLong * 4, GL_DYNAMIC_DRAW)

    val ibuf = BufferUtils.createIntBuffer(indexData.length)
    ibuf.put(indexData).flip()
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, ibuf, GL_STATIC_DRAW)

    glVertexAttribPointer(0, 3, GL_FLOAT, false, FloatsPerVert * 4, 0)
    glEnableVertexAttribArray(0)
    glVertexAttribPointer(1, 2, GL_FLOAT, false, FloatsPerVert * 4, 3 * 4)
    glEnableVertexAttribArray(1)

    glBindVertexArray(0)
    initialized = true

  /** Draw all nameplates. Call after 3D scene, before ImGui panels. */
  def draw(shader: Shader, viewMatrix: Matrix4f,
           characters: scala.collection.Map[Int, opennorrath.state.ZoneCharacter],
           nameplateData: Iterable[(Int, Matrix4f, Float)],
           targetId: Option[Int] = None): Unit =
    ensureInit()

    // Billboard axes from view matrix (camera right and up in world space)
    val rightX = viewMatrix.m00()
    val rightY = viewMatrix.m10()
    val rightZ = viewMatrix.m20()
    val upX = viewMatrix.m01()
    val upY = viewMatrix.m11()
    val upZ = viewMatrix.m21()

    // --- Phase 1: Build all quads into vertexData, track texture per quad ---
    var quadCount = 0
    val identity = Matrix4f()

    // Shadow offset: 1.5 pixels in billboard right/down direction
    val shadowOff = 1.5f * WorldScale
    val soRx = rightX * shadowOff; val soRy = rightY * shadowOff; val soRz = rightZ * shadowOff
    val soDx = -upX * shadowOff;   val soDy = -upY * shadowOff;   val soDz = -upZ * shadowOff

    for (spawnId, modelMatrix, headHeight) <- nameplateData if quadCount + 1 < MaxQuads do
      characters.get(spawnId).foreach { zc =>
        val name = if zc.dead then s"${zc.displayName}'s corpse" else zc.displayName
        if name.nonEmpty then
          val color =
            if zc.dead then Colors.white                      // Dead — corpse nameplate
            else if targetId.contains(spawnId) then Colors.primary
            else if zc.npcType == 0 then Colors.sky           // Player characters
            else Colors.secondary                              // NPCs

          // World position above head
          val wx = modelMatrix.m30()
          val wy = modelMatrix.m31() + headHeight * 0.5f + 0.8f
          val wz = modelMatrix.m32()

          // Black shadow quad (drawn first, offset down-right)
          val shadowTex = getTexture(name, Colors.black)
          buildQuad(quadCount, shadowTex.widthPx, shadowTex.heightPx,
            wx + soRx + soDx, wy + soRy + soDy, wz + soRz + soDz,
            rightX, rightY, rightZ, upX, upY, upZ)
          quadTexIds(quadCount) = shadowTex.texId
          quadCount += 1

          // Foreground colored quad
          val fgTex = getTexture(name, color)
          buildQuad(quadCount, fgTex.widthPx, fgTex.heightPx,
            wx, wy, wz, rightX, rightY, rightZ, upX, upY, upZ)
          quadTexIds(quadCount) = fgTex.texId
          quadCount += 1
      }

    if quadCount == 0 then return

    // --- Phase 2: Sort quad indices by texture ID for batched drawing ---
    for i <- 0 until quadCount do drawOrder(i) = i
    java.util.Arrays.sort(drawOrder, 0, quadCount)
    // Sort by texture ID using a simple insertion sort (stable, fast for small N)
    var i = 1
    while i < quadCount do
      val key = drawOrder(i)
      val keyTex = quadTexIds(key)
      var j = i - 1
      while j >= 0 && quadTexIds(drawOrder(j)) > keyTex do
        drawOrder(j + 1) = drawOrder(j)
        j -= 1
      drawOrder(j + 1) = key
      i += 1

    // --- Phase 3: Reorder vertex data by draw order and upload once ---
    // Build a contiguous vertex buffer sorted by texture so each texture's
    // quads are adjacent, enabling one draw call per unique texture.
    uploadBuf.clear()
    for idx <- 0 until quadCount do
      val qi = drawOrder(idx)
      uploadBuf.put(vertexData, qi * VertsPerQuad * FloatsPerVert, VertsPerQuad * FloatsPerVert)
    uploadBuf.flip()

    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferSubData(GL_ARRAY_BUFFER, 0, uploadBuf)

    // --- Phase 4: Draw batched — one draw call per contiguous texture run ---
    shader.setMatrix4f("model", identity)
    glBindVertexArray(vao)

    var curTex = -1
    var runStart = 0
    for idx <- 0 until quadCount do
      val tex = quadTexIds(drawOrder(idx))
      if tex != curTex then
        // Flush previous run
        if curTex != -1 then
          glDrawElements(GL_TRIANGLES, (idx - runStart) * IndicesPerQuad,
            GL_UNSIGNED_INT, (runStart * IndicesPerQuad).toLong * 4)
        curTex = tex
        glBindTexture(GL_TEXTURE_2D, tex)
        runStart = idx
    // Flush final run
    if curTex != -1 then
      glDrawElements(GL_TRIANGLES, (quadCount - runStart) * IndicesPerQuad,
        GL_UNSIGNED_INT, (runStart * IndicesPerQuad).toLong * 4)

    glBindVertexArray(0)

  /** Write billboard quad vertices into vertexData at the given quad index. */
  private def buildQuad(qi: Int, widthPx: Int, heightPx: Int,
      wx: Float, wy: Float, wz: Float,
      rx: Float, ry: Float, rz: Float,
      ux: Float, uy: Float, uz: Float): Unit =
    val hw = widthPx * WorldScale * 0.5f
    val hh = heightPx * WorldScale
    val vi = qi * VertsPerQuad * FloatsPerVert
    // Bottom-left
    vertexData(vi)      = wx - rx * hw;       vertexData(vi + 1)  = wy - ry * hw;       vertexData(vi + 2)  = wz - rz * hw
    vertexData(vi + 3)  = 0f;                 vertexData(vi + 4)  = 1f
    // Bottom-right
    vertexData(vi + 5)  = wx + rx * hw;       vertexData(vi + 6)  = wy + ry * hw;       vertexData(vi + 7)  = wz + rz * hw
    vertexData(vi + 8)  = 1f;                 vertexData(vi + 9)  = 1f
    // Top-right
    vertexData(vi + 10) = wx + rx * hw + ux * hh; vertexData(vi + 11) = wy + ry * hw + uy * hh; vertexData(vi + 12) = wz + rz * hw + uz * hh
    vertexData(vi + 13) = 1f;                 vertexData(vi + 14) = 0f
    // Top-left
    vertexData(vi + 15) = wx - rx * hw + ux * hh; vertexData(vi + 16) = wy - ry * hw + uy * hh; vertexData(vi + 17) = wz - rz * hw + uz * hh
    vertexData(vi + 18) = 0f;                 vertexData(vi + 19) = 0f

  private def getTexture(name: String, color: (Float, Float, Float, Float)): NameTexture =
    val key = (name, color.hashCode())
    cache.getOrElseUpdate(key, createTexture(name, color))

  private def createTexture(name: String, color: (Float, Float, Float, Float)): NameTexture =
    // Measure text
    val measureImg = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val measureG = measureImg.createGraphics()
    measureG.setFont(TextFont)
    val metrics = measureG.getFontMetrics()
    val textW = metrics.stringWidth(name)
    val textH = metrics.getHeight()
    measureG.dispose()

    // Render text to image
    val padX = 4
    val imgW = textW + padX * 2
    val imgH = textH + 4
    val img = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_ARGB)
    val g = img.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(TextFont)
    g.setColor(Colors.toAwt(color))
    g.drawString(name, padX, metrics.getAscent() + 2)
    g.dispose()

    // Extract RGBA pixels
    val pixels = BufferUtils.createByteBuffer(imgW * imgH * 4)
    for y <- 0 until imgH do
      for x <- 0 until imgW do
        val argb = img.getRGB(x, y)
        pixels.put(((argb >> 16) & 0xFF).toByte) // R
        pixels.put(((argb >> 8) & 0xFF).toByte)  // G
        pixels.put((argb & 0xFF).toByte)          // B
        pixels.put(((argb >> 24) & 0xFF).toByte)  // A
    pixels.flip()

    // Upload to GL
    val texId = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, texId)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, imgW, imgH, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels)

    NameTexture(texId, imgW, imgH)

  def cleanup(): Unit =
    for (_, tex) <- cache do glDeleteTextures(tex.texId)
    cache.clear()
    if initialized then
      glDeleteBuffers(vbo)
      glDeleteBuffers(ebo)
      glDeleteVertexArrays(vao)
      initialized = false
