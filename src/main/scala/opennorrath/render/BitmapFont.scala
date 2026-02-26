package opennorrath.render

import org.lwjgl.stb.STBTruetype.*
import org.lwjgl.stb.{STBTTAlignedQuad, STBTTBakedChar}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.system.MemoryStack

class BitmapFont(resourcePath: String, fontSize: Float, atlasSize: Int = 512):

  private val charData = STBTTBakedChar.malloc(96) // ASCII 32..127
  private val MaxChars = 256

  val textureId: Int = {
    val fontBytes = BitmapFont.loadResource(resourcePath)
    val fontBuffer = BufferUtils.createByteBuffer(fontBytes.length)
    fontBuffer.put(fontBytes).flip()

    val bitmap = BufferUtils.createByteBuffer(atlasSize * atlasSize)
    stbtt_BakeFontBitmap(fontBuffer, fontSize, bitmap, atlasSize, atlasSize, 32, charData)

    val texId = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, texId)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, atlasSize, atlasSize, 0, GL_RED, GL_UNSIGNED_BYTE, bitmap)
    texId
  }

  private val vao = glGenVertexArrays()
  private val vbo = glGenBuffers()

  locally {
    glBindVertexArray(vao)
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferData(GL_ARRAY_BUFFER, MaxChars * 6 * 4 * 4L, GL_DYNAMIC_DRAW)
    glVertexAttribPointer(0, 2, GL_FLOAT, false, 4 * 4, 0)
    glEnableVertexAttribArray(0)
    glVertexAttribPointer(1, 2, GL_FLOAT, false, 4 * 4, 2 * 4L)
    glEnableVertexAttribArray(1)
    glBindVertexArray(0)
  }

  /** Render text at the given position. Caller must have the UI shader active with isText=1. */
  def drawText(text: String, x: Float, y: Float): Unit =
    if text.isEmpty then return

    val stack = MemoryStack.stackPush()
    try
      val xBuf = stack.floats(x)
      val yBuf = stack.floats(y)
      val quad = STBTTAlignedQuad.malloc(stack)

      val charCount = text.length.min(MaxChars)
      val vertices = new Array[Float](charCount * 6 * 4)
      var vi = 0
      var triCount = 0

      for ch <- text.take(MaxChars) do
        if ch >= 32 && ch < 128 then
          stbtt_GetBakedQuad(charData, atlasSize, atlasSize, ch.toInt - 32, xBuf, yBuf, quad, true)
          vertices(vi) = quad.x0(); vertices(vi + 1) = quad.y0(); vertices(vi + 2) = quad.s0(); vertices(vi + 3) = quad.t0(); vi += 4
          vertices(vi) = quad.x1(); vertices(vi + 1) = quad.y0(); vertices(vi + 2) = quad.s1(); vertices(vi + 3) = quad.t0(); vi += 4
          vertices(vi) = quad.x1(); vertices(vi + 1) = quad.y1(); vertices(vi + 2) = quad.s1(); vertices(vi + 3) = quad.t1(); vi += 4
          vertices(vi) = quad.x0(); vertices(vi + 1) = quad.y0(); vertices(vi + 2) = quad.s0(); vertices(vi + 3) = quad.t0(); vi += 4
          vertices(vi) = quad.x1(); vertices(vi + 1) = quad.y1(); vertices(vi + 2) = quad.s1(); vertices(vi + 3) = quad.t1(); vi += 4
          vertices(vi) = quad.x0(); vertices(vi + 1) = quad.y1(); vertices(vi + 2) = quad.s0(); vertices(vi + 3) = quad.t1(); vi += 4
          triCount += 6

      if triCount > 0 then
        val buf = BufferUtils.createFloatBuffer(vi)
        buf.put(vertices, 0, vi).flip()
        glBindBuffer(GL_ARRAY_BUFFER, vbo)
        glBufferSubData(GL_ARRAY_BUFFER, 0, buf)

        glBindTexture(GL_TEXTURE_2D, textureId)
        glBindVertexArray(vao)
        glDrawArrays(GL_TRIANGLES, 0, triCount)
        glBindVertexArray(0)
    finally stack.pop()

  /** Returns (minX, minY, maxX, maxY) bounding box for text at the given position. */
  def textBounds(text: String, x: Float, y: Float): (Float, Float, Float, Float) =
    val stack = MemoryStack.stackPush()
    try
      val xBuf = stack.floats(x)
      val yBuf = stack.floats(y)
      val quad = STBTTAlignedQuad.malloc(stack)
      var minX = Float.MaxValue; var minY = Float.MaxValue
      var maxX = Float.MinValue; var maxY = Float.MinValue
      for ch <- text do
        if ch >= 32 && ch < 128 then
          stbtt_GetBakedQuad(charData, atlasSize, atlasSize, ch.toInt - 32, xBuf, yBuf, quad, true)
          minX = minX.min(quad.x0()); minY = minY.min(quad.y0())
          maxX = maxX.max(quad.x1()); maxY = maxY.max(quad.y1())
      if minX == Float.MaxValue then (x, y, x, y)
      else (minX, minY, maxX, maxY)
    finally stack.pop()

  def textWidth(text: String): Float =
    val stack = MemoryStack.stackPush()
    try
      val xBuf = stack.floats(0f)
      val yBuf = stack.floats(0f)
      val quad = STBTTAlignedQuad.malloc(stack)
      for ch <- text do
        if ch >= 32 && ch < 128 then
          stbtt_GetBakedQuad(charData, atlasSize, atlasSize, ch.toInt - 32, xBuf, yBuf, quad, true)
      xBuf.get(0)
    finally stack.pop()

  def cleanup(): Unit =
    glDeleteTextures(textureId)
    glDeleteBuffers(vbo)
    glDeleteVertexArrays(vao)
    charData.free()

object BitmapFont:
  private def loadResource(path: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(path)
    if stream == null then throw RuntimeException(s"Font resource not found: $path")
    try stream.readAllBytes() finally stream.close()
