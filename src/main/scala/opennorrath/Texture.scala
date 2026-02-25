package opennorrath

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL30.glGenerateMipmap
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils

object Texture:

  def createCheckerboard(size: Int = 256, tileSize: Int = 32): Int =
    val buffer = BufferUtils.createByteBuffer(size * size * 3)

    for
      y <- 0 until size
      x <- 0 until size
    do
      val isWhite = ((x / tileSize) + (y / tileSize)) % 2 == 0
      val color: Byte = if isWhite then 200.toByte else 60.toByte
      buffer.put(color).put(color).put(color)

    buffer.flip()

    val textureId = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, textureId)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, size, size, 0, GL_RGB, GL_UNSIGNED_BYTE, buffer)
    glGenerateMipmap(GL_TEXTURE_2D)

    textureId
