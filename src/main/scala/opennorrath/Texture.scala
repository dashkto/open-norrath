package opennorrath

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL30.glGenerateMipmap
import org.lwjgl.stb.STBImage.*
import org.lwjgl.system.MemoryStack
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

  def loadFromBytes(data: Array[Byte]): Int =
    val stack = MemoryStack.stackPush()
    try
      val inputBuf = BufferUtils.createByteBuffer(data.length)
      inputBuf.put(data).flip()

      val w = stack.mallocInt(1)
      val h = stack.mallocInt(1)
      val channels = stack.mallocInt(1)

      stbi_set_flip_vertically_on_load(true)
      val pixels = stbi_load_from_memory(inputBuf, w, h, channels, 4) // force RGBA
      if pixels == null then
        throw RuntimeException(s"Failed to load texture: ${stbi_failure_reason()}")

      val textureId = glGenTextures()
      glBindTexture(GL_TEXTURE_2D, textureId)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w.get(0), h.get(0), 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels)
      glGenerateMipmap(GL_TEXTURE_2D)

      stbi_image_free(pixels)
      textureId
    finally stack.pop()
