package opennorrath

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
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

  def createSoftCircle(size: Int = 64): Int =
    val buffer = BufferUtils.createByteBuffer(size * size * 4)
    val center = size / 2.0f
    for
      y <- 0 until size
      x <- 0 until size
    do
      val dx = (x - center) / center
      val dy = (y - center) / center
      val dSq = dx * dx + dy * dy
      val alpha = (1f - dSq).max(0f)
      val a = (alpha * alpha * 255).toInt.min(255).toByte
      buffer.put(255.toByte).put(255.toByte).put(255.toByte).put(a)
    buffer.flip()

    val textureId = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, textureId)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)
    textureId

  /** Extract the color key (palette entry 0) from an 8-bit paletted BMP.
    * Returns Some((r, g, b)) if the BMP is paletted, None otherwise.
    */
  private def extractBmpColorKey(data: Array[Byte]): Option[(Int, Int, Int)] =
    if data.length < 30 then return None
    if data(0) != 'B' || data(1) != 'M' then return None
    val bitsPerPixel = (data(28) & 0xFF) | ((data(29) & 0xFF) << 8)
    if bitsPerPixel != 8 then return None
    val dibHeaderSize = (data(14) & 0xFF) | ((data(15) & 0xFF) << 8) |
      ((data(16) & 0xFF) << 16) | ((data(17) & 0xFF) << 24)
    val paletteOffset = 14 + dibHeaderSize
    if data.length < paletteOffset + 4 then return None
    // BMP palette is BGRA
    val b = data(paletteOffset.toInt) & 0xFF
    val g = data(paletteOffset.toInt + 1) & 0xFF
    val r = data(paletteOffset.toInt + 2) & 0xFF
    Some((r, g, b))

  def loadFromBytes(data: Array[Byte], applyColorKey: Boolean = true): Int =
    val colorKey = if applyColorKey then extractBmpColorKey(data) else None

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

      // BMP files have no alpha channel — STB fills alpha=255 for all pixels.
      // EQ uses palette entry 0 as the color key for masked/transparent textures.
      // For 8-bit BMPs we use the actual palette[0] color; fall back to near-black.
      val width = w.get(0)
      val height = h.get(0)
      val pixelCount = width * height
      colorKey match
        case Some((kr, kg, kb)) =>
          // Mask pixels matching the palette color key (with small tolerance)
          for i <- 0 until pixelCount do
            val offset = i * 4
            val r = pixels.get(offset) & 0xFF
            val g = pixels.get(offset + 1) & 0xFF
            val b = pixels.get(offset + 2) & 0xFF
            if math.abs(r - kr) + math.abs(g - kg) + math.abs(b - kb) < 10 then
              pixels.put(offset + 3, 0.toByte)
        case None =>
          // Non-paletted: fall back to near-black masking (only if color keying enabled)
          if applyColorKey then
            for i <- 0 until pixelCount do
              val offset = i * 4
              val r = pixels.get(offset) & 0xFF
              val g = pixels.get(offset + 1) & 0xFF
              val b = pixels.get(offset + 2) & 0xFF
              if r + g + b < 10 then
                pixels.put(offset + 3, 0.toByte)

      // Bleed opaque edge colors into transparent pixels to prevent dark halos
      // from bilinear filtering at color-key boundaries.
      for i <- 0 until pixelCount do
        val offset = i * 4
        if (pixels.get(offset + 3) & 0xFF) == 0 then
          val x = i % width; val y = i / width
          var sr = 0; var sg = 0; var sb = 0; var count = 0
          for (dx, dy) <- Array((-1, 0), (1, 0), (0, -1), (0, 1)) do
            val nx = x + dx; val ny = y + dy
            if nx >= 0 && nx < width && ny >= 0 && ny < height then
              val nOff = (ny * width + nx) * 4
              if (pixels.get(nOff + 3) & 0xFF) > 0 then
                sr += pixels.get(nOff) & 0xFF
                sg += pixels.get(nOff + 1) & 0xFF
                sb += pixels.get(nOff + 2) & 0xFF
                count += 1
          if count > 0 then
            pixels.put(offset, (sr / count).toByte)
            pixels.put(offset + 1, (sg / count).toByte)
            pixels.put(offset + 2, (sb / count).toByte)

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
