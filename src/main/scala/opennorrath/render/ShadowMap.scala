package opennorrath.render

import org.joml.{Matrix4f, Vector3f, Vector4f}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL14.*
import org.lwjgl.opengl.GL30.*

/** Directional shadow map using a 2D depth texture rendered from the sun's perspective.
  *
  * The orthographic frustum is sized to cover the zone AABB from the light's viewpoint.
  * Uses GL_COMPARE_REF_TO_TEXTURE so the shader can use sampler2DShadow for hardware PCF.
  *
  * Usage each frame:
  *   1. shadowMap.bind()          — sets FBO + viewport + clears depth
  *   2. render geometry with depth-only shadow shader
  *   3. shadowMap.unbind(w, h)    — restores default FBO + viewport
  *   4. bind depthTexture to a texture unit and set lightSpaceMatrix on the main shader
  */
class ShadowMap(sceneMin: Vector3f, sceneMax: Vector3f, lightDir: Vector3f, resolution: Int = 2048):

  val depthTexture: Int = glGenTextures()
  glBindTexture(GL_TEXTURE_2D, depthTexture)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, resolution, resolution, 0,
    GL_DEPTH_COMPONENT, GL_FLOAT, 0L)
  // LINEAR filtering enables hardware 2×2 PCF on most drivers
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  // Fragments outside the shadow frustum sample border = 1.0 → not in shadow
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER)
  locally {
    val border = BufferUtils.createFloatBuffer(4)
    border.put(1f).put(1f).put(1f).put(1f).flip()
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, border)
  }
  // Enable depth comparison mode for sampler2DShadow in GLSL
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL)
  glBindTexture(GL_TEXTURE_2D, 0)

  val fbo: Int = glGenFramebuffers()
  glBindFramebuffer(GL_FRAMEBUFFER, fbo)
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, depthTexture, 0)
  // No color attachment — depth-only pass
  glDrawBuffer(GL_NONE)
  glReadBuffer(GL_NONE)
  private val fbStatus = glCheckFramebufferStatus(GL_FRAMEBUFFER)
  if fbStatus != GL_FRAMEBUFFER_COMPLETE then
    throw RuntimeException(s"Shadow map FBO incomplete: 0x${fbStatus.toHexString}")
  glBindFramebuffer(GL_FRAMEBUFFER, 0)

  /** Orthographic light-space matrix covering the scene AABB from lightDir. */
  val lightSpaceMatrix: Matrix4f = computeLightSpaceMatrix(sceneMin, sceneMax, lightDir)

  /** Compute an orthographic light-space matrix that tightly covers the scene AABB.
    * lightDir should point FROM the sun TOWARD the scene (the direction light travels).
    */
  private def computeLightSpaceMatrix(sMin: Vector3f, sMax: Vector3f, ld: Vector3f): Matrix4f =
    val center = Vector3f(
      (sMin.x + sMax.x) * 0.5f,
      (sMin.y + sMax.y) * 0.5f,
      (sMin.z + sMax.z) * 0.5f,
    )
    val dx = sMax.x - sMin.x; val dy = sMax.y - sMin.y; val dz = sMax.z - sMin.z
    val diag = math.sqrt((dx * dx + dy * dy + dz * dz).toDouble).toFloat

    // Place the light "eye" behind the scene along the reversed light direction
    val dir = Vector3f(ld).normalize()
    val eye = Vector3f(center.x - dir.x * diag, center.y - dir.y * diag, center.z - dir.z * diag)

    // Choose up vector not parallel to light direction
    val up = if math.abs(dir.y) > 0.99f then Vector3f(1f, 0f, 0f) else Vector3f(0f, 1f, 0f)

    val lightView = Matrix4f().lookAt(eye, center, up)

    // Project all 8 AABB corners into light view space for tight ortho bounds
    val corners = Array(
      Vector4f(sMin.x, sMin.y, sMin.z, 1f), Vector4f(sMax.x, sMin.y, sMin.z, 1f),
      Vector4f(sMin.x, sMax.y, sMin.z, 1f), Vector4f(sMax.x, sMax.y, sMin.z, 1f),
      Vector4f(sMin.x, sMin.y, sMax.z, 1f), Vector4f(sMax.x, sMin.y, sMax.z, 1f),
      Vector4f(sMin.x, sMax.y, sMax.z, 1f), Vector4f(sMax.x, sMax.y, sMax.z, 1f),
    )
    var lMin = Float.MaxValue; var lMax = -Float.MaxValue
    var uMin = Float.MaxValue; var uMax = -Float.MaxValue
    var nMin = Float.MaxValue; var nMax = -Float.MaxValue
    for c <- corners do
      lightView.transform(c)
      if c.x < lMin then lMin = c.x; if c.x > lMax then lMax = c.x
      if c.y < uMin then uMin = c.y; if c.y > uMax then uMax = c.y
      if c.z < nMin then nMin = c.z; if c.z > nMax then nMax = c.z

    // Padding to avoid geometry clipping at edges
    val pad = diag * 0.05f
    val lightProj = Matrix4f().ortho(lMin - pad, lMax + pad, uMin - pad, uMax + pad, nMin - pad, nMax + pad)
    Matrix4f(lightProj).mul(lightView)

  def bind(): Unit =
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    glViewport(0, 0, resolution, resolution)
    glDepthMask(true)  // required for glClear to work if caller left it false
    glClear(GL_DEPTH_BUFFER_BIT)

  def unbind(windowWidth: Int, windowHeight: Int): Unit =
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(0, 0, windowWidth, windowHeight)

  def cleanup(): Unit =
    glDeleteTextures(depthTexture)
    glDeleteFramebuffers(fbo)
