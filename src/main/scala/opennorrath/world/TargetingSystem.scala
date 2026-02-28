package opennorrath.world

import opennorrath.render.Shader
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.{glEnableVertexAttribArray, glVertexAttrib3f, glVertexAttribPointer}
import org.lwjgl.opengl.GL30.{glBindVertexArray, glDeleteVertexArrays, glGenVertexArrays}

/** Ray-pick spawns and draw wireframe hitboxes around targets. */
class TargetingSystem:

  private var hitboxVao = 0
  private var hitboxVbo = 0
  private var hitboxInited = false
  private val hitboxBuf = BufferUtils.createFloatBuffer(24 * 3)

  /** Cast a ray from screen coordinates through the scene and return the closest hit spawn ID. */
  def pickSpawn(mx: Float, my: Float, screenW: Float, screenH: Float,
                projection: Matrix4f, viewMatrix: Matrix4f,
                hitData: Iterable[(Int, Matrix4f, Float, Float, Float)]): Option[Int] =
    val projView = Matrix4f(projection).mul(viewMatrix)
    val rayOrigin = Vector3f()
    val rayDir = Vector3f()
    projView.unprojectRay(mx, screenH - my, Array(0, 0, screenW.toInt, screenH.toInt), rayOrigin, rayDir)

    var bestId: Option[Int] = None
    var bestDist = Float.MaxValue

    for (spawnId, modelMatrix, height, width, depth) <- hitData do
      val cx = modelMatrix.m30()
      val cy = modelMatrix.m31()
      val cz = modelMatrix.m32()
      val hw = Math.max(width, depth) * 0.6f
      val minX = cx - hw; val maxX = cx + hw
      // AABB spans from model origin (feet) upward to head, NOT centered on origin.
      // Centering on origin (cy ± h/2) puts half the hitbox below the feet — underground
      // for ground characters, empty air for flying ones. This is especially bad for
      // flying creatures: the server sends them at elevated positions (flyMode is often 0),
      // so the model origin is already in the air. A centered hitbox extends into the gap
      // below the model, making the upper body/head unclickable.
      val minY = cy; val maxY = cy + height
      val minZ = cz - hw; val maxZ = cz + hw

      rayAABBIntersect(rayOrigin, rayDir, minX, minY, minZ, maxX, maxY, maxZ) match
        case Some(t) if t < bestDist =>
          bestDist = t
          bestId = Some(spawnId)
        case _ =>

    bestId

  /** Draw a faint wireframe AABB around the targeted spawn. */
  def drawTargetHitbox(spawnId: Int, shader: Shader, identity: Matrix4f,
                       hitData: Iterable[(Int, Matrix4f, Float, Float, Float)]): Unit =
    hitData.find(_._1 == spawnId).foreach { (_, modelMatrix, height, width, depth) =>
      val cx = modelMatrix.m30()
      val cy = modelMatrix.m31()
      val cz = modelMatrix.m32()
      val hw = Math.max(width, depth) * 0.4f

      val x0 = cx - hw; val x1 = cx + hw
      val y0 = cy; val y1 = cy + height  // feet to head (see pickSpawn comment)
      val z0 = cz - hw;  val z1 = cz + hw

      if !hitboxInited then
        hitboxVao = glGenVertexArrays()
        hitboxVbo = glGenBuffers()
        glBindVertexArray(hitboxVao)
        glBindBuffer(GL_ARRAY_BUFFER, hitboxVbo)
        glBufferData(GL_ARRAY_BUFFER, 24L * 3 * 4, GL_DYNAMIC_DRAW)
        glVertexAttribPointer(0, 3, GL_FLOAT, false, 3 * 4, 0)
        glEnableVertexAttribArray(0)
        glBindVertexArray(0)
        hitboxInited = true

      hitboxBuf.clear()
      // Bottom face
      hitboxBuf.put(x0).put(y0).put(z0); hitboxBuf.put(x1).put(y0).put(z0)
      hitboxBuf.put(x1).put(y0).put(z0); hitboxBuf.put(x1).put(y0).put(z1)
      hitboxBuf.put(x1).put(y0).put(z1); hitboxBuf.put(x0).put(y0).put(z1)
      hitboxBuf.put(x0).put(y0).put(z1); hitboxBuf.put(x0).put(y0).put(z0)
      // Top face
      hitboxBuf.put(x0).put(y1).put(z0); hitboxBuf.put(x1).put(y1).put(z0)
      hitboxBuf.put(x1).put(y1).put(z0); hitboxBuf.put(x1).put(y1).put(z1)
      hitboxBuf.put(x1).put(y1).put(z1); hitboxBuf.put(x0).put(y1).put(z1)
      hitboxBuf.put(x0).put(y1).put(z1); hitboxBuf.put(x0).put(y1).put(z0)
      // Vertical edges
      hitboxBuf.put(x0).put(y0).put(z0); hitboxBuf.put(x0).put(y1).put(z0)
      hitboxBuf.put(x1).put(y0).put(z0); hitboxBuf.put(x1).put(y1).put(z0)
      hitboxBuf.put(x1).put(y0).put(z1); hitboxBuf.put(x1).put(y1).put(z1)
      hitboxBuf.put(x0).put(y0).put(z1); hitboxBuf.put(x0).put(y1).put(z1)
      hitboxBuf.flip()

      glBindBuffer(GL_ARRAY_BUFFER, hitboxVbo)
      glBufferSubData(GL_ARRAY_BUFFER, 0, hitboxBuf)

      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glDisable(GL_DEPTH_TEST)
      shader.setMatrix4f("model", identity)
      glVertexAttrib3f(2, 0.4f, 0.8f, 1f)
      shader.setFloat("alphaMultiplier", 0.3f)

      glBindVertexArray(hitboxVao)
      glDrawArrays(GL_LINES, 0, 24)
      glBindVertexArray(0)

      shader.setFloat("alphaMultiplier", 1.0f)
      glEnable(GL_DEPTH_TEST)
      glDisable(GL_BLEND)
    }

  // --- Debug sphere for last confirmed server position ---

  private var sphereVao = 0
  private var sphereVbo = 0
  private var sphereInited = false
  private val Segments = 16
  // 3 rings × Segments line segments × 2 endpoints = 3 * Segments * 2 vertices
  private val SphereVerts = 3 * Segments * 2
  private val sphereBuf = BufferUtils.createFloatBuffer(SphereVerts * 3)

  /** Draw a small wireframe sphere at the given world position (e.g., last server pos). */
  def drawServerPosSphere(pos: Vector3f, shader: Shader, identity: Matrix4f): Unit =
    if !sphereInited then
      sphereVao = glGenVertexArrays()
      sphereVbo = glGenBuffers()
      glBindVertexArray(sphereVao)
      glBindBuffer(GL_ARRAY_BUFFER, sphereVbo)
      glBufferData(GL_ARRAY_BUFFER, SphereVerts.toLong * 3 * 4, GL_DYNAMIC_DRAW)
      glVertexAttribPointer(0, 3, GL_FLOAT, false, 3 * 4, 0)
      glEnableVertexAttribArray(0)
      glBindVertexArray(0)
      sphereInited = true

    val cx = pos.x; val cy = pos.y; val cz = pos.z
    val r = 0.5f

    sphereBuf.clear()
    for i <- 0 until Segments do
      val a0 = (2 * Math.PI * i / Segments).toFloat
      val a1 = (2 * Math.PI * ((i + 1) % Segments) / Segments).toFloat
      val c0 = Math.cos(a0).toFloat; val s0 = Math.sin(a0).toFloat
      val c1 = Math.cos(a1).toFloat; val s1 = Math.sin(a1).toFloat
      // XZ ring (horizontal)
      sphereBuf.put(cx + r * c0).put(cy).put(cz + r * s0)
      sphereBuf.put(cx + r * c1).put(cy).put(cz + r * s1)
      // XY ring (vertical, facing camera-ish)
      sphereBuf.put(cx + r * c0).put(cy + r * s0).put(cz)
      sphereBuf.put(cx + r * c1).put(cy + r * s1).put(cz)
      // YZ ring (vertical, side)
      sphereBuf.put(cx).put(cy + r * c0).put(cz + r * s0)
      sphereBuf.put(cx).put(cy + r * c1).put(cz + r * s1)
    sphereBuf.flip()

    glBindBuffer(GL_ARRAY_BUFFER, sphereVbo)
    glBufferSubData(GL_ARRAY_BUFFER, 0, sphereBuf)

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_DEPTH_TEST)
    shader.setMatrix4f("model", identity)
    glVertexAttrib3f(2, 1f, 0.2f, 0.2f) // red
    shader.setFloat("alphaMultiplier", 0.6f)

    glBindVertexArray(sphereVao)
    glDrawArrays(GL_LINES, 0, SphereVerts)
    glBindVertexArray(0)

    shader.setFloat("alphaMultiplier", 1.0f)
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)

  def cleanup(): Unit =
    if hitboxInited then
      glDeleteBuffers(hitboxVbo)
      glDeleteVertexArrays(hitboxVao)
    if sphereInited then
      glDeleteBuffers(sphereVbo)
      glDeleteVertexArrays(sphereVao)

  private def rayAABBIntersect(origin: Vector3f, dir: Vector3f,
                               minX: Float, minY: Float, minZ: Float,
                               maxX: Float, maxY: Float, maxZ: Float): Option[Float] =
    var tmin = Float.MinValue
    var tmax = Float.MaxValue

    if Math.abs(dir.x) > 1e-6f then
      val t1 = (minX - origin.x) / dir.x
      val t2 = (maxX - origin.x) / dir.x
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.x < minX || origin.x > maxX then return None

    if Math.abs(dir.y) > 1e-6f then
      val t1 = (minY - origin.y) / dir.y
      val t2 = (maxY - origin.y) / dir.y
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.y < minY || origin.y > maxY then return None

    if Math.abs(dir.z) > 1e-6f then
      val t1 = (minZ - origin.z) / dir.z
      val t2 = (maxZ - origin.z) / dir.z
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.z < minZ || origin.z > maxZ then return None

    if tmin <= tmax && tmax >= 0f then Some(Math.max(tmin, 0f))
    else None
