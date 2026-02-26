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
      val minY = cy - height * 0.5f; val maxY = cy + height * 0.5f
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
      val y0 = cy - height * 0.5f; val y1 = cy + height * 0.5f
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

  def cleanup(): Unit =
    if hitboxInited then
      glDeleteBuffers(hitboxVbo)
      glDeleteVertexArrays(hitboxVao)

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
