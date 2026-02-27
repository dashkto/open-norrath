package opennorrath.world

import org.joml.Vector3f
import opennorrath.wld.ZoneMesh

/** CPU-side zone collision mesh for ray-casting line-of-sight checks.
  * Precomputes GL-space vertices at construction time.
  */
class ZoneCollision(zoneMesh: ZoneMesh):

  private val vc = zoneMesh.vertices.length / 3
  private val glVerts: Array[Float] = {
    val arr = new Array[Float](vc * 3)
    var i = 0
    while i < vc do
      val (gx, gy, gz) = EqCoords.s3dToGl(
        zoneMesh.vertices(i * 3), zoneMesh.vertices(i * 3 + 1), zoneMesh.vertices(i * 3 + 2))
      arr(i * 3) = gx
      arr(i * 3 + 1) = gy
      arr(i * 3 + 2) = gz
      i += 1
    arr
  }

  private val indices = zoneMesh.indices
  private val triCount = indices.length / 3

  /** Returns true if zone geometry blocks the line of sight from origin to target. */
  def rayBlocked(origin: Vector3f, target: Vector3f): Boolean =
    val dx = target.x - origin.x
    val dy = target.y - origin.y
    val dz = target.z - origin.z
    val maxDistSq = dx * dx + dy * dy + dz * dz
    if maxDistSq < 0.001f then return false
    val maxDist = Math.sqrt(maxDistSq.toDouble).toFloat
    val invDist = 1f / maxDist
    val dirX = dx * invDist
    val dirY = dy * invDist
    val dirZ = dz * invDist

    // Margins: skip hits near camera (0.1) or near target (0.5) to avoid
    // the floor under the target or geometry touching the camera from blocking.
    val tMin = 0.1f
    val tMax = maxDist - 0.5f
    if tMax <= tMin then return false

    var i = 0
    while i < triCount do
      val idx = i * 3
      val i0 = indices(idx) * 3
      val i1 = indices(idx + 1) * 3
      val i2 = indices(idx + 2) * 3
      val t = rayTriIntersect(
        origin.x, origin.y, origin.z, dirX, dirY, dirZ,
        glVerts(i0), glVerts(i0 + 1), glVerts(i0 + 2),
        glVerts(i1), glVerts(i1 + 1), glVerts(i1 + 2),
        glVerts(i2), glVerts(i2 + 1), glVerts(i2 + 2),
      )
      if t > tMin && t < tMax then return true
      i += 1
    false

  /** Cast a ray from origin toward target. Returns the closest hit distance (0..maxDist), or -1 if clear. */
  def raycast(origin: Vector3f, target: Vector3f): Float =
    val dx = target.x - origin.x
    val dy = target.y - origin.y
    val dz = target.z - origin.z
    val maxDistSq = dx * dx + dy * dy + dz * dz
    if maxDistSq < 0.001f then return -1f
    val maxDist = Math.sqrt(maxDistSq.toDouble).toFloat
    val invDist = 1f / maxDist
    val dirX = dx * invDist
    val dirY = dy * invDist
    val dirZ = dz * invDist
    var closest = -1f
    var i = 0
    while i < triCount do
      val idx = i * 3
      val i0 = indices(idx) * 3
      val i1 = indices(idx + 1) * 3
      val i2 = indices(idx + 2) * 3
      val t = rayTriIntersect(
        origin.x, origin.y, origin.z, dirX, dirY, dirZ,
        glVerts(i0), glVerts(i0 + 1), glVerts(i0 + 2),
        glVerts(i1), glVerts(i1 + 1), glVerts(i1 + 2),
        glVerts(i2), glVerts(i2 + 1), glVerts(i2 + 2),
      )
      if t > 0.1f && t < maxDist && (closest < 0f || t < closest) then
        closest = t
      i += 1
    closest

  /** Cast a ray straight down and return the closest hit distance, or -1 if no hit within maxDist. */
  def raycastDown(origin: Vector3f, maxDist: Float): Float =
    var closest = -1f
    var i = 0
    while i < triCount do
      val idx = i * 3
      val i0 = indices(idx) * 3
      val i1 = indices(idx + 1) * 3
      val i2 = indices(idx + 2) * 3
      val t = rayTriIntersect(
        origin.x, origin.y, origin.z, 0f, -1f, 0f,
        glVerts(i0), glVerts(i0 + 1), glVerts(i0 + 2),
        glVerts(i1), glVerts(i1 + 1), glVerts(i1 + 2),
        glVerts(i2), glVerts(i2 + 1), glVerts(i2 + 2),
      )
      if t > 0f && t <= maxDist && (closest < 0f || t < closest) then
        closest = t
      i += 1
    closest

  /** Möller-Trumbore ray-triangle intersection. Returns t on hit, -1 on miss. */
  private def rayTriIntersect(
    ox: Float, oy: Float, oz: Float,
    dx: Float, dy: Float, dz: Float,
    v0x: Float, v0y: Float, v0z: Float,
    v1x: Float, v1y: Float, v1z: Float,
    v2x: Float, v2y: Float, v2z: Float,
  ): Float =
    val e1x = v1x - v0x; val e1y = v1y - v0y; val e1z = v1z - v0z
    val e2x = v2x - v0x; val e2y = v2y - v0y; val e2z = v2z - v0z
    val hx = dy * e2z - dz * e2y
    val hy = dz * e2x - dx * e2z
    val hz = dx * e2y - dy * e2x
    val a = e1x * hx + e1y * hy + e1z * hz
    if a > -1e-6f && a < 1e-6f then return -1f
    val f = 1f / a
    val sx = ox - v0x; val sy = oy - v0y; val sz = oz - v0z
    val u = f * (sx * hx + sy * hy + sz * hz)
    if u < 0f || u > 1f then return -1f
    val qx = sy * e1z - sz * e1y
    val qy = sz * e1x - sx * e1z
    val qz = sx * e1y - sy * e1x
    val v = f * (dx * qx + dy * qy + dz * qz)
    if v < 0f || u + v > 1f then return -1f
    val t = f * (e2x * qx + e2y * qy + e2z * qz)
    if t > 1e-4f then t else -1f
