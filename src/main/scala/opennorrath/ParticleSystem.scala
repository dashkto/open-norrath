package opennorrath

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import java.nio.file.{Path, Files}
import scala.util.Random

case class Emitter(position: Vector3f, scale: Float)

class ParticleSystem(emitters: List[Emitter]):

  private val MaxParticles = 200
  private val SpawnRate = 15f // per emitter per second
  private val rng = Random()

  private case class Particle(
    var x: Float, var y: Float, var z: Float,
    var vx: Float, var vy: Float, var vz: Float,
    var life: Float, var maxLife: Float, var baseSize: Float,
    var alive: Boolean,
  )

  private val particles = Array.fill(MaxParticles)(
    Particle(0, 0, 0, 0, 0, 0, 0, 1, 5, alive = false)
  )
  private var spawnAccum = 0f

  // Pre-allocated vertex/index buffers: each particle = 4 verts (stride 8) + 6 indices
  private val vertexData = new Array[Float](MaxParticles * 4 * 8)
  private val indexData: Array[Int] = {
    val arr = new Array[Int](MaxParticles * 6)
    for i <- 0 until MaxParticles do
      val base = i * 4
      val idx = i * 6
      arr(idx + 0) = base + 0; arr(idx + 1) = base + 1; arr(idx + 2) = base + 2
      arr(idx + 3) = base + 0; arr(idx + 4) = base + 2; arr(idx + 5) = base + 3
    arr
  }

  private val texture = Texture.createSoftCircle()
  private val mesh = Mesh(vertexData, indexData, dynamic = true, stride = 8)
  private var activeCount = 0

  def update(deltaTime: Float, viewMatrix: Matrix4f): Unit =
    // Age and kill
    for p <- particles if p.alive do
      p.life -= deltaTime
      if p.life <= 0 then p.alive = false
      else
        p.x += p.vx * deltaTime
        p.y += p.vy * deltaTime
        p.z += p.vz * deltaTime
        p.vy += 5f * deltaTime
        p.vx += (rng.nextFloat() - 0.5f) * 8f * deltaTime
        p.vz += (rng.nextFloat() - 0.5f) * 8f * deltaTime

    // Spawn new particles
    spawnAccum += deltaTime * SpawnRate * emitters.size
    while spawnAccum >= 1f do
      spawnAccum -= 1f
      spawnOne()

    // Build billboard vertices
    buildVertices(viewMatrix)

  private def spawnOne(): Unit =
    val slot = particles.indexWhere(!_.alive)
    if slot < 0 then return
    val emitter = emitters(rng.nextInt(emitters.size))
    val s = emitter.scale
    val p = particles(slot)
    p.x = emitter.position.x + (rng.nextFloat() - 0.5f) * 3f * s
    p.y = emitter.position.y + (rng.nextFloat() - 0.5f) * 1f * s
    p.z = emitter.position.z + (rng.nextFloat() - 0.5f) * 3f * s
    p.vx = (rng.nextFloat() - 0.5f) * 3f * s
    p.vy = (10f + rng.nextFloat() * 8f) * s
    p.vz = (rng.nextFloat() - 0.5f) * 3f * s
    val life = 0.6f + rng.nextFloat() * 0.5f
    p.life = life
    p.maxLife = life
    p.baseSize = (3f + rng.nextFloat() * 2f) * s
    p.alive = true

  private def buildVertices(viewMatrix: Matrix4f): Unit =
    val right = Vector3f(viewMatrix.m00(), viewMatrix.m10(), viewMatrix.m20())
    val up = Vector3f(viewMatrix.m01(), viewMatrix.m11(), viewMatrix.m21())

    activeCount = 0
    for p <- particles if p.alive do
      val t = 1f - (p.life / p.maxLife).min(1f) // 0 = young, 1 = old
      // Color: bright yellow → orange → dark red
      val r = if t < 0.5f then 1.0f else 1.0f - (t - 0.5f) * 1.4f
      val g = if t < 0.3f then 0.9f - t * 1.5f else 0.4f - (t - 0.3f) * 0.5f
      val b = if t < 0.2f then 0.5f - t * 2f else 0.1f * (1f - t)
      val size = p.baseSize * (1f + t * 0.3f) // grow slightly over life

      val cx = p.x; val cy = p.y; val cz = p.z
      val rx = right.x * size; val ry = right.y * size; val rz = right.z * size
      val ux = up.x * size; val uy = up.y * size; val uz = up.z * size

      val base = activeCount * 4 * 8
      // Bottom-left
      vertexData(base + 0) = cx - rx - ux; vertexData(base + 1) = cy - ry - uy; vertexData(base + 2) = cz - rz - uz
      vertexData(base + 3) = 0f; vertexData(base + 4) = 0f
      vertexData(base + 5) = r.max(0f); vertexData(base + 6) = g.max(0f); vertexData(base + 7) = b.max(0f)
      // Bottom-right
      vertexData(base + 8) = cx + rx - ux; vertexData(base + 9) = cy + ry - uy; vertexData(base + 10) = cz + rz - uz
      vertexData(base + 11) = 1f; vertexData(base + 12) = 0f
      vertexData(base + 13) = r.max(0f); vertexData(base + 14) = g.max(0f); vertexData(base + 15) = b.max(0f)
      // Top-right
      vertexData(base + 16) = cx + rx + ux; vertexData(base + 17) = cy + ry + uy; vertexData(base + 18) = cz + rz + uz
      vertexData(base + 19) = 1f; vertexData(base + 20) = 1f
      vertexData(base + 21) = r.max(0f); vertexData(base + 22) = g.max(0f); vertexData(base + 23) = b.max(0f)
      // Top-left
      vertexData(base + 24) = cx - rx + ux; vertexData(base + 25) = cy - ry + uy; vertexData(base + 26) = cz - rz + uz
      vertexData(base + 27) = 0f; vertexData(base + 28) = 1f
      vertexData(base + 29) = r.max(0f); vertexData(base + 30) = g.max(0f); vertexData(base + 31) = b.max(0f)

      activeCount += 1

    mesh.updateVertices(vertexData)

  def draw(shader: Shader): Unit =
    if activeCount == 0 then return

    val identity = Matrix4f()
    shader.setMatrix4f("model", identity)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE) // additive blending
    glDepthMask(false)

    glBindTexture(GL_TEXTURE_2D, texture)
    mesh.drawRange(0, activeCount * 6)

    glDepthMask(true)
    glDisable(GL_BLEND)

  def cleanup(): Unit =
    mesh.cleanup()
    glDeleteTextures(texture)

object ParticleSystem:

  def parseEmitters(path: String): List[Emitter] =
    if !java.nio.file.Files.exists(java.nio.file.Path.of(path)) then return Nil
    val source = scala.io.Source.fromFile(path)
    try
      source.getLines().drop(1).flatMap { line =>
        val parts = line.split("\\^")
        if parts.length >= 5 then
          try
            val eqX = parts(2).toFloat
            val eqY = parts(3).toFloat
            val eqZ = parts(4).toFloat
            Some(Emitter(org.joml.Vector3f(eqX, eqZ, -eqY), 1f))
          catch case _: NumberFormatException => None
        else None
      }.toList
    finally source.close()
