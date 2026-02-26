package opennorrath.world

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*

import opennorrath.render.{Mesh, Shader, Texture}
import opennorrath.state.ZoneCharacter
import opennorrath.ui.SpellData

import scala.util.Random

/** Manages temporary spell particle effects triggered by SpellActionTriggered events.
  * Each spell creates an ActiveEffect that spawns colored billboard particles
  * at the target's position for a short duration.
  */
class SpellEffectSystem:

  private val MaxParticles = 500
  private val rng = Random()

  // --- Effect presets by keyword ---

  private case class EffectPreset(
    r1: Float, g1: Float, b1: Float, // start color
    r2: Float, g2: Float, b2: Float, // end color
    particleCount: Int,
    spawnRate: Float,
    lifetime: Float,       // per-particle lifetime
    effectDuration: Float, // how long the effect spawns particles
    size: Float,
    velocityY: Float,
    spread: Float,
  )

  //                                       r1    g1    b1    r2    g2    b2   cnt  rate  life  dur   size  velY  spread
  private val presetHeal   = EffectPreset(1.0f, 1.0f, 1.0f, 0.4f, 0.6f, 1.0f, 25, 22f, 1.0f, 1.5f, 1.2f, 6f,  1.5f)
  private val presetFire   = EffectPreset(1.0f, 0.7f, 0.1f, 0.8f, 0.1f, 0.0f, 20, 18f, 0.8f, 1.2f, 1.0f, 5f,  1.2f)
  private val presetIce    = EffectPreset(0.7f, 0.9f, 1.0f, 0.3f, 0.5f, 0.9f, 20, 18f, 0.8f, 1.2f, 1.0f, 4f,  1.5f)
  private val presetFear   = EffectPreset(0.6f, 0.1f, 0.8f, 0.2f, 0.0f, 0.3f, 15, 14f, 1.0f, 1.5f, 1.2f, 3f,  2.0f)
  private val presetPoison = EffectPreset(0.2f, 0.9f, 0.1f, 0.1f, 0.4f, 0.0f, 15, 14f, 1.0f, 1.5f, 1.0f, 3f,  1.5f)
  private val presetBuff   = EffectPreset(1.0f, 0.9f, 0.3f, 0.8f, 0.7f, 0.1f, 15, 14f, 0.8f, 1.2f, 1.0f, 5f,  1.5f)
  private val presetForce  = EffectPreset(1.0f, 1.0f, 1.0f, 0.8f, 0.8f, 1.0f, 25, 22f, 0.6f, 1.0f, 0.8f, 7f,  1.0f)
  private val presetDefault = EffectPreset(0.9f, 0.9f, 1.0f, 0.5f, 0.5f, 0.7f, 15, 14f, 0.8f, 1.2f, 1.0f, 4f, 1.5f)

  private def presetForEffect(effectName: String): EffectPreset =
    val name = effectName.toLowerCase
    if name.contains("heal") || name.contains("resto") || name.contains("cure") then presetHeal
    else if name.contains("fire") || name.contains("flame") || name.contains("burn") || name.contains("immolat") then presetFire
    else if name.contains("ice") || name.contains("frost") || name.contains("chill") || name.contains("hail") then presetIce
    else if name.contains("fear") || name.contains("darkness") || name.contains("dead") || name.contains("lich") then presetFear
    else if name.contains("poison") || name.contains("disease") || name.contains("crud") || name.contains("slime") || name.contains("venom") then presetPoison
    else if name.contains("haste") || name.contains("clarity") || name.contains("breeze") || name.contains("rune") || name.contains("shield") then presetBuff
    else if name.contains("force") || name.contains("blast") || name.contains("lightning") || name.contains("shock") then presetForce
    else presetDefault

  // --- Active effects ---

  private case class ActiveEffect(
    spawnId: Int,
    preset: EffectPreset,
    var age: Float,
    var spawnAccum: Float,
  )

  private val activeEffects = scala.collection.mutable.ArrayBuffer[ActiveEffect]()

  // --- Particles ---

  private case class Particle(
    var x: Float, var y: Float, var z: Float,
    var vx: Float, var vy: Float, var vz: Float,
    var life: Float, var maxLife: Float, var baseSize: Float,
    var r1: Float, var g1: Float, var b1: Float,
    var r2: Float, var g2: Float, var b2: Float,
    var alive: Boolean,
  )

  private val particles = Array.fill(MaxParticles)(
    Particle(0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 1, 1, alive = false)
  )

  private val vertexData = new Array[Float](MaxParticles * 4 * 8)
  private val indexData: Array[Int] = {
    val arr = new Array[Int](MaxParticles * 6)
    for i <- 0 until MaxParticles do
      val base = i * 4
      val idx = i * 6
      arr(idx + 0) = base; arr(idx + 1) = base + 1; arr(idx + 2) = base + 2
      arr(idx + 3) = base; arr(idx + 4) = base + 2; arr(idx + 5) = base + 3
    arr
  }

  private val texture = Texture.createSoftCircle()
  private val mesh = Mesh(vertexData, indexData, dynamic = true, stride = 8)
  private var activeCount = 0

  // --- Public API ---

  def trigger(spawnId: Int, spellId: Int): Unit =
    val animIdx = SpellData.spellAnim(spellId)
    val effName = SpellData.effectName(animIdx)
    val preset = presetForEffect(effName)
    activeEffects += ActiveEffect(spawnId, preset, age = 0f, spawnAccum = 0f)

  def update(dt: Float, viewMatrix: Matrix4f, characters: scala.collection.Map[Int, ZoneCharacter]): Unit =
    // Age and kill particles
    for p <- particles if p.alive do
      p.life -= dt
      if p.life <= 0 then p.alive = false
      else
        p.x += p.vx * dt
        p.y += p.vy * dt
        p.z += p.vz * dt
        p.vx += (rng.nextFloat() - 0.5f) * 4f * dt
        p.vz += (rng.nextFloat() - 0.5f) * 4f * dt

    // Update active effects — spawn particles at target positions
    val toRemove = scala.collection.mutable.ArrayBuffer[Int]()
    for (eff, i) <- activeEffects.zipWithIndex do
      eff.age += dt
      if eff.age >= eff.preset.effectDuration then
        toRemove += i
      else
        characters.get(eff.spawnId).foreach { zc =>
          eff.spawnAccum += dt * eff.preset.spawnRate
          while eff.spawnAccum >= 1f do
            eff.spawnAccum -= 1f
            spawnParticle(zc.position, eff.preset)
        }

    // Remove expired effects (iterate in reverse)
    for i <- toRemove.reverseIterator do
      activeEffects.remove(i)

    buildVertices(viewMatrix)

  def draw(shader: Shader): Unit =
    if activeCount == 0 then return
    val identity = Matrix4f()
    shader.setMatrix4f("model", identity)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE) // additive
    glDepthMask(false)
    glBindTexture(GL_TEXTURE_2D, texture)
    mesh.drawRange(0, activeCount * 6)
    glDepthMask(true)
    glDisable(GL_BLEND)

  def cleanup(): Unit =
    mesh.cleanup()
    glDeleteTextures(texture)

  // --- Internals ---

  private def spawnParticle(pos: Vector3f, preset: EffectPreset): Unit =
    val slot = particles.indexWhere(!_.alive)
    if slot < 0 then return
    val p = particles(slot)
    val spread = preset.spread
    p.x = pos.x + (rng.nextFloat() - 0.5f) * spread
    p.y = pos.y + rng.nextFloat() * 2f
    p.z = pos.z + (rng.nextFloat() - 0.5f) * spread
    p.vx = (rng.nextFloat() - 0.5f) * 1f
    p.vy = preset.velocityY * (0.7f + rng.nextFloat() * 0.6f)
    p.vz = (rng.nextFloat() - 0.5f) * 1f
    val life = preset.lifetime * (0.7f + rng.nextFloat() * 0.6f)
    p.life = life
    p.maxLife = life
    p.baseSize = preset.size * (0.7f + rng.nextFloat() * 0.6f)
    p.r1 = preset.r1; p.g1 = preset.g1; p.b1 = preset.b1
    p.r2 = preset.r2; p.g2 = preset.g2; p.b2 = preset.b2
    p.alive = true

  private def buildVertices(viewMatrix: Matrix4f): Unit =
    val right = Vector3f(viewMatrix.m00(), viewMatrix.m10(), viewMatrix.m20())
    val up = Vector3f(viewMatrix.m01(), viewMatrix.m11(), viewMatrix.m21())

    activeCount = 0
    for p <- particles if p.alive do
      val t = 1f - (p.life / p.maxLife).min(1f)
      val r = lerp(p.r1, p.r2, t)
      val g = lerp(p.g1, p.g2, t)
      val b = lerp(p.b1, p.b2, t)
      val alpha = if t < 0.2f then t / 0.2f else if t > 0.7f then (1f - t) / 0.3f else 1f
      val size = p.baseSize * alpha // fade size with alpha

      val cx = p.x; val cy = p.y; val cz = p.z
      val rx = right.x * size; val ry = right.y * size; val rz = right.z * size
      val ux = up.x * size; val uy = up.y * size; val uz = up.z * size

      val base = activeCount * 4 * 8
      // Bottom-left
      vertexData(base + 0) = cx - rx - ux; vertexData(base + 1) = cy - ry - uy; vertexData(base + 2) = cz - rz - uz
      vertexData(base + 3) = 0f; vertexData(base + 4) = 0f
      vertexData(base + 5) = r; vertexData(base + 6) = g; vertexData(base + 7) = b
      // Bottom-right
      vertexData(base + 8) = cx + rx - ux; vertexData(base + 9) = cy + ry - uy; vertexData(base + 10) = cz + rz - uz
      vertexData(base + 11) = 1f; vertexData(base + 12) = 0f
      vertexData(base + 13) = r; vertexData(base + 14) = g; vertexData(base + 15) = b
      // Top-right
      vertexData(base + 16) = cx + rx + ux; vertexData(base + 17) = cy + ry + uy; vertexData(base + 18) = cz + rz + uz
      vertexData(base + 19) = 1f; vertexData(base + 20) = 1f
      vertexData(base + 21) = r; vertexData(base + 22) = g; vertexData(base + 23) = b
      // Top-left
      vertexData(base + 24) = cx - rx + ux; vertexData(base + 25) = cy - ry + uy; vertexData(base + 26) = cz - rz + uz
      vertexData(base + 27) = 0f; vertexData(base + 28) = 1f
      vertexData(base + 29) = r; vertexData(base + 30) = g; vertexData(base + 31) = b

      activeCount += 1

    mesh.updateVertices(vertexData)

  private inline def lerp(a: Float, b: Float, t: Float): Float = a + (b - a) * t
