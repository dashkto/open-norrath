package opennorrath.state

import org.joml.Vector3f

import opennorrath.animation.AnimCode
import opennorrath.network.{SpawnData, TintColor, TintProfile}
import opennorrath.ui.EqData
import opennorrath.world.EqCoords

/** Mutable game-logic representation of a non-player entity in the zone.
  * Network events update it. UI and game systems read from it.
  * The player's own spawn is tracked by PlayerCharacter, not here.
  */
class ZoneCharacter(
  val spawnId: Int,
  val name: String,
  val lastName: String,
  val race: Int,
  val classId: Int,
  val gender: Int,
  var level: Int,
  val npcType: Int,
  val modelCode: String,
  val size: Float,
  val position: Vector3f,
  var heading: Int,
  val bodyTexture: Int = 0,
  var curHp: Int = -1,
  var maxHp: Int = -1,
  var moving: Boolean = false,
  var animation: Int = 0,
):
  val displayName: String = ZoneCharacter.cleanName(name)

  /** Per-slot equipment material IDs (9 slots: head/chest/arms/wrist/hands/legs/feet/primary/secondary). */
  val equipment: Array[Int] = Array.fill(9)(0)

  /** Per-slot equipment tint colors. */
  val equipColors: Array[TintColor] = Array.fill(9)(TintColor(0, 0, 0, false))

  /** Update a single equipment slot (from WearChange events). */
  def updateEquipment(slot: Int, material: Int, color: TintColor): Unit =
    if slot >= 0 && slot < 9 then
      equipment(slot) = material
      equipColors(slot) = color

  // --- Animation state ---

  /** Timed animation override (code, remaining seconds). Highest priority while active. */
  private var timedAnim: Option[(String, Float)] = None

  /** Persistent state flags for animation resolution. */
  var dead: Boolean = false
  var airborne: Boolean = false
  var sitting: Boolean = false
  var speed: Float = 0f  // GL units/sec — used to pick walk vs run

  /** Time spent in idle before next fidget. Reset when leaving idle. */
  private var fidgetTimer: Float = ZoneCharacter.randomFidgetDelay()
  private var lastResolvedCode: String = ""

  /** Play a timed animation override that expires after `duration` seconds. */
  def playTimedAnimation(code: String, duration: Float): Unit =
    timedAnim = Some((code, duration))

  /** Resolve the current animation code based on priority. Call once per frame with dt. */
  def resolveAnimation(dt: Float): String =
    // Tick timed override
    timedAnim = timedAnim.flatMap { (code, remaining) =>
      val r = remaining - dt
      if r > 0f then Some((code, r)) else None
    }

    // Timed override wins if active
    timedAnim match
      case Some((code, _)) =>
        fidgetTimer = ZoneCharacter.randomFidgetDelay()
        return code
      case None => ()

    // State priority: dead > airborne > sitting > moving > fidget > idle
    if dead then return AnimCode.Death1.code
    if airborne then return AnimCode.Fall.code
    if sitting then return AnimCode.Sit.code
    if moving then
      fidgetTimer = ZoneCharacter.randomFidgetDelay()
      return if speed > ZoneCharacter.RunThreshold then AnimCode.Run.code else AnimCode.Walk.code

    // Idle — tick fidget timer
    fidgetTimer -= dt
    if fidgetTimer <= 0f then
      fidgetTimer = ZoneCharacter.randomFidgetDelay()
      return AnimCode.Fidget.code

    AnimCode.Idle.code

  /** Update animation on this character. Returns true if the animation code changed. */
  def updateAnimation(dt: Float): Boolean =
    val code = resolveAnimation(dt)
    if code != lastResolvedCode then
      lastResolvedCode = code
      true
    else false

  /** The last resolved animation code. */
  def currentAnimCode: String = lastResolvedCode

  // --- Movement ---

  /** GL-space velocity computed from consecutive server position updates. */
  val velocity: Vector3f = Vector3f()
  private val prevServerPos: Vector3f = Vector3f(position)
  private var lastUpdateNanos: Long = System.nanoTime()

  /** Authoritative server position — we smoothly interpolate `position` toward this. */
  private val serverPos: Vector3f = Vector3f(position)

  def hpFraction: Float =
    if maxHp > 0 then curHp.toFloat / maxHp.toFloat else 1f

  /** Called when a server position update arrives. Uses server-provided velocity
    * if available, falls back to computing from position deltas.
    */
  def onServerPositionUpdate(newPos: Vector3f, serverVel: Vector3f, newHeading: Int, isMoving: Boolean, anim: Int): Unit =
    val stateChanged = isMoving != moving

    if isMoving then
      val hasServerVel = serverVel.x != 0f || serverVel.y != 0f || serverVel.z != 0f
      if hasServerVel then
        velocity.set(serverVel)
      else
        // Fallback: compute velocity from position delta
        val now = System.nanoTime()
        val dtSec = (now - lastUpdateNanos) / 1_000_000_000.0f
        if dtSec > 0.01f then
          velocity.set(
            (newPos.x - prevServerPos.x) / dtSec,
            (newPos.y - prevServerPos.y) / dtSec,
            (newPos.z - prevServerPos.z) / dtSec,
          )
    else
      velocity.set(0f, 0f, 0f)

    serverPos.set(newPos)
    // Snap position on state transitions (idle↔moving) — no drift to correct
    if stateChanged then
      position.set(newPos)
    prevServerPos.set(newPos)
    lastUpdateNanos = System.nanoTime()
    heading = newHeading
    moving = isMoving
    animation = anim

  /** Heading derived from velocity direction (0-255 EQ format, 0=east, CCW).
    * When moving, the server heading can lag behind direction changes.
    */
  def facingHeading: Int =
    if moving && (velocity.x != 0f || velocity.z != 0f) then
      // GL: model faces +X (east) at rest. Convert velocity to CCW angle from east.
      val radians = Math.atan2(-velocity.z, velocity.x).toFloat
      val h = (radians * 256f / (2f * Math.PI.toFloat)).toInt
      ((h % 256) + 256) % 256
    else heading

  /** Advance position along velocity and correct toward server position.
    * Called each frame. When moving, extrapolates along velocity and blends
    * toward the authoritative server position to avoid drift. When stopped,
    * smoothly slides to the final server position.
    */
  def interpolate(dt: Float): Unit =
    if moving then
      position.x += velocity.x * dt
      position.y += velocity.y * dt
      position.z += velocity.z * dt
      // Also advance serverPos along velocity so correction doesn't fight extrapolation
      serverPos.x += velocity.x * dt
      serverPos.y += velocity.y * dt
      serverPos.z += velocity.z * dt
    // Smooth correction toward server position (works for both moving and stopped)
    val correctionRate = if moving then 5f else 10f  // faster snap when stopped
    val t = Math.min(correctionRate * dt, 1f)
    position.x += (serverPos.x - position.x) * t
    position.y += (serverPos.y - position.y) * t
    position.z += (serverPos.z - position.z) * t

object ZoneCharacter:
  /** Speed threshold (GL units/sec) — above this, run; at or below, walk. */
  val RunThreshold = 15f

  private val rng = java.util.concurrent.ThreadLocalRandom.current()
  def randomFidgetDelay(): Float = 5f + rng.nextFloat() * 10f  // 5-15 seconds

  def cleanName(raw: String): String =
    raw.replaceAll("\\d+$", "").replace('_', ' ').trim

  def fromSpawn(s: SpawnData): Option[ZoneCharacter] =
    EqData.raceModelCode(s.race, s.gender).map { code =>
      val zc = ZoneCharacter(
        spawnId = s.spawnId,
        name = s.name,
        lastName = s.lastName,
        race = s.race,
        classId = s.classId,
        gender = s.gender,
        level = s.level,
        npcType = s.npcType,
        modelCode = code,
        size = s.size,
        position = EqCoords.serverToGl(s.y, s.x, s.z),
        heading = s.heading,
        bodyTexture = s.bodyTexture,
      )
      Array.copy(s.equipment, 0, zc.equipment, 0, math.min(s.equipment.length, 9))
      for i <- 0 until math.min(s.equipColors.slots.length, 9) do
        zc.equipColors(i) = s.equipColors.slots(i)
      zc
    }
