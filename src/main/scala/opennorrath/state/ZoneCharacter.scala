package opennorrath.state

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}

import opennorrath.animation.{AnimCode, AnimatedCharacter}
import opennorrath.network.{SpawnAppearanceChange, SpawnData, TintColor, TintProfile}
import opennorrath.ui.EqData
import opennorrath.world.{EqCoords, ZoneRenderer}

/** Mutable representation of an entity in the zone. Holds both game-logic state
  * (position, HP, equipment, animation) and rendering state (GPU mesh, texture
  * overrides, weapon models). Network events update it; the renderer reads from it.
  *
  * Constructed via `ZoneCharacter.fromSpawn`. Rendering fields (`animChar`, `build`)
  * are initialized later by `ZoneRenderer.addSpawn` via `initRendering`.
  */
class ZoneCharacter(
  val spawn: SpawnData,
  val position: Vector3f,
  var heading: Int,
  var curHp: Int = -1,
  var maxHp: Int = -1,
  var moving: Boolean = false,
  var animation: Int = 0,
):
  // --- Spawn delegates ---
  def spawnId: Int = spawn.spawnId
  def name: String = spawn.name
  def lastName: String = spawn.lastName
  def race: Int = spawn.race
  def classId: Int = spawn.classId
  def gender: Int = spawn.gender
  def npcType: Int = spawn.npcType
  def size: Float = spawn.size
  def bodyTexture: Int = spawn.bodyTexture
  def flying: Boolean = spawn.flyMode == 1
  var level: Int = spawn.level
  var face: Int = spawn.face

  val modelCode: String = EqData.raceModelCode(spawn.race, spawn.gender).get
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

  // --- Rendering state (initialized by ZoneRenderer.addSpawn via initRendering) ---

  /** GPU mesh and skeletal animation instance. Null until initRendering is called. */
  var animChar: AnimatedCharacter = uninitialized

  /** Model template (skeleton, clips, metrics). Null until initRendering is called. */
  var build: ZoneRenderer.CharBuild = uninitialized

  /** Effective display size (server size / 6). */
  var effectiveSize: Float = if size > 0f then size / 6f else 1f

  /** Flying creatures hover above their server position by half their model height. */
  var flyOffset: Float = 0f

  /** Equipment texture variant overrides (base texture name → variant name). */
  var textureOverrides: Map[String, String] = Map.empty

  /** IT codes for equipped weapon models. */
  var weaponPrimary: Int = 0
  var weaponSecondary: Int = 0

  /** True if rendering state has been initialized. */
  def hasRendering: Boolean = animChar != null

  /** Initialize rendering state. Called by ZoneRenderer after GPU resources are created. */
  def initRendering(b: ZoneRenderer.CharBuild, ac: AnimatedCharacter): Unit =
    build = b
    animChar = ac
    flyOffset = if flying then b.glHeight * effectiveSize * 0.5f else 0f

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
    if dead then return AnimCode.Damage1.code
    if airborne then return AnimCode.Fall.code
    if sitting then return AnimCode.Sitting.code
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

  /** Called when a server position update arrives.
    *
    * NOTE: The server's delta_yzx velocity field is only populated for player characters.
    * For NPCs, m_Delta is never set by the server (always zero) — the server moves NPCs
    * via waypoint/pathfinding updates and only sends positions. The animType field
    * (pRunAnimSpeed = speed * 10) is the reliable indicator of NPC movement. We compute
    * velocity client-side from consecutive position updates for interpolation.
    */
  def onServerPositionUpdate(newPos: Vector3f, serverVel: Vector3f, newHeading: Int, anim: Int): Unit =
    val hasServerVel = serverVel.x != 0f || serverVel.y != 0f || serverVel.z != 0f
    val now = System.nanoTime()
    val dtSec = (now - lastUpdateNanos) / 1e9f
    if hasServerVel then
      velocity.set(serverVel)
    else if dtSec > 0.01f && anim > 0 then
      // NPC: compute velocity from position delta
      velocity.set(
        (newPos.x - serverPos.x) / dtSec,
        (newPos.y - serverPos.y) / dtSec,
        (newPos.z - serverPos.z) / dtSec,
      )
    else
      velocity.set(0f, 0f, 0f)
    serverPos.set(newPos)
    prevServerPos.set(newPos)
    lastUpdateNanos = now
    heading = newHeading
    moving = hasServerVel || anim > 0
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

  /** Advance position along velocity or correct toward server position.
    * When moving, purely extrapolates along velocity — correcting toward serverPos
    * while moving causes convergence then snap, because serverPos is already stale
    * by the time we receive it. When stopped, smoothly slides to the final position.
    */
  def interpolate(dt: Float): Unit =
    if moving then
      position.x += velocity.x * dt
      position.y += velocity.y * dt
      position.z += velocity.z * dt
    else
      val t = Math.min(10f * dt, 1f)
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
    EqData.raceModelCode(s.race, s.gender).map { _ =>
      val zc = ZoneCharacter(
        spawn = s,
        position = EqCoords.serverToGl(s.y, s.x, s.z),
        heading = s.heading,
      )
      Array.copy(s.equipment, 0, zc.equipment, 0, math.min(s.equipment.length, 9))
      for i <- 0 until math.min(s.equipColors.slots.length, 9) do
        zc.equipColors(i) = s.equipColors.slots(i)
      // Initialize animation state from spawn data
      zc.sitting = s.standState == SpawnAppearanceChange.AnimSit
      zc.dead = s.standState == SpawnAppearanceChange.AnimDead
      zc
    }
