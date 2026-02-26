package opennorrath.state

import org.joml.Vector3f

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

  /** GL-space velocity computed from consecutive server position updates. */
  val velocity: Vector3f = Vector3f()

  /** Previous server position, for computing velocity on next update. */
  val prevServerPos: Vector3f = Vector3f(position)

  /** Timestamp (nanos) of last server position update. */
  var lastUpdateNanos: Long = System.nanoTime()

  def hpFraction: Float =
    if maxHp > 0 then curHp.toFloat / maxHp.toFloat else 1f

  /** Called when a server position update arrives. Computes velocity from
    * the position delta, then snaps to the new server position.
    */
  def onServerPositionUpdate(newPos: Vector3f, newHeading: Int, isMoving: Boolean, anim: Int): Unit =
    val now = System.nanoTime()
    val dtSec = (now - lastUpdateNanos) / 1_000_000_000.0f

    if isMoving && dtSec > 0.01f then
      velocity.set(
        (newPos.x - prevServerPos.x) / dtSec,
        (newPos.y - prevServerPos.y) / dtSec,
        (newPos.z - prevServerPos.z) / dtSec,
      )
    else
      velocity.set(0f, 0f, 0f)

    position.set(newPos)
    prevServerPos.set(newPos)
    lastUpdateNanos = now
    heading = newHeading
    moving = isMoving
    animation = anim

  /** Advance position along velocity. Called each frame for moving characters. */
  def interpolate(dt: Float): Unit =
    if moving then
      position.x += velocity.x * dt
      position.y += velocity.y * dt
      position.z += velocity.z * dt

object ZoneCharacter:
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
