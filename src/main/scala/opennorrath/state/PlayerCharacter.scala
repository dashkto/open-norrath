package opennorrath.state

import org.joml.Vector3f
import opennorrath.Game
import opennorrath.animation.AnimCode
import opennorrath.network.{PlayerProfileData, SpellBuff, ZoneEvent}
import opennorrath.world.{Physics, ZoneCollision}

/** Runtime state for the player's character while in a zone.
  *
  * Single source of truth for the local player's data. Updated by the
  * networking layer as packets arrive. UI panels read from this.
  */
class PlayerCharacter(
  var name: String,
  var level: Int,
  var race: Int,
  var classId: Int,
  var currentHp: Int,
  var maxHp: Int,
  var currentMana: Int,
  var maxMana: Int,
  // Attributes
  var str: Int = 0,
  var sta: Int = 0,
  var agi: Int = 0,
  var dex: Int = 0,
  var wis: Int = 0,
  var int: Int = 0,
  var cha: Int = 0,
):
  def hpPercent: Float = if maxHp > 0 then currentHp.toFloat / maxHp else 0f
  def manaPercent: Float = if maxMana > 0 then currentMana.toFloat / maxMana else 0f

  /** The ZoneCharacter representing this player in the zone.
    * None before zone entry; set when the player spawn is created.
    */
  var zoneChar: Option[ZoneCharacter] = None

  /** Whether auto-attack is currently active. */
  var autoAttacking: Boolean = false

  /** Feet-level GL position — the single source of truth for where the player is. */
  val position: Vector3f = Vector3f()

  /** Player heading as EQ byte (0-255, CCW from east) for model rotation. */
  var heading: Int = 0

  /** Player heading in degrees (0-360, CCW from east) for server packets. */
  var headingDeg: Float = 0f

  /** Whether the player is currently moving (for animation and network sync). */
  var moving: Boolean = false

  /** Update heading fields from camera yaw (degrees, CW). */
  def updateHeading(yaw: Float): Unit =
    val h = -yaw % 360f
    headingDeg = if h < 0 then h + 360f else h
    heading = ((headingDeg * 256f / 360f).toInt % 256 + 256) % 256

  /** Read ZoneCharacter model-origin position back to feet-level. */
  def syncFromZoneChar(): Unit =
    zoneChar.foreach { zc =>
      position.set(zc.position.x, zc.position.y - feetOffset, zc.position.z)
    }

  /** Write current state to ZoneCharacter for rendering. */
  def syncToZoneChar(): Unit =
    zoneChar.foreach { zc =>
      zc.position.set(position.x, position.y + feetOffset, position.z)
      zc.heading = heading
      zc.moving = moving
      zc.airborne = airborne
      zc.speed = if moving then runSpeed else 0f
    }

  /** Teleport to the given GL-space position (feet-level). */
  def teleportTo(glPos: Vector3f): Unit =
    position.set(glPos)
    zoneChar.foreach(_.position.set(glPos.x, glPos.y + feetOffset, glPos.z))

  /** Base run speed in GL units/sec. Can be modified by buffs (SoW etc).
    * EQEmu speed hack detection (Project Speedie, disabled by default) thresholds:
    * 125 units/sec normal, 140 speed-buffed, 160 bard. Only logs, doesn't reject.
    */
  var runSpeed: Float = 25f // tested: 25 GL units/sec matches standard unbuffed player run speed

  /** Zone collision mesh for ground detection. */
  var collision: Option[ZoneCollision] = None

  /** Offset from model origin to feet (positive = origin is above feet).
    * Set from CharBuild.glMinY when the player spawn is created.
    * e.g., if model origin is at hip and glMinY = -3.5, feetOffset = 3.5
    */
  var feetOffset: Float = 0f

  /** Full model height in GL units (scaled). Used to derive eye height. */
  var modelHeight: Float = 0f

  private val JumpSpeed = 30f     // initial upward velocity when jumping
  private val GroundProbe = 200f  // max raycast distance below player
  private val SnapThreshold = 0.5f
  private val FootRadius = 1.5f   // half-width of foot hitbox for multi-ray ground detection
  private val ProbeUp = 2f        // probe starts this far above feet
  private val WallMargin = 0.5f   // stop this far from walls
  private var fallSpeed = 0f
  private var onGround = true

  /** Whether the character has significant vertical momentum (for fall animation). */
  def airborne: Boolean = fallSpeed > Physics.Gravity

  def jump(): Unit =
    if onGround then
      fallSpeed = -JumpSpeed  // negative = upward
      onGround = false
      zoneChar.foreach(_.playTimedAnimation(AnimCode.Crouch.code, 0.3f))

  private val probeOrigin = Vector3f() // reusable to avoid allocation
  private val probeTarget = Vector3f()

  /** Try to move horizontally by (dx, dz) in GL space.
    * Raycasts at knee and chest height to detect walls.
    * Axes are tested independently so the player can slide along walls.
    * @param pos feet-level position to mutate
    * Returns true if any movement occurred.
    */
  def tryMove(pos: Vector3f, dx: Float, dz: Float): Boolean =
    collision match
      case None =>
        pos.x += dx; pos.z += dz
        dx != 0f || dz != 0f
      case Some(col) =>
        val feetY = pos.y - feetOffset
        // Probe at two heights: knee (~30% up) and chest (~70% up)
        val heights = Array(feetY + modelHeight * 0.3f, feetY + modelHeight * 0.7f)
        var movedX = dx
        var movedZ = dz
        // Test X axis
        if dx != 0f then
          val dist = Math.abs(dx) + WallMargin
          var blocked = false
          var h = 0
          while h < heights.length && !blocked do
            probeOrigin.set(pos.x, heights(h), pos.z)
            probeTarget.set(pos.x + dx + WallMargin * Math.signum(dx), heights(h), pos.z)
            val hitDist = col.raycast(probeOrigin, probeTarget)
            if hitDist >= 0f && hitDist < dist then
              blocked = true
            h += 1
          if blocked then movedX = 0f
        // Test Z axis
        if dz != 0f then
          val dist = Math.abs(dz) + WallMargin
          val testX = pos.x + movedX // use adjusted X
          var blocked = false
          var h = 0
          while h < heights.length && !blocked do
            probeOrigin.set(testX, heights(h), pos.z)
            probeTarget.set(testX, heights(h), pos.z + dz + WallMargin * Math.signum(dz))
            val hitDist = col.raycast(probeOrigin, probeTarget)
            if hitDist >= 0f && hitDist < dist then
              blocked = true
            h += 1
          if blocked then movedZ = 0f
        pos.x += movedX
        pos.z += movedZ
        movedX != 0f || movedZ != 0f

  /** Apply gravity using multi-ray ground detection.
    * Casts 5 rays (center + 4 corners of foot hitbox) to find the highest ground.
    * This prevents falling through ramps and narrow geometry.
    * @param pos feet-level position to mutate
    */
  def applyGravity(pos: Vector3f, dt: Float): Unit =
    collision.foreach { col =>
      val feetY = pos.y - feetOffset
      val py = feetY + ProbeUp
      // Cast center + 4 corner rays, take the highest ground hit
      var bestGroundY = Float.MinValue
      var hits = 0
      var i = 0
      while i < 5 do
        val px = pos.x + (i match
          case 1 => -FootRadius
          case 2 => FootRadius
          case 3 => 0f
          case 4 => 0f
          case _ => 0f
        )
        val pz = pos.z + (i match
          case 1 => 0f
          case 2 => 0f
          case 3 => -FootRadius
          case 4 => FootRadius
          case _ => 0f
        )
        probeOrigin.set(px, py, pz)
        val d = col.raycastDown(probeOrigin, GroundProbe)
        if d >= 0f then
          val gy = py - d
          hits += 1
          if gy > bestGroundY then bestGroundY = gy
        i += 1

      // Apply velocity (negative = upward from jump, positive = falling)
      fallSpeed = Math.min(fallSpeed + Physics.Gravity * dt, Physics.MaxFallSpeed)
      pos.y -= fallSpeed * dt

      val newFeetY = pos.y - feetOffset
      if hits > 0 then
        if newFeetY <= bestGroundY then
          // Landed on ground
          pos.y = bestGroundY + feetOffset
          fallSpeed = 0f
          onGround = true
        else
          onGround = false
      else
        onGround = false
    }

  val inventory: Inventory = Inventory()

  /** Known spells from the spell book. */
  val spellBook: scala.collection.mutable.ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer.empty

  /** Active buffs — up to 15 slots. Slot index → SpellBuff. */
  val buffs: scala.collection.mutable.Map[Int, SpellBuff] = scala.collection.mutable.Map.empty

  def loadBuffs(initial: Array[SpellBuff]): Unit =
    buffs.clear()
    for (buff, i) <- initial.zipWithIndex do
      buffs(i) = buff

  /** ZoneClient event listener — handles stat and inventory updates. */
  val listener: ZoneEvent => Unit = {
    case ZoneEvent.LevelChanged(lvl) =>
      level = lvl.level
    case ZoneEvent.HPChanged(hp) if hp.spawnId == Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1) =>
      currentHp = hp.curHp
      maxHp = hp.maxHp
    case ZoneEvent.ManaChanged(mana) =>
      currentMana = mana.curMana
    case ZoneEvent.InventoryLoaded(items) =>
      inventory.load(items)
    case ZoneEvent.InventoryItemUpdated(item) =>
      inventory.update(item)
    case ZoneEvent.InventoryMoved(from, to) =>
      inventory.swap(from, to)
    case ZoneEvent.BuffsLoaded(initial) =>
      loadBuffs(initial)
    case ZoneEvent.BuffUpdated(slot, buff) =>
      if buff.spellId == 0xFFFF then buffs.remove(slot)
      else buffs(slot) = buff
    case _ => ()
  }

object PlayerCharacter:
  def fromProfile(pp: PlayerProfileData): PlayerCharacter =
    val pc = PlayerCharacter(
      name = pp.name, level = pp.level, race = pp.race, classId = pp.classId,
      currentHp = pp.curHp, maxHp = pp.curHp, currentMana = pp.mana, maxMana = pp.mana,
      str = pp.str, sta = pp.sta, agi = pp.agi, dex = pp.dex,
      wis = pp.wis, int = pp.int_, cha = pp.cha,
    )
    pc.loadBuffs(pp.buffs)
    pc.spellBook ++= pp.spellBook
    pc
