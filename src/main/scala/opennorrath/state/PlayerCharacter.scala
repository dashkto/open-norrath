package opennorrath.state

import org.joml.Vector3f
import opennorrath.network.{SpellBuff, ZoneEvent}
import opennorrath.world.ZoneCollision

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

  /** Player foot position in GL space. Updated by CameraController when attached. */
  val position: Vector3f = Vector3f()

  /** Player heading in degrees (0-360, CW from east). Updated when camera yaw changes. */
  var headingDeg: Float = 0f

  /** Player heading as EQ byte (0-255, CCW from east) for model rotation. */
  def headingByte: Int =
    val h = ((headingDeg * 256f / 360f).toInt % 256 + 256) % 256
    h

  /** Whether the player is currently moving. */
  var moving: Boolean = false

  /** Base run speed in GL units/sec. Can be modified by buffs (SoW etc). */
  var runSpeed: Float = 25f

  /** Zone collision mesh for ground detection. */
  var collision: Option[ZoneCollision] = None

  /** Offset from model origin to feet (positive = origin is above feet).
    * Set from CharBuild.glMinY when the player spawn is created.
    * e.g., if model origin is at hip and glMinY = -3.5, feetOffset = 3.5
    */
  var feetOffset: Float = 0f

  /** Full model height in GL units (scaled). Used to derive eye height. */
  var modelHeight: Float = 0f

  private val Gravity = 80f       // GL units/sec² downward acceleration
  private val MaxFallSpeed = Gravity * 4f // terminal velocity
  private val JumpSpeed = 30f     // initial upward velocity when jumping
  private val GroundProbe = 200f  // max raycast distance below player
  private val SnapThreshold = 0.5f
  private val FootRadius = 1.5f   // half-width of foot hitbox for multi-ray ground detection
  private val ProbeUp = 2f        // probe starts this far above feet
  private var fallSpeed = 0f
  private var onGround = true

  /** Whether the character has significant vertical momentum (for fall animation). */
  def airborne: Boolean = fallSpeed > Gravity

  /** Set to true when a jump is initiated; ZoneScreen reads and clears it for animation. */
  var jumped: Boolean = false

  def jump(): Unit =
    if onGround then
      fallSpeed = -JumpSpeed  // negative = upward
      onGround = false
      jumped = true
  private val probeOrigin = Vector3f() // reusable to avoid allocation

  /** Apply gravity using multi-ray ground detection.
    * Casts 5 rays (center + 4 corners of foot hitbox) to find the highest ground.
    * This prevents falling through ramps and narrow geometry.
    */
  def applyGravity(dt: Float): Unit =
    collision.foreach { col =>
      // Feet are below model origin by feetOffset
      val feetY = position.y - feetOffset
      val py = feetY + ProbeUp
      // Cast center + 4 corner rays, take the highest ground hit
      var bestGroundY = Float.MinValue
      var hits = 0
      var i = 0
      while i < 5 do
        val px = position.x + (i match
          case 1 => -FootRadius
          case 2 => FootRadius
          case 3 => 0f
          case 4 => 0f
          case _ => 0f
        )
        val pz = position.z + (i match
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
      fallSpeed = Math.min(fallSpeed + Gravity * dt, MaxFallSpeed)
      position.y -= fallSpeed * dt
      val newFeetY = position.y - feetOffset

      if hits > 0 then
        val targetY = bestGroundY + feetOffset
        if newFeetY <= bestGroundY then
          // Landed on ground
          position.y = targetY
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
    case ZoneEvent.HPChanged(hp) =>
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
