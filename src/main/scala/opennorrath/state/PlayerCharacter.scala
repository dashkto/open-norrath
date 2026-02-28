package opennorrath.state

import org.joml.Vector3f
import opennorrath.Game
import opennorrath.animation.AnimCode
import opennorrath.network.{InventoryItem, ItemType, PlayerProfileData, SpellBuff, ZoneEvent}
import opennorrath.ui.SpellData
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
  // Money
  var platinum: Int = 0,
  var gold: Int = 0,
  var silver: Int = 0,
  var copper: Int = 0,
  // Experience
  var exp: Int = 0,
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

  val DefaultJumpSpeed = 30f
  var jumpSpeed = DefaultJumpSpeed // initial upward velocity when jumping
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
      fallSpeed = -jumpSpeed  // negative = upward
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
        // pos.y is feet-level; probe at knee (~30%) and chest (~70%) above feet
        val heights = Array(pos.y + modelHeight * 0.3f, pos.y + modelHeight * 0.7f)
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
      // pos.y is feet-level — probe starts slightly above feet
      val py = pos.y + ProbeUp
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

      if hits > 0 then
        if pos.y <= bestGroundY then
          // Landed on ground
          pos.y = bestGroundY
          fallSpeed = 0f
          onGround = true
        else
          onGround = false
      else
        onGround = false
    }

  // Hunger/thirst — tracked from OP_Stamina and player profile.
  // Server decrements by 32 every ~46 seconds. Below 3000 = hungry/thirsty, at 0 = famished.
  var hungerLevel: Int = 6000
  var thirstLevel: Int = 6000

  /** Callback for system messages (e.g., auto-eat). Set by ZoneHud to write to chat panel. */
  var onSystemMessage: String => Unit = _ => ()

  /** All inventory items including bag contents (slots 250+).
    * Used by auto-consume to find food/drink in bags.
    */
  var allItems: Vector[InventoryItem] = Vector.empty

  val inventory: Inventory = Inventory()

  /** Known spells from the spell book. */
  val spellBook: scala.collection.mutable.ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer.empty

  /** Memorized spell gem slots (8 slots, -1 = empty). Index = gem slot 0-7. */
  val memSpells: Array[Int] = Array.fill(8)(-1)

  /** Active buffs — up to 15 slots. Slot index → SpellBuff. */
  val buffs: scala.collection.mutable.Map[Int, SpellBuff] = scala.collection.mutable.Map.empty

  def loadBuffs(initial: Array[SpellBuff]): Unit =
    buffs.clear()
    for (buff, i) <- initial.zipWithIndex do
      if buff.spellId > 0 && buff.spellId != 0xFFFF then
        buffs(i) = buff

  /** Accumulator for client-side buff duration tick-down (1 tick = 6 seconds). */
  private var buffTickAccum: Float = 0f

  /** Refresh a buff's duration when the same spell lands on us again.
    * The server doesn't send OP_Buff for same-caster refreshes — the client
    * resets the duration from the OP_Action (spell landed) packet.
    * Server adds +1 extraTick; we include that here to match server state.
    */
  def refreshBuff(spellId: Int, casterLevel: Int): Unit =
    val newDuration = SpellData.calcBuffDuration(spellId, casterLevel) + 1 // +1 extraTick
    if newDuration <= 1 then return // not a buff spell
    buffs.find(_._2.spellId == spellId) match
      case Some((slot, buff)) =>
        buff.duration = newDuration
        buffTickAccum = 0f // reset tick accumulator to sync with server
      case None => // buff not in our list yet — it will come via profile or OP_Buff later

  /** Tick down buff durations client-side. Call once per frame with delta time. */
  def tickBuffs(dt: Float): Unit =
    buffTickAccum += dt
    if buffTickAccum >= 6f then
      val ticks = (buffTickAccum / 6f).toInt
      buffTickAccum -= ticks * 6f
      val expired = scala.collection.mutable.ArrayBuffer.empty[Int]
      for (slot, buff) <- buffs do
        if buff.duration > 0 then
          buff.duration -= ticks
          if buff.duration <= 0 then expired += slot
      expired.foreach(buffs.remove)

  // Consume_Struct type field (different from ItemType enum)
  private val ConsumeFood  = 1
  private val ConsumeDrink = 2
  // Server auto-eat threshold: below 3000 = hungry/thirsty
  private val HungryThreshold = 3000

  /** Find first food or drink item in inventory (general slots + bags). */
  private def findConsumable(wantType: ItemType): Option[InventoryItem] =
    allItems.find(_.itemType == wantType)

  /** Decrement an item's charges by 1, removing it entirely if depleted. */
  private def decrementItem(item: InventoryItem): Unit =
    if item.charges <= 1 then
      // Last charge — remove from inventory
      allItems = allItems.filterNot(_.equipSlot == item.equipSlot)
      inventory.swap(item.equipSlot, -1)
    else
      // Decrement stack count
      val updated = item.copy(charges = item.charges - 1)
      allItems = allItems.filterNot(_.equipSlot == item.equipSlot) :+ updated
      inventory.update(updated)

  /** Auto-consume food/drink when hungry/thirsty.
    * Called when the server sends a stamina update. The original Mac EQ client
    * auto-consumed from inventory whenever hunger or thirst dropped below 3000.
    */
  private def autoConsume(): Unit =
    Game.zoneSession.foreach { session =>
      if hungerLevel < HungryThreshold then
        findConsumable(ItemType.Food).foreach { food =>
          println(s"[AutoEat] Eating '${food.name}' from slot ${food.equipSlot} (hunger=$hungerLevel)")
          session.client.sendConsume(food.equipSlot, ConsumeFood)
          onSystemMessage(s"You ate a ${food.name}.")
          decrementItem(food)
        }
      if thirstLevel < HungryThreshold then
        findConsumable(ItemType.Drink).foreach { drink =>
          println(s"[AutoEat] Drinking '${drink.name}' from slot ${drink.equipSlot} (thirst=$thirstLevel)")
          session.client.sendConsume(drink.equipSlot, ConsumeDrink)
          onSystemMessage(s"You drank a ${drink.name}.")
          decrementItem(drink)
        }
    }

  /** Move bag content items in allItems from one general slot's range to another's. */
  private def relocateAllItemsBagContents(oldGeneral: Int, newGeneral: Int): Unit =
    val oldBase = 250 + (oldGeneral - 22) * 10
    val newBase = 250 + (newGeneral - 22) * 10
    allItems = allItems.map { item =>
      val offset = item.equipSlot - oldBase
      if offset >= 0 && offset < 10 then
        item.copy(equipSlot = newBase + offset)
      else item
    }

  /** ZoneClient event listener — handles stat and inventory updates. */
  val listener: ZoneEvent => Unit = {
    case ZoneEvent.LevelChanged(lvl) =>
      level = lvl.level
    case ZoneEvent.HPChanged(hp) if hp.spawnId == Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1) =>
      currentHp = hp.curHp
      maxHp = hp.maxHp
    case ZoneEvent.ManaChanged(mana) =>
      currentMana = mana.curMana
    case ZoneEvent.ExpChanged(e) =>
      exp = e.exp
    case ZoneEvent.StaminaChanged(sta) =>
      hungerLevel = sta.food
      thirstLevel = sta.water
      autoConsume()
    case ZoneEvent.InventoryLoaded(items) =>
      allItems = items
      inventory.load(items)
    case ZoneEvent.InventoryItemUpdated(item) =>
      allItems = allItems.filterNot(_.equipSlot == item.equipSlot) :+ item
      inventory.update(item)
    case ZoneEvent.InventoryMoved(from, to) =>
      // Update allItems tracking
      if to == -1 || to == 0xFFFFFFFF then
        allItems = allItems.filterNot(_.equipSlot == from)
      else
        val fromItem = allItems.find(_.equipSlot == from)
        val toItem = allItems.find(_.equipSlot == to)
        allItems = allItems.filterNot(i => i.equipSlot == from || i.equipSlot == to)
        fromItem.foreach(i => allItems = allItems :+ i.copy(equipSlot = to))
        toItem.foreach(i => allItems = allItems :+ i.copy(equipSlot = from))

        // When a bag moves between general slots, relocate its content items in allItems too
        val isGeneral = (s: Int) => s >= 22 && s <= 29
        if isGeneral(from) && isGeneral(to) then
          relocateAllItemsBagContents(from, to)
          if toItem.exists(_.isBag) then
            relocateAllItemsBagContents(to, from)

      inventory.swap(from, to)
    case ZoneEvent.BuffsLoaded(initial) =>
      loadBuffs(initial)
    case ZoneEvent.BuffUpdated(slot, buff) =>
      if buff.spellId == 0xFFFF then buffs.remove(slot)
      else buffs(slot) = buff
    case ZoneEvent.SpellActionTriggered(action) =>
      // buffUnknown == 4 means spell landed. If it's a beneficial spell targeting us,
      // refresh the buff duration. The server doesn't send OP_Buff for same-caster
      // refreshes — the client must reset duration from OP_Action.
      val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1)
      if action.buffUnknown == 4 && action.targetId == myId && action.spellId > 0
         && SpellData.isBeneficial(action.spellId) then
        refreshBuff(action.spellId, action.level)
    case _ => ()
  }

object PlayerCharacter:
  def fromProfile(pp: PlayerProfileData): PlayerCharacter =
    val pc = PlayerCharacter(
      name = pp.name, level = pp.level, race = pp.race, classId = pp.classId,
      currentHp = pp.curHp, maxHp = pp.curHp, currentMana = pp.mana, maxMana = pp.mana,
      str = pp.str, sta = pp.sta, agi = pp.agi, dex = pp.dex,
      wis = pp.wis, int = pp.int_, cha = pp.cha,
      platinum = pp.platinum, gold = pp.gold, silver = pp.silver, copper = pp.copper,
      exp = pp.exp,
    )
    pc.hungerLevel = pp.hungerLevel
    pc.thirstLevel = pp.thirstLevel
    pc.loadBuffs(pp.buffs)
    pc.spellBook ++= pp.spellBook
    pp.memSpells.copyToArray(pc.memSpells)
    pc
