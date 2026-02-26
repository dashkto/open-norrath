package opennorrath.state

import opennorrath.network.{SpellBuff, ZoneEvent}

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
