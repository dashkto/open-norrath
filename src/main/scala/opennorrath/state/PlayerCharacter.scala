package opennorrath.state

import opennorrath.network.ZoneEvent

/** Runtime state for the player's character while in a zone.
  *
  * Single source of truth for the local player's data. Updated by the
  * networking layer as packets arrive. UI panels read from this.
  */
class PlayerCharacter(
  var name: String,
  var level: Int,
  var classId: Int,
  var currentHp: Int,
  var maxHp: Int,
  var currentMana: Int,
  var maxMana: Int,
):
  def hpPercent: Float = if maxHp > 0 then currentHp.toFloat / maxHp else 0f
  def manaPercent: Float = if maxMana > 0 then currentMana.toFloat / maxMana else 0f

  val inventory: Inventory = Inventory()

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
    case _ => ()
  }
