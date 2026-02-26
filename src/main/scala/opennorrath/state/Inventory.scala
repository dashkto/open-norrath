package opennorrath.state

import opennorrath.network.InventoryItem

/** Player inventory keyed by slot ID (0-21=equipment, 22-29=general). */
class Inventory:

  var items: Map[Int, InventoryItem] = Map.empty

  def get(slotId: Int): Option[InventoryItem] = items.get(slotId)

  def load(allItems: Vector[InventoryItem]): Unit =
    items = allItems.filter(i => i.equipSlot >= 0 && i.equipSlot <= 29)
                    .map(i => i.equipSlot -> i).toMap

  def update(item: InventoryItem): Unit =
    if item.equipSlot >= 0 && item.equipSlot <= 29 then
      items = items + (item.equipSlot -> item)

  def swap(from: Int, to: Int): Unit =
    if to == -1 || to == 0xFFFFFFFF then
      items = items - from
    else
      val fromItem = items.get(from)
      val toItem = items.get(to)
      var updated = items - from - to
      fromItem.foreach(it => updated = updated + (to -> it.copy(equipSlot = to)))
      toItem.foreach(it => updated = updated + (from -> it.copy(equipSlot = from)))
      items = updated
