package opennorrath.state

import opennorrath.network.InventoryItem

/** Player inventory keyed by slot ID (0-21=equipment, 22-29=general, 250-329=bag contents). */
class Inventory:

  var items: Map[Int, InventoryItem] = Map.empty

  def get(slotId: Int): Option[InventoryItem] = items.get(slotId)

  /** Max bag content slot: 8 bags × 10 slots = slots 250-329. */
  private val MaxBagSlot = 329

  def load(allItems: Vector[InventoryItem]): Unit =
    items = allItems.filter(i => i.equipSlot >= 0 && i.equipSlot <= MaxBagSlot)
                    .map(i => i.equipSlot -> i).toMap

  def update(item: InventoryItem): Unit =
    if item.equipSlot >= 0 && item.equipSlot <= MaxBagSlot then
      items = items + (item.equipSlot -> item)

  /** True if slot is a general inventory slot (22-29) that can hold a bag. */
  private def isGeneralSlot(slot: Int): Boolean = slot >= 22 && slot <= 29

  /** First bag-content slot for a given general slot. */
  private def bagContentBase(generalSlot: Int): Int = 250 + (generalSlot - 22) * 10

  def swap(from: Int, to: Int): Unit =
    if to == -1 || to == 0xFFFFFFFF then
      items = items - from
    else
      val fromItem = items.get(from)
      val toItem = items.get(to)
      var updated = items - from - to
      fromItem.foreach(it => updated = updated + (to -> it.copy(equipSlot = to)))
      toItem.foreach(it => updated = updated + (from -> it.copy(equipSlot = from)))

      // When a bag moves between general slots, relocate its content items
      if isGeneralSlot(from) && isGeneralSlot(to) then
        updated = relocateBagContents(updated, from, to)
        // If there was a bag in the target slot too, relocate its contents back to the source
        if toItem.exists(_.isBag) then
          updated = relocateBagContents(updated, to, from)

      items = updated

  /** Move all bag content items from one general slot's range to another's. */
  private def relocateBagContents(state: Map[Int, InventoryItem], oldGeneral: Int, newGeneral: Int): Map[Int, InventoryItem] =
    val oldBase = bagContentBase(oldGeneral)
    val newBase = bagContentBase(newGeneral)
    var result = state
    for i <- 0 until 10 do
      val oldSlot = oldBase + i
      val newSlot = newBase + i
      result.get(oldSlot) match
        case Some(item) =>
          result = result - oldSlot + (newSlot -> item.copy(equipSlot = newSlot))
        case None => ()
    result
