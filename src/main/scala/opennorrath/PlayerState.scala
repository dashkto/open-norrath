package opennorrath

/** Runtime state for the player's character while in a zone.
  *
  * Updated by the networking layer as packets arrive. UI panels read from this.
  * All fields are mutable for efficient network updates.
  */
class PlayerState(
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
