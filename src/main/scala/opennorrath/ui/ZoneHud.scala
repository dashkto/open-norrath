package opennorrath.ui

import org.joml.Vector3f

import opennorrath.{Game, GameAction, InputManager}
import opennorrath.network.ZoneEvent
import opennorrath.screen.GameContext
import opennorrath.state.ZoneCharacter

/** Owns all ImGui panels and event wiring for the zone screen. */
class ZoneHud(ctx: GameContext, characters: scala.collection.Map[Int, ZoneCharacter]):

  private val charInfoPanel = Game.player.map(CharacterInfoPanel(_))
  private val buffPanel = Game.player.map(BuffPanel(_))
  private val spellBookPanel = Game.player.map(SpellBookPanel(_))
  private val escapeMenu = EscapeMenu(ctx)
  private val inventoryPanel = InventoryPanel()
  val targetPanel = TargetPanel()
  private val groupPanel = GroupPanel(characters)
  private var chatPanel: TextPanel = null
  private var eventHandler: ZoneEventHandler = null

  def target: Option[ZoneCharacter] = targetPanel.target
  def target_=(s: Option[ZoneCharacter]): Unit = targetPanel.target = s
  def isEscapeOpen: Boolean = escapeMenu.isOpen

  val spawnRemovedListener: ZoneEvent => Unit =
    case ZoneEvent.SpawnRemoved(id) =>
      if targetPanel.target.exists(_.spawnId == id) then targetPanel.target = None
    case _ => ()

  val groupListener: ZoneEvent => Unit =
    case ZoneEvent.GroupUpdated(members, leader) =>
      groupPanel.members = members
      groupPanel.leader = leader
    case _ => ()

  def init(): Unit =
    chatPanel = TextPanel("Main", onSubmit = text => {
      eventHandler.submitChat(text)
    })
    eventHandler = ZoneEventHandler(chatPanel, characters)
    Game.zoneSession.foreach { session =>
      session.client.addListener(eventHandler.listener)
      session.client.addListener(spawnRemovedListener)
      session.client.addListener(groupListener)
      // Seed group from player profile (loaded before zone entry)
      session.client.profile.foreach { p =>
        if p.groupMembers.nonEmpty then
          groupPanel.members = p.groupMembers.toVector
      }
    }

  /** A ZoneCharacter proxy for the local player, used for self-targeting. */
  private val selfCharacter: Option[ZoneCharacter] = Game.player.map { pc =>
    val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(0)
    ZoneCharacter(
      spawnId = myId,
      name = pc.name,
      lastName = "",
      race = pc.race,
      classId = pc.classId,
      gender = 0,
      level = pc.level,
      npcType = 0,
      modelCode = "",
      size = 1f,
      position = Vector3f(),
      heading = 0,
      curHp = pc.currentHp,
      maxHp = pc.maxHp,
    )
  }

  def update(input: InputManager): Unit =
    // Keep self-target HP in sync
    selfCharacter.foreach { sc =>
      Game.player.foreach { pc =>
        sc.curHp = pc.currentHp
        sc.maxHp = pc.maxHp
        sc.level = pc.level
      }
    }

    if input.isActionPressed(GameAction.Escape) then
      if targetPanel.target.isDefined then
        targetPanel.target = None
      else
        escapeMenu.toggle()

    if !escapeMenu.isOpen then
      if !imgui.ImGui.getIO().getWantCaptureKeyboard() then
        if input.isActionPressed(GameAction.TargetSelf) then
          selfCharacter.foreach(sc => targetPanel.target = Some(sc))
        if input.isActionPressed(GameAction.ToggleInventory) then
          inventoryPanel.toggle()
        if input.isActionPressed(GameAction.ToggleSpellBook) then
          spellBookPanel.foreach(_.toggle())

  def render(): Unit =
    charInfoPanel.foreach(_.render())
    buffPanel.foreach(_.render())
    targetPanel.render()
    groupPanel.render()
    inventoryPanel.render()
    spellBookPanel.foreach(_.render())
    chatPanel.render()
    escapeMenu.render()

  def dispose(): Unit =
    Game.zoneSession.foreach { session =>
      session.client.removeListener(eventHandler.listener)
      session.client.removeListener(spawnRemovedListener)
      session.client.removeListener(groupListener)
    }
