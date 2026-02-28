package opennorrath.ui

import imgui.{ImGui, ImVec2}
import imgui.flag.{ImGuiCond, ImGuiWindowFlags}

import opennorrath.{Game, GameAction, InputManager}
import opennorrath.network.ZoneEvent
import opennorrath.screen.GameContext
import opennorrath.state.{GameClock, PlayerCharacter, ZoneCharacter}

/** Owns all ImGui panels and event wiring for the zone screen. */
class ZoneHud(ctx: GameContext, characters: scala.collection.Map[Int, ZoneCharacter]):

  private var player: Option[PlayerCharacter] = None
  private var charInfoPanel: Option[CharacterInfoPanel] = None
  private var buffPanel: Option[BuffPanel] = None
  private var spellBookPanel: Option[SpellBookPanel] = None
  private val escapeMenu = EscapeMenu(ctx)
  private var inventoryPanel: InventoryPanel = InventoryPanel()
  private var statsPanel: StatsPanel = StatsPanel()
  val targetPanel = TargetPanel()
  private val groupPanel = GroupPanel(characters)
  private var chatPanel: TextPanel = null
  private var eventHandler: ZoneEventHandler = null

  // FPS counter — smoothed with a running average to avoid jitter
  private var fpsAccum = 0f
  private var fpsFrames = 0
  private var fpsDisplay = 0f
  private val FpsUpdateInterval = 0.5f // refresh the counter twice per second

  def target: Option[ZoneCharacter] = targetPanel.target
  def target_=(s: Option[ZoneCharacter]): Unit = targetPanel.target = s
  def isEscapeOpen: Boolean = escapeMenu.isOpen

  val spawnRemovedListener: ZoneEvent => Unit =
    case ZoneEvent.SpawnRemoved(id) =>
      if targetPanel.target.exists(_.spawnId == id) then
        targetPanel.target = None
        disableAutoAttack()
    case ZoneEvent.EntityDied(info) =>
      if targetPanel.target.exists(_.spawnId == info.spawnId) then
        disableAutoAttack()
    case _ => ()

  val groupListener: ZoneEvent => Unit =
    case ZoneEvent.GroupUpdated(members, leader) =>
      groupPanel.members = members
      groupPanel.leader = leader
    case _ => ()

  def init(pc: Option[PlayerCharacter]): Unit =
    player = pc
    charInfoPanel = pc.map(CharacterInfoPanel(_))
    buffPanel = pc.map(BuffPanel(_))
    spellBookPanel = pc.map(SpellBookPanel(_))
    inventoryPanel = InventoryPanel(pc)
    statsPanel = StatsPanel(pc)
    targetPanel.player = pc
    chatPanel = TextPanel("Main", onSubmit = text => {
      eventHandler.submitChat(text)
    })
    eventHandler = ZoneEventHandler(chatPanel, characters, pc)
    pc.foreach(_.onSystemMessage = msg => chatPanel.addLine(msg))
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

  /** The player's ZoneCharacter, used for self-targeting. */
  private def selfCharacter: Option[ZoneCharacter] = player.flatMap(_.zoneChar)

  def update(input: InputManager): Unit =
    // Keep self-target HP/level in sync (PC is source of truth for stats)
    selfCharacter.foreach { sc =>
      player.foreach { pc =>
        sc.curHp = pc.currentHp
        sc.maxHp = pc.maxHp
        sc.level = pc.level
      }
    }

    if input.isActionPressed(GameAction.Escape) then
      if targetPanel.target.isDefined then
        targetPanel.target = None
        disableAutoAttack()
      else
        escapeMenu.toggle()

    if !escapeMenu.isOpen then
      if !imgui.ImGui.getIO().getWantCaptureKeyboard() then
        if input.isActionPressed(GameAction.TargetSelf) then
          setTarget(selfCharacter)
        if input.isActionPressed(GameAction.AutoAttack) then
          toggleAutoAttack()
        if input.isActionPressed(GameAction.ToggleInventory) then
          inventoryPanel.toggle()
        if input.isActionPressed(GameAction.ToggleSpellBook) then
          spellBookPanel.foreach(_.toggle())
        if input.isActionPressed(GameAction.ToggleStats) then
          statsPanel.toggle()

  /** Update target and notify server. */
  def setTarget(zc: Option[ZoneCharacter]): Unit =
    targetPanel.target = zc
    Game.zoneSession.foreach { session =>
      session.client.target(zc.map(_.spawnId).getOrElse(0))
    }

  private def toggleAutoAttack(): Unit =
    player.foreach { pc =>
      if pc.autoAttacking then
        disableAutoAttack()
      else if targetPanel.target.isDefined then
        pc.autoAttacking = true
        Game.zoneSession.foreach(_.client.autoAttack(true))
    }

  private def disableAutoAttack(): Unit =
    player.foreach { pc =>
      if pc.autoAttacking then
        pc.autoAttacking = false
        Game.zoneSession.foreach(_.client.autoAttack(false))
    }

  /** Call each frame with the current delta time to keep the FPS counter updated. */
  def render(dt: Float = 0f): Unit =
    charInfoPanel.foreach(_.render())
    buffPanel.foreach(_.render())
    targetPanel.render()
    groupPanel.render()
    inventoryPanel.render()
    statsPanel.render()
    spellBookPanel.foreach(_.render())
    chatPanel.render()
    escapeMenu.render()
    renderFpsCounter(dt)

  private def renderFpsCounter(dt: Float): Unit =
    fpsAccum += dt
    fpsFrames += 1
    if fpsAccum >= FpsUpdateInterval then
      fpsDisplay = fpsFrames.toFloat / fpsAccum
      fpsAccum = 0f
      fpsFrames = 0

    val padding = Spacing.menuPad
    val flags = ImGuiWindowFlags.NoDecoration | ImGuiWindowFlags.NoInputs |
      ImGuiWindowFlags.NoBackground | ImGuiWindowFlags.AlwaysAutoResize |
      ImGuiWindowFlags.NoSavedSettings | ImGuiWindowFlags.NoFocusOnAppearing |
      ImGuiWindowFlags.NoNav

    // Game time + FPS in a single string, pinned to top-right corner
    val label = if GameClock.isSynced then
      val t = GameClock.now
      val ampm = if t.hour <= 12 then "AM" else "PM"
      val h12 = if t.hour <= 12 then t.hour else t.hour - 12
      f"$h12%d:${t.minute}%02d $ampm | ${fpsDisplay.toInt}%d fps"
    else s"${fpsDisplay.toInt} fps"
    val textSize = ImGui.calcTextSize(label)
    val x = ctx.windowWidth - textSize.x - padding * 2
    ImGui.setNextWindowPos(x, 0f, ImGuiCond.Always)
    ImGui.begin("##fps", flags)
    ImGui.textColored(1f, 1f, 1f, 0.6f, label)
    ImGui.end()

  def dispose(): Unit =
    Game.zoneSession.foreach { session =>
      session.client.removeListener(eventHandler.listener)
      session.client.removeListener(spawnRemovedListener)
      session.client.removeListener(groupListener)
    }
