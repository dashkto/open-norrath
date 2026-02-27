package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{Game, ZoneSession}
import opennorrath.state.PlayerCharacter
import opennorrath.network.*
import opennorrath.ui.Colors

/** Loading screen shown during zone entry handshake.
  *
  * Polls ZoneClient state directly instead of consuming events, so that
  * all ZoneEvents remain buffered in the queue for ZoneScreen to drain
  * on its first dispatchEvents() call. This prevents late-listener bugs.
  *
  * Used from:
  *   - CharacterSelectScreen (initial zone-in after login)
  *   - ZoneScreen (zone-to-zone transitions via zone lines, gate, etc.)
  */
class ZoneLoadingScreen(
  ctx: GameContext,
  zoneAddr: ZoneAddress,
  charName: String,
) extends Screen:

  private var statusText = s"Connecting to zone..."
  private var statusColor = Colors.text
  private var transitioned = false
  private var lastState: ZoneState = ZoneState.Disconnected

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

    // Stop old zone session if zoning between zones
    Game.zoneSession.foreach(_.stop())
    Game.zoneSession = None

    // Create zone session and connect
    val zc = ZoneClient()
    val znt = NetworkThread(zc)
    Game.zoneSession = Some(ZoneSession(zc, znt))
    znt.start()
    znt.send(NetCommand.Connect(zoneAddr.ip, zoneAddr.port))
    zc.connect(charName)

  override def update(dt: Float): Unit =
    if transitioned then return
    Game.zoneSession.foreach { session =>
      val zc = session.client
      val st = zc.state

      // Create PlayerCharacter when profile arrives
      if zc.profile.isDefined && Game.player.isEmpty then
        val pp = zc.profile.get
        statusText = s"Loading ${pp.name}..."
        val pc = PlayerCharacter(
          name = pp.name,
          level = pp.level,
          race = pp.race,
          classId = pp.classId,
          currentHp = pp.curHp,
          maxHp = pp.curHp,
          currentMana = pp.mana,
          maxMana = pp.mana,
          str = pp.str, sta = pp.sta, agi = pp.agi, dex = pp.dex,
          wis = pp.wis, int = pp.int_, cha = pp.cha,
        )
        pc.loadBuffs(pp.buffs)
        pc.spellBook ++= pp.spellBook
        Game.player = Some(pc)

      // Update status text from zone info
      if zc.zoneInfo.isDefined && st != lastState then
        statusText = s"Entering ${zc.zoneInfo.get.zoneLongName}..."

      lastState = st

      st match
        case ZoneState.InZone =>
          val shortName = zc.zoneInfo.map(_.zoneShortName).getOrElse("arena")
          val zonePath = s"assets/EverQuest/$shortName.s3d"
          transitioned = true
          Game.setScreen(ZoneScreen(ctx, zonePath, zc.selfSpawn, zc.profile))
        case ZoneState.Failed =>
          statusText = "Zone connection failed"
          statusColor = Colors.error
          Game.zoneSession.foreach(_.stop())
          Game.zoneSession = None
        case _ => ()

      // Check for network errors (peek without consuming — they'll become ZoneEvent.Error)
      val err = Option(zc.errors.peek())
      err.foreach { msg =>
        statusText = s"Zone: $msg"
        statusColor = Colors.error
      }
    }

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##zoneloading", flags)

    val titleText = "LOADING"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, h / 2f - 40f)
    ImGui.pushStyleColor(ImGuiCol.Text, Colors.cream._1, Colors.cream._2, Colors.cream._3, Colors.cream._4)
    ImGui.text(titleText)
    ImGui.popStyleColor()

    val statusW = ImGui.calcTextSize(statusText).x
    ImGui.setCursorPos((w - statusW) / 2f, h / 2f + 10f)
    ImGui.pushStyleColor(ImGuiCol.Text, statusColor._1, statusColor._2, statusColor._3, statusColor._4)
    ImGui.text(statusText)
    ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit = ()
