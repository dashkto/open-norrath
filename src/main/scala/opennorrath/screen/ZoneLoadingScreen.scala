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
  * Handles:
  *   - Creating the ZoneSession and connecting to the zone server
  *   - Listening for ZoneEvents until zone entry is complete (InZone)
  *   - Populating PlayerCharacter from the player profile
  *   - Transitioning to ZoneScreen with zone path and camera position
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

  private val listener: ZoneEvent => Unit = {
    case ZoneEvent.ProfileReceived(pp) =>
      statusText = s"Loading ${pp.name}..."
      Game.player = Some(PlayerCharacter(
        name = pp.name,
        level = pp.level,
        classId = pp.classId,
        currentHp = pp.curHp,
        maxHp = pp.curHp,
        currentMana = pp.mana,
        maxMana = pp.mana,
      ))
    case ZoneEvent.ZoneDataReceived(nz) =>
      statusText = s"Entering ${nz.zoneLongName}..."
    case ZoneEvent.StateChanged(ZoneState.InZone) =>
      Game.zoneSession.foreach { session =>
        val zc = session.client
        val shortName = zc.zoneInfo.map(_.zoneShortName).getOrElse("arena")
        val zonePath = s"assets/EverQuest/$shortName.s3d"
        println(s"[ZoneLoading] Zone ready: $zonePath")
        transitioned = true
        Game.setScreen(ZoneScreen(ctx, zonePath, zc.selfSpawn, zc.profile))
      }
    case ZoneEvent.StateChanged(ZoneState.Failed) =>
      statusText = "Zone connection failed"
      statusColor = Colors.error
      Game.zoneSession.foreach(_.stop())
      Game.zoneSession = None
    case ZoneEvent.Error(msg) =>
      statusText = s"Zone: $msg"
      statusColor = Colors.error
    case _ => ()
  }

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

    // Create zone session and connect
    val zc = ZoneClient()
    zc.addListener(listener)
    val znt = NetworkThread(zc)
    Game.zoneSession = Some(ZoneSession(zc, znt))
    znt.start()
    znt.send(NetCommand.Connect(zoneAddr.ip, zoneAddr.port))
    zc.connect(charName)

  override def update(dt: Float): Unit =
    if transitioned then return
    Game.zoneSession.foreach(_.client.dispatchEvents())

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

  override def dispose(): Unit =
    Game.zoneSession.foreach(_.client.removeListener(listener))
