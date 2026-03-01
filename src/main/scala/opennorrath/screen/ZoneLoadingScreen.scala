package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiWindowFlags}

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.{Game, ZoneSession}
import opennorrath.network.*
import opennorrath.network.titanium.TitaniumNetworkThread
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
    val znt: EqNetworkThread =
      if Game.macMode then NetworkThread(zc)
      else TitaniumNetworkThread(zc)
    Game.zoneSession = Some(ZoneSession(zc, znt))
    znt.start()
    znt.send(NetCommand.Connect(zoneAddr.ip, zoneAddr.port))
    zc.connect(charName)

  override def update(dt: Float): Unit =
    if transitioned then return
    Game.zoneSession.foreach { session =>
      val zc = session.client

      // Zone server ACKs but never sends app data — tear down and create fresh connection.
      // The zone process may not have been ready; a new socket forces a clean session.
      if zc.needsReconnect then
        zc.needsReconnect = false
        val attempt = zc.reconnectAttempt
        statusText = s"Reconnecting to zone (attempt $attempt/${zc.MaxReconnectAttempts})..."
        session.stop()
        Game.zoneSession = None
        val newZc = ZoneClient()
        newZc.reconnectAttempt = attempt // preserve attempt count across reconnects
        val newZnt: EqNetworkThread =
          if Game.macMode then NetworkThread(newZc)
          else TitaniumNetworkThread(newZc)
        Game.zoneSession = Some(ZoneSession(newZc, newZnt))
        newZnt.start()
        newZnt.send(NetCommand.Connect(zoneAddr.ip, zoneAddr.port))
        newZc.connect(charName)
        return

      val st = zc.state

      // Update status text from profile/zone info
      if zc.profile.isDefined then
        statusText = s"Loading ${zc.profile.get.name}..."

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
          // Zone reconnects exhausted — fall back to character select so the user can
          // re-enter and trigger a fresh world→zone auth flow. The server's auth entry
          // may have been consumed by a prior failed connection attempt.
          statusText = "Zone connection failed — returning to character select..."
          statusColor = Colors.error
          Game.zoneSession.foreach(_.stop())
          Game.zoneSession = None
          transitioned = true
          Game.worldSession match
            case Some(ws) =>
              Game.setScreen(CharacterSelectScreen(ctx, ws.client.characters))
            case None =>
              Game.setScreen(LoginScreen(ctx))
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
