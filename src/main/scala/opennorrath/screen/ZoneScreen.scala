package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f

import imgui.ImGui

import opennorrath.Game
import opennorrath.render.Shader
import opennorrath.world.{Camera, EqCoords, TargetingSystem, ZoneRenderer}
import opennorrath.network.{PlayerProfileData, SpawnData, ZoneEvent}
import opennorrath.ui.{CharacterInfoPanel, EqData, EscapeMenu, InventoryPanel, NameplateRenderer, TargetPanel, TextPanel, ZoneEventHandler}

class ZoneScreen(ctx: GameContext, zonePath: String, selfSpawn: Option[SpawnData] = None, profile: Option[PlayerProfileData] = None) extends Screen:

  private var shader: Shader = uninitialized
  private var zone: ZoneRenderer = uninitialized
  private var camera: Camera = uninitialized
  private var projection: Matrix4f = uninitialized
  private val model = Matrix4f()
  private val charInfoPanel = Game.player.map(CharacterInfoPanel(_))
  private val escapeMenu = EscapeMenu(ctx)
  private val inventoryPanel = InventoryPanel()
  private val nameplateRenderer = NameplateRenderer()
  private val targetPanel = TargetPanel()
  private val targeting = TargetingSystem()
  private var chatPanel: TextPanel = null
  private var eventHandler: ZoneEventHandler = null
  private def initChat(): Unit =
    chatPanel = TextPanel("Main", 10f, 500f, onSubmit = text => {
      eventHandler.submitChat(text)
    })
    eventHandler = ZoneEventHandler(chatPanel)
    Game.zoneSession.foreach(_.client.addListener(eventHandler.listener))

  // --- Spawn rendering ---

  private val spawnListener: ZoneEvent => Unit =
    case ZoneEvent.SpawnAdded(s) => addSpawnIfKnown(s)
    case ZoneEvent.SpawnRemoved(id) =>
      zone.removeSpawn(id)
      if targetPanel.target.exists(_.spawnId == id) then targetPanel.target = None
    case ZoneEvent.SpawnMoved(upd) =>
      val pos = EqCoords.serverToGl(upd.y, upd.x, upd.z)
      println(f"  [SpawnMoved] id=${upd.spawnId} server(y=${upd.y}%.1f x=${upd.x}%.1f z=${upd.z}%.1f) gl(${pos.x}%.1f,${pos.y}%.1f,${pos.z}%.1f) anim=${upd.animType}")
      zone.updateSpawnPosition(upd.spawnId, pos, upd.heading)
      zone.updateSpawnAnimation(upd.spawnId, moving = upd.animType != 0, upd.animType)
    case ZoneEvent.HPChanged(hp) =>
      targetPanel.updateHp(hp.spawnId, hp.curHp, hp.maxHp)
    case _ => ()

  private def addSpawnIfKnown(s: SpawnData): Unit =
    val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(0)
    if s.spawnId == myId then return
    EqData.raceModelCode(s.race, s.gender) match
      case Some(code) =>
        val pos = EqCoords.serverToGl(s.y, s.x, s.z)
        println(f"  [AddSpawn] ${s.name} id=${s.spawnId} server(y=${s.y}%.1f x=${s.x}%.1f z=${s.z}%.1f) gl(${pos.x}%.1f,${pos.y}%.1f,${pos.z}%.1f) heading=${s.heading}")
        if !zone.addSpawn(s.spawnId, code, pos, s.heading, s.size) then
          println(s"  No model for ${s.name} (code=$code race=${s.race})")
      case None =>
        println(s"  Unknown race for ${s.name} (race=${s.race} gender=${s.gender})")

  private def dumpPositions(): Unit =
    val cp = camera.position
    println(f"[F9 Debug] Camera: gl(${cp.x}%.2f, ${cp.y}%.2f, ${cp.z}%.2f) yaw=${camera.yaw}%.1f")
    for (id, mat, height) <- zone.spawnNameplateData.take(10) do
      // Extract translation column from model matrix
      val wx = mat.m30(); val wy = mat.m31(); val wz = mat.m32()
      val dist = Math.sqrt((cp.x - wx) * (cp.x - wx) + (cp.y - wy) * (cp.y - wy) + (cp.z - wz) * (cp.z - wz))
      println(f"  Spawn $id: gl($wx%.2f, $wy%.2f, $wz%.2f) dist=${dist}%.1f height=$height%.1f")

  private var freeLook = false

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f)

    initChat()
    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderer(zonePath, ctx.settings)

    val EyeHeight = 6f
    val (startPos, startYaw) = selfSpawn match
      case Some(s) =>
        val pos = EqCoords.serverToGl(s.y, s.x, s.z)
        pos.y += EyeHeight
        (pos, EqCoords.spawnHeadingToYaw(s.heading))
      case None => profile match
        case Some(pp) =>
          val pos = EqCoords.serverToGl(pp.y, pp.x, pp.z)
          pos.y += EyeHeight
          (pos, EqCoords.profileHeadingToYaw(pp.heading))
        case None =>
          (Vector3f(-150f, 50f, -460f), 30f)
    camera = Camera(
      position = startPos,
      yaw = startYaw,
      pitch = -5f,
      speed = 100f,
    )
    projection = Matrix4f().perspective(
      Math.toRadians(60.0).toFloat,
      ctx.windowWidth.toFloat / ctx.windowHeight.toFloat,
      0.1f,
      10000f,
    )
    println(s"Loading zone: $zonePath, camera: pos=$startPos yaw=$startYaw")

    // Load initial spawns (SpawnsLoaded was consumed by ZoneLoadingScreen)
    Game.zoneSession.foreach { session =>
      val zc = session.client
      zc.addListener(spawnListener)
      Game.player.foreach { pc =>
        zc.addListener(pc.listener)
        // Load inventory that arrived during zone entry handshake
        if zc.inventory.nonEmpty then
          pc.listener(ZoneEvent.InventoryLoaded(zc.inventory))
      }
      for s <- zc.spawns.values do addSpawnIfKnown(s)
      println(s"  Rendered ${zc.spawns.size} initial spawns")
    }

  override def update(dt: Float): Unit =
    Game.zoneSession.foreach(_.client.dispatchEvents())

    if ctx.input.isKeyPressed(GLFW_KEY_ESCAPE) then
      if targetPanel.target.isDefined then
        targetPanel.target = None
      else
        escapeMenu.toggle()

    if escapeMenu.isOpen then return

    val rightHeld = ctx.input.isMouseHeld(GLFW_MOUSE_BUTTON_RIGHT)
    if rightHeld != freeLook then
      freeLook = rightHeld
      if freeLook then
        glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
      else
        glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

    // Left-click targeting — only change target if we hit something
    if ctx.input.isMousePressed(GLFW_MOUSE_BUTTON_LEFT) && !freeLook && !ImGui.getIO().getWantCaptureMouse() then
      val (mx, my) = ctx.input.mousePos
      targeting.pickSpawn(mx, my, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat,
        projection, camera.viewMatrix, zone.spawnHitData).foreach { id =>
        targetPanel.target = Game.zoneSession.flatMap(_.client.spawns.get(id))
      }

    if !ImGui.getIO().getWantCaptureKeyboard() then
      if ctx.input.isKeyPressed(GLFW_KEY_T) then
        dumpPositions()
      if ctx.input.isKeyPressed(GLFW_KEY_I) then
        inventoryPanel.toggle()
      camera.processMovement(ctx.input, dt)
    if freeLook then
      camera.processLook(ctx.input)

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", projection)
    shader.setMatrix4f("view", camera.viewMatrix)
    shader.setMatrix4f("model", model)

    zone.draw(shader, dt, camera.viewMatrix)

    // Target hitbox wireframe
    targetPanel.target.foreach { s =>
      targeting.drawTargetHitbox(s.spawnId, shader, model, zone.spawnHitData)
    }

    // Nameplates: billboarded 3D quads with depth test (occluded by geometry)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false) // don't write to depth buffer
    shader.setFloat("alphaMultiplier", 1.0f)
    glVertexAttrib3f(2, 1f, 1f, 1f) // white vertex color
    Game.zoneSession.foreach { session =>
      nameplateRenderer.draw(shader, camera.viewMatrix, session.client.spawns, zone.spawnNameplateData)
    }
    glDepthMask(true)
    glDisable(GL_BLEND)

    charInfoPanel.foreach(_.render())
    targetPanel.render()
    inventoryPanel.render()
    chatPanel.render()
    escapeMenu.render()

  override def dispose(): Unit =
    Game.zoneSession.foreach { session =>
      session.client.removeListener(eventHandler.listener)
      session.client.removeListener(spawnListener)
      Game.player.foreach(pc => session.client.removeListener(pc.listener))
    }
    nameplateRenderer.cleanup()
    targeting.cleanup()
    zone.cleanup()
    shader.cleanup()
