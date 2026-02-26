package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.{glEnableVertexAttribArray, glVertexAttrib3f, glVertexAttribPointer}
import org.lwjgl.opengl.GL30.{glBindVertexArray, glDeleteVertexArrays, glGenVertexArrays}

import imgui.ImGui

import opennorrath.{Camera, EqCoords, Game, Shader, ZoneRenderer}
import opennorrath.network.{PlayerProfileData, SpawnData, ZoneEvent}
import opennorrath.ui.{CharacterInfoPanel, EqData, EscapeMenu, InventoryPanel, NameplateRenderer, TargetPanel, TextPanel, ZoneEventHandler}

class ZoneScreen(ctx: GameContext, zonePath: String, selfSpawn: Option[SpawnData] = None, profile: Option[PlayerProfileData] = None) extends Screen:

  private var shader: Shader = uninitialized
  private var zone: ZoneRenderer = uninitialized
  private var camera: Camera = uninitialized
  private var projection: Matrix4f = uninitialized
  private val model = Matrix4f()
  private val charInfoPanel = Game.playerState.map(CharacterInfoPanel(_))
  private val escapeMenu = EscapeMenu(ctx)
  private val inventoryPanel = InventoryPanel()
  private val nameplateRenderer = NameplateRenderer()
  private val targetPanel = TargetPanel()
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

  /** Cast a ray from the camera through the mouse position and return the closest hit spawn ID. */
  private def pickSpawn(): Option[Int] =
    val (mx, my) = ctx.input.mousePos
    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat

    // Unproject mouse to world-space ray via JOML (unprojectRay inverts internally)
    val projView = Matrix4f(projection).mul(camera.viewMatrix)
    val rayOrigin = Vector3f()
    val rayDir = Vector3f()
    projView.unprojectRay(mx, h - my, Array(0, 0, w.toInt, h.toInt), rayOrigin, rayDir)

    var bestId: Option[Int] = None
    var bestDist = Float.MaxValue

    for (spawnId, modelMatrix, height, width, depth) <- zone.spawnHitData do
      val cx = modelMatrix.m30()
      val cy = modelMatrix.m31()
      val cz = modelMatrix.m32()

      val hw = Math.max(width, depth) * 0.6f
      val minX = cx - hw
      val maxX = cx + hw
      val minY = cy - height * 0.5f
      val maxY = cy + height * 0.5f
      val minZ = cz - hw
      val maxZ = cz + hw

      rayAABBIntersect(rayOrigin, rayDir, minX, minY, minZ, maxX, maxY, maxZ) match
        case Some(t) if t < bestDist =>
          bestDist = t
          bestId = Some(spawnId)
        case _ =>

    bestId

  // Wireframe hitbox resources (lazy init)
  private var hitboxVao = 0
  private var hitboxVbo = 0
  private var hitboxInited = false
  private val hitboxBuf = BufferUtils.createFloatBuffer(24 * 3) // 24 vertices (12 lines * 2)

  /** Draw a faint wireframe AABB around the targeted spawn. */
  private def drawTargetHitbox(spawnId: Int): Unit =
    zone.spawnHitData.find(_._1 == spawnId).foreach { (_, modelMatrix, height, width, depth) =>
      val cx = modelMatrix.m30()
      val cy = modelMatrix.m31()
      val cz = modelMatrix.m32()
      val hw = Math.max(width, depth) * 0.6f

      val x0 = cx - hw; val x1 = cx + hw
      val y0 = cy - height * 0.5f; val y1 = cy + height * 0.5f
      val z0 = cz - hw;  val z1 = cz + hw

      if !hitboxInited then
        hitboxVao = glGenVertexArrays()
        hitboxVbo = glGenBuffers()
        glBindVertexArray(hitboxVao)
        glBindBuffer(GL_ARRAY_BUFFER, hitboxVbo)
        glBufferData(GL_ARRAY_BUFFER, 24L * 3 * 4, GL_DYNAMIC_DRAW)
        glVertexAttribPointer(0, 3, GL_FLOAT, false, 3 * 4, 0)
        glEnableVertexAttribArray(0)
        glBindVertexArray(0)
        hitboxInited = true

      // Fill line vertices
      hitboxBuf.clear()
      // Bottom face
      hitboxBuf.put(x0).put(y0).put(z0); hitboxBuf.put(x1).put(y0).put(z0)
      hitboxBuf.put(x1).put(y0).put(z0); hitboxBuf.put(x1).put(y0).put(z1)
      hitboxBuf.put(x1).put(y0).put(z1); hitboxBuf.put(x0).put(y0).put(z1)
      hitboxBuf.put(x0).put(y0).put(z1); hitboxBuf.put(x0).put(y0).put(z0)
      // Top face
      hitboxBuf.put(x0).put(y1).put(z0); hitboxBuf.put(x1).put(y1).put(z0)
      hitboxBuf.put(x1).put(y1).put(z0); hitboxBuf.put(x1).put(y1).put(z1)
      hitboxBuf.put(x1).put(y1).put(z1); hitboxBuf.put(x0).put(y1).put(z1)
      hitboxBuf.put(x0).put(y1).put(z1); hitboxBuf.put(x0).put(y1).put(z0)
      // Vertical edges
      hitboxBuf.put(x0).put(y0).put(z0); hitboxBuf.put(x0).put(y1).put(z0)
      hitboxBuf.put(x1).put(y0).put(z0); hitboxBuf.put(x1).put(y1).put(z0)
      hitboxBuf.put(x1).put(y0).put(z1); hitboxBuf.put(x1).put(y1).put(z1)
      hitboxBuf.put(x0).put(y0).put(z1); hitboxBuf.put(x0).put(y1).put(z1)
      hitboxBuf.flip()

      glBindBuffer(GL_ARRAY_BUFFER, hitboxVbo)
      glBufferSubData(GL_ARRAY_BUFFER, 0, hitboxBuf)

      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glDisable(GL_DEPTH_TEST)
      shader.setMatrix4f("model", model) // identity
      glVertexAttrib3f(2, 0.4f, 0.8f, 1f) // faint cyan
      shader.setFloat("alphaMultiplier", 0.3f)

      glBindVertexArray(hitboxVao)
      glDrawArrays(GL_LINES, 0, 24)
      glBindVertexArray(0)

      shader.setFloat("alphaMultiplier", 1.0f)
      glEnable(GL_DEPTH_TEST)
      glDisable(GL_BLEND)
    }

  /** Ray-AABB intersection test. Returns Some(t) for closest hit distance, None if miss. */
  private def rayAABBIntersect(origin: Vector3f, dir: Vector3f,
                               minX: Float, minY: Float, minZ: Float,
                               maxX: Float, maxY: Float, maxZ: Float): Option[Float] =
    var tmin = Float.MinValue
    var tmax = Float.MaxValue

    // X slab
    if Math.abs(dir.x) > 1e-6f then
      val t1 = (minX - origin.x) / dir.x
      val t2 = (maxX - origin.x) / dir.x
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.x < minX || origin.x > maxX then return None

    // Y slab
    if Math.abs(dir.y) > 1e-6f then
      val t1 = (minY - origin.y) / dir.y
      val t2 = (maxY - origin.y) / dir.y
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.y < minY || origin.y > maxY then return None

    // Z slab
    if Math.abs(dir.z) > 1e-6f then
      val t1 = (minZ - origin.z) / dir.z
      val t2 = (maxZ - origin.z) / dir.z
      tmin = Math.max(tmin, Math.min(t1, t2))
      tmax = Math.min(tmax, Math.max(t1, t2))
    else if origin.z < minZ || origin.z > maxZ then return None

    if tmin <= tmax && tmax >= 0f then Some(Math.max(tmin, 0f))
    else None

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
      zc.addListener(inventoryPanel.listener)
      for s <- zc.spawns.values do addSpawnIfKnown(s)
      println(s"  Rendered ${zc.spawns.size} initial spawns")
      // Load inventory that arrived during zone entry handshake
      if zc.inventory.nonEmpty then
        inventoryPanel.listener(ZoneEvent.InventoryLoaded(zc.inventory))
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
      pickSpawn().foreach { id =>
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
      drawTargetHitbox(s.spawnId)
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
      session.client.removeListener(inventoryPanel.listener)
    }
    nameplateRenderer.cleanup()
    if hitboxInited then
      glDeleteBuffers(hitboxVbo)
      glDeleteVertexArrays(hitboxVao)
    zone.cleanup()
    shader.cleanup()
