package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f, Vector4f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f

import imgui.ImGui

import opennorrath.{Game, GameAction}
import opennorrath.render.Shader
import opennorrath.state.ZoneCharacter
import opennorrath.world.{CameraController, EqCoords, SpellEffectSystem, TargetingSystem, ZoneRenderer}
import opennorrath.network.{PlayerPosition, PlayerProfileData, SpawnData, ZoneEvent}
import opennorrath.ui.{NameplateRenderer, ZoneHud}

class ZoneScreen(ctx: GameContext, zonePath: String, selfSpawn: Option[SpawnData] = None, profile: Option[PlayerProfileData] = None) extends Screen:

  private var shader: Shader = uninitialized
  private var zone: ZoneRenderer = uninitialized
  private var camCtrl: CameraController = uninitialized
  private val model = Matrix4f()
  private val zoneCharacters = scala.collection.mutable.Map[Int, ZoneCharacter]()
  private val nameplateRenderer = NameplateRenderer()
  private val targeting = TargetingSystem()
  private val hud = ZoneHud(ctx, zoneCharacters)
  private val spellEffects = SpellEffectSystem()
  private var posUpdateTimer = 0f
  private val PosUpdateInterval = 0.4f   // ~2.5Hz while moving
  private var lastSentHeading = -1f       // track heading changes for idle sends
  private val lastSentPos = Vector3f()    // track position changes

  // --- Spawn rendering ---

  private val spawnListener: ZoneEvent => Unit =
    case ZoneEvent.SpawnAdded(s) =>
      val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(0)
      if s.spawnId != myId then
        ZoneCharacter.fromSpawn(s) match
          case Some(zc) =>
            zoneCharacters(s.spawnId) = zc
            if zone.addSpawn(s.spawnId, zc.modelCode, zc.position, zc.heading, zc.size) then
              zone.updateSpawnEquipment(s.spawnId, zc.equipment, zc.bodyTexture)
          case None => ()
    case ZoneEvent.SpawnRemoved(id) =>
      zoneCharacters.remove(id)
      zone.removeSpawn(id)
    case ZoneEvent.SpawnMoved(upd) =>
      val pos = EqCoords.serverToGl(upd.y, upd.x, upd.z)
      val isMoving = upd.animType != 0
      zoneCharacters.get(upd.spawnId).foreach { zc =>
        zc.onServerPositionUpdate(pos, upd.heading, isMoving, upd.animType)
      }
      // Only update rendered position if actually moving — MobUpdate uses short
      // integers for x/y, so idle NPCs would snap from their float spawn position.
      if isMoving then
        zone.updateSpawnPosition(upd.spawnId, pos, upd.heading)
      zone.updateSpawnAnimation(upd.spawnId, moving = isMoving, upd.animType)
    case ZoneEvent.HPChanged(hp) =>
      zoneCharacters.get(hp.spawnId).foreach { zc =>
        zc.curHp = hp.curHp
        zc.maxHp = hp.maxHp
      }
    case ZoneEvent.EquipmentChanged(wc) =>
      zoneCharacters.get(wc.spawnId).foreach { zc =>
        zc.updateEquipment(wc.wearSlot, wc.material, wc.color)
        zone.updateSpawnEquipment(wc.spawnId, zc.equipment, zc.bodyTexture)
      }
    case _ => ()

  private val spellListener: ZoneEvent => Unit =
    case ZoneEvent.SpellActionTriggered(action) if action.spellId > 0 =>
      spellEffects.trigger(action.targetId, action.spellId)
    case _ => ()

  private val MaxTabTargetDist = 200f

  /** Resolve tab target using CPU raycasting against zone collision mesh.
    * For each candidate spawn within distance and frustum, cast rays from camera
    * to the spawn's AABB vertices. If any ray is unblocked, the spawn is visible.
    */
  private def resolveTabTarget(): Unit =
    val projView = Matrix4f(camCtrl.projection).mul(camCtrl.viewMatrix)
    val clip = Vector4f()
    val cam = camCtrl.camera.position

    val hitboxes = zone.spawnHitData.map { case (id, mat, h, w, d) => id -> (mat, h, w, d) }.toMap
    val target = Vector3f()

    val visible = zoneCharacters.toVector.flatMap { (id, zc) =>
      val dx = zc.position.x - cam.x
      val dy = zc.position.y - cam.y
      val dz = zc.position.z - cam.z
      val distSq = dx * dx + dy * dy + dz * dz
      if distSq > MaxTabTargetDist * MaxTabTargetDist then None
      else
        clip.set(zc.position.x, zc.position.y, zc.position.z, 1f)
        projView.transform(clip)
        if clip.w <= 0f then None
        else
          val ndcX = clip.x / clip.w
          val ndcY = clip.y / clip.w
          if ndcX < -1f || ndcX > 1f || ndcY < -1f || ndcY > 1f then None
          else
            // LOS: cast rays to AABB center + 8 corners; visible if any unblocked
            val canSee = hitboxes.get(id) match
              case Some((mat, h, w, d)) =>
                val cx = mat.m30(); val cy = mat.m31(); val cz = mat.m32()
                val hw = Math.max(w, d) * 0.6f
                target.set(cx, cy, cz)
                if !zone.collision.rayBlocked(cam, target) then true
                else
                  val y0 = cy - h * 0.5f; val y1 = cy + h * 0.5f
                  var found = false
                  var ci = 0
                  while ci < 8 && !found do
                    target.set(
                      if (ci & 1) == 0 then cx - hw else cx + hw,
                      if (ci & 2) == 0 then y0 else y1,
                      if (ci & 4) == 0 then cz - hw else cz + hw,
                    )
                    if !zone.collision.rayBlocked(cam, target) then found = true
                    ci += 1
                  found
              case None =>
                !zone.collision.rayBlocked(cam, zc.position)
            if canSee then Some((id, distSq)) else None
    }.sortBy(_._2).map(_._1)

    if visible.isEmpty then return
    val currentId = hud.target.map(_.spawnId)
    val curIdx = currentId.flatMap(id => visible.indexOf(id) match { case -1 => None; case i => Some(i) })
    val nextIdx = curIdx.map(i => (i + 1) % visible.size).getOrElse(0)
    hud.target = zoneCharacters.get(visible(nextIdx))

  private def dumpPositions(): Unit =
    val cp = camCtrl.camera.position
    println(f"[F9 Debug] Camera: gl(${cp.x}%.2f, ${cp.y}%.2f, ${cp.z}%.2f) yaw=${camCtrl.camera.yaw}%.1f")
    for (id, mat, height) <- zone.spawnNameplateData.take(10) do
      val wx = mat.m30(); val wy = mat.m31(); val wz = mat.m32()
      val dist = Math.sqrt((cp.x - wx) * (cp.x - wx) + (cp.y - wy) * (cp.y - wy) + (cp.z - wz) * (cp.z - wz))
      println(f"  Spawn $id: gl($wx%.2f, $wy%.2f, $wz%.2f) dist=${dist}%.1f height=$height%.1f")

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f)

    hud.init()
    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderer(zonePath, ctx.settings)
    camCtrl = CameraController(ctx.window, ctx.windowWidth, ctx.windowHeight)
    camCtrl.player = Game.player
    camCtrl.initFromSpawn(selfSpawn, profile)
    println(s"Loading zone: $zonePath")

    // Load initial spawns (SpawnsLoaded was consumed by ZoneLoadingScreen)
    Game.zoneSession.foreach { session =>
      val zc = session.client
      zc.addListener(spawnListener)
      zc.addListener(spellListener)
      Game.player.foreach { pc =>
        zc.addListener(pc.listener)
        if zc.inventory.nonEmpty then
          pc.listener(ZoneEvent.InventoryLoaded(zc.inventory))
      }
      for s <- zc.spawns.values do spawnListener(ZoneEvent.SpawnAdded(s))
      // Add the player's own character model using the real spawn ID
      val myId = zc.mySpawnId
      selfSpawn.foreach { s =>
        ZoneCharacter.fromSpawn(s).foreach { playerZc =>
          zoneCharacters(myId) = playerZc
          if zone.addSpawn(myId, playerZc.modelCode, playerZc.position, playerZc.heading, playerZc.size) then
            zone.updateSpawnEquipment(myId, playerZc.equipment, playerZc.bodyTexture)
        }
      }
      println(s"  Loaded ${zoneCharacters.size} / ${zc.spawns.size} initial spawns")
    }

    // Animation test: spawn fallback races in a line to verify animation matches
    if zonePath.toLowerCase.contains("arena") then
      val testPos = camCtrl.camera.position
      import opennorrath.animation.AnimatedCharacter
      val fallbackModels = AnimatedCharacter.animFallbacks.toList.sortBy(_._1).map(_._1.toLowerCase)
      for (model, idx) <- fallbackModels.zipWithIndex do
        val spawnId = 900000 + idx
        val pos = org.joml.Vector3f(testPos.x + idx * 15f, testPos.y, testPos.z + 20f)
        if zone.addSpawn(spawnId, model, pos, 0, 6f) then
          zone.updateSpawnAnimation(spawnId, moving = true, animType = 1)
          val fb = AnimatedCharacter.animFallbacks.getOrElse(model.toUpperCase, "?")
          zoneCharacters(spawnId) = ZoneCharacter(
            spawnId = spawnId, name = s"$model→$fb", lastName = "", race = 1, classId = 1,
            gender = 0, level = 1, npcType = 1, modelCode = model, size = 6f,
            position = pos, heading = 0,
          )

  override def update(dt: Float): Unit =
    Game.zoneSession.foreach(_.client.dispatchEvents())

    // Client-side interpolation: advance moving spawns along their velocity
    for zc <- zoneCharacters.values if zc.moving do
      zc.interpolate(dt)
      zone.updateSpawnPosition(zc.spawnId, zc.position, zc.facingHeading)

    spellEffects.update(dt, camCtrl.viewMatrix, zoneCharacters)

    hud.update(ctx.input)
    if hud.isEscapeOpen then return

    camCtrl.update(ctx.input, dt)

    // Update player model position/heading to match camera controller
    if camCtrl.attached then
      Game.zoneSession.foreach { session =>
        val myId = session.client.mySpawnId
        zone.updateSpawnPosition(myId, camCtrl.playerPos, camCtrl.playerHeading)
        zone.updateSpawnAnimation(myId, moving = camCtrl.playerMoving, animType = if camCtrl.playerMoving then 1 else 0)
      }

    // Send player position to server when attached — only if something changed
    if camCtrl.attached then
      posUpdateTimer += dt
      if posUpdateTimer >= PosUpdateInterval then
        val pp = camCtrl.playerPos
        val hdeg = camCtrl.playerHeadingDeg
        val posChanged = pp.distanceSquared(lastSentPos) > 0.01f
        val headingChanged = Math.abs(hdeg - lastSentHeading) > 1f
        if posChanged || headingChanged then
          posUpdateTimer = 0f
          lastSentPos.set(pp)
          lastSentHeading = hdeg
          Game.zoneSession.foreach { session =>
            val (sy, sx, sz) = EqCoords.glToServer(pp.x, pp.y, pp.z)
            session.client.sendPosition(PlayerPosition(
              spawnId = session.client.mySpawnId,
              y = sy, x = sx, z = sz,
              heading = hdeg,
              deltaY = 0, deltaX = 0, deltaZ = 0, deltaHeading = 0,
              animation = if camCtrl.playerMoving then 1 else 0,
            ))
          }
        else
          posUpdateTimer = PosUpdateInterval

    // Left-click targeting — only change target if we hit something
    if ctx.input.isActionPressed(GameAction.Target) && !camCtrl.freeLook && !ImGui.getIO().getWantCaptureMouse() then
      val (mx, my) = ctx.input.mousePos
      targeting.pickSpawn(mx, my, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat,
        camCtrl.projection, camCtrl.viewMatrix, zone.spawnHitData).foreach { id =>
        hud.target = zoneCharacters.get(id)
      }

    // Tab target — raycast against zone collision mesh for LOS
    if ctx.input.isActionPressed(GameAction.TabTarget) && !ImGui.getIO().getWantTextInput() then
      resolveTabTarget()

    if !ImGui.getIO().getWantCaptureKeyboard() then
      if ctx.input.isActionPressed(GameAction.DumpDebug) then
        dumpPositions()

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", camCtrl.projection)
    shader.setMatrix4f("view", camCtrl.viewMatrix)
    shader.setMatrix4f("model", model)

    zone.draw(shader, dt, camCtrl.viewMatrix)

    // Target hitbox wireframe
    hud.target.foreach { zc =>
      targeting.drawTargetHitbox(zc.spawnId, shader, model, zone.spawnHitData)
    }

    // Spell particle effects (additive blending, before nameplates)
    spellEffects.draw(shader)

    // Nameplates: billboarded 3D quads with depth test (occluded by geometry)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    shader.setFloat("alphaMultiplier", 1.0f)
    glVertexAttrib3f(2, 1f, 1f, 1f)
    nameplateRenderer.draw(shader, camCtrl.viewMatrix, zoneCharacters, zone.spawnNameplateData, hud.target.map(_.spawnId))
    glDepthMask(true)
    glDisable(GL_BLEND)

    hud.render()

  override def dispose(): Unit =
    hud.dispose()
    Game.zoneSession.foreach { session =>
      session.client.removeListener(spawnListener)
      session.client.removeListener(spellListener)
      Game.player.foreach(pc => session.client.removeListener(pc.listener))
    }
    spellEffects.cleanup()
    nameplateRenderer.cleanup()
    targeting.cleanup()
    zone.cleanup()
    shader.cleanup()
