package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f

import imgui.ImGui

import opennorrath.{Game, GameAction}
import opennorrath.render.Shader
import opennorrath.state.ZoneCharacter
import opennorrath.world.{CameraController, EqCoords, TargetingSystem, ZoneRenderer}
import opennorrath.network.{PlayerProfileData, SpawnData, ZoneEvent}
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
            else
              println(s"  No model for ${zc.name} (code=${zc.modelCode} race=${zc.race})")
          case None =>
            println(s"  Unknown race for ${s.name} (race=${s.race} gender=${s.gender})")
    case ZoneEvent.SpawnRemoved(id) =>
      zoneCharacters.remove(id)
      zone.removeSpawn(id)
    case ZoneEvent.SpawnMoved(upd) =>
      val pos = EqCoords.serverToGl(upd.y, upd.x, upd.z)
      val isMoving = upd.animType != 0
      zoneCharacters.get(upd.spawnId).foreach { zc =>
        zc.onServerPositionUpdate(pos, upd.heading, isMoving, upd.animType)
      }
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
    camCtrl.initFromSpawn(selfSpawn, profile)
    println(s"Loading zone: $zonePath")

    // Load initial spawns (SpawnsLoaded was consumed by ZoneLoadingScreen)
    Game.zoneSession.foreach { session =>
      val zc = session.client
      zc.addListener(spawnListener)
      Game.player.foreach { pc =>
        zc.addListener(pc.listener)
        if zc.inventory.nonEmpty then
          pc.listener(ZoneEvent.InventoryLoaded(zc.inventory))
      }
      for s <- zc.spawns.values do spawnListener(ZoneEvent.SpawnAdded(s))
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
      zone.updateSpawnPosition(zc.spawnId, zc.position, zc.heading)

    hud.update(ctx.input)
    if hud.isEscapeOpen then return

    camCtrl.update(ctx.input, dt)

    // Left-click targeting — only change target if we hit something
    if ctx.input.isActionPressed(GameAction.Target) && !camCtrl.freeLook && !ImGui.getIO().getWantCaptureMouse() then
      val (mx, my) = ctx.input.mousePos
      targeting.pickSpawn(mx, my, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat,
        camCtrl.projection, camCtrl.viewMatrix, zone.spawnHitData).foreach { id =>
        hud.target = zoneCharacters.get(id)
      }

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

    // Nameplates: billboarded 3D quads with depth test (occluded by geometry)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    shader.setFloat("alphaMultiplier", 1.0f)
    glVertexAttrib3f(2, 1f, 1f, 1f)
    nameplateRenderer.draw(shader, camCtrl.viewMatrix, zoneCharacters, zone.spawnNameplateData)
    glDepthMask(true)
    glDisable(GL_BLEND)

    hud.render()

  override def dispose(): Unit =
    hud.dispose()
    Game.zoneSession.foreach { session =>
      session.client.removeListener(spawnListener)
      Game.player.foreach(pc => session.client.removeListener(pc.listener))
    }
    nameplateRenderer.cleanup()
    targeting.cleanup()
    zone.cleanup()
    shader.cleanup()
