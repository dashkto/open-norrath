package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f, Vector4f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f

import imgui.ImGui

import opennorrath.{Game, GameAction, WorldSession}
import opennorrath.animation.AnimCode
import opennorrath.render.Shader
import opennorrath.state.{PlayerCharacter, ZoneCharacter}
import opennorrath.world.{CameraController, EqCoords, SpellEffectSystem, TargetingSystem, ZoneRenderer}
import opennorrath.network.{InventoryItem, NetCommand, NetworkThread, PlayerPosition, PlayerProfileData, SpawnData, WorldClient, WorldEvent, ZoneEvent, ZonePointData}
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
  private var player: Option[PlayerCharacter] = None
  private var posUpdateTimer = 0f
  private val PosUpdateInterval = 0.4f   // ~2.5Hz while moving
  private var lastSentHeading = -1f       // track heading changes for idle sends
  private val lastSentPos = Vector3f()    // track position changes
  private var wasAirborne = false          // track airborne state for fall animation
  private var jumpAnimTimer = 0f          // countdown for crouch animation on jump
  private var zoning = false              // true after server requests zone change
  private var zoneChangeAccepted = false  // true after OP_ZoneChange success=1 received
  private var zoneDeniedUntil = 0L       // cooldown (millis) after zone change denial
  private var zoningStartedAt = 0L       // millis when zoning started (for timeout)
  private val ZoningTimeoutMs = 15000L   // give up if world ZoneServerInfo never arrives
  private var zonePoints = Vector.empty[ZonePointData]
  private val ZoneLineRadius = 30f       // trigger radius in EQ units (~30 feet)

  // --- Spawn rendering ---

  private val spawnListener: ZoneEvent => Unit =
    case ZoneEvent.SpawnsLoaded(spawns) =>
      for s <- spawns do spawnListener(ZoneEvent.SpawnAdded(s))
    case ZoneEvent.SpawnAdded(s) =>
      val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(0)
      val selfId = Game.zoneSession.flatMap(_.client.selfSpawn.map(_.spawnId)).getOrElse(0)
      if s.spawnId != myId && s.spawnId != selfId then
        ZoneCharacter.fromSpawn(s) match
          case Some(zc) =>
            zoneCharacters(s.spawnId) = zc
            zone.initSpawnRendering(zc)
          case None => ()
    case ZoneEvent.SpawnRemoved(id) =>
      zoneCharacters.remove(id).foreach(zone.cleanupSpawn)
    case ZoneEvent.SpawnMoved(upd) =>
      val pos = EqCoords.serverToGl(upd.y, upd.x, upd.z)
      val serverVel = EqCoords.serverToGl(upd.deltaY, upd.deltaX, upd.deltaZ)
      zoneCharacters.get(upd.spawnId).foreach { zc =>
        zc.onServerPositionUpdate(pos, serverVel, upd.heading, upd.animType)
      }
    case ZoneEvent.HPChanged(hp) =>
      zoneCharacters.get(hp.spawnId).foreach { zc =>
        zc.curHp = hp.curHp
        zc.maxHp = hp.maxHp
      }
    case ZoneEvent.EquipmentChanged(wc) =>
      zoneCharacters.get(wc.spawnId).foreach { zc =>
        zc.updateEquipment(wc.wearSlot, wc.material, wc.color)
        zone.updateSpawnEquipment(zc)
      }
    case ZoneEvent.ZoneChangeRequested(req) =>
      println(s"[Zone] Zone change requested → zoneId=${req.zoneId}")
      zoning = true
      zoningStartedAt = System.currentTimeMillis()
    case ZoneEvent.ZoneChangeAccepted if zoning && !zoneChangeAccepted =>
      println(s"[Zone] Zone change accepted — reconnecting to world")
      zoneChangeAccepted = true
      reconnectWorldForZoning()
    case ZoneEvent.ZoneChangeDenied if zoning =>
      println(s"[Zone] Zone change denied by server")
      zoning = false
      zoneDeniedUntil = System.currentTimeMillis() + 3000
    case ZoneEvent.ZonePointsLoaded(points) =>
      // OP_SendZonePoints contains only destination coords (target_x/y/z from the
      // zone_points DB table), NOT trigger locations. The trigger coords (y/x/z) stay
      // server-side for validation only. The client detects zone lines via S3D BSP
      // region data (0x29 drntp/wtntp/lantp regions) and sends OP_ZoneChange with
      // the target zone ID. The server then validates using GetClosestZonePoint
      // against the trigger coords the client never sees.
      zonePoints = points
      for p <- points do
        println(f"[Zone] Zone point #${p.iterator}: target=${p.targetZoneId} dest=(${p.x}%.1f,${p.y}%.1f,${p.z}%.1f)")
    case ZoneEvent.DamageDealt(info) if info.damage != 0 =>
      // Attacker plays attack animation, target plays get-hit animation
      val attackAnim = if java.util.concurrent.ThreadLocalRandom.current().nextBoolean()
        then AnimCode.Attack1.code else AnimCode.Attack2.code
      zoneCharacters.get(info.sourceId).foreach(_.playTimedAnimation(attackAnim, 0.5f))
      zoneCharacters.get(info.targetId).foreach { zc =>
        if info.damage > 0 then zc.playTimedAnimation(AnimCode.GetHit.code, 0.4f)
      }
    case ZoneEvent.EntityDied(info) =>
      zoneCharacters.get(info.spawnId).foreach { zc =>
        zc.dead = true
      }
    case ZoneEvent.InventoryMoved(from, to) =>
      // When items move to/from weapon slots, update the player's visual equipment.
      // Read from session.client.inventory (already updated before event dispatch),
      // not pc.inventory (updated by pc.listener which runs after spawnListener).
      val isPrimary = from == InventoryItem.Primary || to == InventoryItem.Primary
      val isSecondary = from == InventoryItem.Secondary || to == InventoryItem.Secondary
      if isPrimary || isSecondary then
        Game.zoneSession.foreach { session =>
          val myId = session.client.mySpawnId
          zoneCharacters.get(myId).foreach { zc =>
            val inv = session.client.inventory
            if isPrimary then
              zc.equipment(7) = inv.find(_.equipSlot == InventoryItem.Primary).map(_.idFileNum).getOrElse(0)
            if isSecondary then
              zc.equipment(8) = inv.find(_.equipSlot == InventoryItem.Secondary).map(_.idFileNum).getOrElse(0)
            zone.updateSpawnEquipment(zc)
          }
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

    val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1)
    val visible = zoneCharacters.toVector.filter(_._1 != myId).flatMap { (id, zc) =>
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
    hud.setTarget(zoneCharacters.get(visible(nextIdx)))

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.3f, 0.5f, 0.7f, 1.0f)

    player = Game.zoneSession.flatMap(_.client.profile).map(PlayerCharacter.fromProfile)
    hud.init(player)
    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    zone = ZoneRenderer(zonePath, ctx.settings, zoneCharacters)
    camCtrl = CameraController(ctx.window, ctx.windowWidth, ctx.windowHeight)
    camCtrl.player = player
    player.foreach(_.collision = Some(zone.collision))
    camCtrl.initFromSpawn(selfSpawn, profile)
    println(s"Loading zone: $zonePath")

    // Register listeners — events are buffered in ZoneClient's queue (ZoneLoadingScreen
    // polls state directly instead of consuming events). First dispatchEvents() call in
    // update() will drain the full backlog: SpawnsLoaded, ZonePointsLoaded, InventoryLoaded, etc.
    Game.zoneSession.foreach { session =>
      val zc = session.client
      zc.addListener(spawnListener)
      zc.addListener(spellListener)
      player.foreach(pc => zc.addListener(pc.listener))

      // Self-spawn needs special handling: it's registered under mySpawnId (assigned by
      // SpawnAppearance), not the spawnId from OP_ZoneEntry. The SpawnAdded event for
      // self-spawn is skipped by spawnListener (mySpawnId check), so we add it manually.
      val myId = zc.mySpawnId
      selfSpawn.foreach { s =>
        ZoneCharacter.fromSpawn(s).foreach { playerZc =>
          zoneCharacters(myId) = playerZc
          zone.initSpawnRendering(playerZc)
          val (fo, mh) = zone.modelMetrics(playerZc.modelCode, playerZc.size)
          player.foreach { pc => pc.feetOffset = fo; pc.modelHeight = mh; pc.zoneChar = Some(playerZc) }
        }
      }
    }

  override def update(dt: Float): Unit =
    Game.zoneSession.foreach(_.client.dispatchEvents())

    // When zoning, poll world server for new zone address instead of normal updates
    if zoning then
      if System.currentTimeMillis() - zoningStartedAt > ZoningTimeoutMs then
        println("[Zone] Zoning timed out waiting for world ZoneServerInfo")
        zoning = false
        zoneChangeAccepted = false
        zoneDeniedUntil = System.currentTimeMillis() + 5000
      else
        Game.worldSession.foreach { ws =>
          var event = ws.client.pollEvent()
          while event.isDefined do
            event.get match
              case WorldEvent.ZoneInfo(addr) =>
                val charName = player.map(_.name).getOrElse("")
                Game.setScreen(ZoneLoadingScreen(ctx, addr, charName))
                return
              case WorldEvent.Error(msg) =>
                println(s"[Zone] World error during zone change: $msg")
              case _ => ()
            event = ws.client.pollEvent()
        }
        return // skip normal zone update while zoning

    // Client-side interpolation and NPC animation
    val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1)
    for (id, zc) <- zoneCharacters do
      if id != myId then
        // NPC: compute speed from velocity for walk/run threshold
        zc.speed = Math.sqrt(zc.velocity.x * zc.velocity.x + zc.velocity.z * zc.velocity.z).toFloat
        zc.interpolate(dt)
        zone.updateSpawnPosition(zc)
        if zc.updateAnimation(dt) then
          zone.playSpawnAnimation(zc, zc.currentAnimCode)

    spellEffects.update(dt, camCtrl.viewMatrix, zoneCharacters)

    hud.update(ctx.input)
    if hud.isEscapeOpen then return

    camCtrl.update(ctx.input, dt)

    // Update player model position/heading and animation
    if camCtrl.attached then
      Game.zoneSession.foreach { session =>
        val pid = session.client.mySpawnId
        player match
          case Some(pc) =>
            pc.zoneChar.foreach { zc =>
              zone.updateSpawnPosition(zc)
              if zc.updateAnimation(dt) then
                zone.playSpawnAnimation(zc, zc.currentAnimCode)
            }
          case None =>
            zoneCharacters.get(pid).foreach { zc =>
              zc.position.set(camCtrl.playerPos)
              zc.heading = camCtrl.playerHeading
              zone.updateSpawnPosition(zc)
            }
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

    // Zone line detection — check if player is inside a BSP zone line region
    if camCtrl.attached && !zone.zoneLineBsp.isEmpty && System.currentTimeMillis() >= zoneDeniedUntil then
      player.foreach { pc => pc.zoneChar.foreach { zc =>
        val pos = zc.position
        val (s3dX, s3dY, s3dZ) = EqCoords.glToS3d(pos.x, pos.y - pc.feetOffset, pos.z)
        val hit = zone.zoneLineBsp.check(s3dX, s3dY, s3dZ)
          .orElse(zone.zoneLineBsp.checkSphere(s3dX, s3dY, s3dZ))
        hit.foreach { info =>
          // Send zoneId=0 — server resolves target from player position via zone_points table
          println(s"[Zone] Zone line hit: '${info.regionName}' param1=${info.param1} param2=${info.param2}")
          Game.zoneSession.foreach(_.client.sendZoneChange(0))
          zoning = true
          zoningStartedAt = System.currentTimeMillis()
        }
      }}

    // Left-click targeting — only change target if we hit something
    if ctx.input.isActionPressed(GameAction.Target) && !camCtrl.freeLook && !ImGui.getIO().getWantCaptureMouse() then
      val (mx, my) = ctx.input.mousePos
      targeting.pickSpawn(mx, my, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat,
        camCtrl.projection, camCtrl.viewMatrix, zone.spawnHitData).foreach { id =>
        hud.setTarget(zoneCharacters.get(id))
      }

    // Tab target — raycast against zone collision mesh for LOS
    if ctx.input.isActionPressed(GameAction.TabTarget) && !ImGui.getIO().getWantTextInput() then
      resolveTabTarget()

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", camCtrl.projection)
    shader.setMatrix4f("view", camCtrl.viewMatrix)
    shader.setMatrix4f("model", model)

    zone.draw(shader, dt, camCtrl.viewMatrix)

    // Zone line sphere debug wireframes
    zone.drawZoneLineSpheres(shader)

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

  /** Reconnect to world server for zone-to-zone transition.
    * EQ protocol requires: disconnect both zone+world, reconnect to world,
    * authenticate, wait for server's OP_EnterWorld (pZoning=true path),
    * auto-respond, then receive OP_ZoneServerInfo.
    */
  private def reconnectWorldForZoning(): Unit =
    // Stop zone session (server is kicking us out anyway)
    Game.zoneSession.foreach(_.stop())
    Game.zoneSession = None

    // Stop old world session
    Game.worldSession.foreach(_.stop())
    Game.worldSession = None

    // Create new world session in zoning mode
    val charName = player.map(_.name).getOrElse("")
    val wc = WorldClient()
    val wnt = NetworkThread(wc)
    Game.worldSession = Some(WorldSession(wc, wnt))
    wnt.start()
    wnt.send(NetCommand.Connect(Game.worldHost, Game.worldPort))
    wc.connectForZoning(Game.worldAccountId, Game.worldKey, charName)
    zoningStartedAt = System.currentTimeMillis() // reset timeout

  override def dispose(): Unit =
    hud.dispose()
    player.foreach(_.zoneChar = None)
    Game.zoneSession.foreach { session =>
      session.client.removeListener(spawnListener)
      session.client.removeListener(spellListener)
      player.foreach(pc => session.client.removeListener(pc.listener))
    }
    spellEffects.cleanup()
    nameplateRenderer.cleanup()
    targeting.cleanup()
    zone.cleanup()
    shader.cleanup()
