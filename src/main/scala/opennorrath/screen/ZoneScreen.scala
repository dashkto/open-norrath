package opennorrath.screen

import scala.compiletime.uninitialized

import org.joml.{Matrix4f, Vector3f, Vector4f}
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.{glActiveTexture, GL_TEXTURE0, GL_TEXTURE1}
import org.lwjgl.opengl.GL20.glVertexAttrib3f

import imgui.ImGui

import opennorrath.{Game, GameAction, WorldSession}
import opennorrath.animation.AnimCode
import opennorrath.render.Shader
import opennorrath.state.{GameClock, PlayerCharacter, ZoneCharacter}
import opennorrath.world.{CameraController, EqCoords, SpellEffectSystem, TargetingSystem, ZoneRenderer}
import opennorrath.network.{EqNetworkThread, InventoryItem, NetCommand, NetworkThread, PlayerPosition, PlayerProfileData, SpawnAppearanceChange, SpawnData, WorldClient, WorldEvent, ZoneEvent, ZonePointData, ZoneState}
import opennorrath.network.titanium.TitaniumNetworkThread
import opennorrath.ui.{EqClass, NameplateRenderer, ZoneHud}

class ZoneScreen(ctx: GameContext, zonePath: String, selfSpawn: Option[SpawnData] = None, profile: Option[PlayerProfileData] = None) extends Screen:

  private var shader: Shader = uninitialized
  private var shadowShader: Shader = uninitialized
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
  private var saveTimer = 0f
  private val SaveInterval = 60f         // auto-save every 60 seconds
  private var lastSentHeading = -1f       // track heading changes for idle sends
  private val lastSentPos = Vector3f()    // track position changes
  private var wasAirborne = false          // track airborne state for fall animation
  private var jumpAnimTimer = 0f          // countdown for crouch animation on jump
  private var zoning = false              // true after server requests zone change
  private var zoneChangeAccepted = false  // true after OP_ZoneChange success=1 received
  private var zoneDeniedUntil = 0L       // cooldown (millis) after zone change denial
  private var zoningStartedAt = 0L       // millis when zoning started (for timeout)
  private val ZoningTimeoutMs = 15000L   // give up if world ZoneServerInfo never arrives
  private var campComplete = false              // true after server acknowledges camp (OP_LogoutReply)
  private var campTimer = 0f                    // countdown until OP_Logout is sent
  private var zonePoints = Vector.empty[ZonePointData]
  private val ZoneLineRadius = 30f       // trigger radius in EQ units (~30 feet)

  // Right-click interaction — distinguish click vs free-look drag.
  // Store mouse position at press time because cursor is hidden during free-look,
  // making mousePos unreliable on the release frame.
  private var rightMouseWasDown = false
  private var rightDragAccum = 0f
  private var rightClickX = 0f
  private var rightClickY = 0f
  private val RightClickThreshold = 5f   // total pixel movement to count as drag

  // --- Spawn rendering ---

  private val spawnListener: ZoneEvent => Unit =
    case ZoneEvent.TimeReceived(time) =>
      GameClock.sync(time)
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
    case ZoneEvent.FaceChanged(fc) =>
      // FaceChange has no spawn ID — server broadcasts the raw 7-byte struct.
      // We can only apply it to our own character (self-initiated face changes).
      for
        session <- Game.zoneSession
        zc <- zoneCharacters.get(session.client.mySpawnId)
      do
        zc.face = fc.face
        zone.updateSpawnEquipment(zc)
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
    case ZoneEvent.AppearanceChanged(change) if change.appearanceType == SpawnAppearanceChange.Animation =>
      zoneCharacters.get(change.spawnId).foreach { zc =>
        zc.sitting = change.parameter == SpawnAppearanceChange.AnimSit
        zc.dead = change.parameter == SpawnAppearanceChange.AnimDead
      }
    case ZoneEvent.AnimationTriggered(anim) =>
      for
        (code, duration) <- animActionToCode(anim.action)
        zc <- zoneCharacters.get(anim.spawnId)
      do zc.playTimedAnimation(code, duration)
    case ZoneEvent.DamageDealt(info) if info.damage > 0 =>
      // OP_Animation already handles the attacker's swing — only play flinch on target here.
      // D01/D02 are the actual damage-received flinch animations;
      // C05 (GetHit) is misleadingly named — it's the 1H weapon attack anim.
      zoneCharacters.get(info.targetId).foreach { zc =>
        val hitAnim = if java.util.concurrent.ThreadLocalRandom.current().nextBoolean()
          then AnimCode.Damage1.code else AnimCode.Damage2.code
        zc.playTimedAnimation(hitAnim, 0.4f)
      }
      // Auto-stand (and cancel camp) when player takes damage
      val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1)
      if info.targetId == myId then
        zoneCharacters.get(myId).foreach { zc =>
          if zc.sitting then standUp(zc)
        }
    case ZoneEvent.EntityDied(info) =>
      zoneCharacters.get(info.spawnId).foreach { zc =>
        zc.dead = true
        zc.curHp = 0
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
    case ZoneEvent.StateChanged(ZoneState.Disconnected) =>
      campComplete = true
    case _ => ()

  private val spellListener: ZoneEvent => Unit =
    case ZoneEvent.SpellActionTriggered(action) if action.spellId > 0 =>
      spellEffects.trigger(action.targetId, action.spellId)
    case ZoneEvent.BeginCastTriggered(cast) =>
      spellEffects.triggerCast(cast.casterId, cast.spellId, cast.castTime)
    case _ => ()

  /** Map OP_Animation action byte to (AnimCode, duration).
    * Values from EQEmu Animation enum in zone/common.h. */
  private def animActionToCode(action: Int): Option[(String, Float)] = action match
    // Combat — these also come via DamageDealt, but OP_Animation can trigger independently
    case 1  => Some((AnimCode.Attack1.code, 0.5f))      // Kick
    case 2  => Some((AnimCode.Attack2.code, 0.5f))      // Piercing
    case 3  => Some((AnimCode.Slash2H.code, 0.5f))      // 2H Slashing
    case 4  => Some((AnimCode.Weapon2H.code, 0.5f))     // 2H Weapon
    case 5  => Some((AnimCode.Weapon1H.code, 0.5f))      // 1H Weapon
    case 6  => Some((AnimCode.DualWield.code, 0.5f))    // Dual Wield
    case 7  => Some((AnimCode.Bash.code, 0.5f))         // Slam/Bash
    case 8  => Some((AnimCode.HandToHand.code, 0.5f))   // Hand to Hand
    case 9  => Some((AnimCode.AttackOff.code, 0.5f))    // Shoot Bow
    case 11 => Some((AnimCode.RoundKick.code, 0.5f))    // Round Kick
    // Casting
    case 42 => Some((AnimCode.CastPullBack.code, 2.5f)) // Buff cast
    case 43 => Some((AnimCode.CastLoop.code, 2.5f))     // Heal cast
    case 44 => Some((AnimCode.SpellCast.code, 0.8f))    // Damage cast (finish)
    // Monk specials
    case 45 => Some((AnimCode.FlyingKick.code, 0.5f))
    case 46 => Some((AnimCode.TigerClaw.code, 0.5f))
    case 47 => Some((AnimCode.EagleStrike.code, 0.5f))
    // Emotes (T prefix)
    case 29 => Some((AnimCode.Wave.code, 2.0f))
    case 30 => Some((AnimCode.Rude.code, 2.0f))
    case 31 => Some((AnimCode.Yawn.code, 2.0f))
    // Social emotes (S prefix) — action 48..70 maps to S01..S20 with gaps
    case 48 => Some((AnimCode.Agree.code, 2.0f))
    case 49 => Some((AnimCode.Amaze.code, 2.0f))
    case 50 => Some((AnimCode.Plead.code, 2.0f))
    case 51 => Some((AnimCode.Clap.code, 2.0f))
    case 52 => Some((AnimCode.Bleed.code, 2.0f))
    case 54 => Some((AnimCode.Chuckle.code, 2.0f))
    case 55 => Some((AnimCode.Burp.code, 2.0f))
    case 58 => Some((AnimCode.Dance.code, 2.0f))
    case 59 => Some((AnimCode.Veto.code, 2.0f))
    case 60 => Some((AnimCode.Glare.code, 2.0f))
    case 61 => Some((AnimCode.Peer.code, 2.0f))
    case 62 => Some((AnimCode.KneelEmote.code, 2.0f))
    case 63 => Some((AnimCode.Laugh.code, 2.0f))
    case 64 => Some((AnimCode.Point.code, 2.0f))
    case 65 => Some((AnimCode.Shrug.code, 2.0f))
    case 66 => Some((AnimCode.HandRaise.code, 2.0f))
    case 67 => Some((AnimCode.Salute.code, 2.0f))
    case 68 => Some((AnimCode.Shiver.code, 2.0f))
    case 69 => Some((AnimCode.TapFoot.code, 2.0f))
    case 70 => Some((AnimCode.Bow.code, 2.0f))
    case _  => None

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
                target.set(cx, cy + h * 0.5f, cz) // ray to model center, not feet
                if !zone.collision.rayBlocked(cam, target) then true
                else
                  val y0 = cy; val y1 = cy + h  // feet to head (see TargetingSystem.pickSpawn)
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

    player = Game.zoneSession.flatMap(_.client.profile).map(PlayerCharacter.fromProfile)
    hud.init(player)
    shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
    shadowShader = Shader.fromResources("/shaders/shadow.vert", "/shaders/shadow.frag")
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
          playerZc.overrideSpawnId = Some(myId)
          zoneCharacters(myId) = playerZc
          zone.initSpawnRendering(playerZc)
          val (fo, mh) = zone.modelMetrics(playerZc.modelCode, playerZc.size)
          player.foreach { pc => pc.feetOffset = fo; pc.modelHeight = mh; pc.zoneChar = Some(playerZc) }
        }
      }
    }

  override def update(dt: Float): Unit =
    Game.zoneSession.foreach(_.client.dispatchEvents())

    // Camp complete — server acknowledged logout, return to character select
    if campComplete then
      Game.zoneSession.foreach(_.stop())
      Game.zoneSession = None
      Game.worldSession match
        case Some(ws) =>
          Game.setScreen(CharacterSelectScreen(ctx, ws.client.characters))
        case None =>
          Game.setScreen(LoginScreen(ctx))
      return

    // Camp timer — after ~30s, send OP_Logout so server responds with OP_LogoutReply
    Game.zoneSession.foreach { session =>
      if session.client.camping && !campComplete then
        campTimer += dt
        if campTimer >= 30f then
          session.client.sendLogout()
          session.client.camping = false
    }

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
          zone.playSpawnAnimation(zc, zc.currentAnimCode, freezeOnLastFrame = zc.dead)

    spellEffects.update(dt, camCtrl.viewMatrix, zoneCharacters)

    hud.update(ctx.input)
    if hud.isEscapeOpen then return

    // Sit toggle (Q) and auto-stand on movement
    if camCtrl.attached && !ImGui.getIO().getWantTextInput() then
      player.foreach { pc => pc.zoneChar.foreach { zc =>
        val wantMove = ctx.input.isActionHeld(GameAction.MoveForward) ||
          ctx.input.isActionHeld(GameAction.MoveBackward) ||
          ctx.input.isActionHeld(GameAction.StrafeLeft) ||
          ctx.input.isActionHeld(GameAction.StrafeRight)
        if zc.sitting && wantMove then
          standUp(zc)
        else if ctx.input.isActionPressed(GameAction.Sit) then
          if zc.sitting then standUp(zc)
          else
            zc.sitting = true
            Game.zoneSession.foreach { s =>
              s.client.setAppearance(s.client.mySpawnId, SpawnAppearanceChange.Animation, SpawnAppearanceChange.AnimSit)
            }
      }}

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
                zone.playSpawnAnimation(zc, zc.currentAnimCode, freezeOnLastFrame = zc.dead)
            }
          case None =>
            zoneCharacters.get(pid).foreach { zc =>
              zc.position.set(camCtrl.camera.position)
              zone.updateSpawnPosition(zc)
            }
      }

    // Send player position to server when attached — only if something changed
    if camCtrl.attached then
      posUpdateTimer += dt
      if posUpdateTimer >= PosUpdateInterval then
        player.foreach { pc =>
          val pp = pc.position
          val hdeg = pc.headingDeg
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
                animation = if pc.moving then 1 else 0,
              ))
            }
          else
            posUpdateTimer = PosUpdateInterval
        }

    // Periodic save — ask server to persist character state
    saveTimer += dt
    if saveTimer >= SaveInterval then
      saveTimer = 0f
      Game.zoneSession.foreach(_.client.save())

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

    // Right-click interaction — detect click (not drag) on right mouse release.
    // FreeLook uses the same button; track accumulated mouse movement to distinguish.
    // Store mousePos at press time because cursor is hidden during free-look.
    val rightHeld = ctx.input.isActionHeld(GameAction.FreeLook)
    if ctx.input.isActionPressed(GameAction.FreeLook) then
      rightDragAccum = 0f
      val (px, py) = ctx.input.mousePos
      rightClickX = px
      rightClickY = py
    if rightHeld then
      val (dx, dy) = ctx.input.mouseDelta
      rightDragAccum += Math.abs(dx) + Math.abs(dy)
    if rightMouseWasDown && !rightHeld && rightDragAccum < RightClickThreshold then
      if !ImGui.getIO().getWantCaptureMouse() then
        targeting.pickSpawn(rightClickX, rightClickY, ctx.windowWidth.toFloat, ctx.windowHeight.toFloat,
          camCtrl.projection, camCtrl.viewMatrix, zone.spawnHitData).foreach { id =>
          zoneCharacters.get(id).foreach { zc =>
            println(s"[Zone] Right-click on ${zc.name} (id=$id npcType=${zc.npcType} classId=${zc.classId})")
            if zc.dead then // corpse — npcType stays as original, dead flag is set by EntityDied
              hud.requestLoot(id)
            else if zc.classId == EqClass.Merchant.code || zc.classId == EqClass.DiscordMerchant.code then
              hud.openMerchant(id)
          }
        }
    rightMouseWasDown = rightHeld

    // Tab target — raycast against zone collision mesh for LOS
    if ctx.input.isActionPressed(GameAction.TabTarget) && !ImGui.getIO().getWantTextInput() then
      resolveTabTarget()

    // Consider current target
    if ctx.input.isActionPressed(GameAction.Consider) && !ImGui.getIO().getWantTextInput() then
      hud.target.foreach { zc =>
        Game.zoneSession.foreach { s =>
          s.client.consider(s.client.mySpawnId, zc.spawnId)
        }
      }

  override def render(dt: Float): Unit =
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_BLEND)

    // Compute sun direction once — used for both shadow pass and main pass.
    // Must update the shadow map's light-space matrix BEFORE the shadow pass
    // so the depth buffer and the main shader sample with the same projection.
    val sunDir = GameClock.sunDirection
    zone.shadowMap.updateLightDirection(sunDir)

    // Update character distance culling (spread across frames for performance)
    zone.updateCharacterVisibility(camCtrl.camera.position)

    // --- Shadow map pre-pass: render depth from sun's perspective ---
    zone.shadowMap.bind()
    zone.drawShadowPass(shadowShader)
    zone.shadowMap.unbind(ctx.windowWidth, ctx.windowHeight)

    // --- Main color pass ---
    val sky = GameClock.skyColor
    glClearColor(sky.x, sky.y, sky.z, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    shader.use()
    shader.setMatrix4f("projection", camCtrl.projection)
    shader.setMatrix4f("view", camCtrl.viewMatrix)
    shader.setMatrix4f("model", model)

    // Lighting uniforms — derived from game time (sun position, ambient, tint)
    shader.setBool("enableLighting", true)
    shader.setBool("enableShadows", true)
    shader.setVec3("lightDir", sunDir.x, sunDir.y, sunDir.z)
    shader.setFloat("ambientStrength", GameClock.ambientStrength)
    val lc = GameClock.lightColor
    shader.setVec3("lightColor", lc.x, lc.y, lc.z)
    shader.setMatrix4f("lightSpaceMatrix", zone.shadowMap.lightSpaceMatrix)

    // Bind shadow map depth texture to unit 1 (diffuse tex0 stays on unit 0)
    glActiveTexture(GL_TEXTURE1)
    glBindTexture(GL_TEXTURE_2D, zone.shadowMap.depthTexture)
    shader.setInt("shadowMap", 1)
    glActiveTexture(GL_TEXTURE0)

    // Set default normal for meshes without a normal VBO (particles, nameplates, debug)
    glVertexAttrib3f(4, 0f, 1f, 0f)

    zone.draw(shader, dt, camCtrl.viewMatrix)

    // Zone line sphere debug wireframes
    zone.drawZoneLineSpheres(shader)

    // Target hitbox wireframe + server position debug sphere
    hud.target.foreach { zc =>
      targeting.drawTargetHitbox(zc.spawnId, shader, model, zone.spawnHitData)
      targeting.drawServerPosSphere(zc.lastServerPos, shader, model)
    }

    // Spell particle effects (additive blending, before nameplates)
    // ambientStrength=1.0 already set by zone.draw() for particles — keeps spells unlit too
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

    hud.render(dt)

  /** Stand up and cancel camping if active. */
  private def standUp(zc: ZoneCharacter): Unit =
    zc.sitting = false
    Game.zoneSession.foreach { s =>
      s.client.setAppearance(s.client.mySpawnId, SpawnAppearanceChange.Animation, SpawnAppearanceChange.AnimStand)
      if s.client.camping then
        s.client.camping = false
        campTimer = 0f
    }

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
    val wnt: EqNetworkThread =
      if Game.macMode then NetworkThread(wc)
      else TitaniumNetworkThread(wc)
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
    shadowShader.cleanup()
