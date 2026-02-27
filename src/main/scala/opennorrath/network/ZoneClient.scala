package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

enum ZoneState:
  /** Not connected to zone server. */
  case Disconnected
  /** UDP socket opened, waiting for zone entry to complete. */
  case Connecting
  /** Sent OP_ZoneEntry, waiting for OP_PlayerProfile. */
  case WaitingForProfile
  /** Received profile, waiting for OP_NewZone. */
  case WaitingForZone
  /** Received zone data, requesting spawns. */
  case RequestingSpawns
  /** Received all initial data, zone is playable. */
  case InZone
  /** Connection failed. */
  case Failed

/** Events emitted by ZoneClient for the UI and rendering systems. */
enum ZoneEvent:
  case StateChanged(state: ZoneState)

  // Zone entry sequence
  case ProfileReceived(profile: PlayerProfileData)
  case ZoneDataReceived(zone: NewZoneInfo)
  case TimeReceived(time: GameTime)

  // Spawns — rendering system consumes these
  case SpawnsLoaded(spawns: Vector[SpawnData])
  case SpawnAdded(spawn: SpawnData)
  case SpawnRemoved(spawnId: Int)

  // Movement — rendering system interpolates from these
  case SpawnMoved(update: MobPositionUpdate)

  // Appearance — rendering system updates models
  case AppearanceChanged(change: SpawnAppearanceChange)
  case EquipmentChanged(change: WearChangeInfo)
  case AnimationTriggered(anim: AnimationInfo)
  case SpellActionTriggered(action: SpellAction)

  // Combat — UI displays these
  case DamageDealt(info: DamageInfo)
  case EntityDied(info: DeathInfo)
  case ConsiderResult(result: opennorrath.network.ConsiderResult)

  // Stats — UI updates bars
  case HPChanged(update: HPUpdate)
  case ManaChanged(update: ManaChange)
  case ExpChanged(update: ExpChange)
  case LevelChanged(update: LevelChange)
  case SkillChanged(update: SkillChange)

  // Chat — UI chat window
  case ChatReceived(msg: ChatMessage)

  // Inventory — UI inventory panel
  case InventoryLoaded(items: Vector[InventoryItem])
  case InventoryItemUpdated(item: InventoryItem)
  case InventoryMoved(fromSlot: Int, toSlot: Int)

  // Buffs — UI buff panel
  case BuffsLoaded(buffs: Array[SpellBuff])
  case BuffUpdated(slot: Int, buff: SpellBuff)

  // Environment — rendering system updates
  case WeatherChanged(weather: WeatherInfo)

  // Zone objects
  case DoorsLoaded(doors: Vector[DoorData])
  case GroundItemSpawned(item: GroundItemData)
  case ZonePointsLoaded(points: Vector[ZonePointData])

  // Group
  case GroupUpdated(members: Vector[String], leader: String)

  // Zone transitions
  case ZoneChangeRequested(req: ZoneChangeRequest)
  case ZoneChangeAccepted
  case ZoneChangeDenied

  case Error(message: String)

/** Zone server protocol state machine.
  *
  * Thread safety model (mirrors LoginClient/WorldClient):
  * - Game thread calls: connect(), sendPosition(), target(), pollEvent(), state
  * - Network thread calls: handlePacket(), tick(), pollOutgoing()
  * - Cross-thread communication via ConcurrentLinkedQueue (lock-free)
  *
  * Zone Entry Handshake:
  *   1. Client connects UDP to zone IP:port (from WorldClient.ZoneInfo)
  *   2. Client sends OP_ZoneEntry (char name)
  *   3. Server sends OP_PlayerProfile (8460 bytes, fragmented)
  *   4. Server sends OP_NewZone (572 bytes)
  *   5. Server sends OP_TimeOfDay
  *   6. Client sends OP_ReqNewZone
  *   7. Server sends OP_NewZone again
  *   8. Client sends OP_ReqClientSpawn
  *   9. Server sends OP_ZoneSpawns (bulk, heavily fragmented)
  *  10. Server sends OP_SpawnDoor, OP_GroundSpawn, OP_SendZonepoints
  *  11. Zone is ready — ongoing updates flow
  */
class ZoneClient extends PacketHandler:
  @volatile var state: ZoneState = ZoneState.Disconnected
  val events = ConcurrentLinkedQueue[ZoneEvent]()
  val errors = ConcurrentLinkedQueue[String]()

  // Listener-based event dispatch — game thread only
  private val listeners = scala.collection.mutable.ArrayBuffer.empty[ZoneEvent => Unit]

  /** Register a listener for zone events. Called from game thread. */
  def addListener(fn: ZoneEvent => Unit): Unit = listeners += fn

  /** Remove a previously registered listener. Called from game thread. */
  def removeListener(fn: ZoneEvent => Unit): Unit = listeners -= fn

  /** Drain the event queue and broadcast to all listeners. Call once per frame from game thread. */
  def dispatchEvents(): Unit =
    var err = errors.poll()
    while err != null do
      events.add(ZoneEvent.Error(err))
      err = errors.poll()
    var event = events.poll()
    while event != null do
      val snapshot = listeners.toArray
      for fn <- snapshot do fn(event)
      event = events.poll()

  private val outQueue = ConcurrentLinkedQueue[Array[Byte]]()
  private val pendingApps = ConcurrentLinkedQueue[(Short, Array[Byte])]()

  // Sequence tracking (network thread only)
  private var outSeq: Int = 0
  private var outArq: Int = 0
  private var lastInArq: Int = -1
  private var needArsp = false
  private var firstPacket = true
  private var fragSeq: Int = 0

  // Fragment reassembly
  private val assembler = FragmentAssembler()

  // Zone data (populated during zone entry)
  var profile: Option[PlayerProfileData] = None
  var zoneInfo: Option[NewZoneInfo] = None
  var selfSpawn: Option[SpawnData] = None // From ServerZoneEntry — authoritative position
  var spawns: scala.collection.mutable.Map[Int, SpawnData] = scala.collection.mutable.Map.empty
  var inventory: Vector[InventoryItem] = Vector.empty
  var zonePoints: Vector[ZonePointData] = Vector.empty
  var mySpawnId: Int = 0

  // Pending connection info
  @volatile private var pendingCharName = ""

  // Retransmission for zone entry (zone server may not be ready yet)
  private var lastZoneEntrySentMs: Long = 0
  private var zoneEntryRetries: Int = 0
  private val ZoneEntryRetryIntervalMs = 2000
  private val ZoneEntryMaxRetries = 15

  /** Initiate zone connection. Called from game thread. */
  def connect(charName: String): Unit =
    pendingCharName = charName
    state = ZoneState.Connecting
    emit(ZoneEvent.StateChanged(state))
    // Mac zone signature: first packet must be OP_DataRate for the server to identify us
    queueAppPacket(ZoneOpcodes.DataRate, ZoneCodec.encodeDataRate(8.0f))
    queueAppPacket(ZoneOpcodes.ZoneEntry, ZoneCodec.encodeZoneEntry(charName))
    lastZoneEntrySentMs = System.currentTimeMillis()
    zoneEntryRetries = 0
    state = ZoneState.WaitingForProfile
    emit(ZoneEvent.StateChanged(state))

  /** Send player position update. Called from game thread each tick. */
  def sendPosition(pos: PlayerPosition): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(ZoneOpcodes.ClientUpdate, ZoneCodec.encodeClientUpdate(pos))

  /** Target an entity. Called from game thread. */
  def target(targetId: Int): Unit =
    queueAppPacket(ZoneOpcodes.TargetMouse, ZoneCodec.encodeTarget(targetId))

  /** Consider an entity. Called from game thread. */
  def consider(playerId: Int, targetId: Int): Unit =
    queueAppPacket(ZoneOpcodes.Consider, ZoneCodec.encodeConsider(playerId, targetId))

  /** Send a chat message. Called from game thread. */
  def sendChat(sender: String, target: String, channel: Int, language: Int, message: String): Unit =
    queueAppPacket(ZoneOpcodes.ChannelMessage, ZoneCodec.encodeChannelMessage(sender, target, channel, language, message))

  /** Toggle auto-attack. Called from game thread. */
  def autoAttack(enabled: Boolean): Unit =
    queueAppPacket(ZoneOpcodes.AutoAttack, ZoneCodec.encodeAutoAttack(enabled))

  /** Set spawn appearance (sit/stand/etc). Called from game thread. */
  def setAppearance(spawnId: Int, appearanceType: Int, parameter: Int): Unit =
    queueAppPacket(ZoneOpcodes.SpawnAppearance, ZoneCodec.encodeSpawnAppearance(spawnId, appearanceType, parameter))

  /** Whether a camp is in progress (client waiting to send OP_Logout). */
  var camping: Boolean = false

  /** Camp / logout. Called from game thread. */
  def camp(): Unit =
    println("[Zone] Sending OP_Camp")
    camping = true
    queueAppPacket(ZoneOpcodes.Camp, ZoneCodec.encodeCamp)

  /** Send OP_Logout after camp timer expires. Called from game thread. */
  def sendLogout(): Unit =
    println("[Zone] Sending OP_Logout (camp timer expired)")
    queueAppPacket(ZoneOpcodes.Logout, ZoneCodec.encodeLogout)

  /** Move/swap an inventory item. Called from game thread. */
  def sendMoveItem(fromSlot: Int, toSlot: Int, stackCount: Int = 0): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(ZoneOpcodes.MoveItem, ZoneCodec.encodeMoveItem(fromSlot, toSlot, stackCount))
      // Update local inventory and emit event so listeners (e.g. weapon visual updates)
      // react immediately — the server doesn't echo OP_MoveItem back to the originator.
      val fromItem = inventory.find(_.equipSlot == fromSlot)
      val toItem = inventory.find(_.equipSlot == toSlot)
      inventory = inventory.filterNot(i => i.equipSlot == fromSlot || i.equipSlot == toSlot)
      fromItem.foreach(i => inventory = inventory :+ i.copy(equipSlot = toSlot))
      toItem.foreach(i => inventory = inventory :+ i.copy(equipSlot = fromSlot))
      emit(ZoneEvent.InventoryMoved(fromSlot, toSlot))

  /** Request zone change (client-initiated, e.g. zone line). Called from game thread. */
  def sendZoneChange(zoneId: Int): Unit =
    val charName = profile.map(_.name).getOrElse(pendingCharName)
    queueAppPacket(ZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, zoneId))

  /** Save character. Called from game thread. */
  def save(): Unit =
    queueAppPacket(ZoneOpcodes.Save, ZoneCodec.encodeSave)

  /** Notify server the player jumped. Called from game thread. */
  def sendJump(): Unit =
    queueAppPacket(ZoneOpcodes.Jump, ZoneCodec.encodeJump)

  // ===========================================================================
  // PacketHandler implementation — called from network thread
  // ===========================================================================

  def handlePacket(packet: InboundPacket): Unit =
    packet.arq.foreach { arq =>
      lastInArq = arq
      needArsp = true
    }

    // Fragment reassembly
    val pkt = assembler.process(packet) match
      case Some(p) => p
      case None => return

    if pkt.opcode == 0 then
      return // pure ACK

    pkt.opcode match

      // --- Zone Entry Handshake ---

      case ZoneOpcodes.PlayerProfile =>
        // PlayerProfile is encrypted + zlib compressed (mac.cpp:290-293)
        val rawOpt = PacketCrypto.decryptAndInflateProfile(pkt.payload)
        rawOpt.flatMap(ZoneCodec.decodePlayerProfile) match
          case Some(pp) =>
            profile = Some(pp)
            state = ZoneState.WaitingForZone
            emit(ZoneEvent.ProfileReceived(pp))
            emit(ZoneEvent.StateChanged(state))
            // Client must request zone data — server won't send it unprompted
            queueAppPacket(ZoneOpcodes.SetServerFilter, ZoneCodec.encodeServerFilter)
            queueAppPacket(ZoneOpcodes.ReqNewZone, ZoneCodec.encodeReqNewZone)
          case None =>
            emit(ZoneEvent.Error("Failed to decode player profile"))

      case ZoneOpcodes.NewZone =>
        ZoneCodec.decodeNewZone(pkt.payload) match
          case Some(nz) =>
            zoneInfo = Some(nz)
            emit(ZoneEvent.ZoneDataReceived(nz))
            if state == ZoneState.WaitingForZone then
              state = ZoneState.RequestingSpawns
              emit(ZoneEvent.StateChanged(state))
              // Request doors/objects/zonepoints
              queueAppPacket(ZoneOpcodes.ReqClientSpawn, ZoneCodec.encodeReqClientSpawn)
          case None =>
            emit(ZoneEvent.Error("Failed to decode zone data"))

      case ZoneOpcodes.TimeOfDay =>
        ZoneCodec.decodeTimeOfDay(pkt.payload).foreach(time => emit(ZoneEvent.TimeReceived(time)))

      // --- Spawns ---

      case ZoneOpcodes.ZoneEntry =>
        // Server sends our own spawn back as ServerZoneEntry_Struct (356 bytes, unencrypted)
        ZoneCodec.decodeServerZoneEntry(pkt.payload).foreach { spawn =>
          selfSpawn = Some(spawn)
          // The server will later assign our spawn ID via SpawnAppearance(SpawnID)
          emit(ZoneEvent.SpawnAdded(spawn))
        }

      case ZoneOpcodes.ZoneSpawns =>
        // ZoneSpawns are encrypted + zlib compressed (mac.cpp:461-462)
        val rawOpt = PacketCrypto.decryptAndInflateSpawns(pkt.payload)
        val rawData = rawOpt.getOrElse(pkt.payload)
        val spawnList = ZoneCodec.decodeZoneSpawns(rawData)
        for s <- spawnList do spawns(s.spawnId) = s
        emit(ZoneEvent.SpawnsLoaded(spawnList))

      case ZoneOpcodes.NewSpawn =>
        // NewSpawn is encrypted + zlib compressed (same as ZoneSpawns, mac.cpp:356)
        val rawOpt = PacketCrypto.decryptAndInflateSpawns(pkt.payload)
        val rawData = rawOpt.getOrElse(pkt.payload)
        ZoneCodec.decodeSpawn(rawData).foreach { s =>
          spawns(s.spawnId) = s
          emit(ZoneEvent.SpawnAdded(s))
        }

      case ZoneOpcodes.DeleteSpawn =>
        ZoneCodec.decodeDeleteSpawn(pkt.payload).foreach { id =>
          spawns.remove(id)
          emit(ZoneEvent.SpawnRemoved(id))
        }

      // --- Movement ---

      case ZoneOpcodes.MobUpdate =>
        for upd <- ZoneCodec.decodeMobUpdates(pkt.payload) do
          handleMobPositionUpdate(upd)

      case ZoneOpcodes.ClientUpdate =>
        ZoneCodec.decodeClientUpdate(pkt.payload).foreach(handleMobPositionUpdate)

      // --- Combat ---

      case ZoneOpcodes.Damage =>
        ZoneCodec.decodeDamage(pkt.payload).foreach(d => emit(ZoneEvent.DamageDealt(d)))

      case ZoneOpcodes.Death =>
        ZoneCodec.decodeDeath(pkt.payload).foreach { d =>
          spawns.remove(d.spawnId)
          emit(ZoneEvent.EntityDied(d))
        }

      case ZoneOpcodes.Consider =>
        ZoneCodec.decodeConsider(pkt.payload).foreach(c => emit(ZoneEvent.ConsiderResult(c)))

      // --- Appearance / Animation ---

      case ZoneOpcodes.SpawnAppearance =>
        ZoneCodec.decodeSpawnAppearance(pkt.payload).foreach { change =>
          // SpawnAppearance(SpawnID) assigns our spawn ID during zone entry
          if change.appearanceType == SpawnAppearanceChange.SpawnID && state == ZoneState.RequestingSpawns then
            mySpawnId = change.parameter
            state = ZoneState.InZone
            emit(ZoneEvent.StateChanged(state))
            // Send initial position to trigger server's CompleteConnect → ZoneSpawns
            selfSpawn.foreach { s =>
              sendPosition(PlayerPosition(spawnId = mySpawnId, y = s.y, x = s.x, z = s.z,
                heading = s.heading.toFloat, deltaY = 0, deltaX = 0, deltaZ = 0, deltaHeading = 0, animation = 0))
            }
          emit(ZoneEvent.AppearanceChanged(change))
        }

      case ZoneOpcodes.WearChange =>
        ZoneCodec.decodeWearChange(pkt.payload).foreach { wc =>
          emit(ZoneEvent.EquipmentChanged(wc))
        }

      case ZoneOpcodes.Animation =>
        ZoneCodec.decodeAnimation(pkt.payload).foreach(a => emit(ZoneEvent.AnimationTriggered(a)))

      case ZoneOpcodes.Action =>
        ZoneCodec.decodeAction(pkt.payload).foreach(a => emit(ZoneEvent.SpellActionTriggered(a)))

      // --- Stats ---

      case ZoneOpcodes.HPUpdate =>
        ZoneCodec.decodeHPUpdate(pkt.payload).foreach(hp => emit(ZoneEvent.HPChanged(hp)))

      case ZoneOpcodes.ManaChange =>
        ZoneCodec.decodeManaChange(pkt.payload).foreach(m => emit(ZoneEvent.ManaChanged(m)))

      case ZoneOpcodes.ExpUpdate =>
        ZoneCodec.decodeExpUpdate(pkt.payload).foreach(e => emit(ZoneEvent.ExpChanged(e)))

      case ZoneOpcodes.LevelUpdate =>
        ZoneCodec.decodeLevelUpdate(pkt.payload).foreach(l => emit(ZoneEvent.LevelChanged(l)))

      case ZoneOpcodes.SkillUpdate =>
        ZoneCodec.decodeSkillUpdate(pkt.payload).foreach(s => emit(ZoneEvent.SkillChanged(s)))

      // --- Chat ---

      case ZoneOpcodes.ChannelMessage =>
        ZoneCodec.decodeChannelMessage(pkt.payload).foreach(m => emit(ZoneEvent.ChatReceived(m)))

      // --- Environment ---

      case ZoneOpcodes.Weather | ZoneOpcodes.Weather2 =>
        ZoneCodec.decodeWeather(pkt.payload).foreach(w => emit(ZoneEvent.WeatherChanged(w)))

      // --- Zone Objects ---

      case ZoneOpcodes.SpawnDoor =>
        val doors = ZoneCodec.decodeDoors(pkt.payload)
        emit(ZoneEvent.DoorsLoaded(doors))

      case ZoneOpcodes.GroundSpawn =>
        ZoneCodec.decodeGroundItem(pkt.payload).foreach(i => emit(ZoneEvent.GroundItemSpawned(i)))

      case ZoneOpcodes.SendZonepoints =>
        val points = ZoneCodec.decodeZonePoints(pkt.payload)
        zonePoints = points
        emit(ZoneEvent.ZonePointsLoaded(points))

      // --- Group ---

      case ZoneOpcodes.GroupUpdate =>
        ZoneCodec.decodeGroupUpdate(pkt.payload).foreach((members, leader) =>
          emit(ZoneEvent.GroupUpdated(members, leader)))

      // --- Zone Change ---

      case ZoneOpcodes.RequestClientZoneChange =>
        ZoneCodec.decodeRequestClientZoneChange(pkt.payload).foreach { req =>
          // Respond to server — it won't process the zone change without this
          val charName = profile.map(_.name).getOrElse(pendingCharName)
          queueAppPacket(ZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, req.zoneId))
          emit(ZoneEvent.ZoneChangeRequested(req))
        }

      case ZoneOpcodes.ZoneChange =>
        ZoneCodec.decodeZoneChange(pkt.payload).foreach { result =>
          println(s"[Zone] OP_ZoneChange response: success=${result.success}")
          if result.success == 1 then
            emit(ZoneEvent.ZoneChangeAccepted)
          else
            emit(ZoneEvent.ZoneChangeDenied)
        }

      // --- Misc (log but don't emit events) ---

      case ZoneOpcodes.CharInventory =>
        // CharInventory: uint16(item_count) + zlib compressed data (mac.cpp:627-628)
        PacketCrypto.inflateInventory(pkt.payload) match
          case Some((count, raw)) =>
            val items = ZoneCodec.decodeInventory(raw, count)
            inventory = items
            emit(ZoneEvent.InventoryLoaded(items))
          case None => ()

      case ZoneOpcodes.MoveItem =>
        ZoneCodec.decodeMoveItem(pkt.payload).foreach { (from, to) =>
          // Server-initiated move or resync — update local inventory
          if to == -1 || to == 0xFFFFFFFF then
            inventory = inventory.filterNot(_.equipSlot == from)
          else
            val fromItem = inventory.find(_.equipSlot == from)
            val toItem = inventory.find(_.equipSlot == to)
            inventory = inventory.filterNot(i => i.equipSlot == from || i.equipSlot == to)
            fromItem.foreach(i => inventory = inventory :+ i.copy(equipSlot = to))
            toItem.foreach(i => inventory = inventory :+ i.copy(equipSlot = from))
          emit(ZoneEvent.InventoryMoved(from, to))
        }

      case ZoneOpcodes.ItemPacket | ZoneOpcodes.MerchantItemPacket |
           ZoneOpcodes.SummonedItem | ZoneOpcodes.ContainerPacket | ZoneOpcodes.BookPacket =>
        handleItemPacket(pkt.opcode, pkt.payload)

      case ZoneOpcodes.Stamina => ()

      case ZoneOpcodes.SendExpZonein =>
        if state == ZoneState.RequestingSpawns then
          // Server sent SendExpZonein after doors/objects — respond to complete handshake
          // This triggers server to send SpawnAppearance(SpawnID), make us visible, etc.
          queueAppPacket(ZoneOpcodes.SendExpZonein, Array.emptyByteArray)

      case ZoneOpcodes.SafePoint => ()

      case ZoneOpcodes.Buff =>
        ZoneCodec.decodeBuff(pkt.payload).foreach { (slot, buff) =>
          emit(ZoneEvent.BuffUpdated(slot, buff))
        }

      case ZoneOpcodes.LogoutReply =>
        println("[Zone] Received OP_LogoutReply — camp complete")
        state = ZoneState.Disconnected
        emit(ZoneEvent.StateChanged(state))

      case other =>
        val name = ZoneOpcodes.name(other)
        println(f"[Zone] Unhandled opcode: 0x${other & 0xFFFF}%04X ($name) ${pkt.payload.length} bytes")

  def tick(): Unit =
    // Build pending app packets
    var pending = pendingApps.poll()
    while pending != null do
      buildAppPacket(pending._1, pending._2)
      pending = pendingApps.poll()

    // Retransmit ZoneEntry if zone server hasn't responded
    if state == ZoneState.WaitingForProfile then
      val now = System.currentTimeMillis()
      if now - lastZoneEntrySentMs > ZoneEntryRetryIntervalMs then
        zoneEntryRetries += 1
        if zoneEntryRetries > ZoneEntryMaxRetries then
          state = ZoneState.Failed
          emit(ZoneEvent.Error(s"Zone server not responding after $ZoneEntryMaxRetries retries"))
          emit(ZoneEvent.StateChanged(state))
        else
          // Retransmit
          buildAppPacket(ZoneOpcodes.ZoneEntry, ZoneCodec.encodeZoneEntry(pendingCharName))
          lastZoneEntrySentMs = now

    if needArsp then
      val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
      outQueue.add(ack)
      needArsp = false

  def pollOutgoing(): Option[Array[Byte]] =
    Option(outQueue.poll())

  private def handleMobPositionUpdate(upd: MobPositionUpdate): Unit =
    spawns.get(upd.spawnId).foreach { existing =>
      spawns(upd.spawnId) = existing.copy(
        y = upd.y, x = upd.x, z = upd.z,
        heading = upd.heading,
        deltaY = upd.deltaY, deltaX = upd.deltaX, deltaZ = upd.deltaZ,
      )
    }
    emit(ZoneEvent.SpawnMoved(upd))

  /** Handle incoming item packets (resync, summoned items, loot, etc).
    * All item packet opcodes carry a raw Item_Struct (360 bytes) payload.
    */
  private def handleItemPacket(opcode: Short, data: Array[Byte]): Unit =
    if data.length < 360 then return
    val item = ZoneCodec.decodeItem(data, 0)
    if item.name.isEmpty then return
    // Update local inventory
    inventory = inventory.filterNot(_.equipSlot == item.equipSlot) :+ item
    emit(ZoneEvent.InventoryItemUpdated(item))

  // ===========================================================================
  // Internal packet building — mirrors WorldClient
  // ===========================================================================

  private def queueAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    pendingApps.add((opcode, payload))

  private def buildAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    if payload.length > 510 then
      buildFragmentedPacket(opcode, payload)
      return

    val arsp = if needArsp then Some(lastInArq) else None
    needArsp = false
    val isFirst = firstPacket
    firstPacket = false
    val packet = OldPacket.encode(
      opcode = opcode,
      payload = payload,
      seq = nextSeq(),
      arq = Some(nextArq()),
      arsp = arsp,
      includeAsq = true,
      seqStart = isFirst,
    )
    outQueue.add(packet)

  private def buildFragmentedPacket(opcode: Short, payload: Array[Byte]): Unit =
    val fSeq = nextFragSeq()
    val fragments = assembler.fragment(opcode, payload, fSeq)
    for (frag, i) <- fragments.zipWithIndex do
      val arsp = if i == 0 && needArsp then Some(lastInArq) else None
      if i == 0 then needArsp = false
      val isFirst = firstPacket
      if i == 0 then firstPacket = false
      val packet = OldPacket.encodeFragment(
        frag = frag,
        seq = nextSeq(),
        arq = if i == 0 then Some(nextArq()) else None,
        arsp = arsp,
        includeAsq = i == 0,
        seqStart = isFirst,
      )
      outQueue.add(packet)

  private def nextSeq(): Int =
    val s = outSeq; outSeq = (outSeq + 1) & 0xFFFF; s

  private def nextArq(): Int =
    val a = outArq; outArq = (outArq + 1) & 0xFFFF; a

  private def nextFragSeq(): Int =
    val s = fragSeq; fragSeq = (fragSeq + 1) & 0xFFFF; s

  private def emit(event: ZoneEvent): Unit = events.add(event)
