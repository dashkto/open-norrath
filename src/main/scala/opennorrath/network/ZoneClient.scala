package opennorrath.network

import java.util.concurrent.ConcurrentLinkedQueue

import opennorrath.Game
import opennorrath.network.titanium.{TitaniumZoneCodec, TitaniumZoneOpcodes}

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
  case FaceChanged(change: FaceChangeData)
  case EquipmentChanged(change: WearChangeInfo)
  case AnimationTriggered(anim: AnimationInfo)
  case SpellActionTriggered(action: SpellAction)
  case BeginCastTriggered(cast: BeginCast)

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
  case StaminaChanged(update: StaminaInfo)

  // Chat — UI chat window
  case ChatReceived(msg: ChatMessage)
  case SpecialMsgReceived(msg: SpecialMessage)
  case FormattedMsgReceived(msg: FormattedMessage)
  case EmoteReceived(msg: EmoteMessage)
  case MultiLineMsgReceived(text: String)
  case YellReceived(spawnId: Int)
  case WhoAllReceived(result: WhoAllResponse)

  // Inventory — UI inventory panel
  case InventoryLoaded(items: Vector[InventoryItem])
  case InventoryItemUpdated(item: InventoryItem)
  case InventoryMoved(fromSlot: Int, toSlot: Int)

  // Spells — spell book updates
  case SpellScribed(spellId: Int, bookSlot: Int)
  case SpellInterrupted(messageId: Int, color: Int, message: String)

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
  case GroupInviteReceived(inviterName: String)

  // Loot — corpse looting flow
  case LootOpened(corpseId: Int, response: LootResponse)
  case LootItemReceived(item: InventoryItem)
  case LootClosed

  // Merchant — buy from NPC merchants
  case MerchantOpened(open: MerchantOpen, items: Vector[InventoryItem])
  case MerchantClosed
  case MerchantItemAdded(item: InventoryItem)

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
  /** Protocol-specific opcodes — resolved once from Game.macMode. */
  private val opcodes: ZoneOpcodes =
    if Game.macMode then MacZoneOpcodes else TitaniumZoneOpcodes

  // Fixed-size packet validation — catches struct size mismatches before they cause silent failures.
  override val expectedPacketSizes: Map[Short, (Int, String)] =
    if Game.macMode then Map.empty
    else Map(
      // Outgoing (client -> server)
      TitaniumZoneOpcodes.ClientUpdate -> (36, "ClientUpdate"),
      TitaniumZoneOpcodes.TargetMouse -> (4, "TargetMouse"),
      TitaniumZoneOpcodes.Consider -> (24, "Consider"),
      TitaniumZoneOpcodes.SetServerFilter -> (36, "SetServerFilter"),
      // Incoming (server -> client)
      TitaniumZoneOpcodes.PlayerProfile -> (19588, "PlayerProfile"),
      TitaniumZoneOpcodes.Death -> (32, "Death"),
      TitaniumZoneOpcodes.DeleteSpawn -> (4, "DeleteSpawn"),
    )

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

  // Zone entry reconnect logic: if the zone server ACKs at the protocol level but never
  // sends app data (PlayerProfile), the zone process likely isn't ready. Retransmitting
  // ZoneEntry on the same connection doesn't help because the server already ACKed it.
  // Instead, signal the loading screen to tear down and create a fresh connection.
  //
  // Server-side context: the zone's GetAuth() check has a 5-second timeout. If auth
  // hasn't arrived from the world server within 5s, the zone kicks the client. Our timeout
  // needs to be longer than 5s to avoid racing with the server's retry mechanism.
  // After MaxReconnectAttempts, the loading screen falls back to character select
  // so the user can re-enter and trigger a fresh world→zone auth flow.
  private var lastZoneEntrySentMs: Long = 0
  @volatile var reconnectAttempt: Int = 0
  private var receivedAppData: Boolean = false
  @volatile var needsReconnect: Boolean = false
  private val ReconnectTimeoutMs = 8000  // >5s to outlast server's get_auth_timer
  val MaxReconnectAttempts = 3

  /** Initiate zone connection. Called from game thread. */
  def connect(charName: String): Unit =
    pendingCharName = charName
    state = ZoneState.Connecting
    emit(ZoneEvent.StateChanged(state))
    // Mac zone signature: first packet must be OP_DataRate for the server to identify us.
    // Titanium doesn't have DataRate (returns 0), so this is skipped.
    if opcodes.DataRate != 0 then
      queueAppPacket(opcodes.DataRate, ZoneCodec.encodeDataRate(8.0f))
    queueAppPacket(opcodes.ZoneEntry, ZoneCodec.encodeZoneEntry(charName))
    lastZoneEntrySentMs = System.currentTimeMillis()
    receivedAppData = false
    state = ZoneState.WaitingForProfile
    emit(ZoneEvent.StateChanged(state))

  /** Send player position update. Called from game thread each tick. */
  def sendPosition(pos: PlayerPosition): Unit =
    if state == ZoneState.InZone then
      // Encoding differs: Mac 15-byte packed int16, Titanium 36-byte floats
      val payload = if Game.macMode then ZoneCodec.encodeClientUpdate(pos)
                    else TitaniumZoneCodec.encodeClientUpdate(pos)
      queueAppPacket(opcodes.ClientUpdate, payload)

  /** Target an entity. Called from game thread. */
  def target(targetId: Int): Unit =
    // Encoding differs: Mac uint16 (2 bytes), Titanium uint32 (4 bytes)
    val payload = if Game.macMode then ZoneCodec.encodeTarget(targetId)
                  else TitaniumZoneCodec.encodeTarget(targetId)
    queueAppPacket(opcodes.TargetMouse, payload)

  /** Consider an entity. Called from game thread. */
  def consider(playerId: Int, targetId: Int): Unit =
    // Encoding differs: Mac uint16 IDs, Titanium uint32 IDs
    val payload = if Game.macMode then ZoneCodec.encodeConsider(playerId, targetId)
                  else TitaniumZoneCodec.encodeConsider(playerId, targetId)
    queueAppPacket(opcodes.Consider, payload)

  /** Send a chat message. Called from game thread. */
  def sendChat(sender: String, target: String, channel: Int, language: Int, message: String): Unit =
    queueAppPacket(opcodes.ChannelMessage, ZoneCodec.encodeChannelMessage(sender, target, channel, language, message))

  /** Toggle auto-attack. Called from game thread. */
  def autoAttack(enabled: Boolean): Unit =
    queueAppPacket(opcodes.AutoAttack, ZoneCodec.encodeAutoAttack(enabled))

  /** Set spawn appearance (sit/stand/etc). Called from game thread. */
  def setAppearance(spawnId: Int, appearanceType: Int, parameter: Int): Unit =
    queueAppPacket(opcodes.SpawnAppearance, ZoneCodec.encodeSpawnAppearance(spawnId, appearanceType, parameter))

  /** Send face/hair/beard change to server. Called from game thread. */
  def sendFaceChange(data: FaceChangeData): Unit =
    queueAppPacket(opcodes.FaceChange, ZoneCodec.encodeFaceChange(data))

  /** Send a /who or /whoall request to the server. Called from game thread. */
  def sendWhoAll(whom: String = ""): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(opcodes.WhoAllRequest, ZoneCodec.encodeWhoAllRequest(whom = whom))

  /** Whether a camp is in progress (client waiting to send OP_Logout). */
  var camping: Boolean = false

  /** Camp / logout. Called from game thread. */
  def camp(): Unit =
    println("[Zone] Sending OP_Camp")
    camping = true
    queueAppPacket(opcodes.Camp, ZoneCodec.encodeCamp)

  /** Send OP_Logout after camp timer expires. Called from game thread. */
  def sendLogout(): Unit =
    println("[Zone] Sending OP_Logout (camp timer expired)")
    queueAppPacket(opcodes.Logout, ZoneCodec.encodeLogout)

  /** Move/swap an inventory item. Called from game thread. */
  def sendMoveItem(fromSlot: Int, toSlot: Int, stackCount: Int = 0): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(opcodes.MoveItem, ZoneCodec.encodeMoveItem(fromSlot, toSlot, stackCount))
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
    queueAppPacket(opcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, zoneId))

  /** Save character. Called from game thread. */
  def save(): Unit =
    queueAppPacket(opcodes.Save, ZoneCodec.encodeSave)

  /** Notify server the player jumped. Called from game thread. */
  def sendJump(): Unit =
    queueAppPacket(opcodes.Jump, ZoneCodec.encodeJump)

  /** Consume food/drink. Called from game thread.
    * @param slot      inventory slot of the item
    * @param itemType  1=food, 2=water (Consume_Struct type field)
    */
  def sendConsume(slot: Int, itemType: Int): Unit =
    queueAppPacket(opcodes.Consume, ZoneCodec.encodeConsume(slot, itemType))

  /** Memorize a spell into a gem slot. Called from game thread.
    * Sends OP_MemorizeSpell with scribing=1 so the server persists the gem assignment.
    */
  def sendMemorizeSpell(gemSlot: Int, spellId: Int): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(opcodes.MemorizeSpell, ZoneCodec.encodeMemorizeSpell(gemSlot, spellId, 1))

  /** Unmemorize (forget) a spell from a gem slot. Called from game thread. */
  def sendForgetSpell(gemSlot: Int, spellId: Int): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(opcodes.MemorizeSpell, ZoneCodec.encodeMemorizeSpell(gemSlot, spellId, 2))

  /** Cast a memorized spell from a gem slot. Called from game thread.
    * @param gemSlot  gem slot 0-7
    * @param spellId  spell ID memorized in that slot
    * @param targetId spawn ID of the target (0 for self/untargeted)
    */
  def sendCastSpell(gemSlot: Int, spellId: Int, targetId: Int): Unit =
    if state == ZoneState.InZone then
      queueAppPacket(opcodes.CastSpell, ZoneCodec.encodeCastSpell(gemSlot, spellId, targetId))

  /** Scribe a spell scroll into the spell book. Called from game thread.
    * Moves the scroll to cursor (slot 0), then sends OP_MemorizeSpell with scribing=0.
    * The server checks cursor for the scroll, validates Effect == spellId, scribes, and deletes it.
    */
  def sendScribeSpell(scrollSlot: Int, spellId: Int, bookSlot: Int): Unit =
    if state == ZoneState.InZone then
      // Step 1: Move scroll to cursor (server expects scroll on cursor for scribing)
      sendMoveItem(scrollSlot, InventoryItem.Cursor)
      // Step 2: Send OP_MemorizeSpell with scribing=0 (scribe)
      queueAppPacket(opcodes.MemorizeSpell, ZoneCodec.encodeMemorizeSpell(bookSlot, spellId, 0))

  // Merchant state — tracks whether we're waiting for shop inventory
  private var pendingMerchant: Option[MerchantOpen] = None
  private var merchantItems: Vector[InventoryItem] = Vector.empty
  var activeMerchantId: Int = 0

  /** Open a merchant's shop. Called from game thread. */
  def openMerchant(npcId: Int): Unit =
    println(s"[Zone] Sending OP_ShopRequest for npcId=$npcId playerId=$mySpawnId")
    queueAppPacket(opcodes.ShopRequest, ZoneCodec.encodeMerchantClick(npcId, mySpawnId))

  /** Buy an item from the open merchant. Called from game thread. */
  def buyFromMerchant(slot: Int, quantity: Int): Unit =
    if activeMerchantId != 0 then
      queueAppPacket(opcodes.ShopPlayerBuy, ZoneCodec.encodeMerchantBuy(activeMerchantId, mySpawnId, slot, quantity))

  /** Close the merchant window. Called from game thread. */
  def closeMerchant(): Unit =
    if activeMerchantId != 0 then
      queueAppPacket(opcodes.ShopEnd, Array.emptyByteArray)
      activeMerchantId = 0
      pendingMerchant = None
      merchantItems = Vector.empty

  /** Leave / disband the current group. Called from game thread. */
  def disbandGroup(): Unit =
    val myName = profile.map(_.name).getOrElse(pendingCharName)
    queueAppPacket(opcodes.GroupDisband, ZoneCodec.encodeGroupDisband(myName))

  /** Invite a player to our group. Called from game thread. */
  def inviteToGroup(targetName: String): Unit =
    val myName = profile.map(_.name).getOrElse(pendingCharName)
    queueAppPacket(opcodes.GroupInvite, ZoneCodec.encodeGroupInvite(targetName, myName))

  /** Accept a pending group invite. Called from game thread. */
  def acceptGroupInvite(inviterName: String): Unit =
    val myName = profile.map(_.name).getOrElse(pendingCharName)
    queueAppPacket(opcodes.GroupFollow, ZoneCodec.encodeGroupFollow(inviterName, myName))

  /** Decline a pending group invite. Called from game thread. */
  def declineGroupInvite(inviterName: String): Unit =
    val myName = profile.map(_.name).getOrElse(pendingCharName)
    queueAppPacket(opcodes.GroupCancelInvite, ZoneCodec.encodeGroupCancelInvite(inviterName, myName))

  /** Tracks which corpse we're currently requesting loot from. */
  var lootingCorpseId: Int = 0

  /** Request to open a corpse for looting. Called from game thread. */
  def requestLoot(corpseId: Int): Unit =
    lootingCorpseId = corpseId
    queueAppPacket(opcodes.LootRequest, ZoneCodec.encodeLootRequest(corpseId))

  /** Loot a specific item from the open corpse. Called from game thread. */
  def lootItem(corpseId: Int, lootSlot: Int): Unit =
    queueAppPacket(opcodes.LootItem, ZoneCodec.encodeLootItem(corpseId, lootSlot))

  /** Close the loot window. Called from game thread. */
  def endLoot(corpseId: Int): Unit =
    queueAppPacket(opcodes.EndLootRequest, ZoneCodec.encodeLootRequest(corpseId))
    lootingCorpseId = 0

  // ===========================================================================
  // PacketHandler implementation — called from network thread
  // ===========================================================================

  def handlePacket(packet: InboundPacket): Unit =
    if Game.macMode then handleMacPacket(packet)
    else handleTitaniumPacket(packet)

  // ===========================================================================
  // Mac protocol handling
  // ===========================================================================

  private def handleMacPacket(packet: InboundPacket): Unit =
    // Track ARQs for acknowledgment (Mac OldPacket layer)
    packet.arq.foreach { arq =>
      lastInArq = arq
      needArsp = true
    }

    // Fragment reassembly — may buffer and return None until complete
    val pkt = assembler.process(packet) match
      case Some(p) => p
      case None => return

    if pkt.opcode == 0 then return // pure ACK

    receivedAppData = true
    dispatchMacOpcode(pkt)

  private def dispatchMacOpcode(pkt: InboundPacket): Unit =
    pkt.opcode match

      // --- Zone Entry Handshake ---

      case MacZoneOpcodes.PlayerProfile =>
        // PlayerProfile is encrypted + zlib compressed (mac.cpp:290-293)
        val rawOpt = PacketCrypto.decryptAndInflateProfile(pkt.payload)
        rawOpt.flatMap(ZoneCodec.decodePlayerProfile) match
          case Some(pp) =>
            profile = Some(pp)
            state = ZoneState.WaitingForZone
            emit(ZoneEvent.ProfileReceived(pp))
            emit(ZoneEvent.StateChanged(state))
            // Client must request zone data — server won't send it unprompted
            queueAppPacket(MacZoneOpcodes.SetServerFilter, ZoneCodec.encodeServerFilter)
            queueAppPacket(MacZoneOpcodes.ReqNewZone, ZoneCodec.encodeReqNewZone)
          case None =>
            emit(ZoneEvent.Error("Failed to decode player profile"))

      case MacZoneOpcodes.NewZone =>
        ZoneCodec.decodeNewZone(pkt.payload) match
          case Some(nz) =>
            zoneInfo = Some(nz)
            emit(ZoneEvent.ZoneDataReceived(nz))
            if state == ZoneState.WaitingForZone then
              state = ZoneState.RequestingSpawns
              emit(ZoneEvent.StateChanged(state))
              queueAppPacket(MacZoneOpcodes.ReqClientSpawn, ZoneCodec.encodeReqClientSpawn)
          case None =>
            emit(ZoneEvent.Error("Failed to decode zone data"))

      case MacZoneOpcodes.TimeOfDay =>
        ZoneCodec.decodeTimeOfDay(pkt.payload).foreach(time => emit(ZoneEvent.TimeReceived(time)))

      // --- Spawns ---

      case MacZoneOpcodes.ZoneEntry =>
        ZoneCodec.decodeServerZoneEntry(pkt.payload).foreach { spawn =>
          selfSpawn = Some(spawn)
          emit(ZoneEvent.SpawnAdded(spawn))
        }

      case MacZoneOpcodes.ZoneSpawns =>
        // ZoneSpawns are encrypted + zlib compressed (mac.cpp:461-462)
        val rawOpt = PacketCrypto.decryptAndInflateSpawns(pkt.payload)
        val rawData = rawOpt.getOrElse(pkt.payload)
        val spawnList = ZoneCodec.decodeZoneSpawns(rawData)
        for s <- spawnList do spawns(s.spawnId) = s
        emit(ZoneEvent.SpawnsLoaded(spawnList))

      case MacZoneOpcodes.NewSpawn =>
        val rawOpt = PacketCrypto.decryptAndInflateSpawns(pkt.payload)
        val rawData = rawOpt.getOrElse(pkt.payload)
        ZoneCodec.decodeSpawn(rawData).foreach { s =>
          spawns(s.spawnId) = s
          emit(ZoneEvent.SpawnAdded(s))
        }

      case MacZoneOpcodes.DeleteSpawn =>
        ZoneCodec.decodeDeleteSpawn(pkt.payload).foreach { id =>
          spawns.remove(id)
          emit(ZoneEvent.SpawnRemoved(id))
        }

      // --- Movement ---

      case MacZoneOpcodes.MobUpdate =>
        for upd <- ZoneCodec.decodeMobUpdates(pkt.payload) do
          handleMobPositionUpdate(upd)

      case MacZoneOpcodes.ClientUpdate =>
        ZoneCodec.decodeClientUpdate(pkt.payload).foreach(handleMobPositionUpdate)

      // --- Combat ---

      case MacZoneOpcodes.Damage =>
        ZoneCodec.decodeDamage(pkt.payload).foreach(d => emit(ZoneEvent.DamageDealt(d)))

      case MacZoneOpcodes.Death =>
        ZoneCodec.decodeDeath(pkt.payload).foreach { d =>
          spawns.remove(d.spawnId)
          emit(ZoneEvent.EntityDied(d))
          if d.spawnId == mySpawnId then
            println(s"[Zone] Player died! Killed by spawnId=${d.killerId} damage=${d.damage}")
        }

      case MacZoneOpcodes.Consider =>
        ZoneCodec.decodeConsider(pkt.payload).foreach(c => emit(ZoneEvent.ConsiderResult(c)))

      // --- Appearance / Animation ---

      case MacZoneOpcodes.SpawnAppearance =>
        handleSpawnAppearance(pkt.payload)

      case MacZoneOpcodes.WearChange =>
        ZoneCodec.decodeWearChange(pkt.payload).foreach(wc => emit(ZoneEvent.EquipmentChanged(wc)))

      case MacZoneOpcodes.FaceChange =>
        ZoneCodec.decodeFaceChange(pkt.payload).foreach(fc => emit(ZoneEvent.FaceChanged(fc)))

      case MacZoneOpcodes.Animation =>
        ZoneCodec.decodeAnimation(pkt.payload).foreach(a => emit(ZoneEvent.AnimationTriggered(a)))

      case MacZoneOpcodes.Action =>
        ZoneCodec.decodeAction(pkt.payload).foreach(a => emit(ZoneEvent.SpellActionTriggered(a)))

      case MacZoneOpcodes.BeginCast =>
        ZoneCodec.decodeBeginCast(pkt.payload).foreach(c => emit(ZoneEvent.BeginCastTriggered(c)))

      case MacZoneOpcodes.MemorizeSpell =>
        ZoneCodec.decodeMemorizeSpell(pkt.payload).foreach { (bookSlot, spellId, scribing) =>
          if scribing == 0 && spellId > 0 then
            println(s"[Zone] Spell $spellId scribed into book slot $bookSlot")
            emit(ZoneEvent.SpellScribed(spellId, bookSlot))
        }

      case MacZoneOpcodes.InterruptCast =>
        ZoneCodec.decodeInterruptCast(pkt.payload).foreach { (messageId, color, message) =>
          emit(ZoneEvent.SpellInterrupted(messageId, color, message))
        }

      // --- Stats ---

      case MacZoneOpcodes.HPUpdate =>
        ZoneCodec.decodeHPUpdate(pkt.payload).foreach(hp => emit(ZoneEvent.HPChanged(hp)))

      case MacZoneOpcodes.ManaChange =>
        ZoneCodec.decodeManaChange(pkt.payload).foreach(m => emit(ZoneEvent.ManaChanged(m)))

      case MacZoneOpcodes.ManaUpdate =>
        ZoneCodec.decodeManaUpdate(pkt.payload).foreach(m => emit(ZoneEvent.ManaChanged(m)))

      case MacZoneOpcodes.ExpUpdate =>
        ZoneCodec.decodeExpUpdate(pkt.payload).foreach(e => emit(ZoneEvent.ExpChanged(e)))

      case MacZoneOpcodes.LevelUpdate =>
        ZoneCodec.decodeLevelUpdate(pkt.payload).foreach(l => emit(ZoneEvent.LevelChanged(l)))

      case MacZoneOpcodes.SkillUpdate =>
        ZoneCodec.decodeSkillUpdate(pkt.payload).foreach(s => emit(ZoneEvent.SkillChanged(s)))

      // --- Chat ---

      case MacZoneOpcodes.ChannelMessage =>
        ZoneCodec.decodeChannelMessage(pkt.payload).foreach(m => emit(ZoneEvent.ChatReceived(m)))

      case MacZoneOpcodes.SpecialMesg =>
        ZoneCodec.decodeSpecialMesg(pkt.payload).foreach(m => emit(ZoneEvent.SpecialMsgReceived(m)))

      case MacZoneOpcodes.FormattedMessage =>
        ZoneCodec.decodeFormattedMessage(pkt.payload).foreach(m => emit(ZoneEvent.FormattedMsgReceived(m)))

      case MacZoneOpcodes.Emote =>
        ZoneCodec.decodeEmote(pkt.payload).foreach(m => emit(ZoneEvent.EmoteReceived(m)))

      case MacZoneOpcodes.MultiLineMsg =>
        ZoneCodec.decodeMultiLineMsg(pkt.payload).foreach(t => emit(ZoneEvent.MultiLineMsgReceived(t)))

      case MacZoneOpcodes.WhoAllResponse =>
        ZoneCodec.decodeWhoAllResponse(pkt.payload).foreach(w => emit(ZoneEvent.WhoAllReceived(w)))

      case MacZoneOpcodes.YellForHelp =>
        if pkt.payload.length >= 4 then
          val spawnId = java.nio.ByteBuffer.wrap(pkt.payload).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
          emit(ZoneEvent.YellReceived(spawnId))

      // --- Environment ---

      case MacZoneOpcodes.Weather | MacZoneOpcodes.Weather2 =>
        ZoneCodec.decodeWeather(pkt.payload).foreach(w => emit(ZoneEvent.WeatherChanged(w)))

      // --- Zone Objects ---

      case MacZoneOpcodes.SpawnDoor =>
        emit(ZoneEvent.DoorsLoaded(ZoneCodec.decodeDoors(pkt.payload)))

      case MacZoneOpcodes.GroundSpawn =>
        ZoneCodec.decodeGroundItem(pkt.payload).foreach(i => emit(ZoneEvent.GroundItemSpawned(i)))

      case MacZoneOpcodes.SendZonepoints =>
        val points = ZoneCodec.decodeZonePoints(pkt.payload)
        zonePoints = points
        emit(ZoneEvent.ZonePointsLoaded(points))

      // --- Group ---

      case MacZoneOpcodes.GroupInvite | MacZoneOpcodes.GroupInvite2 =>
        ZoneCodec.decodeGroupInvite(pkt.payload).foreach(n => emit(ZoneEvent.GroupInviteReceived(n)))

      case MacZoneOpcodes.GroupUpdate =>
        ZoneCodec.decodeGroupUpdate(pkt.payload).foreach((members, leader) =>
          emit(ZoneEvent.GroupUpdated(members, leader)))

      // --- Zone Change ---

      case MacZoneOpcodes.RequestClientZoneChange =>
        ZoneCodec.decodeRequestClientZoneChange(pkt.payload).foreach { req =>
          val charName = profile.map(_.name).getOrElse(pendingCharName)
          queueAppPacket(MacZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, req.zoneId))
          emit(ZoneEvent.ZoneChangeRequested(req))
        }

      case MacZoneOpcodes.GMGoto =>
        ZoneCodec.decodeGMGoto(pkt.payload).foreach { req =>
          println(s"[Zone] OP_GMGoto received — zoning to bind point (zoneId=${req.zoneId})")
          val charName = profile.map(_.name).getOrElse(pendingCharName)
          queueAppPacket(MacZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, req.zoneId))
          emit(ZoneEvent.ZoneChangeRequested(req))
        }

      case MacZoneOpcodes.ZoneChange =>
        ZoneCodec.decodeZoneChange(pkt.payload).foreach { result =>
          println(s"[Zone] OP_ZoneChange response: success=${result.success}")
          if result.success == 1 then emit(ZoneEvent.ZoneChangeAccepted)
          else emit(ZoneEvent.ZoneChangeDenied)
        }

      // --- Inventory / Items ---

      case MacZoneOpcodes.CharInventory =>
        PacketCrypto.inflateInventory(pkt.payload) match
          case Some((count, raw)) =>
            val items = ZoneCodec.decodeInventory(raw, count)
            inventory = items
            emit(ZoneEvent.InventoryLoaded(items))
          case None => ()

      case MacZoneOpcodes.MoveItem =>
        handleMoveItem(pkt.payload)

      case MacZoneOpcodes.ItemPacket | MacZoneOpcodes.TradeItemPacket |
           MacZoneOpcodes.ObjectItemPacket | MacZoneOpcodes.SummonedItem |
           MacZoneOpcodes.ContainerPacket | MacZoneOpcodes.BookPacket =>
        handleItemPacket(pkt.opcode, pkt.payload)

      // --- Merchant ---

      case MacZoneOpcodes.ShopRequest =>
        handleShopRequest(pkt.payload)

      case MacZoneOpcodes.ShopInventoryPacket =>
        val items = ZoneCodec.decodeShopInventory(pkt.payload)
        println(s"[Zone] Received OP_ShopInventoryPacket: ${items.size} items (${pkt.payload.length} bytes)")
        pendingMerchant match
          case Some(open) =>
            activeMerchantId = open.merchantId
            merchantItems = items
            emit(ZoneEvent.MerchantOpened(open, items))
            pendingMerchant = None
          case None =>
            println(s"[Zone] OP_ShopInventoryPacket without pending ShopRequest (${items.size} items)")

      case MacZoneOpcodes.MerchantItemPacket =>
        if pkt.payload.length >= 360 then
          val item = ZoneCodec.decodeItem(pkt.payload, 0)
          if item.name.nonEmpty && activeMerchantId != 0 then
            emit(ZoneEvent.MerchantItemAdded(item))
          else if item.name.nonEmpty then
            handleItemPacket(pkt.opcode, pkt.payload)

      case MacZoneOpcodes.ShopPlayerBuy => ()

      case MacZoneOpcodes.ShopEndConfirm =>
        activeMerchantId = 0; pendingMerchant = None; merchantItems = Vector.empty
        emit(ZoneEvent.MerchantClosed)

      // --- Loot ---

      case MacZoneOpcodes.LootItemPacket =>
        if pkt.payload.length >= 360 then
          val item = ZoneCodec.decodeItem(pkt.payload, 0)
          if item.name.nonEmpty then emit(ZoneEvent.LootItemReceived(item))

      case MacZoneOpcodes.MoneyOnCorpse =>
        ZoneCodec.decodeMoneyOnCorpse(pkt.payload).foreach(r => emit(ZoneEvent.LootOpened(lootingCorpseId, r)))

      case MacZoneOpcodes.LootComplete => emit(ZoneEvent.LootClosed)
      case MacZoneOpcodes.LootRequest | MacZoneOpcodes.LootItem => ()

      // --- Misc ---

      case MacZoneOpcodes.Stamina =>
        ZoneCodec.decodeStamina(pkt.payload).foreach(s => emit(ZoneEvent.StaminaChanged(s)))

      case MacZoneOpcodes.SendExpZonein =>
        if state == ZoneState.RequestingSpawns then
          queueAppPacket(MacZoneOpcodes.SendExpZonein, Array.emptyByteArray)

      case MacZoneOpcodes.SafePoint => ()

      case MacZoneOpcodes.Buff =>
        ZoneCodec.decodeBuff(pkt.payload).foreach((slot, buff) => emit(ZoneEvent.BuffUpdated(slot, buff)))

      case MacZoneOpcodes.LogoutReply =>
        println("[Zone] Received OP_LogoutReply — camp complete")
        state = ZoneState.Disconnected
        emit(ZoneEvent.StateChanged(state))

      case other =>
        val name = MacZoneOpcodes.name(other)
        println(f"[Zone] Unhandled opcode: 0x${other & 0xFFFF}%04X ($name) ${pkt.payload.length} bytes")

  // ===========================================================================
  // Titanium protocol handling
  // ===========================================================================

  private def handleTitaniumPacket(packet: InboundPacket): Unit =
    // Titanium: EqStream handles fragments and ACKs — packets arrive complete
    if packet.opcode == 0 then return

    receivedAppData = true
    val op = packet.opcode
    val data = packet.payload

    // --- Zone Entry Handshake ---

    if op == TitaniumZoneOpcodes.PlayerProfile then
      // Titanium profiles are NOT encrypted/compressed — decode directly
      TitaniumZoneCodec.decodePlayerProfile(data) match
        case Some(pp) =>
          profile = Some(pp)
          state = ZoneState.WaitingForZone
          emit(ZoneEvent.ProfileReceived(pp))
          emit(ZoneEvent.StateChanged(state))
          queueAppPacket(TitaniumZoneOpcodes.SetServerFilter, ZoneCodec.encodeServerFilter)
          queueAppPacket(TitaniumZoneOpcodes.ReqNewZone, ZoneCodec.encodeReqNewZone)
        case None =>
          emit(ZoneEvent.Error("Failed to decode Titanium player profile"))

    else if op == TitaniumZoneOpcodes.NewZone then
      TitaniumZoneCodec.decodeNewZone(data) match
        case Some(nz) =>
          zoneInfo = Some(nz)
          emit(ZoneEvent.ZoneDataReceived(nz))
          if state == ZoneState.WaitingForZone then
            state = ZoneState.RequestingSpawns
            emit(ZoneEvent.StateChanged(state))
            queueAppPacket(TitaniumZoneOpcodes.ReqClientSpawn, ZoneCodec.encodeReqClientSpawn)
        case None =>
          emit(ZoneEvent.Error("Failed to decode Titanium zone data"))

    else if op == TitaniumZoneOpcodes.TimeOfDay then
      ZoneCodec.decodeTimeOfDay(data).foreach(time => emit(ZoneEvent.TimeReceived(time)))

    // --- Spawns ---

    else if op == TitaniumZoneOpcodes.ZoneEntry then
      // Server sends our own spawn back (same Spawn_Struct as other spawns in Titanium)
      TitaniumZoneCodec.decodeServerZoneEntry(data).foreach { spawn =>
        selfSpawn = Some(spawn)
        emit(ZoneEvent.SpawnAdded(spawn))
      }

    else if op == TitaniumZoneOpcodes.ZoneSpawns then
      // Titanium zone spawns are NOT encrypted — decode directly
      val spawnList = TitaniumZoneCodec.decodeZoneSpawns(data)
      for s <- spawnList do spawns(s.spawnId) = s
      emit(ZoneEvent.SpawnsLoaded(spawnList))

    else if op == TitaniumZoneOpcodes.NewSpawn then
      // Titanium new spawns are NOT encrypted
      TitaniumZoneCodec.decodeSpawn(data).foreach { s =>
        spawns(s.spawnId) = s
        emit(ZoneEvent.SpawnAdded(s))
      }

    else if op == TitaniumZoneOpcodes.DeleteSpawn then
      TitaniumZoneCodec.decodeDeleteSpawn(data).foreach { id =>
        spawns.remove(id)
        emit(ZoneEvent.SpawnRemoved(id))
      }

    // --- Movement ---
    // Titanium: OP_ClientUpdate carries ALL position updates (self and other mobs).
    // No separate MobUpdate opcode (Mac's OP_MobUpdate doesn't exist in Titanium).

    else if op == TitaniumZoneOpcodes.ClientUpdate then
      TitaniumZoneCodec.decodeMobUpdate(data).foreach(handleMobPositionUpdate)

    // --- Combat ---

    else if op == TitaniumZoneOpcodes.Damage then
      ZoneCodec.decodeDamage(data).foreach(d => emit(ZoneEvent.DamageDealt(d)))

    else if op == TitaniumZoneOpcodes.Death then
      TitaniumZoneCodec.decodeDeath(data).foreach { d =>
        spawns.remove(d.spawnId)
        emit(ZoneEvent.EntityDied(d))
        if d.spawnId == mySpawnId then
          println(s"[Zone] Player died! Killed by spawnId=${d.killerId} damage=${d.damage}")
      }

    else if op == TitaniumZoneOpcodes.Consider then
      ZoneCodec.decodeConsider(data).foreach(c => emit(ZoneEvent.ConsiderResult(c)))

    // --- Appearance / Animation (shared struct formats) ---

    else if op == TitaniumZoneOpcodes.SpawnAppearance then
      handleSpawnAppearance(data)

    else if op == TitaniumZoneOpcodes.WearChange then
      ZoneCodec.decodeWearChange(data).foreach(wc => emit(ZoneEvent.EquipmentChanged(wc)))

    else if op == TitaniumZoneOpcodes.FaceChange then
      ZoneCodec.decodeFaceChange(data).foreach(fc => emit(ZoneEvent.FaceChanged(fc)))

    else if op == TitaniumZoneOpcodes.Animation then
      ZoneCodec.decodeAnimation(data).foreach(a => emit(ZoneEvent.AnimationTriggered(a)))

    else if op == TitaniumZoneOpcodes.Action then
      ZoneCodec.decodeAction(data).foreach(a => emit(ZoneEvent.SpellActionTriggered(a)))

    else if op == TitaniumZoneOpcodes.BeginCast then
      ZoneCodec.decodeBeginCast(data).foreach(c => emit(ZoneEvent.BeginCastTriggered(c)))

    else if op == TitaniumZoneOpcodes.MemorizeSpell then
      ZoneCodec.decodeMemorizeSpell(data).foreach { (bookSlot, spellId, scribing) =>
        if scribing == 0 && spellId > 0 then
          println(s"[Zone] Spell $spellId scribed into book slot $bookSlot")
          emit(ZoneEvent.SpellScribed(spellId, bookSlot))
      }

    else if op == TitaniumZoneOpcodes.InterruptCast then
      ZoneCodec.decodeInterruptCast(data).foreach { (messageId, color, message) =>
        emit(ZoneEvent.SpellInterrupted(messageId, color, message))
      }

    // --- Stats ---

    else if op == TitaniumZoneOpcodes.HPUpdate then
      ZoneCodec.decodeHPUpdate(data).foreach(hp => emit(ZoneEvent.HPChanged(hp)))

    else if op == TitaniumZoneOpcodes.ManaChange then
      // Titanium only has ManaChange (no separate ManaUpdate opcode)
      ZoneCodec.decodeManaChange(data).foreach(m => emit(ZoneEvent.ManaChanged(m)))

    else if op == TitaniumZoneOpcodes.ExpUpdate then
      ZoneCodec.decodeExpUpdate(data).foreach(e => emit(ZoneEvent.ExpChanged(e)))

    else if op == TitaniumZoneOpcodes.LevelUpdate then
      ZoneCodec.decodeLevelUpdate(data).foreach(l => emit(ZoneEvent.LevelChanged(l)))

    else if op == TitaniumZoneOpcodes.SkillUpdate then
      ZoneCodec.decodeSkillUpdate(data).foreach(s => emit(ZoneEvent.SkillChanged(s)))

    // --- Chat ---

    else if op == TitaniumZoneOpcodes.ChannelMessage then
      ZoneCodec.decodeChannelMessage(data).foreach(m => emit(ZoneEvent.ChatReceived(m)))

    else if op == TitaniumZoneOpcodes.SpecialMesg then
      ZoneCodec.decodeSpecialMesg(data).foreach(m => emit(ZoneEvent.SpecialMsgReceived(m)))

    else if op == TitaniumZoneOpcodes.FormattedMessage then
      ZoneCodec.decodeFormattedMessage(data).foreach(m => emit(ZoneEvent.FormattedMsgReceived(m)))

    else if op == TitaniumZoneOpcodes.Emote then
      ZoneCodec.decodeEmote(data).foreach(m => emit(ZoneEvent.EmoteReceived(m)))

    // No MultiLineMsg in Titanium (opcode 0x0000 = disabled)

    else if op == TitaniumZoneOpcodes.WhoAllResponse then
      ZoneCodec.decodeWhoAllResponse(data).foreach(w => emit(ZoneEvent.WhoAllReceived(w)))

    else if op == TitaniumZoneOpcodes.YellForHelp then
      if data.length >= 4 then
        val spawnId = java.nio.ByteBuffer.wrap(data).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
        emit(ZoneEvent.YellReceived(spawnId))

    // --- Environment ---

    else if op == TitaniumZoneOpcodes.Weather then
      ZoneCodec.decodeWeather(data).foreach(w => emit(ZoneEvent.WeatherChanged(w)))

    // --- Zone Objects ---

    else if op == TitaniumZoneOpcodes.SpawnDoor then
      emit(ZoneEvent.DoorsLoaded(ZoneCodec.decodeDoors(data)))

    else if op == TitaniumZoneOpcodes.GroundSpawn then
      ZoneCodec.decodeGroundItem(data).foreach(i => emit(ZoneEvent.GroundItemSpawned(i)))

    else if op == TitaniumZoneOpcodes.SendZonepoints then
      val points = ZoneCodec.decodeZonePoints(data)
      zonePoints = points
      emit(ZoneEvent.ZonePointsLoaded(points))

    // --- Group ---

    else if op == TitaniumZoneOpcodes.GroupInvite || op == TitaniumZoneOpcodes.GroupInvite2 then
      ZoneCodec.decodeGroupInvite(data).foreach(n => emit(ZoneEvent.GroupInviteReceived(n)))

    else if op == TitaniumZoneOpcodes.GroupUpdate then
      ZoneCodec.decodeGroupUpdate(data).foreach((members, leader) =>
        emit(ZoneEvent.GroupUpdated(members, leader)))

    // --- Zone Change ---

    else if op == TitaniumZoneOpcodes.RequestClientZoneChange then
      ZoneCodec.decodeRequestClientZoneChange(data).foreach { req =>
        val charName = profile.map(_.name).getOrElse(pendingCharName)
        queueAppPacket(TitaniumZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, req.zoneId))
        emit(ZoneEvent.ZoneChangeRequested(req))
      }

    else if op == TitaniumZoneOpcodes.GMGoto then
      ZoneCodec.decodeGMGoto(data).foreach { req =>
        println(s"[Zone] OP_GMGoto received — zoning to bind point (zoneId=${req.zoneId})")
        val charName = profile.map(_.name).getOrElse(pendingCharName)
        queueAppPacket(TitaniumZoneOpcodes.ZoneChange, ZoneCodec.encodeZoneChange(charName, req.zoneId))
        emit(ZoneEvent.ZoneChangeRequested(req))
      }

    else if op == TitaniumZoneOpcodes.ZoneChange then
      ZoneCodec.decodeZoneChange(data).foreach { result =>
        println(s"[Zone] OP_ZoneChange response: success=${result.success}")
        if result.success == 1 then emit(ZoneEvent.ZoneChangeAccepted)
        else emit(ZoneEvent.ZoneChangeDenied)
      }

    // --- Inventory / Items ---
    // TODO: Titanium items use variable-length serialization (SerializeItem) — not yet implemented.
    // For now we log and skip. The fixed 360-byte Mac Item_Struct won't work here.

    else if op == TitaniumZoneOpcodes.CharInventory then
      println(s"[Zone] Titanium OP_CharInventory (${data.length} bytes) — item serialization not yet implemented")

    else if op == TitaniumZoneOpcodes.ItemPacket then
      println(s"[Zone] Titanium OP_ItemPacket (${data.length} bytes) — item serialization not yet implemented")

    else if op == TitaniumZoneOpcodes.MoveItem then
      handleMoveItem(data)

    // --- Merchant ---

    else if op == TitaniumZoneOpcodes.ShopRequest then
      handleShopRequest(data)

    else if op == TitaniumZoneOpcodes.ShopPlayerBuy then ()

    else if op == TitaniumZoneOpcodes.ShopEndConfirm then
      activeMerchantId = 0; pendingMerchant = None; merchantItems = Vector.empty
      emit(ZoneEvent.MerchantClosed)

    // --- Loot ---

    else if op == TitaniumZoneOpcodes.MoneyOnCorpse then
      ZoneCodec.decodeMoneyOnCorpse(data).foreach(r => emit(ZoneEvent.LootOpened(lootingCorpseId, r)))

    else if op == TitaniumZoneOpcodes.LootComplete then emit(ZoneEvent.LootClosed)
    else if op == TitaniumZoneOpcodes.LootRequest || op == TitaniumZoneOpcodes.LootItem then ()

    // --- Misc ---

    else if op == TitaniumZoneOpcodes.Stamina then
      ZoneCodec.decodeStamina(data).foreach(s => emit(ZoneEvent.StaminaChanged(s)))

    else if op == TitaniumZoneOpcodes.SendExpZonein then
      if state == ZoneState.RequestingSpawns then
        queueAppPacket(TitaniumZoneOpcodes.SendExpZonein, Array.emptyByteArray)

    else if op == TitaniumZoneOpcodes.Buff then
      ZoneCodec.decodeBuff(data).foreach((slot, buff) => emit(ZoneEvent.BuffUpdated(slot, buff)))

    else if op == TitaniumZoneOpcodes.LogoutReply then
      println("[Zone] Received OP_LogoutReply — camp complete")
      state = ZoneState.Disconnected
      emit(ZoneEvent.StateChanged(state))

    else
      val name = TitaniumZoneOpcodes.name(op)
      println(f"[Zone] Unhandled Titanium opcode: 0x${op & 0xFFFF}%04X ($name) ${data.length} bytes")

  def tick(): Unit =
    if Game.macMode then
      // Mac: build pending app packets (queued from game thread, built here on network thread)
      var pending = pendingApps.poll()
      while pending != null do
        buildAppPacket(pending._1, pending._2)
        pending = pendingApps.poll()

      // If zone server ACKs at protocol level but never sends app data, the zone process
      // isn't ready. Signal the loading screen to create a fresh connection (new socket).
      if state == ZoneState.WaitingForProfile && !receivedAppData && !needsReconnect then
        val now = System.currentTimeMillis()
        if now - lastZoneEntrySentMs > ReconnectTimeoutMs then
          reconnectAttempt += 1
          if reconnectAttempt > MaxReconnectAttempts then
            state = ZoneState.Failed
            emit(ZoneEvent.Error(s"Zone server not responding after $MaxReconnectAttempts reconnect attempts"))
            emit(ZoneEvent.StateChanged(state))
          else
            println(s"[Zone] Requesting reconnect (attempt $reconnectAttempt/$MaxReconnectAttempts)")
            needsReconnect = true

      if needArsp then
        val ack = OldPacket.encodeAck(nextSeq(), lastInArq)
        outQueue.add(ack)
        needArsp = false
    else
      // Titanium: app packets go directly to appOutQueue via queueAppPacket().
      // TitaniumNetworkThread handles framing, ACKs, and keepalive.
      if state == ZoneState.WaitingForProfile && !receivedAppData && !needsReconnect then
        val now = System.currentTimeMillis()
        if now - lastZoneEntrySentMs > ReconnectTimeoutMs then
          reconnectAttempt += 1
          if reconnectAttempt > MaxReconnectAttempts then
            state = ZoneState.Failed
            emit(ZoneEvent.Error(s"Zone server not responding after $MaxReconnectAttempts reconnect attempts"))
            emit(ZoneEvent.StateChanged(state))
          else
            println(s"[Zone] Requesting Titanium reconnect (attempt $reconnectAttempt/$MaxReconnectAttempts)")
            needsReconnect = true

  def pollOutgoing(): Option[Array[Byte]] =
    Option(outQueue.poll())

  /** Handle SpawnAppearance — shared by Mac and Titanium. Same struct format.
    * SpawnAppearance(SpawnID) during zone entry assigns our spawn ID and transitions to InZone.
    */
  private def handleSpawnAppearance(data: Array[Byte]): Unit =
    ZoneCodec.decodeSpawnAppearance(data).foreach { change =>
      if change.appearanceType == SpawnAppearanceChange.SpawnID && state == ZoneState.RequestingSpawns then
        mySpawnId = change.parameter
        state = ZoneState.InZone
        emit(ZoneEvent.StateChanged(state))
        // Send initial position to trigger server's CompleteConnect → client_data_loaded.
        // Without this, the server stays in CLIENT_CONNECTING and regen/aggro/etc won't run.
        selfSpawn match
          case Some(s) =>
            println(s"[Zone] Sending initial OP_ClientUpdate (spawnId=$mySpawnId) to trigger CompleteConnect")
            sendPosition(PlayerPosition(spawnId = mySpawnId, y = s.y, x = s.x, z = s.z,
              heading = s.heading.toFloat, deltaY = 0, deltaX = 0, deltaZ = 0, deltaHeading = 0, animation = 0))
          case None =>
            println("[Zone] WARNING: selfSpawn is None — cannot send initial OP_ClientUpdate")
      emit(ZoneEvent.AppearanceChanged(change))
    }

  /** Handle OP_ShopRequest response — shared by Mac and Titanium. Same struct format. */
  private def handleShopRequest(data: Array[Byte]): Unit =
    println(s"[Zone] Received OP_ShopRequest response (${data.length} bytes)")
    ZoneCodec.decodeMerchantClick(data) match
      case Some(open) =>
        println(s"[Zone] Merchant open: npcId=${open.merchantId} rate=${open.rate}")
        pendingMerchant = Some(open)
        merchantItems = Vector.empty
      case None =>
        val hex = data.take(16).map(b => f"${b & 0xFF}%02X").mkString(" ")
        println(s"[Zone] OP_ShopRequest decode failed (${data.length} bytes): $hex")

  /** Handle OP_MoveItem from server — shared by Mac and Titanium. Same struct format. */
  private def handleMoveItem(data: Array[Byte]): Unit =
    ZoneCodec.decodeMoveItem(data).foreach { (from, to) =>
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

  /** Queue an app packet for sending. Safe to call from game thread.
    * Mac: queued in pendingApps, built into OldPacket frames by tick().
    * Titanium: goes directly to appOutQueue for TitaniumNetworkThread.
    */
  private def queueAppPacket(opcode: Short, payload: Array[Byte]): Unit =
    if Game.macMode then
      pendingApps.add((opcode, payload))
    else
      appOutQueue.add((opcode, payload))

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
