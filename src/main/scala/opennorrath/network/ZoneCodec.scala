package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

/** Encode/decode zone server payloads.
  *
  * Reference: EQMacDocker/Server/common/patches/mac_structs.h
  *            EQMacDocker/Server/common/eq_packet_structs.h
  *            EQMacDocker/Server/zone/client_packet.cpp
  *
  * All structs are #pragma pack(1), little-endian.
  */
object ZoneCodec:

  // ===========================================================================
  // Client → Zone encoders
  // ===========================================================================

  /** OP_ZoneEntry: ClientZoneEntry_Struct (68 bytes).
    * uint32 unknown + char name[64]
    */
  def encodeZoneEntry(charName: String): Array[Byte] =
    val buf = ByteBuffer.allocate(68).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(0) // unknown
    val nameBytes = charName.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)
    buf.array()

  /** OP_ClientUpdate: Player position update.
    * The Mac client sends a position struct — we use the same format as
    * SpawnPositionUpdate_Struct (15 bytes) for simplicity, since the server
    * accepts this format from clients.
    */
  def encodeClientUpdate(pos: PlayerPosition): Array[Byte] =
    val buf = ByteBuffer.allocate(15).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((pos.spawnId & 0xFFFF).toShort)
    buf.put(pos.animation.toByte) // anim_type
    buf.put((pos.heading * 256f / 360f).toInt.toByte) // heading 0-255
    buf.put(0.toByte) // delta_heading
    buf.putShort(pos.y.toInt.toShort)
    buf.putShort(pos.x.toInt.toShort)
    buf.putShort((pos.z * 10f).toInt.toShort) // server expects z × 10
    // Pack deltaY:11, deltaZ:11, deltaX:10 into uint32
    val dy = ((pos.deltaY * 16f).toInt & 0x7FF)
    val dz = ((pos.deltaZ * 16f).toInt & 0x7FF)
    val dx = ((pos.deltaX * 16f).toInt & 0x3FF)
    val packed = dy | (dz << 11) | (dx << 22)
    buf.putInt(packed)
    buf.array()

  /** OP_TargetMouse / OP_TargetCommand: ClientTarget_Struct (2 bytes). */
  def encodeTarget(targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((targetId & 0xFFFF).toShort)
    buf.array()

  /** OP_Consider: Consider_Struct (24 bytes), client sends with playerid + targetid. */
  def encodeConsider(playerId: Int, targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(24).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((playerId & 0xFFFF).toShort)
    buf.putShort((targetId & 0xFFFF).toShort)
    // remaining fields zeroed (server fills them in)
    buf.array()

  /** OP_ChannelMessage: ChannelMessage_Struct (136 + message bytes). */
  def encodeChannelMessage(sender: String, target: String, channel: Int, language: Int, message: String): Array[Byte] =
    val msgBytes = message.getBytes(StandardCharsets.US_ASCII)
    val buf = ByteBuffer.allocate(136 + msgBytes.length + 1).order(ByteOrder.LITTLE_ENDIAN)
    putFixedString(buf, target, 64)
    putFixedString(buf, sender, 64)
    buf.putShort((language & 0xFFFF).toShort)
    buf.putShort((channel & 0xFFFF).toShort)
    buf.putShort(0.toShort) // unused_align
    buf.putShort(0.toShort) // skill_in_language
    buf.put(msgBytes)
    buf.put(0.toByte) // null terminator
    buf.array()

  /** OP_AutoAttack: uint32 (4 bytes), 0=off, 1=on. */
  def encodeAutoAttack(enabled: Boolean): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(if enabled then 1 else 0)
    buf.array()

  /** OP_MoveItem: MoveItem_Struct (12 bytes) — 3x uint32: from_slot, to_slot, number_in_stack. */
  def encodeMoveItem(fromSlot: Int, toSlot: Int, stackCount: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(fromSlot)
    buf.putInt(toSlot)
    buf.putInt(stackCount)
    buf.array()

  /** Decode server OP_MoveItem response (12 bytes). Returns (fromSlot, toSlot). */
  def decodeMoveItem(data: Array[Byte]): Option[(Int, Int)] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some((buf.getInt(), buf.getInt()))

  /** OP_Camp: empty payload. */
  def encodeCamp: Array[Byte] = Array.emptyByteArray
  def encodeLogout: Array[Byte] = Array.emptyByteArray

  /** OP_ReqNewZone: empty payload. */
  def encodeReqNewZone: Array[Byte] = Array.emptyByteArray

  /** OP_ReqClientSpawn: empty payload. */
  def encodeReqClientSpawn: Array[Byte] = Array.emptyByteArray

  /** OP_SpawnAppearance: SpawnAppearance_Struct (8 bytes). */
  def encodeSpawnAppearance(spawnId: Int, appearanceType: Int, parameter: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((spawnId & 0xFFFF).toShort)
    buf.putShort((appearanceType & 0xFFFF).toShort)
    buf.putInt(parameter)
    buf.array()

  /** OP_FaceChange: FaceChange_Struct (7 bytes). */
  def encodeFaceChange(data: FaceChangeData): Array[Byte] =
    val buf = ByteBuffer.allocate(7).order(ByteOrder.LITTLE_ENDIAN)
    buf.put((data.hairColor & 0xFF).toByte)
    buf.put((data.beardColor & 0xFF).toByte)
    buf.put((data.eyeColor1 & 0xFF).toByte)
    buf.put((data.eyeColor2 & 0xFF).toByte)
    buf.put((data.hairStyle & 0xFF).toByte)
    buf.put((data.beard & 0xFF).toByte)
    buf.put((data.face & 0xFF).toByte)
    buf.array()

  /** OP_Save: empty payload. */
  def encodeSave: Array[Byte] = Array.emptyByteArray

  /** OP_Jump: empty payload — tells server the player jumped. */
  def encodeJump: Array[Byte] = Array.emptyByteArray

  /** OP_Consume: Consume_Struct (16 bytes).
    * Tells the server to eat/drink an item.
    * @param slot      inventory slot of the food/drink item
    * @param itemType  1=food, 2=water
    */
  def encodeConsume(slot: Int, itemType: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(slot)           // slot
    buf.putInt(-1)             // auto_consumed = -1 (auto-eat)
    buf.putInt(-1)             // c_unknown1
    buf.putInt(itemType)       // type: 1=food, 2=water
    buf.array()

  /** Encode CastSpell_Struct (12 bytes):
    * slot(uint16) + spell_id(uint16) + inventoryslot(uint16) + target_id(uint16) + spell_crc(uint32).
    * inventoryslot = 0xFFFF for normal gem casts (non-clicky).
    */
  def encodeCastSpell(gemSlot: Int, spellId: Int, targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort(gemSlot.toShort)
    buf.putShort(spellId.toShort)
    buf.putShort(0xFFFF.toShort)     // inventoryslot: 0xFFFF = normal cast (not clicky item)
    buf.putShort(targetId.toShort)
    buf.putInt(0)                    // spell_crc: server doesn't validate this
    buf.array()

  /** Encode MemorizeSpell_Struct (12 bytes): slot(uint32) + spell_id(uint32) + scribing(uint32).
    * scribing: 0=scribe to book, 1=memorize to gem, 2=forget.
    */
  def encodeMemorizeSpell(bookSlot: Int, spellId: Int, scribing: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(bookSlot)
    buf.putInt(spellId)
    buf.putInt(scribing)
    buf.array()

  /** Decode MemorizeSpell_Struct (12 bytes): slot(uint32) + spell_id(uint32) + scribing(uint32).
    * Returns (bookSlot, spellId, scribing).
    */
  def decodeMemorizeSpell(data: Array[Byte]): Option[(Int, Int, Int)] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val slot = buf.getInt()
    val spellId = buf.getInt()
    val scribing = buf.getInt()
    Some((slot, spellId, scribing))

  /** Decode InterruptCast_Struct: messageid(uint16) + color(uint16) + message(char[0]).
    * messageid is an eqstr_us.txt string table ID. color is a message type code.
    * The message field is variable-length (often empty — the string table provides the text).
    * Returns (messageid, color, message).
    */
  def decodeInterruptCast(data: Array[Byte]): Option[(Int, Int, String)] =
    if data.length < 4 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val messageid = buf.getShort() & 0xFFFF
    val color = buf.getShort() & 0xFFFF
    val message = if data.length > 4 then
      val msgBytes = new Array[Byte](data.length - 4)
      buf.get(msgBytes)
      val end = msgBytes.indexOf(0.toByte)
      new String(msgBytes, 0, if end >= 0 then end else msgBytes.length, StandardCharsets.US_ASCII)
    else ""
    Some((messageid, color, message))

  /** OP_LootRequest / OP_EndLootRequest: 2-byte uint16 corpse entity ID. */
  def encodeLootRequest(corpseId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort(corpseId.toShort)
    buf.array()

  /** OP_LootItem: 12-byte LootingItem_Struct. */
  def encodeLootItem(corpseId: Int, lootSlot: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort(corpseId.toShort) // lootee (corpse entity ID)
    buf.putShort(0.toShort)        // looter (0 from client)
    buf.putShort(lootSlot.toShort) // slot_id
    buf.putShort(0.toShort)        // unknown padding
    buf.putInt(1)                  // auto_loot (1 = server finds empty inventory slot)
    buf.array()

  /** OP_ShopRequest: Merchant_Click_Struct (12 bytes).
    * Client sends to request opening a merchant's shop window.
    */
  def encodeMerchantClick(npcId: Int, playerId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((npcId & 0xFFFF).toShort)    // npcid
    buf.putShort((playerId & 0xFFFF).toShort)  // playerid
    buf.put(0.toByte)                           // command (0 = open)
    buf.put(0.toByte); buf.put(0.toByte); buf.put(0.toByte) // unknown[3]
    buf.putFloat(0f)                            // rate (server fills this in)
    buf.array()

  /** OP_ShopPlayerBuy: Merchant_Sell_Struct (16 bytes).
    * Client sends to purchase an item from a merchant.
    */
  def encodeMerchantBuy(npcId: Int, playerId: Int, slot: Int, quantity: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((npcId & 0xFFFF).toShort)     // npcid
    buf.putShort((playerId & 0xFFFF).toShort)   // playerid
    buf.putShort((slot & 0xFFFF).toShort)       // itemslot
    buf.put(0.toByte)                            // IsSold
    buf.put(0.toByte)                            // unknown001
    buf.put((quantity & 0xFF).toByte)            // quantity
    buf.put(0.toByte); buf.put(0.toByte); buf.put(0.toByte) // unknown004[3]
    buf.putInt(0)                                // price (server calculates)
    buf.array()

  /** OP_SetServerFilter: 36 bytes of filter flags (all zeros = accept all). */
  def encodeServerFilter: Array[Byte] = new Array[Byte](36)

  /** OP_DataRate: SetDataRate_Struct (4 bytes). */
  def encodeDataRate(rate: Float): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putFloat(rate)
    buf.array()

  // ===========================================================================
  // Zone → Client decoders
  // ===========================================================================

  /** Decode OP_NewSpawn / individual spawn from OP_ZoneSpawns.
    * Mac Spawn_Struct is 224 bytes (packed).
    */
  def decodeSpawn(data: Array[Byte], offset: Int = 0): Option[SpawnData] =
    if data.length - offset < 224 then return None

    try
      val buf = ByteBuffer.wrap(data, offset, 224).order(ByteOrder.LITTLE_ENDIAN)

      buf.getInt() // random_dontuse (0)
      val accel = buf.get() // 4
      val heading = buf.get() & 0xFF // 5
      val deltaHeading = buf.get() // 6
      val y = buf.getShort().toFloat // 7
      val x = buf.getShort().toFloat // 9
      val z = buf.getShort().toFloat / 10f // 11 — server encodes z × 10

      // Packed velocity: deltaY:11, deltaZ:11, deltaX:10 (13)
      val packed = buf.getInt()
      val rawDY = packed & 0x7FF
      val rawDZ = (packed >> 11) & 0x7FF
      val rawDX = (packed >> 22) & 0x3FF
      val deltaY = signExtend(rawDY, 11) * 0.0625f
      val deltaZ = signExtend(rawDZ, 11) * 0.0625f
      val deltaX = signExtend(rawDX, 10) * 0.0625f

      buf.get() // void1 (17)
      val petOwnerId = buf.getShort() & 0xFFFF // 18
      val animation = buf.get() & 0xFF // 20
      val hairColor = buf.get() & 0xFF // 21
      val beardColor = buf.get() & 0xFF // 22
      val eyeColor1 = buf.get() & 0xFF // 23
      val eyeColor2 = buf.get() & 0xFF // 24
      val hairStyle = buf.get() & 0xFF // 25
      val beard = buf.get() & 0xFF // 26
      buf.get() // title/aa_title (27)
      val size = buf.getFloat() // 28
      val walkSpeed = buf.getFloat() // 32
      val runSpeed = buf.getFloat() // 36

      // TintProfile: 9 slots × 4 bytes = 36 bytes (40)
      val equipColors = readTintProfile(buf)

      val spawnId = buf.getShort() & 0xFFFF // 76
      val bodytype = buf.getShort() // 78
      val curHp = buf.getShort() // 80
      val guildId = buf.getShort() & 0xFFFF // 82
      val race = buf.getShort() & 0xFFFF // 84
      val npcType = buf.get() & 0xFF // 86
      val classId = buf.get() & 0xFF // 87
      val gender = buf.get() & 0xFF // 88
      val level = buf.get() & 0xFF // 89
      val invis = buf.get() & 0xFF // 90
      val sneaking = buf.get() & 0xFF // 91
      val pvp = buf.get() & 0xFF // 92
      val standState = buf.get() & 0xFF // 93
      val light = buf.get() & 0xFF // 94
      val anon = buf.get() // 95
      val afk = buf.get() // 96
      buf.get() // summoned_pc (97)
      val ld = buf.get() // 98
      val gm = buf.get() // 99
      val flyMode = buf.get() & 0xFF // 100
      val bodyTexture = buf.get() & 0xFF // 101
      val helm = buf.get() & 0xFF // 102
      val face = buf.get() & 0xFF // 103

      // equipment[9] — uint16 each (104)
      val equipment = Array.fill(9)(buf.getShort() & 0xFFFF)

      val guildRank = buf.getShort() // 122
      val deity = buf.getShort() & 0xFFFF // 124
      buf.get() // temporaryPet (126)

      // name[64] (127)
      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)

      // lastName[32] (191)
      val lastNameBytes = new Array[Byte](32)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes)

      // void_ (223) — end of 224 byte Mac struct

      Some(SpawnData(
        spawnId = spawnId, name = name, lastName = lastName,
        y = y, x = x, z = z, heading = heading,
        deltaY = deltaY, deltaX = deltaX, deltaZ = deltaZ, deltaHeading = deltaHeading,
        race = race, classId = classId, gender = gender, level = level,
        bodytype = bodytype, deity = deity,
        npcType = npcType, petOwnerId = petOwnerId,
        face = face, hairColor = hairColor, beardColor = beardColor,
        eyeColor1 = eyeColor1, eyeColor2 = eyeColor2,
        hairStyle = hairStyle, beard = beard,
        bodyTexture = bodyTexture, helm = helm,
        equipment = equipment, equipColors = equipColors,
        size = size, walkSpeed = walkSpeed, runSpeed = runSpeed, animation = animation,
        light = light, flyMode = flyMode,
        isInvis = invis != 0, isSneaking = sneaking != 0, isPvp = pvp != 0,
        isAfk = afk != 0, isLd = ld != 0, isGm = gm != 0,
        anon = anon, guildId = guildId, guildRank = guildRank,
        standState = standState,
      ))
    catch
      case e: Exception =>
        None

  /** Decode OP_ZoneSpawns: multiple Spawn_Struct packed back to back. */
  def decodeZoneSpawns(data: Array[Byte]): Vector[SpawnData] =
    val spawns = Vector.newBuilder[SpawnData]
    var offset = 0
    while offset + 224 <= data.length do
      decodeSpawn(data, offset).foreach(spawns += _)
      offset += 224
    spawns.result()

  /** Decode a single SpawnPositionUpdate_Struct (15 bytes) from a ByteBuffer. */
  private def decodeSingleMobUpdate(buf: ByteBuffer): MobPositionUpdate =
    val spawnId = buf.getShort() & 0xFFFF
    val animType = buf.get() & 0xFF
    val heading = buf.get() & 0xFF
    val deltaHeading = buf.get()
    val y = buf.getShort().toFloat
    val x = buf.getShort().toFloat
    val z = buf.getShort().toFloat / 10f // Mac encodes z_pos = z * 10
    val packed = buf.getInt()
    val rawDY = packed & 0x7FF
    val rawDZ = (packed >> 11) & 0x7FF
    val rawDX = (packed >> 22) & 0x3FF
    MobPositionUpdate(
      spawnId = spawnId, y = y, x = x, z = z,
      heading = heading, deltaHeading = deltaHeading, animType = animType,
      deltaY = signExtend(rawDY, 11) * 0.0625f,
      deltaX = signExtend(rawDX, 10) * 0.0625f,
      deltaZ = signExtend(rawDZ, 11) * 0.0625f,
    )

  /** Decode OP_ClientUpdate: bare SpawnPositionUpdate_Struct (15 bytes, no header). */
  def decodeClientUpdate(data: Array[Byte]): Option[MobPositionUpdate] =
    if data.length < 15 then return None
    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      Some(decodeSingleMobUpdate(buf))
    catch
      case _: Exception => None

  /** Decode OP_MobUpdate: SpawnPositionUpdates_Struct.
    * 4-byte uint32 count header followed by N × 15-byte SpawnPositionUpdate_Struct.
    */
  def decodeMobUpdates(data: Array[Byte]): Vector[MobPositionUpdate] =
    if data.length < 19 then return Vector.empty // 4 header + at least 15
    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      val count = buf.getInt() & 0xFFFFFFF // num_updates
      val results = Vector.newBuilder[MobPositionUpdate]
      var i = 0
      while i < count && buf.remaining() >= 15 do
        results += decodeSingleMobUpdate(buf)
        i += 1
      results.result()
    catch
      case _: Exception => Vector.empty

  /** Decode OP_NewZone: Mac NewZone_Struct (572 bytes). */
  def decodeNewZone(data: Array[Byte]): Option[NewZoneInfo] =
    if data.length < 516 then return None

    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

      val charNameBytes = new Array[Byte](64)
      buf.get(charNameBytes)
      val charName = readNullStr(charNameBytes)

      val shortNameBytes = new Array[Byte](32)
      buf.get(shortNameBytes)
      val shortName = readNullStr(shortNameBytes)

      val longNameBytes = new Array[Byte](278)
      buf.get(longNameBytes)
      val longName = readNullStr(longNameBytes)

      val ztype = buf.get() & 0xFF // 374

      val fogRed = Array.fill(4)(buf.get() & 0xFF) // 375
      val fogGreen = Array.fill(4)(buf.get() & 0xFF) // 379
      val fogBlue = Array.fill(4)(buf.get() & 0xFF) // 383
      buf.get() // unknown387

      val fogMinClip = Array.fill(4)(buf.getFloat()) // 388
      val fogMaxClip = Array.fill(4)(buf.getFloat()) // 404

      val gravity = buf.getFloat() // 420
      val timeType = buf.get() & 0xFF // 424

      val rainChance = Array.fill(4)(buf.get() & 0xFF) // 425
      val rainDuration = Array.fill(4)(buf.get() & 0xFF) // 429
      val snowChance = Array.fill(4)(buf.get() & 0xFF) // 433
      val snowDuration = Array.fill(4)(buf.get() & 0xFF) // 437

      // skip specialdates[16], specialcodes[16], timezone (441-473)
      buf.position(buf.position() + 33)

      val sky = buf.get() & 0xFF // 474
      buf.get() // unknown475

      val waterMusic = buf.getShort() // 476
      val normalMusicDay = buf.getShort() // 478
      val normalMusicNight = buf.getShort() // 480
      buf.getShort() // unknown482

      val expMultiplier = buf.getFloat() // 484
      val safeY = buf.getFloat() // 488
      val safeX = buf.getFloat() // 492
      val safeZ = buf.getFloat() // 496
      val maxZ = buf.getFloat() // 500
      val underworld = buf.getFloat() // 504
      val minClip = buf.getFloat() // 508
      val maxClip = buf.getFloat() // 512

      Some(NewZoneInfo(
        charName = charName, zoneShortName = shortName, zoneLongName = longName,
        zoneType = ztype,
        fogRed = fogRed, fogGreen = fogGreen, fogBlue = fogBlue,
        fogMinClip = fogMinClip, fogMaxClip = fogMaxClip,
        gravity = gravity, sky = sky, timeType = timeType,
        rainChance = rainChance, rainDuration = rainDuration,
        snowChance = snowChance, snowDuration = snowDuration,
        minClip = minClip, maxClip = maxClip,
        safeY = safeY, safeX = safeX, safeZ = safeZ,
        maxZ = maxZ, underworld = underworld,
        expMultiplier = expMultiplier,
      ))
    catch
      case e: Exception =>
        // decode error
        None

  /** Decode OP_PlayerProfile: Mac PlayerProfile_Struct (8460 bytes).
    * Extracts the fields useful for UI/gameplay; skips internal-only data.
    */
  def decodePlayerProfile(data: Array[Byte]): Option[PlayerProfileData] =
    if data.length < 5612 then
      // too short
      return None

    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

      buf.getInt() // checksum (0)
      buf.getShort() // unknown (4)

      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes) // 6

      val lastNameBytes = new Array[Byte](66)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes) // 70

      val uniqueGuildId = buf.getInt() // 136
      val gender = buf.get() & 0xFF // 140
      buf.get() // genderchar (141)
      val race = buf.getShort() & 0xFFFF // 142
      val classId = buf.getShort() & 0xFFFF // 144
      val bodytype = buf.getShort() // 146
      val level = buf.get() & 0xFF // 148
      buf.get(); buf.get(); buf.get() // levelchar (149)
      val exp = buf.getInt() // 152
      val points = buf.getShort() // 156
      val mana = buf.getShort() // 158
      val curHp = buf.getShort() // 160
      val status = buf.getShort() & 0xFFFF // 162
      val str = buf.getShort() // 164
      val sta = buf.getShort() // 166
      val cha = buf.getShort() // 168
      val dex = buf.getShort() // 170
      val int_ = buf.getShort() // 172
      val agi = buf.getShort() // 174
      val wis = buf.getShort() // 176
      val face = buf.get() & 0xFF // 178

      // Skip EquipType[9] + EquipColor (TintProfile, 36 bytes) + inventory[30] + languages[32]
      // + invItemProperties[30] (300 bytes)
      // to get to buffs at offset 616
      buf.position(616)

      // buffs[15] — SpellBuff_Struct, 10 bytes each = 150 bytes
      val buffs = Array.fill(15) {
        val buffType = buf.get() & 0xFF
        val bLevel = buf.get() & 0xFF
        val bardMod = buf.get() & 0xFF
        val activated = buf.get() & 0xFF
        val spellId = buf.getShort() & 0xFFFF
        val duration = buf.getShort() & 0xFFFF
        val counters = buf.getShort() & 0xFFFF
        SpellBuff(buffType, bLevel, bardMod, spellId, duration, counters)
      }.filter(_.spellId != 0xFFFF)

      // spell_book[256] at offset 1846 — int16, -1 = empty
      buf.position(1846)
      val spellBook = Array.fill(256)(buf.getShort().toInt).filter(_ >= 0)

      // mem_spells[8] at offset 2870
      buf.position(2870)
      val memSpells = Array.fill(8)(buf.getShort().toInt)

      // Skip to position at offset 2904
      buf.position(2904)
      val posY = buf.getFloat() // 2904
      val posX = buf.getFloat() // 2908
      val posZ = buf.getFloat() // 2912
      val heading = buf.getFloat() // 2916
      buf.getInt() // position placeholder (2920)

      val platinum = buf.getInt() // 2924
      val gold = buf.getInt() // 2928
      val silver = buf.getInt() // 2932
      val copper = buf.getInt() // 2936
      val platinumBank = buf.getInt() // 2940
      val goldBank = buf.getInt() // 2944
      val silverBank = buf.getInt() // 2948
      val copperBank = buf.getInt() // 2952

      // Skip cursor money + unused currency (2956-2987)
      buf.position(2988)
      val skills = Array.fill(100)(buf.getShort().toInt) // 2988

      // Skip innate_skills[25] + misc to hunger/thirst at 3416
      buf.position(3416)
      val hungerLevel = buf.getInt() // 3416
      val thirstLevel = buf.getInt() // 3420

      // Skip to zone_id at 3444
      buf.position(3444)
      val zoneId = buf.getInt() // 3444

      // Skip to bind points at 3784
      buf.position(3784)
      val bindZones = Array.fill(5)(buf.getInt())
      val bindY = Array.fill(5)(buf.getFloat())
      val bindX = Array.fill(5)(buf.getFloat())
      val bindZ = Array.fill(5)(buf.getFloat())

      // Skip to deity at 4944
      buf.position(4944)
      val deity = buf.getShort() & 0xFFFF // 4944
      val guildId = buf.getShort() & 0xFFFF // 4946

      // Skip to guildrank at 4967
      buf.position(4967)
      val guildRank = buf.get() & 0xFF

      // Skip to groupMembers at 5012
      buf.position(5012)
      val groupMembers = Array.fill(6) {
        val gb = new Array[Byte](64)
        buf.get(gb)
        readNullStr(gb)
      }.filter(_.nonEmpty)

      // Skip to expAA at 5420
      buf.position(5420)
      val aaExp = buf.getInt() // 5420
      buf.get() // title (5424)
      val aaPercentage = buf.get() & 0xFF // 5425
      val hairColor = buf.get() & 0xFF // 5426
      val beardColor = buf.get() & 0xFF // 5427
      val eyeColor1 = buf.get() & 0xFF // 5428
      val eyeColor2 = buf.get() & 0xFF // 5429
      val hairStyle = buf.get() & 0xFF // 5430
      val beard = buf.get() & 0xFF // 5431

      // aapoints at offset 5902
      buf.position(5902)
      val aaPoints = buf.getShort() & 0xFFFF

      Some(PlayerProfileData(
        name = name, lastName = lastName, race = race, classId = classId,
        gender = gender, level = level, exp = exp, points = points,
        mana = mana, curHp = curHp,
        str = str, sta = sta, cha = cha, dex = dex, int_ = int_, agi = agi, wis = wis,
        y = posY, x = posX, z = posZ, heading = heading, zoneId = zoneId,
        platinum = platinum, gold = gold, silver = silver, copper = copper,
        platinumBank = platinumBank, goldBank = goldBank, silverBank = silverBank, copperBank = copperBank,
        face = face, hairColor = hairColor, beardColor = beardColor,
        eyeColor1 = eyeColor1, eyeColor2 = eyeColor2, hairStyle = hairStyle, beard = beard,
        deity = deity, guildId = guildId, guildRank = guildRank,
        spellBook = spellBook, memSpells = memSpells, skills = skills, buffs = buffs,
        hungerLevel = hungerLevel, thirstLevel = thirstLevel,
        aaExp = aaExp, aaPoints = aaPoints, aaPercentage = aaPercentage,
        expansions = 0, // TODO: read from offset 3392 if needed
        bindZones = bindZones, bindY = bindY, bindX = bindX, bindZ = bindZ,
        groupMembers = groupMembers,
      ))
    catch
      case e: Exception =>
        // decode error
        None

  /** Decode ServerZoneEntry_Struct (356 bytes) — the server's response to our
    * OP_ZoneEntry, containing the player's own spawn data.
    * This is NOT encrypted or compressed — just raw struct with CRC32 checksum.
    *
    * We parse it into SpawnData for convenience, though it's a different struct
    * than the 224-byte Mac Spawn_Struct used in OP_ZoneSpawns.
    */
  def decodeServerZoneEntry(data: Array[Byte]): Option[SpawnData] =
    if data.length < 356 then
      // too short
      return None

    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

      buf.getInt() // checksum (0)
      val npcType = buf.get() & 0xFF // type (4)

      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes) // 5

      buf.get() // unknown (69)
      buf.getShort() // unknown (70)
      val zoneId = buf.getInt() // 72
      val y = buf.getFloat() // 76
      val x = buf.getFloat() // 80
      val z = buf.getFloat() // 84
      val heading = buf.getFloat() // 88

      // Skip physicsinfo[8] (32 bytes), prev/next/corpse/LocalInfo/My_Char (92-143)
      buf.position(144)
      val viewHeight = buf.getFloat() // 144
      val spriteOHeight = buf.getFloat() // 148
      buf.getShort() // sprite_oheights (152)
      val petOwnerId = buf.getShort() & 0xFFFF // 154
      val maxHp = buf.getInt() // 156
      val curHp = buf.getInt() // 160
      val guildId = buf.getShort() & 0xFFFF // 164

      buf.position(172)
      val npc = buf.get() & 0xFF // NPC (172)
      val classId = buf.get() & 0xFF // 173
      val race = buf.getShort() & 0xFFFF // 174
      val gender = buf.get() & 0xFF // 176
      val level = buf.get() & 0xFF // 177
      val invis = buf.get() & 0xFF // 178
      val sneaking = buf.get() & 0xFF // 179
      val pvp = buf.get() & 0xFF // 180
      val animType = buf.get() & 0xFF // 181
      val light = buf.get() & 0xFF // 182
      val face = buf.get() & 0xFF // 183

      // equipment[9] (184) + unknown (202)
      val equipment = Array.fill(9)(buf.getShort() & 0xFFFF)
      buf.getShort() // unknown (202)

      // equipcolors — TintProfile (204)
      val equipColors = readTintProfile(buf)

      val bodyTexture = buf.getInt() // 240 (uint32 in ServerZoneEntry vs uint8 in Spawn)
      val size = buf.getFloat() // 244
      buf.getFloat() // width (248)
      buf.getFloat() // length (252)
      val helm = buf.getInt() // 256 (uint32)
      val walkSpeed = buf.getFloat() // 260
      val runSpeed = buf.getFloat() // 264
      val ld = buf.get() // 268
      val gm = buf.get() // 269
      val flyMode = buf.getShort() & 0xFFFF // 270
      val bodytype = buf.getInt() // 272

      buf.position(280)
      val anon = buf.get() // 280
      buf.getShort() // avatar (281)
      val afk = buf.get() // 283
      buf.get() // summoned_pc (284)
      buf.get() // title (285)
      buf.position(304) // skip extra[18]

      val lastNameBytes = new Array[Byte](32)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes) // 304

      val guildRank = buf.getShort() // 336
      val deity = buf.getShort() & 0xFFFF // 338
      val animation = buf.get() & 0xFF // 340

      val hairColor = buf.get() & 0xFF // 341
      val beardColor = buf.get() & 0xFF // 342
      val eyeColor1 = buf.get() & 0xFF // 343
      val eyeColor2 = buf.get() & 0xFF // 344
      val hairStyle = buf.get() & 0xFF // 345
      val beard = buf.get() & 0xFF // 346

      // spawnId not in ServerZoneEntry — use 0, will be set by SpawnAppearance(SpawnID)
      Some(SpawnData(
        spawnId = 0, name = name, lastName = lastName,
        y = y, x = x, z = z, heading = (heading * 256f / 512f).toInt, // float heading to 0-255
        deltaY = 0, deltaX = 0, deltaZ = 0, deltaHeading = 0,
        race = race, classId = classId, gender = gender, level = level,
        bodytype = bodytype, deity = deity,
        npcType = npcType, petOwnerId = petOwnerId,
        face = face, hairColor = hairColor, beardColor = beardColor,
        eyeColor1 = eyeColor1, eyeColor2 = eyeColor2,
        hairStyle = hairStyle, beard = beard,
        bodyTexture = bodyTexture & 0xFF, helm = helm & 0xFF,
        equipment = equipment, equipColors = equipColors,
        size = size, walkSpeed = walkSpeed, runSpeed = runSpeed, animation = animation,
        light = light, flyMode = flyMode,
        isInvis = invis != 0, isSneaking = sneaking != 0, isPvp = pvp != 0,
        isAfk = afk != 0, isLd = ld != 0, isGm = gm != 0,
        anon = anon, guildId = guildId, guildRank = guildRank,
        standState = animType,
      ))
    catch
      case e: Exception =>
        // decode error
        None

  /** Decode OP_HPUpdate / OP_MobHealth.
    * Two formats share the same opcode:
    *   SpawnHPUpdate_Struct  (12 bytes): int32 spawnId, int32 curHp, int32 maxHp — literal values (player self)
    *   SpawnHPUpdate_Struct2 (3 bytes):  int16 spawnId, uint8 hpPercent — percentage 0-100 (other mobs)
    */
  def decodeHPUpdate(data: Array[Byte]): Option[HPUpdate] =
    if data.length >= 12 then
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      Some(HPUpdate(buf.getInt(), buf.getInt(), buf.getInt()))
    else if data.length >= 3 then
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      val spawnId = buf.getShort() & 0xFFFF
      val hpPct = buf.get() & 0xFF
      Some(HPUpdate(spawnId, hpPct, 100))
    else None

  /** Decode OP_ManaChange: ManaUpdate_Struct (4 bytes). */
  /** Decode OP_ManaUpdate: ManaUpdate_Struct (4 bytes).
    * Fields: uint16 spawn_id, uint16 cur_mana.
    */
  def decodeManaUpdate(data: Array[Byte]): Option[ManaChange] =
    if data.length < 4 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(ManaChange(buf.getShort() & 0xFFFF, buf.getShort() & 0xFFFF))

  /** Decode OP_ManaChange: ManaChange_Struct (4 bytes).
    * Fields: uint16 new_mana, uint16 spell_id.
    * Self-only packet — no spawnId. The spell_id is the last spell cast (for spell bar re-enable).
    */
  def decodeManaChange(data: Array[Byte]): Option[ManaChange] =
    if data.length < 4 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val newMana = buf.getShort() & 0xFFFF
    // second field is spell_id, not mana — ignore it here
    Some(ManaChange(0, newMana))

  /** Decode OP_BeginCast: BeginCast_Struct (8 bytes). */
  def decodeBeginCast(data: Array[Byte]): Option[BeginCast] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val casterId = buf.getShort() & 0xFFFF
    val spellId = buf.getShort() & 0xFFFF
    val castTime = buf.getShort() & 0xFFFF
    Some(BeginCast(casterId, spellId, castTime))

  /** Decode OP_Damage: Damage_Struct (24 bytes). */
  def decodeDamage(data: Array[Byte]): Option[DamageInfo] =
    if data.length < 24 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(DamageInfo(
      targetId = buf.getShort() & 0xFFFF,
      sourceId = buf.getShort() & 0xFFFF,
      damageType = buf.getShort() & 0xFFFF,
      spellId = buf.getShort() & 0xFFFF,
      damage = buf.getInt(),
      force = buf.getFloat(),
      pushHeading = buf.getFloat(),
      pushUpAngle = buf.getFloat(),
    ))

  /** Decode OP_Death: Death_Struct (20 bytes). */
  def decodeDeath(data: Array[Byte]): Option[DeathInfo] =
    if data.length < 20 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val dSpawnId = buf.getShort() & 0xFFFF
    val dKillerId = buf.getShort() & 0xFFFF
    val dCorpseId = buf.getShort() & 0xFFFF
    val dLevel = buf.get() & 0xFF
    buf.get() // unknown
    val dSpellId = buf.getShort().toInt
    val dSkill = buf.get() & 0xFF
    buf.get() // unknown
    val dDamage = buf.getInt()
    val dIsPC = buf.get() != 0
    Some(DeathInfo(dSpawnId, dKillerId, dCorpseId, dLevel, dSpellId, dSkill, dDamage, dIsPC))

  /** Decode OP_Consider: Consider_Struct (24 bytes). */
  def decodeConsider(data: Array[Byte]): Option[ConsiderResult] =
    if data.length < 24 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(ConsiderResult(
      playerId = buf.getShort() & 0xFFFF,
      targetId = buf.getShort() & 0xFFFF,
      faction = buf.getInt(),
      conLevel = buf.getInt(),
      curHp = buf.getInt(),
      maxHp = buf.getInt(),
      pvpCon = buf.get() != 0,
    ))

  /** Decode OP_ChannelMessage: ChannelMessage_Struct (136 + variable message). */
  def decodeChannelMessage(data: Array[Byte]): Option[ChatMessage] =
    if data.length < 136 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val targetBytes = new Array[Byte](64)
    buf.get(targetBytes)
    val senderBytes = new Array[Byte](64)
    buf.get(senderBytes)
    val language = buf.getShort() & 0xFFFF
    val channel = buf.getShort() & 0xFFFF
    buf.getShort() // unused_align
    val languageSkill = buf.getShort() & 0xFFFF
    val msgBytes = new Array[Byte](data.length - 136)
    buf.get(msgBytes)
    Some(ChatMessage(
      sender = readNullStr(senderBytes),
      target = readNullStr(targetBytes),
      language = language,
      channel = channel,
      languageSkill = languageSkill,
      message = readNullStr(msgBytes),
    ))

  /** Decode OP_SpecialMesg: SpecialMesg_Struct.
    * Wire format: 3-byte header + uint32 msg_type + uint32 target_spawn_id
    * + null-term sayer + 12 unknown bytes + null-term message.
    * The server often sends header as all zeros and an empty sayer field.
    */
  def decodeSpecialMesg(data: Array[Byte]): Option[SpecialMessage] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.position(3) // skip 3-byte header
    val msgType = buf.getInt()
    val targetSpawnId = buf.getInt()
    // Read null-terminated sayer name
    val sayerStart = buf.position()
    var i = sayerStart
    while i < data.length && data(i) != 0 do i += 1
    val sayer = if i > sayerStart then new String(data, sayerStart, i - sayerStart, StandardCharsets.US_ASCII) else ""
    val afterSayer = i + 1 // skip null terminator
    // Skip 12 unknown bytes after sayer
    val msgStart = afterSayer + 12
    if msgStart >= data.length then return Some(SpecialMessage(msgType, targetSpawnId, sayer, ""))
    val message = readNullStr(data.drop(msgStart))
    Some(SpecialMessage(msgType, targetSpawnId, sayer, message))

  /** Decode OP_FormattedMessage: FormattedMessage_Struct.
    * Wire format: uint16 unknown + uint16 string_id + uint16 type
    * + sequence of null-terminated argument strings.
    */
  def decodeFormattedMessage(data: Array[Byte]): Option[FormattedMessage] =
    if data.length < 6 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.getShort() // unknown
    val stringId = buf.getShort() & 0xFFFF
    val msgType = buf.getShort() & 0xFFFF
    // Remaining bytes are null-terminated argument strings
    val args = Vector.newBuilder[String]
    var pos = 6
    while pos < data.length do
      val start = pos
      while pos < data.length && data(pos) != 0 do pos += 1
      if pos > start then
        args += new String(data, start, pos - start, StandardCharsets.US_ASCII)
      pos += 1 // skip null terminator
    Some(FormattedMessage(stringId, msgType, args.result()))

  /** Decode OP_Emote: Emote_Struct.
    * Broadcast wire format: uint16 unknown + variable-length message text
    * (server prepends sender name to message before broadcast).
    */
  def decodeEmote(data: Array[Byte]): Option[EmoteMessage] =
    if data.length < 3 then return None
    // Skip 2-byte unknown header, read the rest as message
    val message = readNullStr(data.drop(2))
    if message.isEmpty then None
    else Some(EmoteMessage(message))

  /** Decode OP_MultiLineMsg: raw null-terminated string. */
  def decodeMultiLineMsg(data: Array[Byte]): Option[String] =
    if data.isEmpty then None
    else
      val text = readNullStr(data)
      if text.isEmpty then None else Some(text)

  /** Decode OP_SpawnAppearance: SpawnAppearance_Struct (8 bytes). */
  def decodeSpawnAppearance(data: Array[Byte]): Option[SpawnAppearanceChange] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(SpawnAppearanceChange(
      spawnId = buf.getShort() & 0xFFFF,
      appearanceType = buf.getShort() & 0xFFFF,
      parameter = buf.getInt(),
    ))

  /** Decode OP_FaceChange: FaceChange_Struct (7 bytes). */
  def decodeFaceChange(data: Array[Byte]): Option[FaceChangeData] =
    if data.length < 7 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(FaceChangeData(
      hairColor = buf.get() & 0xFF,
      beardColor = buf.get() & 0xFF,
      eyeColor1 = buf.get() & 0xFF,
      eyeColor2 = buf.get() & 0xFF,
      hairStyle = buf.get() & 0xFF,
      beard = buf.get() & 0xFF,
      face = buf.get() & 0xFF,
    ))

  /** Decode OP_WearChange: WearChange_Struct (12 bytes). */
  def decodeWearChange(data: Array[Byte]): Option[WearChangeInfo] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val spawnId = buf.getShort() & 0xFFFF
    val slot = buf.get() & 0xFF
    buf.get() // alignment
    val material = buf.getShort() & 0xFFFF
    buf.getShort() // alignment
    val color = readTintColor(buf)
    Some(WearChangeInfo(spawnId, slot, material, color))

  /** Decode OP_Animation: Animation_Struct (12 bytes). */
  def decodeAnimation(data: Array[Byte]): Option[AnimationInfo] =
    if data.length < 6 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(AnimationInfo(
      spawnId = buf.getShort() & 0xFFFF,
      targetId = buf.getShort() & 0xFFFF,
      action = buf.get() & 0xFF,
      value = buf.get() & 0xFF,
    ))

  /** Decode OP_Action: Action_Struct (36 bytes). */
  def decodeAction(data: Array[Byte]): Option[SpellAction] =
    if data.length < 36 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val aTarget = buf.getShort() & 0xFFFF
    val aSource = buf.getShort() & 0xFFFF
    val aLevel = buf.getShort() & 0xFFFF
    buf.getShort() // target_level (unused)
    val aInstrument = buf.getInt()
    val aForce = buf.getFloat()
    val aPushHeading = buf.getFloat()
    val aPushUpAngle = buf.getFloat()
    val aType = buf.get() & 0xFF
    buf.get() // unknown25
    buf.getShort() // spell_id_unused
    val aTap = buf.getShort().toInt
    val aSpellId = buf.getShort() & 0xFFFF
    buf.get() // unknown32
    val buffUnknown = buf.get() & 0xFF // 1 = cast begin, 4 = spell landed/success
    Some(SpellAction(aTarget, aSource, aLevel, aInstrument, aForce, aPushHeading, aPushUpAngle, aType, aSpellId, aTap, buffUnknown))

  /** Decode OP_TimeOfDay: TimeOfDay_Struct (6 bytes). */
  def decodeTimeOfDay(data: Array[Byte]): Option[GameTime] =
    if data.length < 6 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(GameTime(
      hour = buf.get() & 0xFF,
      minute = buf.get() & 0xFF,
      day = buf.get() & 0xFF,
      month = buf.get() & 0xFF,
      year = buf.getShort() & 0xFFFF,
    ))

  /** Decode OP_Weather: Weather_Struct (8 bytes). */
  def decodeWeather(data: Array[Byte]): Option[WeatherInfo] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(WeatherInfo(buf.getInt(), buf.getInt()))

  /** Decode OP_DeleteSpawn: DeleteSpawn_Struct (2 bytes). */
  def decodeDeleteSpawn(data: Array[Byte]): Option[Int] =
    if data.length < 2 then return None
    Some(ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getShort() & 0xFFFF)

  /** Decode OP_ExpUpdate: ExpUpdate_Struct (4 bytes). */
  def decodeExpUpdate(data: Array[Byte]): Option[ExpChange] =
    if data.length < 4 then return None
    Some(ExpChange(ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt()))

  /** Decode OP_LevelUpdate: LevelUpdate_Struct (12 bytes). */
  def decodeLevelUpdate(data: Array[Byte]): Option[LevelChange] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(LevelChange(buf.getInt(), buf.getInt(), buf.getInt()))

  /** Decode OP_SkillUpdate: SkillUpdate_Struct (8 bytes). */
  def decodeSkillUpdate(data: Array[Byte]): Option[SkillChange] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(SkillChange(buf.getInt(), buf.getInt()))

  /** Decode OP_Stamina: Stamina_Struct (5 bytes).
    * Server sends this every ~46 seconds as hunger/thirst tick down,
    * and after consuming food/drink.
    */
  def decodeStamina(data: Array[Byte]): Option[StaminaInfo] =
    if data.length < 5 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val food = buf.getShort().toInt    // int16: hunger level (0–32000)
    val water = buf.getShort().toInt   // int16: thirst level (0–32000)
    val fatigue = buf.get() & 0xFF     // uint8: fatigue (0–100)
    Some(StaminaInfo(food, water, fatigue))

  /** Decode OP_Buff: SpellBuffFade_Struct (20 bytes).
    * Server sends this for buff add/update (bufffade=0) and remove (bufffade=1).
    * For removes, the server sends two packets: one with buff data + bufffade=1,
    * then one with spellId=0xFFFF (SPELL_UNKNOWN) + bufffade=1.
    * Returns (slotIndex, SpellBuff). */
  def decodeBuff(data: Array[Byte]): Option[(Int, SpellBuff)] =
    if data.length < 20 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.getShort() // entityid (unused — always the player)
    val buffType = buf.get() & 0xFF
    val bLevel = buf.get() & 0xFF
    val bardMod = buf.get() & 0xFF
    val activated = buf.get() & 0xFF
    val spellId = buf.getShort() & 0xFFFF
    val duration = buf.getShort() & 0xFFFF
    val counters = buf.getShort() & 0xFFFF
    val slotId = buf.getShort() & 0xFFFF
    buf.getShort() // unk14
    val buffFade = buf.getInt() // 1=remove, 0=update/add
    Some((slotId, SpellBuff(buffType, bLevel, bardMod, spellId, duration, counters)))

  /** Decode OP_MoneyOnCorpse: moneyOnCorpseStruct (20 bytes).
    * response: 0=someone else looting, 1=success, 2=not allowed.
    */
  def decodeMoneyOnCorpse(data: Array[Byte]): Option[LootResponse] =
    if data.length < 20 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val response = buf.get() & 0xFF
    buf.get(); buf.get(); buf.get() // unknown1, unknown2, unknown3
    val platinum = buf.getInt()
    val gold = buf.getInt()
    val silver = buf.getInt()
    val copper = buf.getInt()
    Some(LootResponse(response, platinum, gold, silver, copper))

  /** Decode OP_ShopRequest response: Merchant_Click_Struct (12 bytes).
    * Server sends back with command=1 and rate filled in when merchant is ready.
    */
  def decodeMerchantClick(data: Array[Byte]): Option[MerchantOpen] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val npcId = buf.getShort() & 0xFFFF
    val playerId = buf.getShort() & 0xFFFF
    val command = buf.get() & 0xFF
    buf.get(); buf.get(); buf.get() // unknown[3]
    val rate = buf.getFloat()
    // command=1 means merchant is open and ready
    if command == 1 then Some(MerchantOpen(npcId, rate))
    else None

  /** Decode OP_ShopInventoryPacket.
    * Mac wire format (mac.cpp ENCODE(OP_ShopInventoryPacket)):
    *   byte[0] = item count (uint8)
    *   byte[1] = padding
    *   byte[2..] = zlib compressed MerchantItemsPacket_Struct entries
    * Each MerchantItemsPacket_Struct = uint16 itemtype + Item_Struct (362 bytes).
    */
  def decodeShopInventory(data: Array[Byte]): Vector[InventoryItem] =
    if data.length < 3 then return Vector.empty
    val itemCount = data(0) & 0xFF
    val compressed = data.drop(2)
    PacketCrypto.inflatePacket(compressed, maxSize = 262144) match
      case Some(raw) =>
        val entrySize = 2 + 360 // uint16 itemtype + Item_Struct
        val items = Vector.newBuilder[InventoryItem]
        for i <- 0 until itemCount do
          val offset = i * entrySize
          if offset + entrySize <= raw.length then
            // Skip the 2-byte itemtype prefix, decode the Item_Struct
            val item = decodeItem(raw, offset + 2)
            if item.name.nonEmpty then items += item
        items.result()
      case None =>
        println(s"[Zone] Failed to inflate OP_ShopInventoryPacket (${compressed.length} compressed bytes)")
        Vector.empty

  /** Decode OP_SpawnDoor: DoorSpawns_Struct — count(u16) + Door_Struct[count] (44 bytes each). */
  def decodeDoors(data: Array[Byte]): Vector[DoorData] =
    if data.length < 2 then return Vector.empty
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val count = buf.getShort() & 0xFFFF
    val doors = Vector.newBuilder[DoorData]
    for _ <- 0 until count do
      if buf.remaining() < 44 then return doors.result()
      val nameBytes = new Array[Byte](16)
      buf.get(nameBytes)
      val dName = readNullStr(nameBytes)
      val dY = buf.getFloat()
      val dX = buf.getFloat()
      val dZ = buf.getFloat()
      val dHeading = buf.getFloat()
      val dIncline = buf.getShort() & 0xFFFF
      val dSize = buf.getShort() & 0xFFFF
      buf.getShort() // unknown
      val dDoorId = buf.get() & 0xFF
      val dOpenType = buf.get() & 0xFF
      val dIsOpen = buf.get() != 0
      val dInverted = buf.get() != 0
      val dParam = buf.getShort() & 0xFFFF
      doors += DoorData(dName, dY, dX, dZ, dHeading, dIncline, dSize, dDoorId, dOpenType, dIsOpen, dInverted, dParam)
    doors.result()

  /** Decode OP_GroundSpawn: Object_Struct (224 bytes). */
  def decodeGroundItem(data: Array[Byte]): Option[GroundItemData] =
    if data.length < 224 then return None
    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      buf.getLong() // linked_list_addr (0)
      val itemId = buf.getShort() & 0xFFFF // 8
      buf.getShort() // unknown (10)
      val dropId = buf.getInt() // 12
      val zoneId = buf.getShort() // 16
      buf.position(24) // skip unknown
      val charges = buf.get() & 0xFF // 24
      buf.get() // unknown (25)
      val maxCharges = buf.get() & 0xFF // 26
      buf.position(140) // skip to heading
      val heading = buf.getFloat() // 140
      val z = buf.getFloat() // 144
      val x = buf.getFloat() // 148
      val y = buf.getFloat() // 152
      val objNameBytes = new Array[Byte](16) // 156
      buf.get(objNameBytes)
      buf.position(212) // skip to object_type
      val objectType = buf.getInt() // 212
      Some(GroundItemData(itemId, dropId, zoneId, charges, maxCharges, heading, y, x, z,
        readNullStr(objNameBytes), objectType))
    catch
      case _: Exception => None

  /** Decode OP_SendZonepoints: ZonePoints (count + ZonePoint_Entry[count], 24 bytes each). */
  def decodeZonePoints(data: Array[Byte]): Vector[ZonePointData] =
    if data.length < 4 then return Vector.empty
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val count = buf.getInt()
    val points = Vector.newBuilder[ZonePointData]
    for _ <- 0 until count do
      if buf.remaining() < 24 then return points.result()
      points += ZonePointData(
        iterator = buf.getInt(),
        y = buf.getFloat(),
        x = buf.getFloat(),
        z = buf.getFloat(),
        heading = buf.getFloat(),
        targetZoneId = buf.getShort() & 0xFFFF,
      )
      buf.getShort() // unused
    points.result()

  /** Decode OP_ZoneChange: ZoneChange_Struct (76 bytes). */
  def decodeZoneChange(data: Array[Byte]): Option[ZoneChangeResult] =
    if data.length < 76 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = new Array[Byte](64)
    buf.get(nameBytes)
    val zcName = readNullStr(nameBytes)
    val zcZoneId = buf.getShort() & 0xFFFF
    val zcReason = buf.getShort() & 0xFFFF
    buf.getShort() // unknown
    buf.getShort() // unknown
    val zcSuccess = buf.get().toInt
    Some(ZoneChangeResult(zcName, zcZoneId, zcReason, zcSuccess))

  /** Encode OP_ZoneChange: ZoneChange_Struct (76 bytes).
    * Client sends this in response to OP_RequestClientZoneChange.
    */
  def encodeZoneChange(charName: String, zoneId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(76).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = charName.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)
    buf.putShort((zoneId & 0xFFFF).toShort)
    // reason, unknown, unknown, success — all zero
    buf.array()

  /** Decode OP_RequestClientZoneChange (24 bytes). */
  def decodeRequestClientZoneChange(data: Array[Byte]): Option[ZoneChangeRequest] =
    if data.length < 24 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(ZoneChangeRequest(
      zoneId = buf.getInt(),
      y = buf.getFloat(),
      x = buf.getFloat(),
      z = buf.getFloat(),
      heading = buf.getFloat(),
    ))

  // ===========================================================================
  // Inventory — OP_CharInventory
  // ===========================================================================

  /** Decode decompressed inventory data into a list of items.
    *
    * After decompression, the data is N concatenated PlayerItemsPacket_Struct entries,
    * each 362 bytes: int16 opcode + Item_Struct (360 bytes).
    *
    * Layout of Item_Struct (mac_structs.h, #pragma pack(1)):
    *   0000: Name[64]        — item name
    *   0064: Lore[80]        — lore text
    *   0144: IDFile[30]      — model filename
    *   0174: Weight (uint8)
    *   0175: NoRent (uint8)  — 1=normal, 0=nosave
    *   0176: NoDrop (uint8)  — 1=normal, 0=nodrop
    *   0177: Size (uint8)
    *   0178: ItemClass (int16) — 0=common, 1=container, 2=book
    *   0180: ID (int16)
    *   0182: Icon (uint16)
    *   0184: equipSlot (int16) — current slot location
    *   ...
    *   0228-0276: union { common, book, container }
    *     common.AC at 0244 (int16), common.HP at 0240 (int16), common.Mana at 0242 (int16)
    *     common.Damage at 0250 (uint8), common.Delay at 0249 (uint8), common.Magic at 0254 (uint8)
    *   0278: Charges (int8)
    */
  private val PlayerItemsPacketSize = 362
  private val ItemStructSize = 360

  def decodeInventory(data: Array[Byte], count: Int): Vector[InventoryItem] =
    val items = Vector.newBuilder[InventoryItem]
    var offset = 0
    var i = 0
    while i < count && offset + PlayerItemsPacketSize <= data.length do
      val item = decodeItem(data, offset + 2) // skip 2-byte opcode
      if item.name.nonEmpty then
        items += item
      offset += PlayerItemsPacketSize
      i += 1
    items.result()

  /** Decode a single Item_Struct (360 bytes) at the given offset. */
  def decodeItem(data: Array[Byte], base: Int): InventoryItem =
    val buf = ByteBuffer.wrap(data, base, ItemStructSize).order(ByteOrder.LITTLE_ENDIAN)

    val nameBytes = new Array[Byte](64)
    buf.get(nameBytes)
    val loreBytes = new Array[Byte](80)
    buf.get(loreBytes)
    val idFileBytes = new Array[Byte](30)
    buf.get(idFileBytes)
    val idFile = readNullStr(idFileBytes).toUpperCase
    val idFileNum = if idFile.startsWith("IT") then
      try idFile.drop(2).toInt catch case _: NumberFormatException => 0
    else 0

    val weight = buf.get() & 0xFF        // 0174
    val noRent = buf.get() & 0xFF        // 0175
    val noDrop = buf.get() & 0xFF        // 0176

    buf.get()                             // 0177: Size
    val itemClass = buf.getShort() & 0xFFFF // 0178
    val id = buf.getShort()               // 0180
    val icon = buf.getShort() & 0xFFFF    // 0182
    val equipSlot = buf.getShort()        // 0184
    buf.getShort()                        // 0186: unknown
    val slots = buf.getInt()              // 0188: bitmask of valid equipment slots

    buf.position(base + 192)
    val price = buf.getInt()              // 0192: price in copper

    // Common union stats (itemClass == 0)
    buf.position(base + 228)
    val aStr = buf.get()                  // 0228
    val aSta = buf.get()                  // 0229
    val aCha = buf.get()                  // 0230
    val aDex = buf.get()                  // 0231
    val aInt = buf.get()                  // 0232
    val aAgi = buf.get()                  // 0233
    val aWis = buf.get()                  // 0234
    val mr = buf.get()                    // 0235
    val fr = buf.get()                    // 0236
    val cr = buf.get()                    // 0237
    val dr = buf.get()                    // 0238
    val pr = buf.get()                    // 0239
    val hp = buf.getShort()               // 0240
    val mana = buf.getShort()             // 0242
    val ac = buf.getShort()               // 0244
    buf.position(base + 249)
    val delay = buf.get() & 0xFF          // 0249
    val damage = buf.get() & 0xFF         // 0250
    buf.position(base + 253)
    val itemTypeRaw = buf.get() & 0xFF    // 0253: ItemType
    val itemType = ItemType.fromCode(itemTypeRaw).getOrElse(ItemType.Misc)
    val magic = buf.get() & 0xFF          // 0254

    // Effect1 at offset 266 — spell ID for scroll items (Scroll.Effect in mac_structs.h)
    buf.position(base + 266)
    val effect1 = buf.getShort()          // 0266: scroll spell ID (int16)

    // Stackable at offset 0276 (inside common union)
    buf.position(base + 276)
    val stackable = buf.get()             // 0276: 1=stackable, 3=normal, 0=not stackable

    // Charges at offset 0278
    buf.position(base + 278)
    val charges = buf.get()               // 0278 (signed)

    val common = itemClass == 0

    // Container union (itemClass == 1) — BagSlots@269, BagSize@271, BagWR@272 per mac_structs.h
    val container = itemClass == 1
    buf.position(base + 269)
    val bagSlots = if container then buf.get() & 0xFF else 0
    buf.position(base + 271)
    val bagSize = if container then buf.get() & 0xFF else 0
    val bagWR = if container then buf.get() & 0xFF else 0

    InventoryItem(
      name = readNullStr(nameBytes),
      lore = readNullStr(loreBytes),
      equipSlot = equipSlot,
      itemClass = itemClass,
      id = id,
      icon = icon,
      slots = slots,
      weight = weight,
      noRent = noRent == 0,
      noDrop = noDrop == 0,
      magic = magic != 0,
      aStr = if common then aStr else 0,
      aSta = if common then aSta else 0,
      aCha = if common then aCha else 0,
      aDex = if common then aDex else 0,
      aInt = if common then aInt else 0,
      aAgi = if common then aAgi else 0,
      aWis = if common then aWis else 0,
      mr = if common then mr else 0,
      fr = if common then fr else 0,
      cr = if common then cr else 0,
      dr = if common then dr else 0,
      pr = if common then pr else 0,
      ac = if common then ac else 0,
      hp = if common then hp else 0,
      mana = if common then mana else 0,
      damage = if common then damage else 0,
      delay = if common then delay else 0,
      charges = charges,
      stackable = common && stackable == 1,
      itemType = if common then itemType else ItemType.Misc,
      price = price,
      idFileNum = idFileNum,
      scrollSpellId = if common then effect1.toInt else 0,
      bagSlots = bagSlots,
      bagSize = bagSize,
      bagWR = bagWR,
    )

  // ===========================================================================
  // Group
  // ===========================================================================

  /** Encode GroupInvite_Struct (193 bytes) for OP_GroupInvite.
    * invitee_name[64] + inviter_name[64] + unknown[65].
    */
  def encodeGroupInvite(inviteeName: String, inviterName: String): Array[Byte] =
    val data = new Array[Byte](193)
    writeNullStr(data, 0, inviteeName, 64)
    writeNullStr(data, 64, inviterName, 64)
    data

  /** Decode GroupInvite_Struct (193 bytes): invitee_name[64] + inviter_name[64] + unknown[65].
    * Returns the inviter's name.
    */
  def decodeGroupInvite(data: Array[Byte]): Option[String] =
    if data.length < 128 then return None
    val inviterBytes = new Array[Byte](64)
    System.arraycopy(data, 64, inviterBytes, 0, 64)
    val inviter = readNullStr(inviterBytes)
    if inviter.nonEmpty then Some(inviter) else None

  /** Encode GroupGeneric_Struct (128 bytes) for OP_GroupFollow (accept invite).
    * name1 = inviter, name2 = acceptor (our character).
    */
  def encodeGroupFollow(inviterName: String, myName: String): Array[Byte] =
    val data = new Array[Byte](128)
    writeNullStr(data, 0, inviterName, 64)
    writeNullStr(data, 64, myName, 64)
    data

  /** Encode GroupGeneric_Struct (128 bytes) for OP_GroupDisband.
    * name1 = our character, name2 = our character (server only checks name1).
    */
  def encodeGroupDisband(myName: String): Array[Byte] =
    val data = new Array[Byte](128)
    writeNullStr(data, 0, myName, 64)
    writeNullStr(data, 64, myName, 64)
    data

  /** Encode GroupCancel_Struct (129 bytes) for OP_GroupCancelInvite (decline invite).
    * name1 = inviter, name2 = invitee (our character), toggle = 2.
    */
  def encodeGroupCancelInvite(inviterName: String, myName: String): Array[Byte] =
    val data = new Array[Byte](129)
    writeNullStr(data, 0, inviterName, 64)
    writeNullStr(data, 64, myName, 64)
    data(128) = 2.toByte
    data

  /** Decode GroupUpdate_Struct: action(4) + yourname(64) + 5×membername(64) + leadername(64).
    * Returns (memberNames, leaderName). Only emitted for action=0 (full update).
    */
  def decodeGroupUpdate(data: Array[Byte]): Option[(Vector[String], String)] =
    if data.length < 452 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val action = buf.getInt()
    // Skip yourname (64 bytes)
    buf.position(buf.position() + 64)
    val members = Vector.newBuilder[String]
    for _ <- 0 until 5 do
      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)
      if name.nonEmpty then members += name
    val leaderBytes = new Array[Byte](64)
    buf.get(leaderBytes)
    val leader = readNullStr(leaderBytes)
    Some((members.result(), leader))

  // ===========================================================================
  // WhoAll — OP_WhoAllRequest / OP_WhoAllResponse
  // ===========================================================================

  /** OP_WhoAllRequest: Who_All_Struct (140 bytes).
    * Sends search filters to the server. 0xFFFF means "no filter" for each field.
    */
  def encodeWhoAllRequest(whom: String = "", wrace: Int = -1, wclass: Int = -1,
                          lvllow: Int = -1, lvlhigh: Int = -1,
                          gmlookup: Int = -1, guildid: Int = -1): Array[Byte] =
    val buf = ByteBuffer.allocate(140).order(ByteOrder.LITTLE_ENDIAN)
    putFixedString(buf, whom, 64)
    buf.putShort((wrace & 0xFFFF).toShort)
    buf.putShort((wclass & 0xFFFF).toShort)
    buf.putShort((lvllow & 0xFFFF).toShort)
    buf.putShort((lvlhigh & 0xFFFF).toShort)
    buf.putShort((gmlookup & 0xFFFF).toShort)
    buf.putShort((guildid & 0xFFFF).toShort)
    // remaining 64 bytes are zeroed by allocate()
    buf.array()

  /** OP_WhoAllResponse: WhoAllReturnStruct (variable length).
    *
    * Wire layout — header (58 bytes):
    *   uint32  id                    (requesting client's spawn ID)
    *   uint16  playerineqstring      (string table ID for header)
    *   char    line[27]              (separator line "----...")
    *   uint8   unknown35             (0x0A)
    *   uint16  unknown36             (0)
    *   uint16  playersinzonestring   (string table ID)
    *   uint16  unknown44[5]          (0s)
    *   uint32  unknown52             (total player count)
    *   uint32  unknown56             (1)
    *   uint16  playercount           (number of player records following)
    *
    * Per-player record (variable length):
    *   uint16  formatstring
    *   uint16  pidstring
    *   char[]  name                  (null-terminated, variable length)
    *   uint16  rankstring
    *   char[]  guild                 (null-terminated, variable length)
    *   uint16  unknown_gm            (GM admin level, 0xFFFF = not GM)
    *   uint16  unknown80             (0xFFFF)
    *   uint16  flag                  (LFG flag, 0xFFFF = none)
    *   uint16  zonestring
    *   uint32  zone                  (zone ID)
    *   uint16  class_
    *   uint16  level
    *   uint16  race
    *   char[]  account               (null-terminated, variable length)
    *   uint16  ending                (211)
    */
  def decodeWhoAllResponse(data: Array[Byte]): Option[WhoAllResponse] =
    // Header is 58 bytes minimum
    if data.length < 58 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.getInt()   // id (requester spawn ID)
    buf.getShort() // playerineqstring
    buf.position(buf.position() + 27) // line[27]
    buf.get()      // unknown35
    buf.getShort() // unknown36
    buf.getShort() // playersinzonestring
    for _ <- 0 until 5 do buf.getShort() // unknown44[5]
    buf.getInt()   // unknown52
    buf.getInt()   // unknown56
    val playerCount = buf.getShort() & 0xFFFF

    val players = Vector.newBuilder[WhoAllPlayerEntry]
    for _ <- 0 until playerCount do
      if buf.remaining() < 4 then return Some(WhoAllResponse(playerCount, players.result()))
      val formatString = buf.getShort() & 0xFFFF
      val pidString = buf.getShort() & 0xFFFF
      val name = readNullStrFromBuffer(buf)
      val rankString = buf.getShort() & 0xFFFF
      val guild = readNullStrFromBuffer(buf)
      buf.getShort() // unknown_gm
      buf.getShort() // unknown80
      buf.getShort() // flag
      val zoneString = buf.getShort() & 0xFFFF
      val zone = buf.getInt()
      val classId = buf.getShort() & 0xFFFF
      val level = buf.getShort() & 0xFFFF
      val race = buf.getShort() & 0xFFFF
      val account = readNullStrFromBuffer(buf)
      buf.getShort() // ending (211)
      players += WhoAllPlayerEntry(
        name = name, guild = guild, level = level, classId = classId,
        race = race, zone = zone, account = account,
        formatString = formatString, rankString = rankString, zoneString = zoneString,
      )
    Some(WhoAllResponse(playerCount, players.result()))

  // ===========================================================================
  // Helpers
  // ===========================================================================

  /** Write a null-terminated string into a pre-zeroed byte array at the given offset. */
  private def writeNullStr(dest: Array[Byte], offset: Int, s: String, maxLen: Int): Unit =
    val bytes = s.getBytes(StandardCharsets.US_ASCII)
    val copyLen = Math.min(bytes.length, maxLen - 1)
    System.arraycopy(bytes, 0, dest, offset, copyLen)

  private def readNullStr(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()

  /** Read a null-terminated string from the current ByteBuffer position, advancing past the null. */
  private def readNullStrFromBuffer(buf: ByteBuffer): String =
    val sb = StringBuilder()
    while buf.hasRemaining do
      val b = buf.get()
      if b == 0 then return sb.result()
      sb += b.toChar
    sb.result()

  private def putFixedString(buf: ByteBuffer, s: String, len: Int): Unit =
    val bytes = s.getBytes(StandardCharsets.US_ASCII)
    val fixed = new Array[Byte](len)
    System.arraycopy(bytes, 0, fixed, 0, Math.min(bytes.length, len - 1))
    buf.put(fixed)

  private def readTintColor(buf: ByteBuffer): TintColor =
    val blue = buf.get() & 0xFF
    val green = buf.get() & 0xFF
    val red = buf.get() & 0xFF
    val useTint = buf.get() != 0
    TintColor(red, green, blue, useTint)

  private def readTintProfile(buf: ByteBuffer): TintProfile =
    TintProfile(Array.fill(9)(readTintColor(buf)))

  /** Sign-extend a value from `bits` width to Int. */
  private def signExtend(value: Int, bits: Int): Int =
    val shift = 32 - bits
    (value << shift) >> shift
