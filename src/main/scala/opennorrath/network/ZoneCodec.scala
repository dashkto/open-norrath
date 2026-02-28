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

  /** Decode OP_HPUpdate: SpawnHPUpdate_Struct (12 bytes). */
  def decodeHPUpdate(data: Array[Byte]): Option[HPUpdate] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(HPUpdate(buf.getInt(), buf.getInt(), buf.getInt()))

  /** Decode OP_ManaChange: ManaUpdate_Struct (4 bytes). */
  def decodeManaChange(data: Array[Byte]): Option[ManaChange] =
    if data.length < 4 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(ManaChange(buf.getShort() & 0xFFFF, buf.getShort() & 0xFFFF))

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
    Some(SpellAction(aTarget, aSource, aLevel, aInstrument, aForce, aPushHeading, aPushUpAngle, aType, aSpellId, aTap))

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

  /** Decode OP_Buff: SpellBuffFade_Struct (12 bytes).
    * Returns (slotIndex, SpellBuff). */
  def decodeBuff(data: Array[Byte]): Option[(Int, SpellBuff)] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val buffType = buf.get() & 0xFF
    val bLevel = buf.get() & 0xFF
    val bardMod = buf.get() & 0xFF
    val activated = buf.get() & 0xFF
    val spellId = buf.getShort() & 0xFFFF
    val duration = buf.getShort() & 0xFFFF
    val counters = buf.getShort() & 0xFFFF
    val slotId = buf.getShort() & 0xFFFF
    Some((slotId, SpellBuff(buffType, bLevel, bardMod, spellId, duration, counters)))

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
    val itemType = buf.get() & 0xFF       // 0253: ItemType (14=food, 15=drink)
    val magic = buf.get() & 0xFF          // 0254

    // Stackable at offset 0276 (inside common union)
    buf.position(base + 276)
    val stackable = buf.get()             // 0276: 1=stackable, 3=normal, 0=not stackable

    // Charges at offset 0278
    buf.position(base + 278)
    val charges = buf.get()               // 0278 (signed)

    val common = itemClass == 0
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
      itemType = itemType,
      idFileNum = idFileNum,
    )

  // ===========================================================================
  // Group
  // ===========================================================================

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
  // Helpers
  // ===========================================================================

  private def readNullStr(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
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
