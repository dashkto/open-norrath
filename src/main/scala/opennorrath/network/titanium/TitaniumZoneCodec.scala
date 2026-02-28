package opennorrath.network.titanium

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

import opennorrath.network.*

/** Encode/decode zone server payloads for the Titanium (PC) protocol.
  *
  * Key differences from Mac ZoneCodec:
  * - Spawn_Struct: 385 bytes with 19-bit bitfield positions (vs Mac 224 bytes with int16)
  * - PlayerProfile_Struct: 19,588 bytes with uint32 stats (vs Mac 8,460 bytes with int16)
  * - NewZone_Struct: 700 bytes (vs Mac 572), extra zone_id/instance at end
  * - ClientUpdate: 36 bytes with floats (vs Mac 15 bytes packed int16)
  * - MobUpdate (PositionUpdateServer): 22 bytes with bitfield positions (vs Mac 15 bytes)
  * - No encryption — Titanium zone packets are NOT encrypted (Mac encrypts profile + spawns)
  * - No DataRate — Titanium doesn't require OP_DataRate before OP_ZoneEntry
  * - Z position is NOT multiplied by 10 (Mac multiplies z×10)
  * - DeleteSpawn uses uint32 (vs Mac uint16)
  * - Death_Struct uses uint32 fields (vs Mac uint16)
  *
  * Many simpler structs (SpawnAppearance, ChannelMessage, etc.) share the same format
  * and can reuse Mac ZoneCodec methods directly.
  *
  * Reference: EQEmu/common/patches/titanium_structs.h, titanium.cpp
  */
object TitaniumZoneCodec:

  // ===========================================================================
  // Client → Zone encoders
  // ===========================================================================

  // encodeZoneEntry: Same 68-byte ClientZoneEntry_Struct as Mac — reuse ZoneCodec.encodeZoneEntry

  /** OP_ClientUpdate: Titanium PlayerPositionUpdateClient_Struct (36 bytes).
    *
    * Layout: uint16 spawn_id + uint16 sequence + float y + float delta_z +
    *         float delta_y + float delta_x + packed(animation:10, delta_heading:10, pad:12) +
    *         float x + float z + packed(heading:12, pad:20)
    *
    * Unlike Mac (15 bytes with packed int16 coords), Titanium uses raw floats.
    * No z×10 scaling needed.
    */
  def encodeClientUpdate(pos: PlayerPosition): Array[Byte] =
    val buf = ByteBuffer.allocate(36).order(ByteOrder.LITTLE_ENDIAN)
    buf.putShort((pos.spawnId & 0xFFFF).toShort)      // spawn_id
    buf.putShort(0.toShort)                            // sequence (ignored by server)
    buf.putFloat(pos.y)                                // y_pos
    buf.putFloat(pos.deltaZ)                           // delta_z
    buf.putFloat(pos.deltaY)                           // delta_y
    buf.putFloat(pos.deltaX)                           // delta_x
    // Pack: animation:10 | delta_heading:10 | padding:12
    val anim = pos.animation & 0x3FF
    val dHead = (pos.deltaHeading.toInt & 0x3FF)
    buf.putInt(anim | (dHead << 10))
    buf.putFloat(pos.x)                                // x_pos
    buf.putFloat(pos.z)                                // z_pos (no ×10)
    // Pack: heading:12 | padding:20
    val headBits = ((pos.heading * 4096f / 360f).toInt & 0xFFF)
    buf.putInt(headBits)
    buf.array()

  /** OP_TargetMouse / OP_TargetCommand: Titanium uses uint32 spawn IDs (4 bytes). */
  def encodeTarget(targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(targetId)
    buf.array()

  /** OP_Consider: Titanium Consider_Struct — uint32 player_id + uint32 target_id + padding. */
  def encodeConsider(playerId: Int, targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(24).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(playerId)
    buf.putInt(targetId)
    buf.array()

  /** OP_SetServerFilter: Titanium ServerFilter_Struct (36 bytes). */
  def encodeServerFilter: Array[Byte] = new Array[Byte](36)

  // Simple encoders that share the same format as Mac — delegate to ZoneCodec:
  // encodeAutoAttack, encodeChannelMessage, encodeMoveItem, encodeSpawnAppearance,
  // encodeFaceChange, encodeCamp, encodeLogout, encodeReqNewZone, encodeReqClientSpawn,
  // encodeSave, encodeJump, etc.

  // ===========================================================================
  // Zone → Client decoders
  // ===========================================================================

  /** Decode OP_PlayerProfile: Titanium PlayerProfile_Struct (19,588 bytes).
    *
    * NOT encrypted — Titanium zone packets are plaintext (Mac encrypts+zlib compresses).
    * All stat fields are uint32 (Mac uses int16). Spell book uses uint32[400] (Mac: int16[256]).
    * Buffs are 48 bytes each with 25 slots (Mac: 10 bytes each with 15 slots).
    *
    * Key offsets (from titanium_structs.h):
    *   name[64] @ 0x3274, lastName[32] @ 0x32B4, gender @ 0x0004, race @ 0x0008,
    *   class_ @ 0x000C, level @ 0x0014, mana @ 0x08B4, cur_hp @ 0x08B8,
    *   STR-WIS @ 0x08BC-0x08D4, face @ 0x08D8, position x/y/z/heading @ 0x3324-0x3330,
    *   money @ 0x1168-0x1184, spell_book[400] @ 0x0908, mem_spells[8] @ 0x1128,
    *   skills[100] @ 0x1188, buffs[25] @ 0x1390 (48B each), hunger/thirst @ 0x1388-0x138C,
    *   zone_id @ 0x33C4, deity @ 0x007C, guild_id @ 0x32D4, guild_rank @ 0x32E7,
    *   bind_points @ 0x0018, group_members[6] @ 0x33C8, exp @ 0x32F4,
    *   aa_exp @ 0x4C58, aa_points @ 0x184C
    */
  def decodePlayerProfile(data: Array[Byte]): Option[PlayerProfileData] =
    if data.length < 19588 then
      println(s"[TitaniumZoneCodec] PlayerProfile too short: ${data.length}B (expected 19588)")
      return None

    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

      buf.getInt() // checksum (0x0000)
      val gender = buf.getInt() // 0x0004
      val race = buf.getInt() // 0x0008
      val classId = buf.getInt() // 0x000C
      buf.getInt() // unknown (0x0010)
      val level = buf.get() & 0xFF // 0x0014
      buf.get() // level1 (0x0015)
      buf.getShort() // unknown (0x0016)

      // Bind points: 5 × BindStruct (20 bytes each: zoneId(u32) + x/y/z(f32) + heading(f32))
      // at offset 0x0018
      val bindZones = Array.fill(5)(buf.getInt()) // zone IDs
      val bindX = new Array[Float](5)
      val bindY = new Array[Float](5)
      val bindZ = new Array[Float](5)
      // Read each bind's x, y, z, heading
      buf.position(0x0018) // re-position to read properly
      for i <- 0 until 5 do
        buf.position(0x0018 + i * 20)
        bindZones(i) = buf.getInt()
        bindX(i) = buf.getFloat()
        bindY(i) = buf.getFloat()
        bindZ(i) = buf.getFloat()
        buf.getFloat() // heading

      val deity = {
        buf.position(0x007C)
        buf.getInt()
      }

      // Appearance
      buf.position(0x00A8)
      val hairColor = buf.get() & 0xFF
      val beardColor = buf.get() & 0xFF
      val eyeColor1 = buf.get() & 0xFF
      val eyeColor2 = buf.get() & 0xFF
      val hairStyle = buf.get() & 0xFF
      val beard = buf.get() & 0xFF

      // Stats at 0x08B0
      buf.position(0x08B0)
      val points = buf.getInt() // practice points
      val mana = buf.getInt()
      val curHp = buf.getInt()
      val str = buf.getInt()
      val sta = buf.getInt()
      val cha = buf.getInt()
      val dex = buf.getInt()
      val int_ = buf.getInt()
      val agi = buf.getInt()
      val wis = buf.getInt()
      val face = buf.get() & 0xFF // 0x08D8

      // Spell book: uint32[400] at 0x0908 (-1 = empty, capped at 65535 for our model)
      buf.position(0x0908)
      val spellBook = Array.fill(400)(buf.getInt()).filter(_ >= 0).map(_ & 0xFFFF)

      // Memorized spells: uint32[8] at 0x1128
      buf.position(0x1128)
      val memSpells = Array.fill(8)(buf.getInt())

      // Money at 0x1168
      buf.position(0x1168)
      val platinum = buf.getInt()
      val gold = buf.getInt()
      val silver = buf.getInt()
      val copper = buf.getInt()

      // Skills: uint32[100] at 0x1188
      buf.position(0x1188)
      val skills = Array.fill(100)(buf.getInt())

      // Hunger/thirst at 0x1388
      buf.position(0x1388)
      val thirstLevel = buf.getInt()
      val hungerLevel = buf.getInt()

      // Buffs: 25 × 48-byte SpellBuff_Struct at 0x1390
      buf.position(0x1390)
      val buffs = Array.fill(25) {
        val buffType = buf.get() & 0xFF
        val bLevel = buf.get() & 0xFF
        val bardMod = buf.get() & 0xFF
        val activated = buf.get() & 0xFF
        val spellId = buf.getShort() & 0xFFFF
        val duration = buf.getInt()
        val counters = buf.getInt()
        // Skip remaining fields: player_id(4), unknown(4), melee_rune(4),
        // magic_rune(4), dot_rune(4), caston_x/y/z(12), unknown(2)
        buf.position(buf.position() + 34)
        SpellBuff(buffType, bLevel, bardMod, spellId, duration, counters)
      }.filter(_.spellId != 0xFFFF)

      // AA points at 0x184C
      buf.position(0x184C)
      val aaPoints = buf.getInt()

      // Name at 0x3274
      buf.position(0x3274)
      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)

      // Last name at 0x32B4
      buf.position(0x32B4)
      val lastNameBytes = new Array[Byte](32)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes)

      // Guild ID at 0x32D4
      buf.position(0x32D4)
      val guildId = buf.getInt()

      // Guild rank at 0x32E7
      buf.position(0x32E7)
      val guildRank = buf.get() & 0xFF

      // Experience at 0x32F4
      buf.position(0x32F4)
      val exp = buf.getInt()

      // Position at 0x3324
      buf.position(0x3324)
      val posX = buf.getFloat()
      val posY = buf.getFloat()
      val posZ = buf.getFloat()
      val heading = buf.getFloat()

      // Bank money at 0x3338
      buf.position(0x3338)
      val platinumBank = buf.getInt()
      val goldBank = buf.getInt()
      val silverBank = buf.getInt()
      val copperBank = buf.getInt()

      // Expansions at 0x33A0
      buf.position(0x33A0)
      val expansions = buf.getInt()

      // Zone ID at 0x33C4
      buf.position(0x33C4)
      val zoneId = buf.getShort() & 0xFFFF

      // Group members at 0x33C8 — 6 × 64 bytes
      buf.position(0x33C8)
      val groupMembers = Array.fill(6) {
        val gb = new Array[Byte](64)
        buf.get(gb)
        readNullStr(gb)
      }.filter(_.nonEmpty)

      // AA exp at 0x4C58
      buf.position(0x4C58)
      val aaExp = buf.getInt()

      Some(PlayerProfileData(
        name = name, lastName = lastName, race = race, classId = classId,
        gender = gender, level = level, exp = exp, points = points,
        mana = mana, curHp = curHp,
        str = str, sta = sta, cha = cha, dex = dex, int_ = int_, agi = agi, wis = wis,
        y = posY, x = posX, z = posZ, heading = heading, zoneId = zoneId,
        platinum = platinum, gold = gold, silver = silver, copper = copper,
        platinumBank = platinumBank, goldBank = goldBank,
        silverBank = silverBank, copperBank = copperBank,
        face = face, hairColor = hairColor, beardColor = beardColor,
        eyeColor1 = eyeColor1, eyeColor2 = eyeColor2, hairStyle = hairStyle, beard = beard,
        deity = deity, guildId = guildId, guildRank = guildRank,
        spellBook = spellBook, memSpells = memSpells, skills = skills, buffs = buffs,
        hungerLevel = hungerLevel, thirstLevel = thirstLevel,
        aaExp = aaExp, aaPoints = aaPoints, aaPercentage = 0,
        expansions = expansions,
        bindZones = bindZones, bindY = bindY, bindX = bindX, bindZ = bindZ,
        groupMembers = groupMembers,
      ))
    catch
      case e: Exception =>
        println(s"[TitaniumZoneCodec] Error decoding player profile: ${e.getMessage}")
        None

  /** Decode OP_NewZone: Titanium NewZone_Struct (700 bytes).
    *
    * Same initial layout as Mac through maxClip (offset 0x208), then extra fields:
    * zone_short_name2[68], zone_id(uint16), zone_instance(uint16).
    */
  def decodeNewZone(data: Array[Byte]): Option[NewZoneInfo] =
    if data.length < 516 then
      println(s"[TitaniumZoneCodec] NewZone too short: ${data.length}B")
      return None

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
      val fogRed = Array.fill(4)(buf.get() & 0xFF)
      val fogGreen = Array.fill(4)(buf.get() & 0xFF)
      val fogBlue = Array.fill(4)(buf.get() & 0xFF)
      buf.get() // unknown

      val fogMinClip = Array.fill(4)(buf.getFloat())
      val fogMaxClip = Array.fill(4)(buf.getFloat())

      val gravity = buf.getFloat()
      val timeType = buf.get() & 0xFF

      val rainChance = Array.fill(4)(buf.get() & 0xFF)
      val rainDuration = Array.fill(4)(buf.get() & 0xFF)
      val snowChance = Array.fill(4)(buf.get() & 0xFF)
      val snowDuration = Array.fill(4)(buf.get() & 0xFF)

      // Titanium has unknown360[33] then sky, then unknown331[13]
      buf.position(buf.position() + 33) // skip unknown
      val sky = buf.get() & 0xFF
      buf.position(buf.position() + 13) // skip unknown331

      val expMultiplier = buf.getFloat()
      val safeY = buf.getFloat()
      val safeX = buf.getFloat()
      val safeZ = buf.getFloat()
      val maxZ = buf.getFloat()
      val underworld = buf.getFloat()
      val minClip = buf.getFloat()
      val maxClip = buf.getFloat()

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
        println(s"[TitaniumZoneCodec] Error decoding new zone: ${e.getMessage}")
        None

  /** Decode Titanium Spawn_Struct (385 bytes).
    *
    * Completely different layout from Mac's 224-byte Spawn_Struct.
    * Positions use 19-bit bitfield packing instead of int16.
    * No z×10 scaling. Race/spawnId are uint32.
    *
    * Position bitfields (bytes 0x5E-0x72):
    *   [0x5E] deltaHeading:10, x:19, pad:3
    *   [0x62] y:19, animation:10, pad:3
    *   [0x66] z:19, deltaY:13
    *   [0x6A] deltaX:13, heading:12, pad:7
    *   [0x6E] deltaZ:13, pad:19
    */
  def decodeSpawn(data: Array[Byte], offset: Int = 0): Option[SpawnData] =
    if data.length - offset < 385 then return None

    try
      val buf = ByteBuffer.wrap(data, offset, 385).order(ByteOrder.LITTLE_ENDIAN)

      buf.get() // unknown0000 (0x00)
      val gm = buf.get() & 0xFF // 0x01
      buf.get() // unknown0003 (0x02)
      buf.get() // aatitle (0x03)
      buf.get() // unknown0004 (0x04)
      val anon = buf.get() // 0x05
      val face = buf.get() & 0xFF // 0x06

      // name[64] at 0x07
      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)

      val deity = buf.getShort() & 0xFFFF // 0x47
      buf.getShort() // unknown0073 (0x49)
      val size = buf.getFloat() // 0x4B
      buf.getInt() // unknown0079 (0x4F)
      val npcType = buf.get() & 0xFF // 0x53
      val invis = buf.get() & 0xFF // 0x54
      val hairColor = buf.get() & 0xFF // 0x55
      val curHp = buf.get() & 0xFF // 0x56 (percent)
      buf.get() // max_hp (0x57)
      buf.get() // findable (0x58)
      buf.position(buf.position() + 5) // unknown0089[5]

      // Position bitfields at 0x5E (16 bytes)
      val packed0 = buf.getInt() // deltaHeading:10, x:19, pad:3
      val packed1 = buf.getInt() // y:19, animation:10, pad:3
      val packed2 = buf.getInt() // z:19, deltaY:13
      val packed3 = buf.getInt() // deltaX:13, heading:12, pad:7
      val packed4 = buf.getInt() // deltaZ:13, pad:19

      val deltaHeading = signExtend(packed0 & 0x3FF, 10)
      val x = signExtend((packed0 >> 10) & 0x7FFFF, 19).toFloat
      val y = signExtend(packed1 & 0x7FFFF, 19).toFloat
      val animation = (packed1 >> 19) & 0x3FF
      val z = signExtend(packed2 & 0x7FFFF, 19).toFloat
      val rawDY = signExtend((packed2 >> 19) & 0x1FFF, 13)
      val rawDX = signExtend(packed3 & 0x1FFF, 13)
      val heading = (packed3 >> 13) & 0xFFF // 12-bit unsigned (0-4095)
      val rawDZ = signExtend(packed4 & 0x1FFF, 13)

      // Convert heading from 0-4095 to 0-255 for SpawnData compatibility
      val heading8 = (heading * 256 / 4096)

      val eyeColor1 = buf.get() & 0xFF // 0x72
      buf.position(buf.position() + 24) // unknown0115[24]
      buf.get() // showhelm (0x8B)
      buf.position(buf.position() + 4) // unknown0140[4]
      buf.get() // is_npc (0x90)
      val hairStyle = buf.get() & 0xFF // 0x91
      val beardColor = buf.get() & 0xFF // 0x92
      buf.position(buf.position() + 4) // unknown0147[4]
      val level = buf.get() & 0xFF // 0x97
      buf.getInt() // PlayerState (0x98)
      val beard = buf.get() & 0xFF // 0x9C

      // suffix[32] at 0x9D
      buf.position(buf.position() + 32)

      val petOwnerId = buf.getInt() // 0xBD (uint32 in Titanium vs uint16 in Mac)
      val guildRank = buf.get() & 0xFF // 0xC1
      buf.position(buf.position() + 3) // unknown0194[3]

      // TextureProfile (equipment materials): 9 × uint32 at 0xC5
      val equipMaterials = Array.fill(9)(buf.getInt())

      val runSpeed = buf.getFloat() // 0xE9
      val afk = buf.get() // 0xED
      val guildId = buf.getInt() // 0xEE (uint32 in Titanium)

      // title[32] at 0xF2
      buf.position(buf.position() + 32)

      buf.get() // unknown0274 (0x112)
      val helm = buf.get() & 0xFF // 0x113
      buf.position(buf.position() + 8) // set_to_0xFF[8]

      val race = buf.getInt() // 0x11C (uint32 in Titanium)
      buf.getInt() // unknown0288 (0x120)

      // lastName[32] at 0x124
      val lastNameBytes = new Array[Byte](32)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes)

      val walkSpeed = buf.getFloat() // 0x144
      buf.get() // unknown0328 (0x148)
      buf.get() // is_pet (0x149)
      val light = buf.get() & 0xFF // 0x14A
      val classId = buf.get() & 0xFF // 0x14B
      val eyeColor2 = buf.get() & 0xFF // 0x14C
      val flyMode = buf.get() & 0xFF // 0x14D
      val gender = buf.get() & 0xFF // 0x14E
      val bodytype = buf.get() & 0xFF // 0x14F (uint8 in Titanium vs int16 in Mac)
      buf.position(buf.position() + 3) // unknown0336[3]
      val bodyTexture = buf.get() & 0xFF // 0x153

      val spawnId = buf.getInt() // 0x154 (uint32 in Titanium)
      buf.getFloat() // bounding_radius (0x158)

      // TintProfile (equipment tints): 9 × 4 bytes (BGRA) at 0x15C
      val equipColors = TintProfile(Array.fill(9) {
        val blue = buf.get() & 0xFF
        val green = buf.get() & 0xFF
        val red = buf.get() & 0xFF
        val useTint = buf.get() != 0
        TintColor(red, green, blue, useTint)
      })

      Some(SpawnData(
        spawnId = spawnId, name = name, lastName = lastName,
        y = y, x = x, z = z, heading = heading8,
        deltaY = rawDY.toFloat, deltaX = rawDX.toFloat, deltaZ = rawDZ.toFloat,
        deltaHeading = deltaHeading,
        race = race, classId = classId, gender = gender, level = level,
        bodytype = bodytype, deity = deity,
        npcType = npcType, petOwnerId = petOwnerId,
        face = face, hairColor = hairColor, beardColor = beardColor,
        eyeColor1 = eyeColor1, eyeColor2 = eyeColor2,
        hairStyle = hairStyle, beard = beard,
        bodyTexture = bodyTexture, helm = helm,
        equipment = equipMaterials, equipColors = equipColors,
        size = size, walkSpeed = walkSpeed, runSpeed = runSpeed, animation = animation,
        light = light, flyMode = flyMode,
        isInvis = invis != 0, isSneaking = false, isPvp = false,
        isAfk = afk != 0, isLd = false, isGm = gm != 0,
        anon = anon, guildId = guildId, guildRank = guildRank,
        standState = 0,
      ))
    catch
      case e: Exception =>
        println(s"[TitaniumZoneCodec] Error decoding spawn: ${e.getMessage}")
        None

  /** Decode OP_ZoneSpawns: multiple Titanium Spawn_Struct (385 bytes each). */
  def decodeZoneSpawns(data: Array[Byte]): Vector[SpawnData] =
    val spawns = Vector.newBuilder[SpawnData]
    var offset = 0
    while offset + 385 <= data.length do
      decodeSpawn(data, offset).foreach(spawns += _)
      offset += 385
    spawns.result()

  /** Decode ServerZoneEntry for Titanium: same as Spawn_Struct (385 bytes).
    * Titanium's ServerZoneEntry_Struct contains a NewSpawn_Struct which
    * contains a Spawn_Struct. We parse it as a regular spawn.
    */
  def decodeServerZoneEntry(data: Array[Byte]): Option[SpawnData] =
    // ServerZoneEntry wraps NewSpawn_Struct which wraps Spawn_Struct
    // For Titanium, just decode as a spawn
    decodeSpawn(data)

  /** Decode Titanium PlayerPositionUpdateServer_Struct (22 bytes).
    *
    * Same bitfield layout as spawn positions:
    *   uint16 spawn_id
    *   [+2] deltaHeading:10, x:19, pad:3
    *   [+6] y:19, animation:10, pad:3
    *   [+10] z:19, deltaY:13
    *   [+14] deltaX:13, heading:12, pad:7
    *   [+18] deltaZ:13, pad:19
    *
    * No z×10 scaling, no velocity÷16 scaling.
    */
  def decodeMobUpdate(data: Array[Byte]): Option[MobPositionUpdate] =
    if data.length < 22 then return None
    try
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      val spawnId = buf.getShort() & 0xFFFF

      val packed0 = buf.getInt()
      val packed1 = buf.getInt()
      val packed2 = buf.getInt()
      val packed3 = buf.getInt()
      val packed4 = buf.getInt()

      val deltaHeading = signExtend(packed0 & 0x3FF, 10)
      val x = signExtend((packed0 >> 10) & 0x7FFFF, 19).toFloat
      val y = signExtend(packed1 & 0x7FFFF, 19).toFloat
      val animType = (packed1 >> 19) & 0x3FF
      val z = signExtend(packed2 & 0x7FFFF, 19).toFloat
      val deltaY = signExtend((packed2 >> 19) & 0x1FFF, 13).toFloat
      val deltaX = signExtend(packed3 & 0x1FFF, 13).toFloat
      val heading = (packed3 >> 13) & 0xFFF
      val deltaZ = signExtend(packed4 & 0x1FFF, 13).toFloat

      // Convert heading from 0-4095 to 0-255
      val heading8 = (heading * 256 / 4096)

      Some(MobPositionUpdate(
        spawnId = spawnId, y = y, x = x, z = z,
        heading = heading8, deltaHeading = deltaHeading, animType = animType,
        deltaY = deltaY, deltaX = deltaX, deltaZ = deltaZ,
      ))
    catch
      case _: Exception => None

  /** Decode Titanium OP_DeleteSpawn: uint32 spawn_id (4 bytes, vs Mac uint16). */
  def decodeDeleteSpawn(data: Array[Byte]): Option[Int] =
    if data.length < 4 then None
    else Some(ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt())

  /** Decode Titanium OP_Death: Death_Struct (32 bytes with uint32 fields).
    * Mac uses uint16 fields (20 bytes). Titanium uses uint32 throughout.
    */
  def decodeDeath(data: Array[Byte]): Option[DeathInfo] =
    if data.length < 32 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val dSpawnId = buf.getInt()
    val dKillerId = buf.getInt()
    val dCorpseId = buf.getInt()
    val dSkill = buf.getInt() // attack_skill
    val dSpellId = buf.getInt()
    val dBindZone = buf.getInt()
    val dDamage = buf.getInt()
    buf.getInt() // unknown028
    Some(DeathInfo(dSpawnId, dKillerId, dCorpseId, 0, dSpellId, dSkill, dDamage, false))

  // ===========================================================================
  // Helpers
  // ===========================================================================

  /** Sign-extend a value from `bits`-wide to 32-bit signed integer.
    * E.g., signExtend(0x7FFFF, 19) correctly handles negative 19-bit values.
    */
  private def signExtend(value: Int, bits: Int): Int =
    val shift = 32 - bits
    (value << shift) >> shift

  private def readNullStr(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()
