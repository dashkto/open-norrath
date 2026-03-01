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

  /** OP_SetServerFilter: Titanium ServerFilter_Struct (116 bytes).
    * uint32 filters[29] — Mac uses 36 bytes (9 uint32s), Titanium has 29 filter categories.
    */
  def encodeServerFilter: Array[Byte] = new Array[Byte](116)

  /** OP_ChannelMessage: Titanium ChannelMessage_Struct (148-byte header + message).
    * Mac has 136-byte header. Titanium adds uint32 cm_unknown4[2] (8 bytes extra).
    * Layout: char target[64], char sender[64], uint32 language, uint32 chan_num,
    *         uint32 cm_unknown4[2], uint32 skill_in_language, char message[0]
    */
  def encodeChannelMessage(sender: String, target: String, channel: Int,
                           language: Int, message: String): Array[Byte] =
    val msgBytes = message.getBytes(StandardCharsets.US_ASCII)
    val buf = ByteBuffer.allocate(148 + msgBytes.length + 1).order(ByteOrder.LITTLE_ENDIAN)
    val targetBuf = new Array[Byte](64)
    val senderBuf = new Array[Byte](64)
    val tBytes = target.getBytes(StandardCharsets.US_ASCII)
    val sBytes = sender.getBytes(StandardCharsets.US_ASCII)
    System.arraycopy(tBytes, 0, targetBuf, 0, Math.min(tBytes.length, 63))
    System.arraycopy(sBytes, 0, senderBuf, 0, Math.min(sBytes.length, 63))
    buf.put(targetBuf)          // 000-063: target name
    buf.put(senderBuf)          // 064-127: sender name
    buf.putInt(language)        // 128: language
    buf.putInt(channel)         // 132: chan_num
    buf.putInt(0)               // 136: cm_unknown4[0]
    buf.putInt(0)               // 140: cm_unknown4[1]
    buf.putInt(0)               // 144: skill_in_language
    buf.put(msgBytes)           // 148+: message
    buf.put(0.toByte)           // null terminator
    buf.array()

  /** OP_MemorizeSpell: Titanium MemorizeSpell_Struct (16 bytes).
    * Mac is 12 bytes (3 × uint32). Titanium adds uint32 reduction field.
    */
  def encodeMemorizeSpell(bookSlot: Int, spellId: Int, scribing: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(bookSlot)
    buf.putInt(spellId)
    buf.putInt(scribing)
    buf.putInt(0) // reduction
    buf.array()

  /** OP_CastSpell: Titanium CastSpell_Struct (20 bytes).
    * Mac is 12 bytes (slot + spell_id + target_id). Titanium adds inventoryslot + cs_unknown[4].
    * Layout: uint32 slot, uint32 spell_id, uint32 inventoryslot, uint32 target_id, uint8 cs_unknown[4]
    */
  def encodeCastSpell(gemSlot: Int, spellId: Int, targetId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(20).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(gemSlot)
    buf.putInt(spellId)
    buf.putInt(0xFFFF)  // inventoryslot: 0xFFFF = normal cast (not clicky)
    buf.putInt(targetId)
    buf.putInt(0)       // cs_unknown[4]
    buf.array()

  /** OP_ShopRequest: Titanium Merchant_Click_Struct (16 bytes).
    * Mac is 12 bytes (uint16 npc_id + uint16 player_id + command + rate).
    * Titanium uses uint32 IDs: uint32 npc_id, uint32 player_id, uint32 command, float rate.
    */
  def encodeMerchantClick(npcId: Int, playerId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(npcId)
    buf.putInt(playerId)
    buf.putInt(0) // command: 0=close, 1=open (not used for client→server request)
    buf.putFloat(0f)
    buf.array()

  /** OP_ShopPlayerBuy: Titanium Merchant_Purchase_Struct (16 bytes).
    * Mac uses uint16 IDs. Titanium uses all uint32 fields.
    * Layout: uint32 npcid, uint32 itemslot, uint32 quantity, uint32 price
    */
  def encodeMerchantBuy(npcId: Int, playerId: Int, slot: Int, quantity: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(npcId)
    buf.putInt(slot)
    buf.putInt(quantity)
    buf.putInt(0) // price — server calculates
    buf.array()

  /** OP_GroupInvite: Titanium GroupInvite_Struct (128 bytes).
    * Mac is 193 bytes (invitee[64] + inviter[64] + unknown[65]).
    * Titanium is just invitee_name[64] + inviter_name[64].
    */
  def encodeGroupInvite(targetName: String, myName: String): Array[Byte] =
    val buf = ByteBuffer.allocate(128).order(ByteOrder.LITTLE_ENDIAN)
    val target = new Array[Byte](64)
    val self = new Array[Byte](64)
    val tBytes = targetName.getBytes(StandardCharsets.US_ASCII)
    val sBytes = myName.getBytes(StandardCharsets.US_ASCII)
    System.arraycopy(tBytes, 0, target, 0, Math.min(tBytes.length, 63))
    System.arraycopy(sBytes, 0, self, 0, Math.min(sBytes.length, 63))
    buf.put(target)
    buf.put(self)
    buf.array()

  /** OP_LootRequest / OP_EndLootRequest: Titanium uses uint32 corpseId (4 bytes).
    * Mac uses uint16 (2 bytes). Server checks `sizeof(uint32)` and rejects wrong size.
    */
  def encodeLootRequest(corpseId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(corpseId)
    buf.array()

  /** OP_LootItem: Titanium LootingItem_Struct (16 bytes).
    * Mac is 12 bytes (uint16 IDs). Titanium uses uint32 lootee/looter.
    * Layout: uint32 lootee, uint32 looter, uint16 slot_id, uint8 unknown[2], int32 auto_loot
    */
  def encodeLootItem(corpseId: Int, lootSlot: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(16).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(corpseId)           // lootee
    buf.putInt(0)                  // looter (0 from client)
    buf.putShort(lootSlot.toShort) // slot_id
    buf.putShort(0)                // unknown padding
    buf.putInt(1)                  // auto_loot (1 = server finds empty slot)
    buf.array()

  // Simple encoders that share the same format as Mac — delegate to ZoneCodec:
  // encodeAutoAttack, encodeMoveItem, encodeSpawnAppearance, encodeFaceChange,
  // encodeCamp, encodeLogout, encodeReqNewZone, encodeReqClientSpawn,
  // encodeSave, encodeJump, encodeGroupFollow, encodeGroupDisband,
  // encodeGroupCancelInvite, encodeConsume, encodeWhoAllRequest, etc.

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
    *   name[64] @ 0x328C, lastName[32] @ 0x32CC, gender @ 0x0004, race @ 0x0008,
    *   class_ @ 0x000C, level @ 0x0014, mana @ 0x08B4, cur_hp @ 0x08B8,
    *   STR-WIS @ 0x08BC-0x08D4, face @ 0x08D8, position x/y/z/heading @ 0x333C-0x3348,
    *   money @ 0x114C-0x1158, spell_book[400] @ 0x0908, mem_spells[9] @ 0x1108,
    *   skills[100] @ 0x116C, buffs[25] @ 0x1390 (48B each), hunger/thirst @ 0x1388-0x138C,
    *   zone_id @ 0x33DC, deity @ 0x007C, guild_id @ 0x32EC, guild_rank @ 0x32FF,
    *   bind_points @ 0x0018, group_members[6] @ 0x33E0, exp @ 0x330C,
    *   aa_exp @ 0x4C28, aa_points @ 0x180C
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

      // Memorized spells: uint32[9] at 0x1108 (SPELL_GEM_COUNT=9 for Titanium)
      buf.position(0x1108)
      val memSpells = Array.fill(9)(buf.getInt())

      // Money at 0x114C
      buf.position(0x114C)
      val platinum = buf.getInt()
      val gold = buf.getInt()
      val silver = buf.getInt()
      val copper = buf.getInt()

      // Skills: uint32[100] at 0x116C
      buf.position(0x116C)
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

      // AA points at 0x180C
      buf.position(0x180C)
      val aaPoints = buf.getInt()

      // Name at 0x328C
      buf.position(0x328C)
      val nameBytes = new Array[Byte](64)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)

      // Last name at 0x32CC
      buf.position(0x32CC)
      val lastNameBytes = new Array[Byte](32)
      buf.get(lastNameBytes)
      val lastName = readNullStr(lastNameBytes)

      // Guild ID at 0x32EC
      buf.position(0x32EC)
      val guildId = buf.getInt()

      // Guild rank at 0x32FF
      buf.position(0x32FF)
      val guildRank = buf.get() & 0xFF

      // Experience at 0x330C
      buf.position(0x330C)
      val exp = buf.getInt()

      // Position at 0x333C
      buf.position(0x333C)
      val posX = buf.getFloat()
      val posY = buf.getFloat()
      val posZ = buf.getFloat()
      val heading = buf.getFloat()

      // Bank money at 0x3350
      buf.position(0x3350)
      val platinumBank = buf.getInt()
      val goldBank = buf.getInt()
      val silverBank = buf.getInt()
      val copperBank = buf.getInt()

      // Expansions at 0x33B8
      buf.position(0x33B8)
      val expansions = buf.getInt()

      // Zone ID at 0x33DC
      buf.position(0x33DC)
      val zoneId = buf.getShort() & 0xFFFF

      // Group members at 0x33E0 — 6 × 64 bytes
      buf.position(0x33E0)
      val groupMembers = Array.fill(6) {
        val gb = new Array[Byte](64)
        buf.get(gb)
        readNullStr(gb)
      }.filter(_.nonEmpty)

      // AA exp at 0x4C28
      buf.position(0x4C28)
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
    *
    * Server packs positions with FloatToEQ19 (×8) and deltas with FloatToEQ13 (×64).
    * Must divide by 8.0 / 64.0 respectively to recover float values.
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
      // EQ19toFloat: packed value / 8.0 (server packs with FloatToEQ19 = float * 8.0)
      val x = signExtend((packed0 >> 10) & 0x7FFFF, 19).toFloat / 8.0f
      val y = signExtend(packed1 & 0x7FFFF, 19).toFloat / 8.0f
      val animation = (packed1 >> 19) & 0x3FF
      val z = signExtend(packed2 & 0x7FFFF, 19).toFloat / 8.0f
      // EQ13toFloat: packed value / 64.0 (server packs with FloatToEQ13 = float * 64.0)
      val rawDY = signExtend((packed2 >> 19) & 0x1FFF, 13).toFloat / 64.0f
      val rawDX = signExtend(packed3 & 0x1FFF, 13).toFloat / 64.0f
      val heading = (packed3 >> 13) & 0xFFF // 12-bit unsigned (0-4095)
      val rawDZ = signExtend(packed4 & 0x1FFF, 13).toFloat / 64.0f

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
        deltaY = rawDY, deltaX = rawDX, deltaZ = rawDZ,
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
    * Positions use EQ19 packing (÷8.0), deltas use EQ13 packing (÷64.0).
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
      // EQ19toFloat: packed value / 8.0 (server packs with FloatToEQ19 = float * 8.0)
      val x = signExtend((packed0 >> 10) & 0x7FFFF, 19).toFloat / 8.0f
      val y = signExtend(packed1 & 0x7FFFF, 19).toFloat / 8.0f
      val animType = (packed1 >> 19) & 0x3FF
      val z = signExtend(packed2 & 0x7FFFF, 19).toFloat / 8.0f
      // EQ13toFloat: packed value / 64.0 (server packs with FloatToEQ13 = float * 64.0)
      val deltaY = signExtend((packed2 >> 19) & 0x1FFF, 13).toFloat / 64.0f
      val deltaX = signExtend(packed3 & 0x1FFF, 13).toFloat / 64.0f
      val heading = (packed3 >> 13) & 0xFFF
      val deltaZ = signExtend(packed4 & 0x1FFF, 13).toFloat / 64.0f

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

  /** Decode Titanium OP_Damage: CombatDamage_Struct (23 bytes).
    * Mac uses uint16 damageType (24 bytes). Titanium uses uint8 type (23 bytes).
    * The 1-byte difference shifts all subsequent field offsets.
    */
  def decodeDamage(data: Array[Byte]): Option[DamageInfo] =
    if data.length < 23 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(DamageInfo(
      targetId = buf.getShort() & 0xFFFF,     // 00-01
      sourceId = buf.getShort() & 0xFFFF,     // 02-03
      damageType = buf.get() & 0xFF,          // 04 (uint8, not uint16!)
      spellId = buf.getShort() & 0xFFFF,      // 05-06
      damage = buf.getInt(),                  // 07-10
      force = buf.getFloat(),                 // 11-14
      pushHeading = buf.getFloat(),           // 15-18
      pushUpAngle = buf.getFloat(),           // 19-22
    ))

  /** Decode Titanium OP_FormattedMessage: FormattedMessage_Struct.
    * Titanium uses uint32 fields (12-byte header). Mac uses uint16 (6-byte header).
    * Layout: uint32 unknown, uint32 string_id, uint32 type, char message[0] (variable args).
    */
  def decodeFormattedMessage(data: Array[Byte]): Option[FormattedMessage] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.getInt() // unknown0
    val stringId = buf.getInt()
    val msgType = buf.getInt()
    // Remaining bytes are null-terminated argument strings
    val args = Vector.newBuilder[String]
    var pos = 12
    while pos < data.length do
      val start = pos
      while pos < data.length && data(pos) != 0 do pos += 1
      if pos > start then
        args += new String(data, start, pos - start, StandardCharsets.US_ASCII)
      pos += 1 // skip null terminator
    Some(FormattedMessage(stringId, msgType, args.result()))

  // ===========================================================================
  // Titanium-specific decoders for structs that differ from Mac
  // ===========================================================================

  /** Decode Titanium OP_Consider: Consider_Struct (28 bytes).
    * Mac uses uint16 playerid/targetid (24 bytes). Titanium uses uint32 (28 bytes).
    */
  def decodeConsider(data: Array[Byte]): Option[ConsiderResult] =
    if data.length < 28 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(ConsiderResult(
      playerId = buf.getInt(),
      targetId = buf.getInt(),
      faction = buf.getInt(),
      conLevel = buf.getInt(),
      curHp = buf.getInt(),
      maxHp = buf.getInt(),
      pvpCon = (buf.get() & 0xFF) != 0,
    ))

  /** Decode Titanium OP_WearChange: WearChange_Struct (9 bytes).
    * Mac is 12 bytes with different field order.
    * Titanium: uint16 spawn_id, uint16 material, Tint_Struct(4B) color, uint8 wear_slot_id
    */
  def decodeWearChange(data: Array[Byte]): Option[WearChangeInfo] =
    if data.length < 9 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val spawnId = buf.getShort() & 0xFFFF
    val material = buf.getShort() & 0xFFFF
    val blue = buf.get() & 0xFF
    val green = buf.get() & 0xFF
    val red = buf.get() & 0xFF
    val useTint = buf.get() != 0
    val wearSlot = buf.get() & 0xFF
    Some(WearChangeInfo(spawnId, wearSlot, material, TintColor(red, green, blue, useTint)))

  /** Decode Titanium OP_Animation: Animation_Struct (4 bytes).
    * Mac is 6 bytes (has extra uint16 targetId). Titanium: uint16 spawnid, uint8 speed, uint8 action
    */
  def decodeAnimation(data: Array[Byte]): Option[AnimationInfo] =
    if data.length < 4 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(AnimationInfo(
      spawnId = buf.getShort() & 0xFFFF,
      targetId = 0, // Titanium doesn't have targetId
      action = buf.get() & 0xFF, // speed field
      value = buf.get() & 0xFF, // action field
    ))

  /** Decode Titanium OP_Action: Action_Struct (31 bytes).
    * Mac is 36 bytes (extra uint16 target_level and different field layout after type).
    * Titanium: uint16 target, uint16 source, uint16 level, uint32 instrument_mod,
    *   float force, float hit_heading, float hit_pitch, uint8 type,
    *   uint16 unknown23, uint16 unknown25, uint16 spell, uint8 spell_level, uint8 effect_flag
    */
  def decodeAction(data: Array[Byte]): Option[SpellAction] =
    if data.length < 31 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val aTarget = buf.getShort() & 0xFFFF     // 00
    val aSource = buf.getShort() & 0xFFFF     // 02
    val aLevel = buf.getShort() & 0xFFFF      // 04
    val aInstrument = buf.getInt()             // 06 (no target_level gap like Mac)
    val aForce = buf.getFloat()               // 10
    val aPushHeading = buf.getFloat()          // 14
    val aPushUpAngle = buf.getFloat()          // 18
    val aType = buf.get() & 0xFF              // 22
    buf.getShort()                             // 23: unknown23
    buf.getShort()                             // 25: unknown25
    val aSpellId = buf.getShort() & 0xFFFF    // 27
    buf.get()                                  // 29: spell_level
    val effectFlag = buf.get() & 0xFF          // 30: effect_flag
    Some(SpellAction(aTarget, aSource, aLevel, aInstrument, aForce, aPushHeading,
      aPushUpAngle, aType, aSpellId, 0, effectFlag))

  /** Decode Titanium OP_MemorizeSpell: MemorizeSpell_Struct (16 bytes).
    * Mac is 12 bytes (3 × uint32). Titanium adds uint32 reduction.
    */
  def decodeMemorizeSpell(data: Array[Byte]): Option[(Int, Int, Int)] =
    if data.length < 16 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val slot = buf.getInt()
    val spellId = buf.getInt()
    val scribing = buf.getInt()
    // uint32 reduction (ignored)
    Some((slot, spellId, scribing))

  /** Decode Titanium OP_InterruptCast: InterruptCast_Struct (8+ bytes).
    * Mac has uint16 messageid + uint16 color + message. Titanium has uint32 spawnid + uint32 messageid + message.
    */
  def decodeInterruptCast(data: Array[Byte]): Option[(Int, Int, String)] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val spawnId = buf.getInt()
    val messageId = buf.getInt()
    val msg = if data.length > 8 then
      new String(data, 8, data.length - 8, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
    else ""
    Some((messageId, 0, msg)) // color=0 (Titanium doesn't have color field)

  /** Decode Titanium OP_ManaChange: ManaChange_Struct (16 bytes).
    * Mac is 4 bytes (uint16 mana + uint16 spawnId). Titanium has:
    * uint32 new_mana, uint32 stamina, uint32 spell_id, uint8 keepcasting, uint8 padding[3]
    */
  def decodeManaChange(data: Array[Byte]): Option[ManaChange] =
    if data.length < 16 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val newMana = buf.getInt()
    // stamina, spell_id, keepcasting — ignored for ManaChange event
    Some(ManaChange(spawnId = 0, curMana = newMana))

  /** Decode Titanium OP_ExpUpdate: ExpUpdate_Struct (8 bytes).
    * Mac is 4 bytes (uint32 exp only). Titanium adds uint32 aaxp.
    */
  def decodeExpUpdate(data: Array[Byte]): Option[ExpChange] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val exp = buf.getInt()
    // uint32 aaxp — not used in ExpChange model currently
    Some(ExpChange(exp))

  /** Decode Titanium OP_ChannelMessage: ChannelMessage_Struct (148-byte header + message).
    * Mac has 136-byte header. Titanium adds uint32 cm_unknown4[2] (8 bytes extra at offset 136).
    */
  def decodeChannelMessage(data: Array[Byte]): Option[ChatMessage] =
    if data.length < 148 then return None
    val target = new String(data, 0, 64, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
    val sender = new String(data, 64, 64, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
    val buf = ByteBuffer.wrap(data, 128, 20).order(ByteOrder.LITTLE_ENDIAN)
    val language = buf.getInt()      // 128
    val channel = buf.getInt()       // 132
    buf.getInt()                     // 136: cm_unknown4[0]
    buf.getInt()                     // 140: cm_unknown4[1]
    val langSkill = buf.getInt()     // 144: skill_in_language
    val msg = if data.length > 148 then
      new String(data, 148, data.length - 148, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
    else ""
    Some(ChatMessage(sender, target, language, channel, langSkill, msg))

  /** Decode Titanium OP_Emote: Emote_Struct (1028 bytes).
    * Mac has uint16 unknown + variable message. Titanium: uint32 unknown01 + char message[1024].
    */
  def decodeEmote(data: Array[Byte]): Option[EmoteMessage] =
    if data.length < 5 then return None
    // Skip uint32 unknown01, read message starting at offset 4
    val msg = new String(data, 4, Math.min(data.length - 4, 1024),
      StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')
    Some(EmoteMessage(msg))

  /** Decode Titanium OP_Weather: Weather_Struct (12 bytes).
    * Mac is 8 bytes (uint32 val1 + uint32 type). Titanium adds uint32 mode.
    */
  def decodeWeather(data: Array[Byte]): Option[WeatherInfo] =
    if data.length < 12 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val intensity = buf.getInt() // val1: 0x000000FF = max intensity
    val weatherType = buf.getInt() // 0x31=rain, 0x02=snow, 0=none
    // uint32 mode — ignored
    Some(WeatherInfo(weatherType, intensity & 0xFF))

  /** Decode Titanium OP_SpawnDoor: Door_Struct array (80 bytes each).
    * Mac Door_Struct is 44 bytes with name[16]. Titanium is 80 bytes with name[32].
    * No count header — count is inferred from packet size / 80.
    */
  def decodeDoors(data: Array[Byte]): Vector[DoorData] =
    val doors = Vector.newBuilder[DoorData]
    var offset = 0
    while offset + 80 <= data.length do
      val buf = ByteBuffer.wrap(data, offset, 80).order(ByteOrder.LITTLE_ENDIAN)
      val nameBytes = new Array[Byte](32)
      buf.get(nameBytes)
      val name = readNullStr(nameBytes)
      val y = buf.getFloat()     // 32
      val x = buf.getFloat()     // 36
      val z = buf.getFloat()     // 40
      val heading = buf.getFloat() // 44
      val incline = buf.getInt() // 48
      val size = buf.getShort() & 0xFFFF // 52
      buf.position(buf.position() + 6)   // unknown0038[6] at 54
      val doorId = buf.get() & 0xFF      // 60
      val openType = buf.get() & 0xFF    // 61
      val stateAtSpawn = buf.get() & 0xFF // 62
      val invertState = buf.get() & 0xFF // 63
      val doorParam = buf.getInt()       // 64
      // unknown0052[12] at 68 — skip
      doors += DoorData(name, y, x, z, heading, incline, size, doorId,
        openType, stateAtSpawn != 0, invertState != 0, doorParam)
      offset += 80
    doors.result()

  /** Decode Titanium OP_GroundSpawn: Object_Struct (92 bytes).
    * Mac Object_Struct is 224 bytes with very different layout.
    * Titanium layout: linked_list_addr[2](8B), unknown(4B), drop_id(u32), zone_id(u16),
    *   zone_instance(u16), unknown(8B), heading(f32), z(f32), x(f32), y(f32),
    *   object_name[32], unknown(4B), object_type(u32), unknown(4B), spawn_id(u32) = 92 bytes
    */
  def decodeGroundItem(data: Array[Byte]): Option[GroundItemData] =
    if data.length < 92 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.getInt(); buf.getInt()       // 00-07: linked_list_addr[2]
    buf.getShort(); buf.getShort()   // 08-11: unknown008[2]
    val dropId = buf.getInt()        // 12: drop_id
    val zoneId = buf.getShort() & 0xFFFF // 16: zone_id
    buf.getShort()                   // 18: zone_instance
    buf.getInt(); buf.getInt()       // 20-27: unknown020, unknown024
    val heading = buf.getFloat()     // 28: heading
    val z = buf.getFloat()           // 32: z
    val x = buf.getFloat()           // 36: x
    val y = buf.getFloat()           // 40: y
    val nameBytes = new Array[Byte](32) // 44: object_name[32]
    buf.get(nameBytes)
    val objName = readNullStr(nameBytes)
    buf.getInt()                     // 76: unknown076
    val objType = buf.getInt()       // 80: object_type
    // unknown084(4), spawn_id(4) — not needed for GroundItemData
    Some(GroundItemData(
      itemId = 0, dropId = dropId, zoneId = zoneId, charges = 0, maxCharges = 0,
      heading = heading, y = y, x = x, z = z, objectName = objName, objectType = objType,
    ))

  /** Decode Titanium OP_Buff: SpellBuffPacket_Struct (32 bytes).
    * Mac is 20 bytes. Titanium: uint32 entityid(4) + SpellBuff_Struct(20) + uint32 slotid(4) + uint32 bufffade(4).
    * SpellBuff_Struct: uint8 effect_type, uint8 level, uint8 bard_modifier, uint8 activated,
    *   uint16 spellid, uint32 duration, uint32 counters, uint32 player_id, uint16 unknown = 20 bytes
    */
  def decodeBuff(data: Array[Byte]): Option[(Int, SpellBuff)] =
    if data.length < 32 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val entityId = buf.getInt()        // 00: entityid
    val effectType = buf.get() & 0xFF  // 04: effect_type
    val level = buf.get() & 0xFF       // 05: level
    val bardMod = buf.get() & 0xFF     // 06: bard_modifier
    buf.get()                          // 07: activated
    val spellId = buf.getShort() & 0xFFFF // 08: spellid
    val duration = buf.getInt()        // 10: duration
    val counters = buf.getInt()        // 14: counters
    buf.getInt()                       // 18: player_id
    buf.getShort()                     // 22: unknown
    val slotId = buf.getInt()          // 24: slotid
    val buffFade = buf.getInt()        // 28: bufffade
    Some((slotId, SpellBuff(effectType, level, bardMod, spellId, duration, counters)))

  /** Decode Titanium OP_ShopRequest response: Merchant_Click_Struct (16 bytes).
    * Mac is 12 bytes (uint16 npc_id + uint16 player_id + uint32 command + float rate).
    * Titanium: uint32 npc_id, uint32 player_id, uint32 command, float rate.
    */
  def decodeMerchantClick(data: Array[Byte]): Option[MerchantOpen] =
    if data.length < 16 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val npcId = buf.getInt()
    val playerId = buf.getInt()
    val command = buf.getInt()
    val rate = buf.getFloat()
    Some(MerchantOpen(npcId, rate))

  /** Decode Titanium OP_Stamina: Stamina_Struct (8 bytes).
    * Mac is 5 bytes (int16 food, int16 water, uint8 fatigue).
    * Titanium: uint32 food, uint32 water. No fatigue field.
    */
  def decodeStamina(data: Array[Byte]): Option[StaminaInfo] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    Some(StaminaInfo(food = buf.getInt(), water = buf.getInt(), fatigue = 0))

  /** Decode Titanium OP_TimeOfDay: TimeOfDay_Struct (8 bytes).
    * Mac is 6 bytes (uint16 year). Titanium: uint8 hour, uint8 minute, uint8 day, uint8 month, uint32 year.
    */
  def decodeTimeOfDay(data: Array[Byte]): Option[GameTime] =
    if data.length < 8 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val hour = buf.get() & 0xFF
    val minute = buf.get() & 0xFF
    val day = buf.get() & 0xFF
    val month = buf.get() & 0xFF
    val year = buf.getInt()
    Some(GameTime(hour, minute, day, month, year))

  // ===========================================================================
  // Zone Change
  // ===========================================================================

  /** Encode Titanium OP_ZoneChange: ZoneChange_Struct (88 bytes).
    *
    * Titanium layout differs from Mac (76 bytes):
    *   char[64] char_name, uint16 zoneID, uint16 instanceID,
    *   float y, float x, float z, uint32 zone_reason, int32 success
    *
    * Mac layout is smaller and puts reason/success at different offsets.
    */
  def encodeZoneChange(charName: String, zoneId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(88).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = charName.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)                    // 0-63: char_name
    buf.putShort((zoneId & 0xFFFF).toShort) // 64-65: zoneID
    buf.putShort(0)                     // 66-67: instanceID
    // y, x, z, zone_reason, success — all zero (client→server)
    buf.array()

  /** Decode Titanium OP_ZoneChange response: ZoneChange_Struct (88 bytes).
    *
    * The `success` field is at offset 84 (int32), not offset 72 like in the Mac struct.
    */
  def decodeZoneChange(data: Array[Byte]): Option[ZoneChangeResult] =
    if data.length < 88 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = new Array[Byte](64)
    buf.get(nameBytes)
    val name = readNullStr(nameBytes)
    val zoneId = buf.getShort() & 0xFFFF  // offset 64
    val instanceId = buf.getShort() & 0xFFFF // offset 66
    buf.getFloat() // y (offset 68)
    buf.getFloat() // x (offset 72)
    buf.getFloat() // z (offset 76)
    val reason = buf.getInt()             // offset 80: zone_reason
    val success = buf.getInt()            // offset 84: success
    Some(ZoneChangeResult(name, zoneId, reason, success))

  /** Decode Titanium OP_RequestClientZoneChange (24 bytes).
    *
    * Titanium uses uint16 zone_id + uint16 instance_id (Mac uses a single uint32 zone_id).
    */
  def decodeRequestClientZoneChange(data: Array[Byte]): Option[ZoneChangeRequest] =
    if data.length < 24 then return None
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val zoneId = buf.getShort() & 0xFFFF  // uint16 zone_id
    buf.getShort()                         // uint16 instance_id (skip)
    Some(ZoneChangeRequest(
      zoneId = zoneId,
      y = buf.getFloat(),
      x = buf.getFloat(),
      z = buf.getFloat(),
      heading = buf.getFloat(),
    ))

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
