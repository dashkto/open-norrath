package opennorrath.network.titanium

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

import opennorrath.network.{CharacterInfo, ExpansionFlags, GuildInfo, ZoneAddress}

/** Encode/decode world server payloads for the Titanium (PC) protocol.
  *
  * Key differences from Mac WorldCodec:
  * - LoginInfo_Struct: 464 bytes (vs Mac 200)
  * - CharacterSelect_Struct: 1704 bytes with completely different field layout
  * - EnterWorld_Struct: 72 bytes (vs Mac 64), extra tutorial + return_home fields
  * - GuildsList_Struct: fixed 96064 bytes (64-byte header + 1500 × 64-byte entries)
  * - CharCreate_Struct: 80 bytes of uint32 fields (vs Mac 8452)
  * - NameApproval: 76 bytes (name[64] + uint32 race + uint32 class_ + uint32 deity)
  * - ZoneServerInfo port is little-endian (Mac uses big-endian)
  *
  * Reference: EQEmu/common/patches/titanium_structs.h, titanium.cpp
  */
object TitaniumWorldCodec:

  // ---- Outgoing (client -> server) ----

  /** OP_SendLoginInfo: Titanium LoginInfo_Struct (464 bytes).
    *
    * Layout: login_info[64] + unknown064[124] + zoning(uint8) + unknown189[275] = 464
    * login_info contains "<account_id>\0<session_key>" as a null-separated string.
    * Note: titanium_structs.h has a misleading /×488×/ end comment, but sizeof() is 464.
    */
  def encodeLoginInfo(accountId: Int, sessionKey: String): Array[Byte] =
    val buf = new Array[Byte](464)
    val loginStr = s"$accountId\u0000$sessionKey"
    val bytes = loginStr.getBytes(StandardCharsets.US_ASCII)
    System.arraycopy(bytes, 0, buf, 0, Math.min(bytes.length, 63))
    // zoning at offset 188 = 0 (default, character select)
    buf

  /** OP_SendLoginInfo for zoning reconnect (zoning flag = 1). */
  def encodeLoginInfoZoning(accountId: Int, sessionKey: String): Array[Byte] =
    val buf = encodeLoginInfo(accountId, sessionKey)
    buf(188) = 1 // zoning = true
    buf

  /** OP_EnterWorld: Titanium EnterWorld_Struct (72 bytes).
    * name[64] + tutorial(uint32) + return_home(uint32)
    */
  def encodeEnterWorld(charName: String, tutorial: Boolean = false, returnHome: Boolean = false): Array[Byte] =
    val buf = ByteBuffer.allocate(72).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = charName.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)
    buf.putInt(if tutorial then 1 else 0)
    buf.putInt(if returnHome then 1 else 0)
    buf.array()

  /** OP_ApproveName: Titanium NameApproval (76 bytes).
    * name[64] + uint32 race + uint32 class_ + uint32 deity
    */
  def encodeApproveName(name: String, race: Int, classId: Int, deity: Int = 0): Array[Byte] =
    val buf = ByteBuffer.allocate(76).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = name.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)
    buf.putInt(race)
    buf.putInt(classId)
    buf.putInt(deity)
    buf.array()

  /** OP_CharacterCreate: Titanium CharCreate_Struct (80 bytes).
    * All fields are uint32, tightly packed at 4-byte intervals.
    */
  def encodeCharCreate(
    name: String,
    gender: Int, race: Int, classId: Int,
    str: Int, sta: Int, cha: Int, dex: Int, int_ : Int, agi: Int, wis: Int,
    startZone: Int, deity: Int,
    hairColor: Int = 0, beardColor: Int = 0,
    eyeColor1: Int = 0, eyeColor2: Int = 0,
    hairStyle: Int = 0, beard: Int = 0, face: Int = 0,
  ): Array[Byte] =
    val buf = ByteBuffer.allocate(80).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(classId)     // offset 0
    buf.putInt(hairColor)   // offset 4
    buf.putInt(beardColor)  // offset 8
    buf.putInt(beard)       // offset 12
    buf.putInt(gender)      // offset 16
    buf.putInt(race)        // offset 20
    buf.putInt(startZone)   // offset 24
    buf.putInt(hairStyle)   // offset 28
    buf.putInt(deity)       // offset 32
    buf.putInt(str)         // offset 36
    buf.putInt(sta)         // offset 40
    buf.putInt(agi)         // offset 44
    buf.putInt(dex)         // offset 48
    buf.putInt(wis)         // offset 52
    buf.putInt(int_)        // offset 56
    buf.putInt(cha)         // offset 60
    buf.putInt(face)        // offset 64
    buf.putInt(eyeColor1)   // offset 68
    buf.putInt(eyeColor2)   // offset 72
    buf.putInt(0)           // offset 76: tutorial
    buf.array()

  // ---- Incoming (server -> client) ----

  /** Decode OP_SendCharInfo: Titanium CharacterSelect_Struct (1704 bytes).
    *
    * Completely different layout from Mac. Fields are grouped by type, not by character:
    *   Race[10](uint32,40) | CS_Colors[10](TintProfile,360) | BeardColor[10](uint8,10) |
    *   HairStyle[10](uint8,10) | Equip[10](TextureProfile,360) | SecondaryIDFile[10](uint32,40) |
    *   Unknown820[10](10) | Unknown830[2](2) | Deity[10](uint32,40) | GoHome[10](10) |
    *   Tutorial[10](10) | Beard[10](10) | Unknown902[10](10) | PrimaryIDFile[10](uint32,40) |
    *   HairColor[10](10) | Unknown0962[2](2) | Zone[10](uint32,40) | Class[10](uint8,10) |
    *   Face[10](uint8,10) | Name[10][64](640) | Gender[10](uint8,10) | EyeColor1[10](10) |
    *   EyeColor2[10](10) | Level[10](uint8,10)
    */
  def decodeCharacterSelect(data: Array[Byte]): Vector[CharacterInfo] =
    if data.length < 1704 then
      println(s"[TitaniumWorldCodec] CharacterSelect too short: ${data.length}B (expected 1704)")
      return Vector.empty

    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val chars = Vector.newBuilder[CharacterInfo]

    try
      // Race[10] — uint32 each (40 bytes, offset 0)
      val races = Array.fill(10)(buf.getInt())

      // CS_Colors[10] — TintProfile: 9 × uint32 per char (360 bytes, offset 40)
      buf.position(buf.position() + 360) // skip tint data

      // BeardColor[10] — uint8 each (10 bytes, offset 400)
      buf.position(buf.position() + 10) // skip

      // HairStyle[10] — uint8 each (10 bytes, offset 410)
      buf.position(buf.position() + 10) // skip

      // Equip[10] — TextureProfile: 9 × uint32 per char (360 bytes, offset 420)
      val equips = Array.fill(10)(Array.fill(9)(buf.getInt()))

      // SecondaryIDFile[10] — uint32 each (40 bytes, offset 780)
      val secondaries = Array.fill(10)(buf.getInt())

      // Unknown820[10] (10 bytes, offset 820)
      buf.position(buf.position() + 10) // skip

      // Unknown830[2] (2 bytes, offset 830)
      buf.position(buf.position() + 2) // skip

      // Deity[10] — uint32 each (40 bytes, offset 832)
      buf.position(buf.position() + 40) // skip deity

      // GoHome[10] (10 bytes, offset 872)
      buf.position(buf.position() + 10) // skip

      // Tutorial[10] (10 bytes, offset 882)
      buf.position(buf.position() + 10) // skip

      // Beard[10] (10 bytes, offset 892)
      buf.position(buf.position() + 10) // skip

      // Unknown902[10] (10 bytes, offset 902)
      buf.position(buf.position() + 10) // skip

      // PrimaryIDFile[10] — uint32 each (40 bytes, offset 912)
      val primaries = Array.fill(10)(buf.getInt())

      // HairColor[10] (10 bytes, offset 952)
      buf.position(buf.position() + 10) // skip

      // Unknown0962[2] (2 bytes, offset 962)
      buf.position(buf.position() + 2) // skip

      // Zone[10] — uint32 each (40 bytes, offset 964)
      val zones = Array.fill(10)(buf.getInt())

      // Class[10] — uint8 each (10 bytes, offset 1004)
      val classes = Array.fill(10)(buf.get() & 0xFF)

      // Face[10] — uint8 each (10 bytes, offset 1014)
      val faces = Array.fill(10)(buf.get() & 0xFF)

      // Name[10][64] — 640 bytes (offset 1024)
      val names = Array.fill(10) {
        val nameBytes = new Array[Byte](64)
        buf.get(nameBytes)
        readNullString(nameBytes)
      }

      // Gender[10] — uint8 each (10 bytes, offset 1664)
      val genders = Array.fill(10)(buf.get() & 0xFF)

      // EyeColor1[10] (10 bytes, offset 1674) — skip
      buf.position(buf.position() + 10)

      // EyeColor2[10] (10 bytes, offset 1684) — skip
      buf.position(buf.position() + 10)

      // Level[10] — uint8 each (10 bytes, offset 1694)
      val levels = Array.fill(10)(buf.get() & 0xFF)

      for i <- 0 until 10 do
        if names(i).nonEmpty && levels(i) > 0 then
          // Merge armor materials (slots 0-6) with weapon IDFile numbers (slots 7-8)
          val merged = new Array[Int](9)
          for j <- 0 until 7 do merged(j) = equips(i)(j)
          merged(7) = primaries(i)
          merged(8) = secondaries(i)
          chars += CharacterInfo(
            name = names(i),
            level = levels(i),
            classId = classes(i),
            race = races(i),
            gender = genders(i),
            zone = zones(i),
            face = faces(i),
            equipment = merged,
          )
    catch
      case e: Exception =>
        println(s"[TitaniumWorldCodec] Error decoding character select: ${e.getMessage}")

    chars.result()

  /** Decode OP_ZoneServerInfo: ip[128] + uint16 port (little-endian for Titanium). */
  def decodeZoneServerInfo(data: Array[Byte]): Option[ZoneAddress] =
    if data.length < 130 then
      println(s"[TitaniumWorldCodec] ZoneServerInfo too short: ${data.length}B")
      None
    else
      val ip = readNullString(data.take(128))
      // Titanium uses little-endian port (unlike Mac's big-endian)
      val port = (data(128) & 0xFF) | ((data(129) & 0xFF) << 8)
      Some(ZoneAddress(ip, port))

  /** Decode OP_GuildsList: Titanium fixed-size GuildsList_Struct.
    * 64-byte header + 1500 × 64-byte GuildsListEntry (guild name per entry).
    * Guild ID is the array index.
    */
  def decodeGuildsList(data: Array[Byte]): Vector[GuildInfo] =
    val guilds = Vector.newBuilder[GuildInfo]
    val headerSize = 64
    val entrySize = 64
    val maxGuilds = 1500

    if data.length < headerSize then return Vector.empty

    var i = 0
    while i < maxGuilds && (headerSize + i * entrySize + entrySize) <= data.length do
      val offset = headerSize + i * entrySize
      val name = readNullStringAt(data, offset)
      if name.nonEmpty then guilds += GuildInfo(i, name)
      i += 1

    guilds.result()

  /** Decode OP_ApproveName reply (1 byte): 1=approved, 0=rejected. Same as Mac. */
  def decodeNameApproval(data: Array[Byte]): Boolean =
    data.nonEmpty && data(0) != 0

  // Simple decoders — same format as Mac

  /** Decode OP_MOTD: null-terminated string. */
  def decodeMOTD(data: Array[Byte]): String = readNullString(data)

  /** Decode OP_ExpansionInfo: uint32 expansion flags. */
  def decodeExpansionInfo(data: Array[Byte]): ExpansionFlags =
    if data.length < 4 then ExpansionFlags(0)
    else ExpansionFlags(ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt())

  /** Decode OP_SetChatServer: null-terminated host:port string. */
  def decodeChatServer(data: Array[Byte]): String = readNullString(data)

  /** Decode OP_LogServer: null-terminated server name/host. */
  def decodeLogServer(data: Array[Byte]): String = readNullString(data)

  // ---- Helpers ----

  private def readNullString(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()

  private def readNullStringAt(data: Array[Byte], offset: Int): String =
    val sb = StringBuilder()
    var i = offset
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()
