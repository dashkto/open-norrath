package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

/** Character info from the character select screen. */
case class CharacterInfo(
  name: String,
  level: Int,
  classId: Int,
  race: Int,
  gender: Int,
  zone: Int,
  face: Int = 0,
  equipment: Array[Int] = Array.fill(9)(0), // slots 0-6=armor material, 7=primary IT, 8=secondary IT
)

/** Zone server connection info. */
case class ZoneAddress(ip: String, port: Int)

/** Guild entry from OP_GuildsList. */
case class GuildInfo(id: Int, name: String)

/** Expansion flags from OP_ExpansionInfo. */
case class ExpansionFlags(flags: Int):
  def hasKunark: Boolean = (flags & 0x01) != 0
  def hasVelious: Boolean = (flags & 0x02) != 0
  def hasLuclin: Boolean = (flags & 0x04) != 0
  def hasPlanes: Boolean = (flags & 0x08) != 0

/** Encode/decode world server payloads.
  *
  * Reference: EQMacEmu/Server/common/eq_packet_structs.h
  *            EQMacEmu/Server/world/client.cpp
  */
object WorldCodec:

  /** OP_SendLoginInfo: Mac LoginInfo_Struct (200 bytes for Intel Mac).
    *
    * Mac-specific layout (patches/mac_structs.h):
    *   AccountName[127]  — "<account_id>\0<session_key>\0" (name+pass in first 64 bytes)
    *   Password[24]      — zeros (not used; password is in AccountName after null)
    *   unknown189[41]    — zeros
    *   zoning (uint8)    — 0 = character select, 1 = zoning
    *   unknown193[3]     — zeros
    *   +4 extra bytes    — Intel Mac sends 200 bytes total (PPC sends 196)
    *
    * The signature matcher checks: opcode == 0x5818 AND size == 200 for Intel.
    */
  def encodeLoginInfo(accountId: Int, sessionKey: String): Array[Byte] =
    val buf = new Array[Byte](200)
    val loginStr = s"$accountId\u0000$sessionKey"
    val bytes = loginStr.getBytes(StandardCharsets.US_ASCII)
    System.arraycopy(bytes, 0, buf, 0, Math.min(bytes.length, 127))
    // zoning at offset 192 = 0 (default)
    buf

  /** OP_EnterWorld: EnterWorld_Struct — name[64] = 64 bytes. */
  def encodeEnterWorld(charName: String): Array[Byte] =
    val buf = new Array[Byte](64)
    val bytes = charName.getBytes(StandardCharsets.US_ASCII)
    System.arraycopy(bytes, 0, buf, 0, Math.min(bytes.length, 63))
    buf

  /** Decode OP_SendCharInfo: CharacterSelect_Struct (1620 bytes).
    *
    * Layout (eq_packet_structs.h):
    *   name[10][64]    — 640 bytes (offset 0)
    *   level[10]       — 10 bytes (uint8)
    *   class_[10]      — 10 bytes (uint8)
    *   race[10]        — 20 bytes (uint16)
    *   zone[10]        — 40 bytes (uint32)
    *   gender[10]      — 10 bytes (uint8)
    *   face[10]        — 10 bytes (uint8)
    *   equip[10][9]    — 180 bytes (uint16 per slot per char)
    */
  def decodeCharacterSelect(data: Array[Byte]): Vector[CharacterInfo] =
    if data.length < 720 then return Vector.empty // need at least names + levels + classes + races

    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val chars = Vector.newBuilder[CharacterInfo]

    try
      // name[10][64] — 640 bytes (offset 0)
      val names = Array.fill(10) {
        val nameBytes = new Array[Byte](64)
        buf.get(nameBytes)
        readNullString(nameBytes)
      }
      // level[10] — uint8 each (10 bytes, offset 640)
      val levels = Array.fill(10)(buf.get() & 0xFF)
      // class_[10] — uint8 each (10 bytes, offset 650)
      val classes = Array.fill(10)(buf.get() & 0xFF)
      // race[10] — uint16 each (20 bytes, offset 660)
      val races = Array.fill(10)(buf.getShort() & 0xFFFF)
      // zone[10] — uint32 each (40 bytes, offset 680)
      val zones = Array.fill(10)(buf.getInt())
      // gender[10] — uint8 each (10 bytes, offset 720)
      val genders = Array.fill(10)(buf.get() & 0xFF)
      // face[10] — uint8 each (10 bytes, offset 730)
      val faces = if buf.remaining() >= 10 then Array.fill(10)(buf.get() & 0xFF)
        else Array.fill(10)(0)
      // EQ::TextureProfile equip[10] — 9 × uint32 per char (360 bytes, offset 740)
      val equips = if buf.remaining() >= 360 then
        Array.fill(10)(Array.fill(9)(buf.getInt()))
      else Array.fill(10)(Array.fill(9)(0))
      // EQ::TintProfile cs_colors[10] — 9 × uint32 per char (360 bytes, offset 1100) — skip
      if buf.remaining() >= 360 then buf.position(buf.position() + 360)
      // deity[10] — uint16 each (20 bytes, offset 1460) — skip
      if buf.remaining() >= 20 then buf.position(buf.position() + 20)
      // primary[10] — uint32 IDFile each (40 bytes, offset 1480)
      val primaries = if buf.remaining() >= 40 then Array.fill(10)(buf.getInt())
        else Array.fill(10)(0)
      // secondary[10] — uint32 IDFile each (40 bytes, offset 1520)
      val secondaries = if buf.remaining() >= 40 then Array.fill(10)(buf.getInt())
        else Array.fill(10)(0)

      for i <- 0 until 10 do
        if names(i).nonEmpty && levels(i) > 0 then
          // Merge armor materials (slots 0-6) with weapon IDFile numbers (slots 7-8)
          val merged = new Array[Int](9)
          for j <- 0 until 7 do merged(j) = equips(i)(j)
          merged(7) = primaries(i)
          merged(8) = secondaries(i)
          val info = CharacterInfo(
            name = names(i),
            level = levels(i),
            classId = classes(i),
            race = races(i),
            gender = genders(i),
            zone = zones(i),
            face = faces(i),
            equipment = merged,
          )
          chars += info
    catch
      case e: Exception =>
        println(s"[WorldCodec] Error decoding character select: ${e.getMessage}")

    chars.result()

  /** Decode OP_ZoneServerInfo: ZoneServerInfo_Struct.
    * ip[128] + port(uint16)
    */
  def decodeZoneServerInfo(data: Array[Byte]): Option[ZoneAddress] =
    if data.length < 130 then
      println(s"[WorldCodec] ZoneServerInfo too short: ${data.length}B")
      None
    else
      val ip = readNullString(data.take(128))
      val port = ((data(128) & 0xFF) << 8) | (data(129) & 0xFF) // big-endian (network byte order)
      Some(ZoneAddress(ip, port))

  /** Encode OP_ApproveName.
    * NameApproval_Struct: name[64] + race(u16) + unknown066(u16) + class_(u16) + unknown070(u32×2)
    * Server checks app->Size() == sizeof(NameApproval_Struct) where Size() = payload + 2 (opcode).
    * sizeof(NameApproval_Struct) = 78, so payload must be 76.
    */
  def encodeApproveName(name: String, race: Int, classId: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(76).order(ByteOrder.LITTLE_ENDIAN)
    val nameBytes = name.getBytes(StandardCharsets.US_ASCII)
    val nameBuf = new Array[Byte](64)
    System.arraycopy(nameBytes, 0, nameBuf, 0, Math.min(nameBytes.length, 63))
    buf.put(nameBuf)
    buf.putShort((race & 0xFFFF).toShort)
    buf.putShort(0.toShort) // unknown066
    buf.putShort((classId & 0xFFFF).toShort)
    buf.putInt(0) // unknown070[0]
    // unknown070[1] covered by remaining zero-initialized bytes
    buf.array()

  /** Decode OP_ApproveName reply (1 byte): 1=approved, 0=rejected. */
  def decodeNameApproval(data: Array[Byte]): Boolean =
    data.nonEmpty && data(0) != 0

  /** Encode OP_CharacterCreate: CharCreate_Struct (8452 bytes).
    *
    * Key offsets:
    *   136: gender(u8), 138: race(u16), 140: class(u16)
    *   160-172: STR/STA/CHA/DEX/INT/AGI/WIS (u16 each)
    *   3440: start_zone(u32), 4940: deity(u16)
    *   5422-5428: haircolor/beardcolor/eyecolor1/eyecolor2/hairstyle/beard/face
    */
  def encodeCharCreate(
    name: String,
    gender: Int,
    race: Int,
    classId: Int,
    str: Int, sta: Int, cha: Int, dex: Int, int_ : Int, agi: Int, wis: Int,
    startZone: Int,
    deity: Int,
    hairColor: Int = 0, beardColor: Int = 0,
    eyeColor1: Int = 0, eyeColor2: Int = 0,
    hairStyle: Int = 0, beard: Int = 0, face: Int = 0,
  ): Array[Byte] =
    val buf = ByteBuffer.allocate(8452).order(ByteOrder.LITTLE_ENDIAN)
    // Name at offset 0 (64 bytes, null-padded)
    val nameBytes = name.getBytes(StandardCharsets.US_ASCII)
    buf.put(nameBytes, 0, Math.min(nameBytes.length, 63))
    buf.position(136); buf.put(gender.toByte)
    buf.position(138); buf.putShort((race & 0xFFFF).toShort)
    buf.position(140); buf.putShort((classId & 0xFFFF).toShort)
    buf.position(160); buf.putShort((str & 0xFFFF).toShort)
    buf.position(162); buf.putShort((sta & 0xFFFF).toShort)
    buf.position(164); buf.putShort((cha & 0xFFFF).toShort)
    buf.position(166); buf.putShort((dex & 0xFFFF).toShort)
    buf.position(168); buf.putShort((int_ & 0xFFFF).toShort)
    buf.position(170); buf.putShort((agi & 0xFFFF).toShort)
    buf.position(172); buf.putShort((wis & 0xFFFF).toShort)
    buf.position(3440); buf.putInt(startZone)
    buf.position(4940); buf.putShort((deity & 0xFFFF).toShort)
    buf.position(5422); buf.put(hairColor.toByte)
    buf.position(5423); buf.put(beardColor.toByte)
    buf.position(5424); buf.put(eyeColor1.toByte)
    buf.position(5425); buf.put(eyeColor2.toByte)
    buf.position(5426); buf.put(hairStyle.toByte)
    buf.position(5427); buf.put(beard.toByte)
    buf.position(5428); buf.put(face.toByte)
    buf.array()

  /** Decode OP_GuildsList: repeated null-terminated guild names, indexed by position.
    * The server sends names for guilds that exist; empty names are gaps.
    * Format: consecutive null-terminated strings, guild ID is the ordinal index.
    */
  def decodeGuildsList(data: Array[Byte]): Vector[GuildInfo] =
    val guilds = Vector.newBuilder[GuildInfo]
    var offset = 0
    var id = 0
    while offset < data.length do
      val name = readNullStringAt(data, offset)
      if name.nonEmpty then guilds += GuildInfo(id, name)
      offset += name.length + 1
      id += 1
    guilds.result()

  /** Decode OP_MOTD: null-terminated string. */
  def decodeMOTD(data: Array[Byte]): String =
    readNullString(data)

  /** Decode OP_ExpansionInfo: uint32 expansion flags. */
  def decodeExpansionInfo(data: Array[Byte]): ExpansionFlags =
    if data.length < 4 then ExpansionFlags(0)
    else
      val flags = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN).getInt()
      ExpansionFlags(flags)

  /** Decode OP_SetChatServer: null-terminated host:port string. */
  def decodeChatServer(data: Array[Byte]): String =
    readNullString(data)

  /** Decode OP_LogServer: null-terminated server name/host. */
  def decodeLogServer(data: Array[Byte]): String =
    readNullString(data)

  private def readNullStringAt(data: Array[Byte], offset: Int): String =
    val sb = StringBuilder()
    var i = offset
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()

  private def readNullString(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()
