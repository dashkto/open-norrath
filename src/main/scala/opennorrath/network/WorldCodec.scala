package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

/** Character info from the character select screen. */
case class CharacterInfo(
  name: String,
  level: Int,
  classId: Int,
  race: Int,
  zone: Int,
)

/** Zone server connection info. */
case class ZoneAddress(ip: String, port: Int)

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

  /** Decode OP_SendCharInfo: CharacterSelect_Struct.
    *
    * Layout (EQMac):
    *   race[10]     (uint16 each, offset 0)
    *   class_[10]   (uint8 each, offset 20)
    *   level[10]    (uint8 each, offset 30)
    *   name[10][64] (char[64] each, offset varies)
    *   zone[10]     (uint32 each)
    *
    * This is a best-effort decode — the actual struct size varies by server version.
    * We extract what we can and log failures.
    */
  def decodeCharacterSelect(data: Array[Byte]): Vector[CharacterInfo] =
    if data.length < 40 then return Vector.empty

    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val chars = Vector.newBuilder[CharacterInfo]

    try
      // race[10] — uint16 each (20 bytes)
      val races = Array.fill(10)(if buf.remaining() >= 2 then buf.getShort() & 0xFFFF else 0)
      // class_[10] — uint8 each (10 bytes)
      val classes = Array.fill(10)(if buf.remaining() >= 1 then buf.get() & 0xFF else 0)
      // level[10] — uint8 each (10 bytes)
      val levels = Array.fill(10)(if buf.remaining() >= 1 then buf.get() & 0xFF else 0)

      // name[10][64] — 640 bytes
      val names = Array.fill(10) {
        if buf.remaining() >= 64 then
          val nameBytes = new Array[Byte](64)
          buf.get(nameBytes)
          readNullString(nameBytes)
        else ""
      }

      // Skip to zones — there are more fields between names and zones
      // For now, just use zone=0 and we'll refine with packet captures
      for i <- 0 until 10 do
        if names(i).nonEmpty then
          chars += CharacterInfo(
            name = names(i),
            level = levels(i),
            classId = classes(i),
            race = races(i),
            zone = 0,
          )
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
      val port = ((data(128) & 0xFF) | ((data(129) & 0xFF) << 8)) // little-endian uint16
      Some(ZoneAddress(ip, port))

  private def readNullString(data: Array[Byte]): String =
    val sb = StringBuilder()
    var i = 0
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()
