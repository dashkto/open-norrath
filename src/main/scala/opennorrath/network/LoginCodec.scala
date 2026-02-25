package opennorrath.network

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

case class ServerInfo(
  name: String,
  ip: String,
  greenName: Boolean,
  flags: Int,
  worldId: Int,
  userCount: Int,
)

case class ServerList(servers: Vector[ServerInfo], showUserCount: Boolean)

/** Encode/decode application-layer login payloads.
  *
  * All structs use #pragma pack(1) and native (little-endian) byte order
  * in the EQEmu server code.
  *
  * Reference: EQMacDocker/Server/loginserver/login_structures.h
  *            EQMacDocker/Server/loginserver/client.cpp
  */
object LoginCodec:

  /** OP_SessionReady payload: mode=2 (lm_initial, fresh login).
    * The old client code path ignores the payload, but we send it anyway.
    */
  def encodeSessionReady: Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(2) // lm_initial
    buf.array()

  /** OP_LoginOSX: "user/pass eqworld-52.989studios.com\0"
    * Payload must be >= 40 bytes (sizeof(LoginServerInfo_Struct)).
    */
  def encodeLoginOSX(user: String, pass: String): Array[Byte] =
    val s = s"$user/$pass eqworld-52.989studios.com"
    val bytes = s.getBytes(StandardCharsets.US_ASCII)
    val minSize = 40 // LoginServerInfo_Struct.crypt[40]
    val result = new Array[Byte](Math.max(bytes.length + 1, minSize))
    System.arraycopy(bytes, 0, result, 0, bytes.length)
    result

  /** OP_ServerListRequest: empty payload. */
  def encodeServerListRequest: Array[Byte] = Array.emptyByteArray

  /** OP_PlayEverquestRequest for old client: server IP string null-terminated.
    * The old code path passes the raw payload to SendOldUserToWorldRequest
    * which compares it against world server IPs as a string.
    */
  def encodePlayRequest(serverIp: String): Array[Byte] =
    val bytes = serverIp.getBytes(StandardCharsets.US_ASCII)
    val result = new Array[Byte](bytes.length + 1)
    System.arraycopy(bytes, 0, result, 0, bytes.length)
    result

  /** OP_LoginComplete: empty payload (server responds with 20-byte packet). */
  def encodeLoginComplete: Array[Byte] = Array.emptyByteArray

  /** Decode OP_SessionReady response: null-terminated date string ("12-4-2002 1800"). */
  def decodeSessionReadyResponse(data: Array[Byte]): String =
    readNullString(data, 0)

  /** Decode OP_LoginAccepted: SessionIdEQMacPPC_Struct (28 bytes for OSX).
    * Layout: session_id[10] + unused[7] + unknown(uint32) + padding[7]
    * session_id format: "LS#<account_id>"
    */
  def decodeLoginAccepted(data: Array[Byte]): String =
    readNullString(data, 0) // session_id is null-terminated within 10 bytes

  /** Decode OP_ServerName: null-terminated IP string. */
  def decodeServerName(data: Array[Byte]): String =
    readNullString(data, 0)

  /** Decode OP_ClientError: null-terminated error message. */
  def decodeClientError(data: Array[Byte]): String =
    readNullString(data, 0)

  /** Decode OP_ServerListRequest response.
    *
    * ServerList_Struct: numservers(uint16) + padding(2) + showusercount(uint8)
    * Per server: name\0 + ip\0 + ServerListServerFlags_Struct:
    *   greenname(uint8) + flags(int32) + worldid(int32) + usercount(uint32)
    * End: ServerListEndFlags_Struct (25 bytes) + 1 byte
    *
    * All multi-byte integers in struct fields are LITTLE-ENDIAN (native x86/arm).
    */
  def decodeServerList(data: Array[Byte]): ServerList =
    if data.length < 5 then ServerList(Vector.empty, false)
    else
      val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
      val numServers = buf.getShort() & 0xFFFF
      buf.get() // padding
      buf.get() // padding
      val showUserCount = (buf.get() & 0xFF) == 0xFF

      val servers = Vector.newBuilder[ServerInfo]
      var done = false
      var i = 0
      while i < numServers && !done do
        if buf.remaining() < 3 then done = true
        else
          val name = readNullStringFromBuf(buf)
          val ip = readNullStringFromBuf(buf)
          if buf.remaining() < 13 then done = true
          else
            val greenName = buf.get() != 0
            val flags = buf.getInt()
            val worldId = buf.getInt()
            val userCount = buf.getInt()
            servers += ServerInfo(name, ip, greenName, flags, worldId, userCount)
        i += 1

      ServerList(servers.result(), showUserCount)

  /** Decode play response. For old clients, the server reuses OP_PlayEverquestRequest.
    * Payload[0] = unused, Payload[1..10] = 10-char session key.
    */
  def decodePlayResponse(data: Array[Byte]): String =
    if data.length < 2 then return ""
    val keyLen = Math.min(10, data.length - 1)
    new String(data, 1, keyLen, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')

  /** Decode OP_LoginComplete response: 20 bytes, first byte = 1 on success. */
  def decodeLoginComplete(data: Array[Byte]): Boolean =
    data.nonEmpty && data(0) != 0

  private def readNullString(data: Array[Byte], offset: Int): String =
    val sb = StringBuilder()
    var i = offset
    while i < data.length && data(i) != 0 do
      sb += data(i).toChar
      i += 1
    sb.result()

  private def readNullStringFromBuf(buf: ByteBuffer): String =
    val sb = StringBuilder()
    while buf.hasRemaining do
      val b = buf.get()
      if b == 0 then return sb.result()
      sb += b.toChar
    sb.result()
