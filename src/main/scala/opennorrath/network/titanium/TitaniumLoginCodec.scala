package opennorrath.network.titanium

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import opennorrath.network.{ServerInfo, ServerList}

/** Encode/decode application-layer login payloads for the Titanium (PC) protocol.
  *
  * Key differences from Mac LoginCodec:
  * - DES-CBC encryption (all-zero key/IV) for credentials and login response
  * - LoginBaseMessage header on most packets (10 bytes: seq + compressed + encrypt_type + unk3)
  * - Server list uses server_number IDs instead of IP strings for play requests
  * - Session key comes from LoginAccepted, not from PlayEverquestResponse
  * - No OP_LoginComplete step — client proceeds to world after PlayEverquestResponse
  *
  * Reference: EQEmu/loginserver/login_types.h, client.cpp, encryption.cpp
  */
object TitaniumLoginCodec:

  // LoginBaseMessage: int32 sequence(4) + bool compressed(1) + int8 encrypt_type(1) + int32 unk3(4)
  private val LoginBaseMessageSize = 10

  // ---- Outgoing (client -> server) ----

  /** OP_SessionReady payload: int32(2) for lm_initial (fresh login). */
  def encodeSessionReady: Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(2) // lm_initial
    buf.array()

  /** OP_Login: LoginBaseMessage header + DES-encrypted [username\0][password\0].
    *
    * The header is NOT encrypted. Credentials are DES-CBC encrypted with
    * all-zero key and IV, padded to 8-byte block boundary.
    */
  def encodeLogin(user: String, pass: String): Array[Byte] =
    // LoginBaseMessage header (unencrypted)
    val header = ByteBuffer.allocate(LoginBaseMessageSize).order(ByteOrder.LITTLE_ENDIAN)
    header.putInt(3)       // sequence = 3 (login)
    header.put(0: Byte)    // compressed = false
    header.put(2: Byte)    // encrypt_type = 2 (DES)
    header.putInt(0)       // unk3

    // Build credentials: [username\0][password\0]
    val userBytes = user.getBytes(StandardCharsets.US_ASCII)
    val passBytes = pass.getBytes(StandardCharsets.US_ASCII)
    val credLen = userBytes.length + 1 + passBytes.length + 1
    val credBuf = new Array[Byte](credLen)
    System.arraycopy(userBytes, 0, credBuf, 0, userBytes.length)
    // credBuf[userBytes.length] = 0 already
    System.arraycopy(passBytes, 0, credBuf, userBytes.length + 1, passBytes.length)
    // credBuf[last] = 0 already

    val encrypted = desEncrypt(credBuf)

    // Combine: header + encrypted
    val result = new Array[Byte](LoginBaseMessageSize + encrypted.length)
    System.arraycopy(header.array(), 0, result, 0, LoginBaseMessageSize)
    System.arraycopy(encrypted, 0, result, LoginBaseMessageSize, encrypted.length)
    result

  /** OP_ServerListRequest payload: just the sequence number as int32. */
  def encodeServerListRequest(sequence: Int = 4): Array[Byte] =
    val buf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(sequence)
    buf.array()

  /** OP_PlayEverquestRequest: LoginBaseMessage + uint32 server_number. */
  def encodePlayRequest(serverNumber: Int, sequence: Int = 5): Array[Byte] =
    val buf = ByteBuffer.allocate(LoginBaseMessageSize + 4).order(ByteOrder.LITTLE_ENDIAN)
    buf.putInt(sequence)   // sequence
    buf.put(0: Byte)       // compressed
    buf.put(0: Byte)       // encrypt_type
    buf.putInt(0)          // unk3
    buf.putInt(serverNumber)
    buf.array()

  // ---- Incoming (server -> client) ----

  /** Decode OP_ChatMessage (handshake reply to SessionReady).
    * Layout: LoginBaseMessage(10) + LoginBaseReplyMessage(success + error_str_id + str)
    * Returns true if handshake was successful.
    */
  def decodeChatMessage(data: Array[Byte]): Boolean =
    if data.length < LoginBaseMessageSize + 2 then return false
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.position(LoginBaseMessageSize) // skip header
    val success = buf.get() != 0
    success

  /** Result of decoding OP_LoginAccepted. */
  case class LoginResult(
    success: Boolean,
    accountId: Int,
    key: String,
    errorStrId: Int,
    showPlayerCount: Boolean,
    expansionBitmask: Int,
  )

  /** Decode OP_LoginAccepted: LoginBaseMessage (unencrypted) + 80-byte DES-encrypted PlayerLoginReply.
    *
    * PlayerLoginReply layout (inside encrypted block):
    *   success(1) + error_str_id(4) + str[1](1) +
    *   unk1(1) + unk2(1) + lsid(4) + key[11] + failed_attempts(4) +
    *   show_player_count(1) + offer_min_days(4) + ...
    *
    * For Titanium clients, offer_min_days holds the expansion bitmask.
    */
  def decodeLoginAccepted(data: Array[Byte]): LoginResult =
    if data.length < LoginBaseMessageSize + 8 then
      return LoginResult(false, 0, "", 0, false, 0)

    // DES decrypt the part after the LoginBaseMessage header
    val encryptedLen = data.length - LoginBaseMessageSize
    val alignedLen = (encryptedLen / 8) * 8
    if alignedLen == 0 then
      return LoginResult(false, 0, "", 0, false, 0)

    val encrypted = new Array[Byte](alignedLen)
    System.arraycopy(data, LoginBaseMessageSize, encrypted, 0, alignedLen)

    val decrypted = desDecrypt(encrypted)
    if decrypted == null then
      return LoginResult(false, 0, "", 0, false, 0)

    val buf = ByteBuffer.wrap(decrypted).order(ByteOrder.LITTLE_ENDIAN)

    // LoginBaseReplyMessage
    val success = buf.get() != 0          // offset 0
    val errorStrId = buf.getInt()          // offset 1
    val _str = buf.get()                   // offset 5 (str[1], usually null terminator)

    if !success then
      return LoginResult(false, 0, "", errorStrId, false, 0)

    if buf.remaining() < 22 then
      return LoginResult(false, 0, "", errorStrId, false, 0)

    // PlayerLoginReply fields after LoginBaseReplyMessage
    val _unk1 = buf.get()                  // offset 6
    val _unk2 = buf.get()                  // offset 7
    val lsid = buf.getInt()                // offset 8 (account ID)

    // key[11] — null-terminated string within 11 bytes
    val keyBytes = new Array[Byte](11)
    buf.get(keyBytes)                      // offset 12
    val key = new String(keyBytes, StandardCharsets.US_ASCII).takeWhile(_ != '\u0000')

    val _failedAttempts = buf.getInt()     // offset 23
    val showPlayerCount = buf.get() != 0   // offset 27

    // offer_min_days — for Titanium, holds the expansion bitmask
    val expansionBitmask = if buf.remaining() >= 4 then buf.getInt() else 0

    LoginResult(success, lsid, key, errorStrId, showPlayerCount, expansionBitmask)

  /** Decode OP_ServerListResponse.
    *
    * Layout: LoginBaseMessage(10) + LoginBaseReplyMessage(success + error_str_id + str\0)
    *   + int32 server_count + per server: [ip\0][int32 server_type][uint32 server_id]
    *   [name\0][country_code\0][language_code\0][int32 status][uint32 player_count]
    *
    * Returns a ServerList compatible with the Mac format, using worldId for server_id.
    */
  def decodeServerList(data: Array[Byte]): ServerList =
    if data.length < LoginBaseMessageSize + 6 then
      return ServerList(Vector.empty, false)

    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

    // Skip LoginBaseMessage header
    buf.position(LoginBaseMessageSize)

    // LoginBaseReplyMessage
    val success = buf.get() != 0
    val _errorStrId = buf.getInt()
    readNullStringFromBuf(buf) // variable-length str (usually empty)

    if !success || buf.remaining() < 4 then
      return ServerList(Vector.empty, false)

    val serverCount = buf.getInt()
    val servers = Vector.newBuilder[ServerInfo]

    var i = 0
    while i < serverCount && buf.remaining() > 0 do
      try
        val ip = readNullStringFromBuf(buf)
        val serverType = buf.getInt()
        val serverId = buf.getInt()
        val name = readNullStringFromBuf(buf)
        val _countryCode = readNullStringFromBuf(buf)
        val _languageCode = readNullStringFromBuf(buf)
        val status = buf.getInt()
        val playerCount = buf.getInt()

        // Map server_type flags to greenName (Preferred or Legends servers show green)
        val greenName = (serverType & 0x18) != 0 // Preferred=8, Legends=16

        servers += ServerInfo(
          name = name,
          ip = ip,
          greenName = greenName,
          flags = status,
          worldId = serverId,
          userCount = playerCount,
        )
      catch
        case _: java.nio.BufferUnderflowException => i = serverCount // stop parsing

      i += 1

    ServerList(servers.result(), showUserCount = true)

  /** Decode OP_PlayEverquestResponse.
    * Layout: LoginBaseMessage(10) + LoginBaseReplyMessage(success + error_str_id + str[1])
    *   + uint32 server_number
    *
    * Returns (success, serverId). The session key was already obtained from LoginAccepted.
    */
  def decodePlayResponse(data: Array[Byte]): (Boolean, Int) =
    if data.length < LoginBaseMessageSize + 6 then return (false, 0)

    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    buf.position(LoginBaseMessageSize)

    val success = buf.get() != 0
    val errorStrId = buf.getInt()
    val _str = buf.get() // str[1]

    val serverId = if buf.remaining() >= 4 then buf.getInt() else 0

    if !success then
      println(s"[TitaniumLogin] Play rejected: error=$errorStrId")

    (success, serverId)

  // ---- DES-CBC encryption (all-zero key and IV, matching EQEmu's eqcrypt_block) ----

  private val DesZeroKey = new Array[Byte](8)
  private val DesZeroIv = new Array[Byte](8)

  /** DES-CBC encrypt, padding input to 8-byte block boundary. */
  private def desEncrypt(data: Array[Byte]): Array[Byte] =
    // Pad to 8-byte boundary (zero-pad, not PKCS5)
    val paddedLen = ((data.length + 7) / 8) * 8
    val padded = new Array[Byte](paddedLen)
    System.arraycopy(data, 0, padded, 0, data.length)

    val cipher = Cipher.getInstance("DES/CBC/NoPadding")
    val keySpec = SecretKeySpec(DesZeroKey, "DES")
    val ivSpec = IvParameterSpec(DesZeroIv)
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec)
    cipher.doFinal(padded)

  /** DES-CBC decrypt. Input must be 8-byte aligned. Returns null on failure. */
  private def desDecrypt(data: Array[Byte]): Array[Byte] =
    if data.length == 0 || data.length % 8 != 0 then return null
    try
      val cipher = Cipher.getInstance("DES/CBC/NoPadding")
      val keySpec = SecretKeySpec(DesZeroKey, "DES")
      val ivSpec = IvParameterSpec(DesZeroIv)
      cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)
      cipher.doFinal(data)
    catch
      case _: Exception => null

  private def readNullStringFromBuf(buf: ByteBuffer): String =
    val sb = StringBuilder()
    while buf.hasRemaining do
      val b = buf.get()
      if b == 0 then return sb.result()
      sb += b.toChar
    sb.result()
