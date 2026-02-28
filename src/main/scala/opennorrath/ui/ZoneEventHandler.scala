package opennorrath.ui

import opennorrath.Game
import opennorrath.network.{ChatMessage, FormattedMessage, SpecialMessage, SpawnAppearanceChange, ZoneEvent}
import opennorrath.state.{PlayerCharacter, ZoneCharacter}

/** Processes ZoneClient events — routes messages to the text panel
  * and updates PlayerCharacter. Registers as a ZoneClient listener.
  */
class ZoneEventHandler(chatPanel: TextPanel, characters: scala.collection.Map[Int, ZoneCharacter], player: Option[PlayerCharacter] = None):

  private var seenFirstExp = false
  val listener: ZoneEvent => Unit = handle

  /** Send a chat message from the input field. Parses /commands. */
  def submitChat(text: String): Unit =
    if tryClientCommand(text) then return
    val session = Game.zoneSession
    if session.isDefined && player.isDefined then
      val name = player.get.name
      val (channel, target, msg) = parseCommand(text)
      session.get.client.sendChat(name, target, channel, 0, msg)
      // Server echoes the message back via OP_ChannelMessage — no client-side echo needed
    else
      chatPanel.addLine(text)

  // ===========================================================================
  // Event dispatch
  // ===========================================================================

  private def handle(event: ZoneEvent): Unit = event match
    case ZoneEvent.ChatReceived(msg) =>
      chatPanel.addLine(formatChat(msg), channelColor(msg.channel))

    case ZoneEvent.SpecialMsgReceived(msg) =>
      if msg.message.nonEmpty then
        chatPanel.addLine(msg.message, msgTypeColor(msg.msgType))

    case ZoneEvent.FormattedMsgReceived(msg) =>
      val text = EqStringTable.format(msg.stringId, msg.arguments)
        .getOrElse(s"[${msg.stringId}] ${msg.arguments.mkString(", ")}")
      if text.nonEmpty then
        chatPanel.addLine(text, msgTypeColor(msg.msgType))

    case ZoneEvent.EmoteReceived(msg) =>
      chatPanel.addLine(msg.message, Colors.textDim)

    case ZoneEvent.MultiLineMsgReceived(text) =>
      // Multi-line text uses backtick (`) as line separator in EQ
      for line <- text.split('`') if line.nonEmpty do
        chatPanel.addLine(line, Colors.text)

    case ZoneEvent.YellReceived(spawnId) =>
      val name = spawnName(spawnId)
      chatPanel.addLine(s"$name yells for help!", Colors.coral)

    case ZoneEvent.DamageDealt(info) =>
      val src = spawnName(info.sourceId)
      val tgt = spawnName(info.targetId)
      if info.damage == 0 then
        chatPanel.addLine(s"$src tries to hit $tgt, but misses!", Colors.textDim)
      else if info.damage > 0 then
        chatPanel.addLine(s"$src hits $tgt for ${info.damage} points of damage.", Colors.text)

    case ZoneEvent.EntityDied(info) =>
      val who = spawnName(info.spawnId)
      val killer = spawnName(info.killerId)
      chatPanel.addLine(s"$who has been slain by $killer!", Colors.danger)

    case ZoneEvent.ConsiderResult(result) =>
      val tgt = spawnName(result.targetId)
      val factionStr = factionName(result.faction)
      chatPanel.addLine(s"$tgt $factionStr", conColor(result.conLevel))

    case ZoneEvent.ExpChanged(_) =>
      if seenFirstExp then
        chatPanel.addLine("You gain experience!", Colors.gold)
      else
        seenFirstExp = true

    case ZoneEvent.LevelChanged(lvl) =>
      chatPanel.addLine(s"You have reached level ${lvl.level}!", Colors.gold)

    case ZoneEvent.SkillChanged(sk) =>
      val name = EqData.skillName(sk.skillId)
      chatPanel.addLine(s"You have become better at $name! (${ sk.value })", Colors.secondary)

    case ZoneEvent.SpellActionTriggered(action) =>
      if action.spellId > 0 then
        val src = spawnName(action.sourceId)
        val tgt = spawnName(action.targetId)
        if action.sourceId == action.targetId then
          chatPanel.addLine(s"$src begins to cast a spell.", Colors.secondary)
        else
          chatPanel.addLine(s"$src begins to cast a spell on $tgt.", Colors.secondary)

    case ZoneEvent.AppearanceChanged(change) =>
      change.appearanceType match
        case SpawnAppearanceChange.LinkDead =>
          val name = spawnName(change.spawnId)
          if change.parameter != 0 then
            chatPanel.addLine(s"$name has gone linkdead.", Colors.textDim)
          else
            chatPanel.addLine(s"$name has reconnected.", Colors.textDim)
        case SpawnAppearanceChange.AFK =>
          val name = spawnName(change.spawnId)
          if change.parameter != 0 then
            chatPanel.addLine(s"$name is AFK.", Colors.textDim)
        case _ => ()

    case ZoneEvent.ZoneDataReceived(info) =>
      chatPanel.addLine(s"You have entered ${info.zoneLongName}.", Colors.gold)

    case ZoneEvent.WeatherChanged(weather) =>
      weather.weatherType match
        case 1 => chatPanel.addLine("It begins to rain.", Colors.secondary)
        case 2 => chatPanel.addLine("It begins to snow.", Colors.secondary)
        case 0 if weather.intensity == 0 => chatPanel.addLine("The sky clears.", Colors.secondary)
        case _ => ()

    case ZoneEvent.Error(msg) =>
      chatPanel.addLine(s"Error: $msg", Colors.danger)

    case _ => ()

  // ===========================================================================
  // Chat formatting
  // ===========================================================================

  private def formatChat(msg: ChatMessage): String =
    val isMe = player.exists(_.name.equalsIgnoreCase(msg.sender))
    if isMe then formatOutgoing(msg.channel, "", msg.message)
    else msg.channel match
      case ChatMessage.Say     => s"${msg.sender} says, '${msg.message}'"
      case ChatMessage.Shout   => s"${msg.sender} shouts, '${msg.message}'"
      case ChatMessage.OOC     => s"${msg.sender} says out of character, '${msg.message}'"
      case ChatMessage.Auction => s"${msg.sender} auctions, '${msg.message}'"
      case ChatMessage.Tell    => s"${msg.sender} tells you, '${msg.message}'"
      case ChatMessage.Group   => s"${msg.sender} tells the group, '${msg.message}'"
      case ChatMessage.Guild   => s"${msg.sender} tells the guild, '${msg.message}'"
      case ChatMessage.GMSay   => s"[GM] ${msg.sender}: ${msg.message}"
      case _                   => s"${msg.sender}: ${msg.message}"

  private def formatOutgoing(channel: Int, target: String, msg: String): String = channel match
    case ChatMessage.Say     => s"You say, '$msg'"
    case ChatMessage.Shout   => s"You shout, '$msg'"
    case ChatMessage.OOC     => s"You say out of character, '$msg'"
    case ChatMessage.Auction => s"You auction, '$msg'"
    case ChatMessage.Tell    => s"You told $target, '$msg'"
    case ChatMessage.Group   => s"You tell the group, '$msg'"
    case ChatMessage.Guild   => s"You tell the guild, '$msg'"
    case _                   => s"You say, '$msg'"

  private def channelColor(channel: Int): (Float, Float, Float, Float) = channel match
    case ChatMessage.Say     => Colors.text
    case ChatMessage.Shout   => Colors.coral
    case ChatMessage.OOC     => Colors.secondary
    case ChatMessage.Auction => Colors.heal
    case ChatMessage.Tell    => Colors.violet
    case ChatMessage.Group   => Colors.secondary2
    case ChatMessage.Guild   => Colors.heal
    case ChatMessage.GMSay   => Colors.gold
    case _                   => Colors.text

  /** Map MT_* msg_type codes (from OP_SpecialMesg / OP_FormattedMessage) to display colors.
    * Values from EQMacDocker/Server/common/eq_constants.h.
    */
  private def msgTypeColor(msgType: Int): (Float, Float, Float, Float) = msgType match
    case 256     => Colors.text       // MT_Say
    case 257     => Colors.violet     // MT_Tell
    case 258     => Colors.secondary2 // MT_Group
    case 259     => Colors.heal       // MT_Guild
    case 260     => Colors.secondary  // MT_OOC
    case 261     => Colors.heal       // MT_Auction
    case 262     => Colors.coral      // MT_Shout
    case 263     => Colors.rose       // MT_Emote
    case 264     => Colors.teal       // MT_Spells
    case 265 | 266 | 267 | 268 => Colors.text // MT_YouHitOther / OtherHitsYou / misses
    case 269     => Colors.sky        // MT_Broadcasts
    case 270     => Colors.teal       // MT_Skills
    case 271     => Colors.amber      // MT_Disciplines
    case 273     => Colors.text       // MT_DefaultText
    case 275     => Colors.sand       // MT_MerchantOffer
    case 301     => Colors.gold       // MT_CritMelee
    case 302     => Colors.gold       // MT_SpellCrits
    case _       => Colors.text

  // ===========================================================================
  // Input parsing
  // ===========================================================================

  private def parseCommand(text: String): (Int, String, String) =
    val parts = text.split("\\s+", 2)
    val cmd = parts(0).toLowerCase
    val rest = if parts.length > 1 then parts(1) else ""
    cmd match
      case "/say" | "/s"       => (ChatMessage.Say, "", rest)
      case "/shout" | "/sho"   => (ChatMessage.Shout, "", rest)
      case "/ooc"              => (ChatMessage.OOC, "", rest)
      case "/auction" | "/auc" => (ChatMessage.Auction, "", rest)
      case "/tell" | "/t" =>
        val tellParts = rest.split("\\s+", 2)
        val target = tellParts(0)
        val msg = if tellParts.length > 1 then tellParts(1) else ""
        (ChatMessage.Tell, target, msg)
      case "/group" | "/g"     => (ChatMessage.Group, "", rest)
      case "/guild" | "/gu"    => (ChatMessage.Guild, "", rest)
      case _ =>
        (ChatMessage.Say, "", text) // Default to say

  // ===========================================================================
  // Client commands — delegates to ClientCommands registry
  // ===========================================================================

  private val commandContext = ClientCommands.Context(chatPanel, characters, player)

  private def tryClientCommand(text: String): Boolean =
    ClientCommands.tryExecute(text, commandContext)

  // ===========================================================================
  // Helpers
  // ===========================================================================

  private def spawnName(id: Int): String =
    characters.get(id).map(_.displayName)
      .orElse(player.filter(_ => Game.zoneSession.exists(_.client.mySpawnId == id)).map(_.name))
      .orElse(Game.zoneSession.flatMap(_.client.spawns.get(id)).map(s => ZoneCharacter.cleanName(s.name)))
      .getOrElse(s"#$id")

  /** Map con color code from GetLevelCon() to display color.
    * Green=2, Blue=4, Red=13, Yellow=15, LightBlue=18, White=20.
    */
  private def conColor(conLevel: Int): (Float, Float, Float, Float) = conLevel match
    case 2  => Colors.heal       // Green
    case 4  => Colors.secondary  // Blue
    case 13 => Colors.danger     // Red
    case 15 => Colors.gold       // Yellow
    case 18 => Colors.secondary2 // Light blue
    case 20 => Colors.text       // White (even con)
    case _  => Colors.text

  /** Map faction standing code to EQ-style text. */
  private def factionName(faction: Int): String = faction match
    case 1 => "regards you as an ally"
    case 2 => "looks upon you warmly"
    case 3 => "kindly considers you"
    case 4 => "judges you amiably"
    case 5 => "regards you indifferently"
    case 6 => "looks upon you apprehensively"
    case 7 => "scowls at you dubiously"
    case 8 => "glares at you threateningly"
    case 9 => "scowls at you, ready to attack"
    case _ => ""
