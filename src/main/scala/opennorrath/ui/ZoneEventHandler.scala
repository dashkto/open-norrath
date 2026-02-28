package opennorrath.ui

import opennorrath.Game
import opennorrath.network.{ChatMessage, FormattedMessage, MessageType, SpecialMessage, SpawnAppearanceChange, ZoneEvent}
import opennorrath.state.{PlayerCharacter, ZoneCharacter}

/** Processes ZoneClient events — routes messages to the text panel
  * and updates PlayerCharacter. Registers as a ZoneClient listener.
  */
class ZoneEventHandler(chatPanel: TextPanel, characters: scala.collection.Map[Int, ZoneCharacter], player: Option[PlayerCharacter] = None):

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

    case ZoneEvent.DamageDealt(info) if info.sourceId != info.targetId || info.damage > 0 =>
      val src = spawnName(info.sourceId)
      val tgt = spawnName(info.targetId)
      // EQEmu damage codes: 0=miss, -1=block, -2=parry, -3=dodge, -4=invulnerable, -5=riposte
      info.damage match
        case d if d > 0  => chatPanel.addLine(s"$src hits $tgt for $d points of damage.", Colors.text)
        case 0           => chatPanel.addLine(s"$src tries to hit $tgt, but misses!", Colors.textDim)
        case -1          => chatPanel.addLine(s"$tgt blocks the attack from $src!", Colors.textDim)
        case -2          => chatPanel.addLine(s"$tgt parries the attack from $src!", Colors.textDim)
        case -3          => chatPanel.addLine(s"$tgt dodges the attack from $src!", Colors.textDim)
        case -4          => chatPanel.addLine(s"$src tries to hit $tgt, but $tgt is INVULNERABLE!", Colors.textDim)
        case -5          => chatPanel.addLine(s"$tgt ripostes the attack from $src!", Colors.textDim)
        case _           => () // Unknown negative — ignore

    case ZoneEvent.EntityDied(info) =>
      val who = spawnName(info.spawnId)
      val killer = spawnName(info.killerId)
      chatPanel.addLine(s"$who has been slain by $killer!", Colors.danger)

    case ZoneEvent.ConsiderResult(result) =>
      val tgt = spawnName(result.targetId)
      val factionStr = factionName(result.faction)
      chatPanel.addLine(s"$tgt $factionStr", conColor(result.conLevel))

    case ZoneEvent.ExpChanged(_) =>
      () // Server sends its own "You gain experience" text via OP_SpecialMesg

    case ZoneEvent.LevelChanged(_) =>
      () // Server sends the formatted level-up message via OP_SpecialMesg

    case ZoneEvent.SkillChanged(sk) =>
      val name = EqData.skillName(sk.skillId)
      chatPanel.addLine(s"You have become better at $name! (${ sk.value })", Colors.secondary)

    case ZoneEvent.SpellInterrupted(messageId, color, message) =>
      // Resolve string table ID; fall back to inline message if present
      val text = EqStringTable.get(messageId)
        .orElse(Option.when(message.nonEmpty)(message))
        .getOrElse("Your spell is interrupted.")
      chatPanel.addLine(text, msgTypeColor(color))

    case ZoneEvent.SpellActionTriggered(action) =>
      // buffUnknown: 1 = cast begin, 4 = spell landed. Only show chat for cast begin.
      if action.spellId > 0 && action.buffUnknown == 1 then
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

  /** Map MT_* message types to display colors. */
  private def msgTypeColor(msgType: Int): (Float, Float, Float, Float) =
    messageTypeColor(MessageType.fromCode(msgType))

  private def messageTypeColor(mt: MessageType): (Float, Float, Float, Float) = mt match
    // Chat channels
    case MessageType.Say              => Colors.text
    case MessageType.Tell             => Colors.violet
    case MessageType.Group            => Colors.secondary2
    case MessageType.Guild            => Colors.heal
    case MessageType.OOC              => Colors.secondary
    case MessageType.Auction          => Colors.heal
    case MessageType.Shout            => Colors.coral
    case MessageType.Emote            => Colors.rose
    // Combat & spells
    case MessageType.Spells           => Colors.teal
    case MessageType.YouHitOther      => Colors.text
    case MessageType.OtherHitsYou     => Colors.text
    case MessageType.YouMissOther     => Colors.text
    case MessageType.OtherMissesYou   => Colors.text
    case MessageType.Broadcasts       => Colors.sky
    case MessageType.Skills           => Colors.teal
    case MessageType.Disciplines      => Colors.amber
    case MessageType.DefaultText      => Colors.text
    case MessageType.MerchantOffer    => Colors.sand
    case MessageType.MerchantBuySell  => Colors.sand
    case MessageType.YourDeath        => Colors.danger
    case MessageType.OtherDeath       => Colors.textDim
    case MessageType.OtherHits        => Colors.text
    case MessageType.OtherMisses      => Colors.textDim
    case MessageType.Who              => Colors.text
    case MessageType.YellForHelp      => Colors.coral
    case MessageType.NonMelee         => Colors.text
    case MessageType.WornOff          => Colors.teal
    case MessageType.MoneySplit       => Colors.sand
    case MessageType.LootMessages     => Colors.sand
    case MessageType.DiceRoll         => Colors.text
    case MessageType.OtherSpells      => Colors.teal
    case MessageType.SpellFailure     => Colors.danger
    // Custom chat channels
    case MessageType.Chat | MessageType.Channel1 | MessageType.Channel2 | MessageType.Channel3 |
         MessageType.Channel4 | MessageType.Channel5 | MessageType.Channel6 | MessageType.Channel7 |
         MessageType.Channel8 | MessageType.Channel9 | MessageType.Channel10 => Colors.secondary
    // Crits & special combat
    case MessageType.CritMelee        => Colors.gold
    case MessageType.SpellCrits       => Colors.gold
    case MessageType.TooFarAway       => Colors.textDim
    case MessageType.NPCRampage       => Colors.coral
    case MessageType.NPCFlurry        => Colors.coral
    case MessageType.NPCEnrage        => Colors.danger
    // Echo channels — your own messages echoed back
    case MessageType.SayEcho          => Colors.text
    case MessageType.TellEcho         => Colors.violet
    case MessageType.GroupEcho        => Colors.secondary2
    case MessageType.GuildEcho        => Colors.heal
    case MessageType.OOCEcho          => Colors.secondary
    case MessageType.AuctionEcho      => Colors.heal
    case MessageType.ShoutEcho        => Colors.coral
    case MessageType.EmoteEcho        => Colors.rose
    case MessageType.Chat1Echo | MessageType.Chat2Echo | MessageType.Chat3Echo | MessageType.Chat4Echo |
         MessageType.Chat5Echo | MessageType.Chat6Echo | MessageType.Chat7Echo | MessageType.Chat8Echo |
         MessageType.Chat9Echo | MessageType.Chat10Echo => Colors.secondary
    // DoTs, pets, misc
    case MessageType.DoTDamage        => Colors.danger
    case MessageType.ItemLink         => Colors.secondary
    case MessageType.RaidSay          => Colors.secondary2
    case MessageType.MyPet            => Colors.teal
    case MessageType.DamageShield     => Colors.text
    case MessageType.Leadership       => Colors.amber
    case MessageType.PetFlurry        => Colors.coral
    case MessageType.PetCrit          => Colors.gold
    case MessageType.FocusEffect      => Colors.teal
    case MessageType.Experience       => Colors.gold
    case MessageType.System           => Colors.sky
    case MessageType.PetSpell         => Colors.teal
    case MessageType.PetResponse      => Colors.teal
    case MessageType.ItemSpeech       => Colors.sand
    case MessageType.StrikeThrough    => Colors.gold
    case MessageType.Stun             => Colors.amber

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
