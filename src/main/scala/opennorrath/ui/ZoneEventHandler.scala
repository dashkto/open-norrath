package opennorrath.ui

import opennorrath.Game
import opennorrath.network.{ChatMessage, ZoneEvent}

/** Processes ZoneClient events — routes messages to the text panel
  * and updates PlayerState. Registers as a ZoneClient listener.
  */
class ZoneEventHandler(chatPanel: TextPanel):

  private var seenFirstExp = false
  val listener: ZoneEvent => Unit = handle

  /** Send a chat message from the input field. Parses /commands. */
  def submitChat(text: String): Unit =
    val session = Game.zoneSession
    val ps = Game.playerState
    if session.isDefined && ps.isDefined then
      val name = ps.get.name
      val (channel, target, msg) = parseCommand(text)
      session.get.client.sendChat(name, target, channel, 0, msg)
      chatPanel.addLine(formatOutgoing(channel, target, msg), channelColor(channel))
    else
      chatPanel.addLine(text)

  // ===========================================================================
  // Event dispatch
  // ===========================================================================

  private def handle(event: ZoneEvent): Unit = event match
    case ZoneEvent.ChatReceived(msg) =>
      chatPanel.addLine(formatChat(msg), channelColor(msg.channel))

    case ZoneEvent.DamageDealt(info) =>
      val src = spawnName(info.sourceId)
      val tgt = spawnName(info.targetId)
      if info.damage < 0 then
        chatPanel.addLine(s"$src tries to hit $tgt, but misses!", Colors.textDim)
      else
        chatPanel.addLine(s"$src hits $tgt for ${info.damage} points of damage.", Colors.text)

    case ZoneEvent.EntityDied(info) =>
      val who = spawnName(info.spawnId)
      val killer = spawnName(info.killerId)
      chatPanel.addLine(s"$who has been slain by $killer!", Colors.danger)

    case ZoneEvent.ConsiderResult(result) =>
      val tgt = spawnName(result.targetId)
      chatPanel.addLine(s"$tgt [${result.level}]", conLevelColor(result.level))

    case ZoneEvent.ExpChanged(_) =>
      if seenFirstExp then
        chatPanel.addLine("You gain experience!", Colors.gold)
      else
        seenFirstExp = true

    case ZoneEvent.LevelChanged(lvl) =>
      chatPanel.addLine(s"You have reached level ${lvl.level}!", Colors.gold)
      Game.playerState.foreach(_.level = lvl.level)

    case ZoneEvent.HPChanged(hp) =>
      Game.playerState.foreach { ps =>
        ps.currentHp = hp.curHp
        ps.maxHp = hp.maxHp
      }

    case ZoneEvent.ManaChanged(mana) =>
      Game.playerState.foreach(_.currentMana = mana.curMana)

    case ZoneEvent.Error(msg) =>
      chatPanel.addLine(s"Error: $msg", Colors.danger)

    case _ => () // Spawns, movement, appearance, etc. handled by other listeners

  // ===========================================================================
  // Chat formatting
  // ===========================================================================

  private def formatChat(msg: ChatMessage): String = msg.channel match
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
  // Helpers
  // ===========================================================================

  private def spawnName(id: Int): String =
    Game.zoneSession.flatMap(_.client.spawns.get(id).map(_.name)).getOrElse(s"#$id")

  private def conLevelColor(targetLevel: Int): (Float, Float, Float, Float) =
    val myLevel = Game.playerState.map(_.level).getOrElse(1)
    val diff = targetLevel - myLevel
    if diff >= 3 then Colors.danger
    else if diff >= 1 then Colors.gold
    else if diff >= -2 then Colors.text
    else if diff >= -5 then Colors.secondary
    else Colors.textDim
