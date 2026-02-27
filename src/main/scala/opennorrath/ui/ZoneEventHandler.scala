package opennorrath.ui

import opennorrath.Game
import opennorrath.network.{ChatMessage, PlayerPosition, SpawnAppearanceChange, ZoneEvent}
import opennorrath.state.{PlayerCharacter, ZoneCharacter}
import opennorrath.world.EqCoords

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
      chatPanel.addLine(s"$tgt [${result.level}]", conLevelColor(result.level))

    case ZoneEvent.ExpChanged(_) =>
      if seenFirstExp then
        chatPanel.addLine("You gain experience!", Colors.gold)
      else
        seenFirstExp = true

    case ZoneEvent.LevelChanged(lvl) =>
      chatPanel.addLine(s"You have reached level ${lvl.level}!", Colors.gold)

    case ZoneEvent.SkillChanged(sk) =>
      val name = EqData.skillName(sk.skillId)
      chatPanel.addLine(s"You have become better at $name! (${ sk.value })", Colors.gold)

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
  // Client commands
  // ===========================================================================

  private def tryClientCommand(text: String): Boolean =
    if !text.startsWith("/") then return false
    val parts = text.split("\\s+")
    parts(0).toLowerCase match
      case "/tp" => handleTp(parts.drop(1)); true
      case "/loc" => handleLoc(); true
      case "/speedup" => handleSpeedUp(); true
      case "/slowdown" => handleSlowDown(); true
      case "/attack" | "/a" => handleAttackToggle(); true
      case "/who" => handleWho(); true
      // Chat channel commands — pass through to parseCommand/sendChat
      case "/say" | "/s" | "/shout" | "/sho" | "/ooc" | "/auction" | "/auc"
           | "/tell" | "/t" | "/group" | "/g" | "/guild" | "/gu" => false
      case cmd =>
        chatPanel.addLine(s"Unknown command: $cmd", Colors.danger)
        true

  private def handleLoc(): Unit =
    player match
      case Some(pc) =>
        pc.zoneChar match
          case Some(zc) =>
            // ZC position is model-origin; subtract feetOffset for ground-level server coords
            val pos = zc.position
            val (sy, sx, sz) = EqCoords.glToServer(pos.x, pos.y - pc.feetOffset, pos.z)
            chatPanel.addLine(f"Your Location is $sx%.2f, $sy%.2f, $sz%.2f", Colors.textDim)
          case None =>
            chatPanel.addLine("Position unknown", Colors.danger)
      case None =>
        chatPanel.addLine("Position unknown", Colors.danger)

  private def handleTp(args: Array[String]): Unit =
    if args.length != 3 then
      chatPanel.addLine("Usage: /tp x y z", Colors.danger)
      return
    try
      val sx = args(0).toFloat
      val sy = args(1).toFloat
      val sz = args(2).toFloat
      player.foreach { pc =>
        pc.teleportTo(EqCoords.serverToGl(sy, sx, sz))
      }
      Game.zoneSession.foreach { session =>
        val client = session.client
        client.sendPosition(PlayerPosition(
          spawnId = client.mySpawnId,
          y = sy, x = sx, z = sz,
          heading = 0, deltaY = 0, deltaX = 0, deltaZ = 0, deltaHeading = 0,
          animation = 0,
        ))
      }
      chatPanel.addLine(s"Teleported to ($sx, $sy, $sz)", Colors.gold)
    catch case _: NumberFormatException =>
      chatPanel.addLine("Usage: /tp x y z (numbers required)", Colors.danger)

  private def handleSpeedUp(): Unit =
    player.foreach { pc =>
      pc.runSpeed = 100f
      chatPanel.addLine("Speed set to 100", Colors.gold)
    }

  private def handleSlowDown(): Unit =
    player.foreach { pc =>
      pc.runSpeed = 25f
      chatPanel.addLine("Speed set to 25 (default)", Colors.gold)
    }

  private def handleAttackToggle(): Unit =
    player.foreach { pc =>
      val enable = !pc.autoAttacking
      pc.autoAttacking = enable
      Game.zoneSession.foreach(_.client.autoAttack(enable))
      if enable then chatPanel.addLine("Auto-attack on.", Colors.text)
      else chatPanel.addLine("Auto-attack off.", Colors.text)
    }

  private def handleWho(): Unit =
    val myId = Game.zoneSession.map(_.client.mySpawnId).getOrElse(-1)
    val players = characters.values.filter(zc => zc.npcType == 0 && zc.spawnId != myId).toVector.sortBy(_.level)
    if players.isEmpty then
      chatPanel.addLine("No other players in zone.", Colors.textDim)
    else
      chatPanel.addLine(s"Players in zone:", Colors.gold)
      for pc <- players do
        val cls = EqData.className(pc.classId)
        chatPanel.addLine(s"  [${pc.level} $cls] ${pc.displayName}", Colors.text)
      chatPanel.addLine(s"${players.size} player(s) found.", Colors.gold)

  // ===========================================================================
  // Helpers
  // ===========================================================================

  private def spawnName(id: Int): String =
    characters.get(id).map(_.displayName)
      .orElse(player.filter(_ => Game.zoneSession.exists(_.client.mySpawnId == id)).map(_.name))
      .orElse(Game.zoneSession.flatMap(_.client.spawns.get(id)).map(s => ZoneCharacter.cleanName(s.name)))
      .getOrElse(s"#$id")

  private def conLevelColor(targetLevel: Int): (Float, Float, Float, Float) =
    val myLevel = player.map(_.level).getOrElse(1)
    val diff = targetLevel - myLevel
    if diff >= 3 then Colors.danger
    else if diff >= 1 then Colors.gold
    else if diff >= -2 then Colors.text
    else if diff >= -5 then Colors.secondary
    else Colors.textDim
