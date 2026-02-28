package opennorrath.ui

import opennorrath.Game
import opennorrath.network.PlayerPosition
import opennorrath.state.{GameClock, PlayerCharacter, ZoneCharacter}
import opennorrath.world.{EqCoords, Physics}

/** Registry of client console commands (slash commands like /loc, /camp, /who).
  *
  * Each command has a name (the part after the slash, no spaces), optional aliases,
  * a usage string, and a handler that receives the execution context and arguments.
  * Commands are defined alongside their handlers for easy navigation.
  */
object ClientCommands:

  /** Context available to all command handlers. */
  case class Context(
    chatPanel: TextPanel,
    characters: scala.collection.Map[Int, ZoneCharacter],
    player: Option[PlayerCharacter],
  ):
    /** Convenience: print a line to the chat panel. */
    def print(msg: String, color: (Float, Float, Float, Float) = Colors.text): Unit = chatPanel.addLine(msg, color)

  /** A registered client command. */
  case class Command(
    name: String,
    aliases: List[String],
    usage: String,
    description: String,
    handler: (Context, Array[String]) => Unit,
  )

  // =========================================================================
  // Command registry — each entry is followed by its handler method
  // =========================================================================

  val all: Vector[Command] = Vector(
    Command("loc",      Nil,       "/loc",        "Show your current location",    handleLoc),
    Command("tp",       Nil,       "/tp x y z",   "Teleport to coordinates",       handleTp),
    Command("camp",     Nil,       "/camp",       "Log out (30 second timer)",     handleCamp),
    Command("who",      Nil,       "/who [name]",  "List players (server query)",   handleWho),
    Command("attack",   List("a"), "/attack",     "Toggle auto-attack",            handleAttack),
    Command("speedup",  Nil,       "/speedup",    "Increase run speed",            handleSpeedUp),
    Command("slowdown", Nil,       "/slowdown",   "Reset run speed to default",    handleSlowDown),
    Command("help",        Nil,       "/help",        "List available commands",        handleHelp),
    Command("settime",     Nil,       "/settime h m", "Set game time (resets on server sync)", handleSetTime),
    Command("gravitydown", Nil,       "/gravitydown", "Decrease gravity to 30%",       handleGravityDown),
    Command("gravityup",   Nil,       "/gravityup",   "Reset gravity to default",      handleGravityUp),
    Command("jumpup",      Nil,       "/jumpup",      "Increase jump power to 100",    handleJumpUp),
    Command("jumpdown",    Nil,       "/jumpdown",    "Reset jump power to default",   handleJumpDown),
  )

  // --- /loc ----------------------------------------------------------------

  private def handleLoc(ctx: Context, args: Array[String]): Unit =
    ctx.player match
      case Some(pc) =>
        pc.zoneChar match
          case Some(zc) =>
            val pos = zc.position
            val (sy, sx, sz) = EqCoords.glToServer(pos.x, pos.y - pc.feetOffset, pos.z)
            ctx.print(f"Your Location is $sx%.2f, $sy%.2f, $sz%.2f", Colors.textDim)
          case None =>
            ctx.print("Position unknown", Colors.danger)
      case None =>
        ctx.print("Position unknown", Colors.danger)

  // --- /tp x y z -----------------------------------------------------------

  private def handleTp(ctx: Context, args: Array[String]): Unit =
    if args.length != 3 then
      ctx.print("Usage: /tp x y z", Colors.danger)
      return
    try
      val sx = args(0).toFloat
      val sy = args(1).toFloat
      val sz = args(2).toFloat
      ctx.player.foreach { pc =>
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
      ctx.print(s"Teleported to ($sx, $sy, $sz)", Colors.gold)
    catch case _: NumberFormatException =>
      ctx.print("Usage: /tp x y z (numbers required)", Colors.danger)

  // --- /camp ---------------------------------------------------------------

  private def handleCamp(ctx: Context, args: Array[String]): Unit =
    Game.zoneSession.foreach(_.client.camp())
    ctx.print("It will take about 30 seconds to camp.", Colors.gold)

  // --- /who ----------------------------------------------------------------

  private def handleWho(ctx: Context, args: Array[String]): Unit =
    // Send server-side /who request — results come back via OP_WhoAllResponse
    val whom = args.mkString(" ")
    Game.zoneSession match
      case Some(session) =>
        session.client.sendWhoAll(whom)
      case None =>
        ctx.print("Not connected to zone server.", Colors.danger)

  // --- /attack (alias: /a) -------------------------------------------------

  private def handleAttack(ctx: Context, args: Array[String]): Unit =
    ctx.player.foreach { pc =>
      val enable = !pc.autoAttacking
      pc.autoAttacking = enable
      Game.zoneSession.foreach(_.client.autoAttack(enable))
      if enable then ctx.print("Auto-attack on.")
      else ctx.print("Auto-attack off.")
    }

  // --- /speedup ------------------------------------------------------------

  private def handleSpeedUp(ctx: Context, args: Array[String]): Unit =
    ctx.player.foreach { pc =>
      pc.runSpeed = 100f
      ctx.print("Speed set to 100", Colors.gold)
    }

  // --- /slowdown -----------------------------------------------------------

  private def handleSlowDown(ctx: Context, args: Array[String]): Unit =
    ctx.player.foreach { pc =>
      pc.runSpeed = 25f
      ctx.print("Speed set to 25 (default)", Colors.gold)
    }

  // --- /settime h m --------------------------------------------------------

  private def handleSetTime(ctx: Context, args: Array[String]): Unit =
    if args.length != 2 then
      ctx.print("Usage: /settime hour minute (e.g. /settime 6 0 for dawn)", Colors.danger)
      return
    try
      val h = args(0).toInt
      val m = args(1).toInt
      if h < 1 || h > 24 || m < 0 || m > 59 then
        ctx.print("Hour must be 1-24, minute 0-59", Colors.danger)
        return
      GameClock.setTime(h, m)
      val ampm = if h <= 12 then "AM" else "PM"
      val h12 = if h <= 12 then h else h - 12
      ctx.print(f"Game time set to $h12%d:$m%02d $ampm", Colors.gold)
    catch case _: NumberFormatException =>
      ctx.print("Usage: /settime hour minute (numbers required)", Colors.danger)

  // --- /gravitydown --------------------------------------------------------

  private def handleGravityDown(ctx: Context, args: Array[String]): Unit =
    Physics.Gravity = Physics.DefaultGravity * 0.3f
    ctx.print(f"Gravity set to ${Physics.Gravity}%.1f (30%% of default)", Colors.gold)

  // --- /gravityup ----------------------------------------------------------

  private def handleGravityUp(ctx: Context, args: Array[String]): Unit =
    Physics.Gravity = Physics.DefaultGravity
    ctx.print(f"Gravity reset to ${Physics.Gravity}%.1f", Colors.gold)

  // --- /jumpup -------------------------------------------------------------

  private def handleJumpUp(ctx: Context, args: Array[String]): Unit =
    ctx.player.foreach { pc =>
      pc.jumpSpeed = 100f
      ctx.print("Jump power set to 100", Colors.gold)
    }

  // --- /jumpdown -----------------------------------------------------------

  private def handleJumpDown(ctx: Context, args: Array[String]): Unit =
    ctx.player.foreach { pc =>
      pc.jumpSpeed = pc.DefaultJumpSpeed
      ctx.print(s"Jump power reset to ${pc.DefaultJumpSpeed.toInt} (default)", Colors.gold)
    }

  // --- /help ---------------------------------------------------------------

  private def handleHelp(ctx: Context, args: Array[String]): Unit =
    ctx.print("Available commands:", Colors.gold)
    for cmd <- all.sortBy(_.name) do
      val aliasStr = if cmd.aliases.nonEmpty then s" (${cmd.aliases.map("/" + _).mkString(", ")})" else ""
      ctx.print(s"  ${cmd.usage}$aliasStr - ${cmd.description}", Colors.textDim)

  // =========================================================================
  // Lookup and dispatch
  // =========================================================================

  /** Map from command name/alias → Command for fast lookup. */
  private val commandMap: Map[String, Command] =
    val builder = Map.newBuilder[String, Command]
    for cmd <- all do
      builder += cmd.name -> cmd
      for alias <- cmd.aliases do builder += alias -> cmd
    builder.result()

  /** Names of chat channel commands that should NOT be handled here. */
  private val chatCommands = Set("say", "s", "shout", "sho", "ooc", "auction", "auc",
    "tell", "t", "group", "g", "guild", "gu")

  /** Try to execute a slash command. Returns true if handled (or unknown), false if
    * it's a chat channel command that should pass through to the server.
    */
  def tryExecute(input: String, ctx: Context): Boolean =
    if !input.startsWith("/") then return false
    val parts = input.split("\\s+")
    val name = parts(0).substring(1).toLowerCase // strip leading /
    if chatCommands.contains(name) then return false
    commandMap.get(name) match
      case Some(cmd) => cmd.handler(ctx, parts.drop(1)); true
      case None =>
        ctx.print(s"Unknown command: /${name}", Colors.danger)
        true
