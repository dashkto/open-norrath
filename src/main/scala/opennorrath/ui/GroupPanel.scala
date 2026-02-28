package opennorrath.ui

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiWindowFlags}

import opennorrath.state.ZoneCharacter

/** Displays party/group members with name, level, class, and HP bar.
  * Always visible — shows "No Group" when solo.
  * Also displays pending group invites with Accept / Ignore buttons.
  */
class GroupPanel(characters: scala.collection.Map[Int, ZoneCharacter]) extends Panel:

  val title = "Group"
  val defaultX = 10f
  val defaultY = 110f
  val defaultWidth = Spacing.panelWidthNarrow
  val defaultHeight = 200f
  override def extraFlags: Int =
    ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize | ImGuiWindowFlags.NoScrollbar

  private val MaxGroupSize = 6

  var members: Vector[String] = Vector.empty
  var leader: String = ""
  var playerName: String = ""
  var target: () => Option[ZoneCharacter] = () => None
  opacity = 0.5f

  // Pending invite state
  private var pendingInviter: Option[String] = None
  var onAcceptInvite: String => Unit = _ => ()
  var onDeclineInvite: String => Unit = _ => ()
  var onInvitePlayer: String => Unit = _ => ()
  var onDisband: () => Unit = () => ()

  /** Show a group invite from the given player. */
  def showInvite(inviterName: String): Unit =
    pendingInviter = Some(inviterName)

  /** Clear pending invite (e.g. when we join the group or zone changes). */
  def clearInvite(): Unit =
    pendingInviter = None

  override protected def renderContent(): Unit =
    // Pending invite banner
    pendingInviter.foreach { inviter =>
      pushColor(ImGuiCol.Text, Colors.gold)
      ImGui.text(s"$inviter invites you")
      ImGui.popStyleColor()
      if ImGui.button("Accept", 80f, Spacing.buttonHeight) then
        onAcceptInvite(inviter)
        pendingInviter = None
      ImGui.sameLine()
      if ImGui.button("Ignore", 80f, Spacing.buttonHeight) then
        onDeclineInvite(inviter)
        pendingInviter = None
      ImGui.separator()
    }

    if members.isEmpty then
      pushColor(ImGuiCol.Text, Colors.textDim)
      ImGui.text("No Group")
      ImGui.popStyleColor()
    else
      for name <- members do
        val zc = findByName(name)
        // Name (bold, gold for leader)
        ImGui.pushFont(Fonts.defaultBold)
        val nameColor = if name == leader then Colors.gold else Colors.secondary
        pushColor(ImGuiCol.Text, nameColor)
        ImGui.text(name)
        ImGui.popStyleColor()
        ImGui.popFont()

        zc match
          case Some(c) =>
            // "Lv50 CLR" on same line
            ImGui.sameLine()
            pushColor(ImGuiCol.Text, Colors.textDim)
            ImGui.text(s"Lv${c.level} ${EqData.classAbbrev(c.classId)}")
            ImGui.popStyleColor()
            // HP bar
            bar(c.hpFraction, Colors.danger, Spacing.barHeightSmall)
          case None =>
            ImGui.sameLine()
            pushColor(ImGuiCol.Text, Colors.textDim)
            ImGui.text("(not in zone)")
            ImGui.popStyleColor()

        ImGui.spacing()

    // Disband button: always shown when in a group
    if members.nonEmpty then
      ImGui.separator()
      if ImGui.button("Disband", -1f, Spacing.buttonHeight) then
        onDisband()

    // "Invite" button: shown when targeting a PC and we can invite
    // (not in a group, or we're the leader with room)
    renderInviteButton()

  /** Show an "Invite" button when we have a player targeted and can invite. */
  private def renderInviteButton(): Unit =
    val canInvite = members.isEmpty || (leader.equalsIgnoreCase(playerName) && members.size < MaxGroupSize)
    if !canInvite then return
    target() match
      case Some(zc) if zc.npcType == 0 && !zc.name.equalsIgnoreCase(playerName) =>
        // Don't show if target is already in our group
        if members.exists(_.equalsIgnoreCase(zc.name)) then return
        ImGui.separator()
        if ImGui.button(s"Invite ${zc.displayName}", -1f, Spacing.buttonHeight) then
          onInvitePlayer(zc.name)
      case _ => ()

  private def findByName(name: String): Option[ZoneCharacter] =
    characters.values.find(_.name.equalsIgnoreCase(name))
