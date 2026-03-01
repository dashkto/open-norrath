package opennorrath.screen

import imgui.ImGui
import imgui.flag.{ImGuiCol, ImGuiCond, ImGuiKey, ImGuiWindowFlags}
import imgui.`type`.ImString

import org.lwjgl.glfw.GLFW.*
import org.lwjgl.opengl.GL11.*

import opennorrath.Game
import opennorrath.network.{WorldClient, WorldEvent}
import opennorrath.ui.Colors

/** Multi-step character creation screen.
  *
  * Steps: 1) Race & Class → 2) Details (gender, deity) → 3) Name → 4) Create
  */
class CharacterCreateScreen(
  ctx: GameContext,
) extends Screen:

  private def worldClient: WorldClient = Game.worldSession.get.client

  private var step = 1
  private var statusText = ""
  private var statusColor = Colors.textDim

  // Step 1: Race & Class
  private var selectedRace = 0
  private var selectedClass = 0

  // Step 2: Details
  private var selectedGender = 0
  private var selectedDeity = 0

  // Step 3: Name
  private val nameInput = new ImString(64)
  private var nameApproved = false
  private var nameChecking = false
  private var focusName = true

  // Step 4: Creating
  private var creating = false

  // Race/class data
  private val races = Vector(
    RaceInfo(1,   "Human",     Vector(1,2,3,4,5,6,7,8,9,11,12,13,14)),
    RaceInfo(2,   "Barbarian", Vector(1,8,10)),
    RaceInfo(3,   "Erudite",   Vector(2,3,5,11,12,13,14)),
    RaceInfo(4,   "Wood Elf",  Vector(1,4,6,8)),
    RaceInfo(5,   "High Elf",  Vector(2,3,12,13,14)),
    RaceInfo(6,   "Dark Elf",  Vector(1,2,5,9,11,12,13,14)),
    RaceInfo(7,   "Half Elf",  Vector(1,2,4,6,8,9)),
    RaceInfo(8,   "Dwarf",     Vector(1,2,3,8,9)),
    RaceInfo(9,   "Troll",     Vector(1,5,10)),
    RaceInfo(10,  "Ogre",      Vector(1,5)),
    RaceInfo(11,  "Halfling",  Vector(1,2,4,6,8,9)),
    RaceInfo(12,  "Gnome",     Vector(1,2,3,5,7,9,11,12,13,14)),
    RaceInfo(128, "Iksar",     Vector(1,5,7,10,11)),
    RaceInfo(130, "Vah Shir",  Vector(1,8,15)),
  )

  private val classNames = Map(
    1 -> "Warrior", 2 -> "Cleric", 3 -> "Paladin", 4 -> "Ranger",
    5 -> "Shadow Knight", 6 -> "Druid", 7 -> "Monk", 8 -> "Bard",
    9 -> "Rogue", 10 -> "Shaman", 11 -> "Necromancer", 12 -> "Wizard",
    13 -> "Magician", 14 -> "Enchanter", 15 -> "Beastlord",
  )

  private val genders = Vector("Male", "Female")

  // All deities — filtered dynamically by valid combos for selected (race, class)
  private val allDeities = Map(
    396 -> "Agnostic", 201 -> "Bertoxxulous", 202 -> "Brell Serilis",
    203 -> "Cazic Thule", 204 -> "Cazic Thule", 205 -> "Erollisi Marr",
    206 -> "Bristlebane", 207 -> "Innoruuk", 208 -> "Karana",
    209 -> "Mithaniel Marr", 210 -> "Prexus", 211 -> "Quellious",
    212 -> "Rallos Zek", 213 -> "Rodcet Nife", 214 -> "Solusek Ro",
    215 -> "The Tribunal", 216 -> "Tunare", 217 -> "Veeshan",
  )

  // Valid (race, class, deity) → start_zone from server's char_create_combinations table
  private val charCreateCombos: Map[(Int,Int,Int), Int] = Map(
    (1,1,201)->45, (1,1,204)->10, (1,1,206)->10, (1,1,207)->1, (1,1,208)->10,
    (1,1,211)->1, (1,1,212)->1, (1,1,396)->1,
    (1,2,201)->45, (1,2,204)->9, (1,2,206)->10, (1,2,207)->1, (1,2,208)->9, (1,2,212)->2,
    (1,3,204)->9, (1,3,207)->1, (1,3,208)->9, (1,3,212)->2,
    (1,4,207)->3, (1,4,215)->3,
    (1,5,201)->45, (1,5,206)->10,
    (1,6,207)->3, (1,6,215)->3,
    (1,7,210)->9, (1,7,396)->2,
    (1,8,202)->1, (1,8,204)->10, (1,8,205)->1, (1,8,207)->1, (1,8,208)->10,
    (1,8,209)->1, (1,8,210)->1, (1,8,211)->1, (1,8,212)->1, (1,8,213)->1,
    (1,8,214)->1, (1,8,215)->1, (1,8,216)->1, (1,8,396)->1,
    (1,9,201)->2, (1,9,204)->10, (1,9,205)->10, (1,9,206)->10,
    (1,9,207)->2, (1,9,212)->2, (1,9,396)->10,
    (1,11,201)->45, (1,11,206)->10,
    (1,12,201)->45, (1,12,204)->9, (1,12,206)->10, (1,12,207)->1,
    (1,12,208)->9, (1,12,212)->1, (1,12,213)->1, (1,12,396)->1,
    (1,13,201)->45, (1,13,204)->9, (1,13,206)->10, (1,13,207)->1,
    (1,13,208)->9, (1,13,212)->1, (1,13,396)->1,
    (1,14,201)->45, (1,14,204)->9, (1,14,206)->10, (1,14,207)->1,
    (1,14,208)->9, (1,14,212)->1, (1,14,396)->1,
    (2,1,211)->29, (2,1,214)->29, (2,1,396)->29,
    (2,9,205)->29, (2,9,214)->29, (2,9,396)->29,
    (2,10,214)->29,
    (2,15,208)->29, (2,15,214)->29,
    (3,2,203)->75, (3,2,209)->24, (3,2,210)->24,
    (3,3,209)->24, (3,3,210)->24,
    (3,5,203)->75, (3,11,203)->75,
    (3,12,209)->23, (3,12,210)->23, (3,12,213)->23, (3,12,396)->23,
    (3,13,209)->23, (3,13,210)->23, (3,13,396)->23,
    (3,14,209)->23, (3,14,210)->23, (3,14,396)->23,
    (4,1,207)->54, (4,1,211)->54, (4,1,215)->54, (4,1,396)->54,
    (4,4,215)->54, (4,6,215)->54,
    (4,8,202)->54, (4,8,204)->54, (4,8,205)->54, (4,8,207)->54, (4,8,208)->54,
    (4,8,209)->54, (4,8,210)->54, (4,8,211)->54, (4,8,212)->54, (4,8,213)->54,
    (4,8,214)->54, (4,8,215)->54, (4,8,216)->54, (4,8,396)->54,
    (4,9,205)->54, (4,9,207)->54, (4,9,215)->54, (4,9,396)->54,
    (5,2,215)->61, (5,3,215)->61,
    (5,12,204)->62, (5,12,207)->62, (5,12,208)->62, (5,12,213)->62,
    (5,12,215)->62, (5,12,396)->62,
    (5,13,204)->62, (5,13,207)->62, (5,13,208)->62, (5,13,215)->62, (5,13,396)->62,
    (5,14,204)->62, (5,14,207)->62, (5,14,208)->62, (5,14,215)->62, (5,14,396)->62,
    (6,1,206)->41, (6,1,211)->41, (6,1,396)->41,
    (6,2,206)->42, (6,5,206)->42,
    (6,9,205)->42, (6,9,206)->42, (6,9,396)->42,
    (6,11,206)->42,
    (6,12,206)->41, (6,12,213)->41, (6,12,396)->41,
    (6,13,206)->41, (6,13,396)->41,
    (6,14,206)->41, (6,14,396)->41,
    (7,1,201)->45, (7,1,204)->10, (7,1,206)->10, (7,1,207)->1, (7,1,208)->10,
    (7,1,209)->1, (7,1,211)->1, (7,1,212)->1, (7,1,214)->1, (7,1,215)->54, (7,1,396)->1,
    (7,3,204)->9, (7,3,207)->1, (7,3,208)->9, (7,3,212)->2, (7,3,215)->61,
    (7,4,207)->3, (7,4,215)->3,
    (7,6,207)->3, (7,6,215)->3,
    (7,8,202)->1, (7,8,204)->10, (7,8,205)->1, (7,8,207)->1, (7,8,208)->10,
    (7,8,209)->1, (7,8,210)->1, (7,8,211)->1, (7,8,212)->1, (7,8,213)->1,
    (7,8,214)->1, (7,8,215)->1, (7,8,216)->1, (7,8,396)->1,
    (7,9,201)->2, (7,9,204)->10, (7,9,205)->10, (7,9,207)->2,
    (7,9,212)->2, (7,9,215)->54, (7,9,396)->10,
    (8,1,202)->60, (8,1,396)->60,
    (8,2,202)->67, (8,3,202)->67,
    (8,9,202)->67, (8,9,205)->67, (8,9,396)->67,
    (9,1,203)->52, (9,1,206)->52, (9,1,211)->52, (9,1,396)->52,
    (9,5,203)->52, (9,5,206)->52,
    (9,10,203)->52, (9,10,206)->52,
    (9,15,203)->52, (9,15,206)->52,
    (10,1,203)->49, (10,1,211)->49, (10,1,396)->49,
    (10,5,203)->49, (10,5,211)->49,
    (10,10,211)->49, (10,15,211)->49,
    (11,1,202)->19, (11,1,211)->19, (11,1,396)->19,
    (11,2,205)->19, (11,3,207)->19, (11,4,207)->19, (11,6,207)->19,
    (11,9,202)->19, (11,9,205)->19, (11,9,396)->19,
    (12,1,201)->55, (12,1,202)->55, (12,1,211)->55, (12,1,396)->55,
    (12,2,201)->55, (12,2,202)->55, (12,2,205)->55,
    (12,3,202)->55,
    (12,5,201)->55,
    (12,9,201)->55, (12,9,202)->55, (12,9,205)->55, (12,9,396)->55,
    (12,11,201)->55,
    (12,12,201)->55, (12,12,202)->55, (12,12,213)->55, (12,12,396)->55,
    (12,13,201)->55, (12,13,202)->55, (12,13,396)->55,
    (12,14,201)->55, (12,14,202)->55, (12,14,396)->55,
    (128,1,203)->106, (128,5,203)->106, (128,7,203)->106,
    (128,10,203)->106, (128,11,203)->394, (128,15,203)->106,
    (130,1,396)->155, (130,8,396)->155, (130,9,396)->155,
    (130,10,396)->155, (130,15,396)->155,
  )

  // Base stats per (race, class) from server's char_create_point_allocations table.
  // Values: (STR, STA, CHA, DEX, INT, AGI, WIS)
  private val baseStats: Map[(Int,Int), (Int,Int,Int,Int,Int,Int,Int)] = Map(
    (1,1)   -> (85,85,75,75,75,80,75),    // Human Warrior
    (1,2)   -> (80,80,75,75,75,75,85),    // Human Cleric
    (1,3)   -> (85,80,85,75,75,75,80),    // Human Paladin
    (1,4)   -> (80,85,75,75,75,85,80),    // Human Ranger
    (1,5)   -> (85,80,80,75,85,75,75),    // Human Shadow Knight
    (1,6)   -> (75,85,75,75,75,75,85),    // Human Druid
    (1,7)   -> (80,80,75,85,75,85,75),    // Human Monk
    (1,8)   -> (80,75,85,85,75,75,75),    // Human Bard
    (1,9)   -> (75,75,75,85,75,85,75),    // Human Rogue
    (1,11)  -> (75,75,75,85,85,75,75),    // Human Necromancer
    (1,12)  -> (75,85,75,75,85,75,75),    // Human Wizard
    (1,13)  -> (75,85,75,75,85,75,75),    // Human Magician
    (1,14)  -> (75,75,85,75,85,75,75),    // Human Enchanter
    (2,1)   -> (113,105,55,70,60,87,70),  // Barbarian Warrior
    (2,9)   -> (103,95,55,80,60,92,70),   // Barbarian Rogue
    (2,10)  -> (103,100,60,70,60,82,80),  // Barbarian Shaman
    (2,15)  -> (103,105,60,70,60,87,80),  // Barbarian Beastlord
    (3,2)   -> (65,75,70,70,107,70,93),   // Erudite Cleric
    (3,3)   -> (70,75,80,70,107,70,88),   // Erudite Paladin
    (3,5)   -> (70,75,75,70,117,70,83),   // Erudite Shadow Knight
    (3,11)  -> (60,70,70,80,117,70,83),   // Erudite Necromancer
    (3,12)  -> (60,80,70,70,117,70,83),   // Erudite Wizard
    (3,13)  -> (60,80,70,70,117,70,83),   // Erudite Magician
    (3,14)  -> (60,70,80,70,117,70,83),   // Erudite Enchanter
    (4,1)   -> (75,75,75,80,75,100,80),   // Wood Elf Warrior
    (4,4)   -> (70,75,75,80,75,105,85),   // Wood Elf Ranger
    (4,6)   -> (65,75,75,80,75,95,90),    // Wood Elf Druid
    (4,8)   -> (70,65,85,90,75,95,80),    // Wood Elf Bard
    (4,9)   -> (65,65,75,90,75,105,80),   // Wood Elf Rogue
    (5,2)   -> (60,70,80,70,92,85,105),   // High Elf Cleric
    (5,3)   -> (65,70,90,70,92,85,100),   // High Elf Paladin
    (5,12)  -> (55,75,80,70,102,85,95),   // High Elf Wizard
    (5,13)  -> (55,75,80,70,102,85,95),   // High Elf Magician
    (5,14)  -> (55,65,90,70,102,85,95),   // High Elf Enchanter
    (6,1)   -> (70,75,60,75,99,95,83),    // Dark Elf Warrior
    (6,2)   -> (65,70,60,75,99,90,93),    // Dark Elf Cleric
    (6,5)   -> (70,70,65,75,109,90,83),   // Dark Elf Shadow Knight
    (6,9)   -> (60,65,60,85,99,100,83),   // Dark Elf Rogue
    (6,11)  -> (60,65,60,85,109,90,83),   // Dark Elf Necromancer
    (6,12)  -> (60,75,60,75,109,90,83),   // Dark Elf Wizard
    (6,13)  -> (60,75,60,75,109,90,83),   // Dark Elf Magician
    (6,14)  -> (60,65,70,75,109,90,83),   // Dark Elf Enchanter
    (7,1)   -> (80,80,75,85,75,95,60),    // Half Elf Warrior
    (7,3)   -> (80,75,85,85,75,90,65),    // Half Elf Paladin
    (7,4)   -> (75,80,75,85,75,100,65),   // Half Elf Ranger
    (7,6)   -> (70,80,75,85,75,90,70),    // Half Elf Druid
    (7,8)   -> (75,70,85,95,75,90,60),    // Half Elf Bard
    (7,9)   -> (70,70,75,95,75,100,60),   // Half Elf Rogue
    (8,1)   -> (100,100,45,90,60,75,83),  // Dwarf Warrior
    (8,2)   -> (95,95,45,90,60,70,93),    // Dwarf Cleric
    (8,3)   -> (100,95,55,90,60,70,88),   // Dwarf Paladin
    (8,9)   -> (90,90,45,100,60,80,83),   // Dwarf Rogue
    (9,1)   -> (118,119,40,75,52,88,60),  // Troll Warrior
    (9,5)   -> (118,114,45,75,62,83,60),  // Troll Shadow Knight
    (9,10)  -> (108,114,45,75,52,83,70),  // Troll Shaman
    (9,15)  -> (108,119,45,75,52,88,70),  // Troll Beastlord
    (10,1)  -> (140,132,37,70,60,75,67),  // Ogre Warrior
    (10,5)  -> (140,127,42,70,70,70,67),  // Ogre Shadow Knight
    (10,10) -> (130,127,42,70,60,70,77),  // Ogre Shaman
    (10,15) -> (130,132,42,70,60,75,77),  // Ogre Beastlord
    (11,1)  -> (80,85,50,90,67,100,80),   // Halfling Warrior
    (11,2)  -> (75,80,50,90,67,95,90),    // Halfling Cleric
    (11,3)  -> (80,80,60,90,67,95,85),    // Halfling Paladin
    (11,4)  -> (75,85,50,90,67,105,85),   // Halfling Ranger
    (11,6)  -> (70,85,50,90,67,95,90),    // Halfling Druid
    (11,9)  -> (70,75,50,100,67,105,80),  // Halfling Rogue
    (12,1)  -> (70,80,60,85,98,90,67),    // Gnome Warrior
    (12,2)  -> (65,75,60,85,98,85,77),    // Gnome Cleric
    (12,3)  -> (70,75,70,85,98,85,72),    // Gnome Paladin
    (12,5)  -> (70,75,65,85,108,85,67),   // Gnome Shadow Knight
    (12,9)  -> (60,70,60,95,98,95,67),    // Gnome Rogue
    (12,11) -> (60,70,60,95,108,85,67),   // Gnome Necromancer
    (12,12) -> (60,80,60,85,108,85,67),   // Gnome Wizard
    (12,13) -> (60,80,60,85,108,85,67),   // Gnome Magician
    (12,14) -> (60,70,70,85,108,85,67),   // Gnome Enchanter
    (128,1) -> (80,80,55,85,75,95,80),    // Iksar Warrior
    (128,5) -> (80,75,60,85,85,90,80),    // Iksar Shadow Knight
    (128,7) -> (75,75,55,95,75,100,80),   // Iksar Monk
    (128,10)-> (70,75,60,85,75,90,90),    // Iksar Shaman
    (128,11)-> (70,70,55,95,85,90,80),    // Iksar Necromancer
    (128,15)-> (70,80,60,85,75,95,90),    // Iksar Beastlord
    (130,1) -> (100,85,65,70,65,95,70),   // Vah Shir Warrior
    (130,8) -> (95,75,75,80,65,90,70),    // Vah Shir Bard
    (130,9) -> (90,75,65,80,65,100,70),   // Vah Shir Rogue
    (130,10)-> (90,80,70,70,65,90,80),    // Vah Shir Shaman
    (130,15)-> (90,85,70,70,65,95,80),    // Vah Shir Beastlord
  )

  override def show(): Unit =
    glfwSetInputMode(ctx.window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    glClearColor(0.08f, 0.08f, 0.12f, 1f)

  override def update(dt: Float): Unit =
    // Escape: go back a step or exit
    if ImGui.isKeyPressed(ImGuiKey.Escape) then
      if step > 1 then
        step -= 1
        if step == 3 then
          nameApproved = false
          nameChecking = false
        statusText = ""
      else
        Game.setScreen(CharacterSelectScreen(ctx, worldClient.characters))
        return

    // Poll world events
    var event = worldClient.pollEvent()
    while event.isDefined do
      event.get match
        case WorldEvent.NameApproved(approved) =>
          nameChecking = false
          nameApproved = approved
          statusText = if approved then "Name approved!" else "Name rejected - try another"
          statusColor = if approved then Colors.success else Colors.error
        case WorldEvent.CharacterList(chars) =>
          if creating then
            creating = false
            Game.setScreen(CharacterSelectScreen(ctx, chars))
            return
        case WorldEvent.Error(msg) =>
          statusText = msg
          statusColor = Colors.error
          creating = false
          nameChecking = false
        case _ => ()
      event = worldClient.pollEvent()

  override def render(dt: Float): Unit =
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val w = ctx.windowWidth.toFloat
    val h = ctx.windowHeight.toFloat
    val flags = ImGuiWindowFlags.NoTitleBar | ImGuiWindowFlags.NoResize |
      ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoScrollbar

    ImGui.setNextWindowPos(0f, 0f, ImGuiCond.Always)
    ImGui.setNextWindowSize(w, h, ImGuiCond.Always)
    ImGui.begin("##charcreate", flags)

    // Title
    val titleText = s"CREATE CHARACTER - Step $step of 4"
    val titleW = ImGui.calcTextSize(titleText).x
    ImGui.setCursorPos((w - titleW) / 2f, 60f)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text(titleText)
    ImGui.popStyleColor()

    step match
      case 1 => renderRaceClass(w, h)
      case 2 => renderDetails(w, h)
      case 3 => renderName(w, h)
      case 4 => renderCreate(w, h)
      case _ =>

    // Status bar
    if statusText.nonEmpty then
      val statusW = ImGui.calcTextSize(statusText).x
      ImGui.setCursorPos((w - statusW) / 2f, h - 65f)
      pushColor(ImGuiCol.Text, statusColor)
      ImGui.text(statusText)
      ImGui.popStyleColor()

    ImGui.end()

  override def dispose(): Unit = ()

  // --- Step 1: Race & Class ---
  private def renderRaceClass(w: Float, h: Float): Unit =
    val colW = 200f
    val gap = 60f
    val startX = (w - colW * 2 - gap) / 2f
    val startY = 120f

    // Race list
    ImGui.setCursorPos(startX, startY)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Race")
    ImGui.popStyleColor()

    for (race, i) <- races.zipWithIndex do
      val y = startY + 30f + i * 28f
      ImGui.setCursorPos(startX, y)
      val selected = i == selectedRace
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  ${race.name}", selected, 0, colW, 0f) then
        selectedRace = i
        selectedDeity = 0
        // Reset class selection if current class not valid for new race
        val validClasses = races(i).classes
        if !validClasses.contains(availableClasses(selectedClass)._1) then
          selectedClass = 0
      if selected then ImGui.popStyleColor()

    // Class list (filtered by selected race)
    val classX = startX + colW + gap
    ImGui.setCursorPos(classX, startY)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Class")
    ImGui.popStyleColor()

    val classes = availableClasses
    for (cls, i) <- classes.zipWithIndex do
      val y = startY + 30f + i * 28f
      ImGui.setCursorPos(classX, y)
      val selected = i == selectedClass
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  ${cls._2}", selected, 0, colW, 0f) then
        selectedClass = i
        selectedDeity = 0
      if selected then ImGui.popStyleColor()

    // Stats preview
    val statsY = startY + 30f + Math.max(races.size, classes.size) * 28f + 20f
    val (str, sta, cha, dex, int_, agi, wis) = computeStats
    ImGui.setCursorPos(startX, statsY)
    pushColor(ImGuiCol.Text, Colors.textDim)
    ImGui.text(s"STR $str  STA $sta  CHA $cha  DEX $dex  INT $int_  AGI $agi  WIS $wis")
    ImGui.popStyleColor()

    // Next button
    renderNextButton(w, h, "Next: Details")

  // --- Step 2: Details ---
  private def renderDetails(w: Float, h: Float): Unit =
    val fieldW = 300f
    val fieldX = (w - fieldW) / 2f

    // Scrollable content area above the button
    ImGui.setCursorPos(0f, 100f)
    ImGui.beginChild("##details-scroll", w, h - 220f, false, 0)

    val innerX = fieldX
    ImGui.setCursorPosX(innerX)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Gender")
    ImGui.popStyleColor()
    for (g, i) <- genders.zipWithIndex do
      ImGui.setCursorPosX(innerX)
      val selected = i == selectedGender
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  $g##gender$i", selected, 0, fieldW, 0f) then
        selectedGender = i
      if selected then ImGui.popStyleColor()

    ImGui.spacing(); ImGui.spacing()
    val validDeities = availableDeities
    ImGui.setCursorPosX(innerX)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Deity")
    ImGui.popStyleColor()
    for ((deityId, deityName), i) <- validDeities.zipWithIndex do
      ImGui.setCursorPosX(innerX)
      val selected = i == selectedDeity
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  $deityName##deity$i", selected, 0, fieldW, 0f) then
        selectedDeity = i
      if selected then ImGui.popStyleColor()

    ImGui.endChild()

    renderNextButton(w, h, "Next: Name")

  // --- Step 3: Name ---
  private def renderName(w: Float, h: Float): Unit =
    val fieldW = 400f
    val fieldX = (w - fieldW) / 2f

    ImGui.setCursorPos(fieldX, 200f)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Character Name")
    ImGui.popStyleColor()

    ImGui.setCursorPosX(fieldX)
    ImGui.pushItemWidth(fieldW)
    if focusName then
      ImGui.setKeyboardFocusHere()
      focusName = false
    if ImGui.inputText("##charname", nameInput) then
      nameApproved = false // reset on change

    ImGui.popItemWidth()

    // Check Name button
    val checkW = 140f
    ImGui.setCursorPos((w - checkW) / 2f, 290f)
    pushColor(ImGuiCol.Button, Colors.secondary)
    pushColor(ImGuiCol.ButtonHovered, Colors.secondary2)
    pushColor(ImGuiCol.Text, Colors.hex("1A1A1A"))
    val checkLabel = if nameChecking then "Checking..." else "Check Name"
    if ImGui.button(checkLabel, checkW, 36f) && !nameChecking then
      val name = nameInput.get().trim
      if name.nonEmpty then
        nameChecking = true
        nameApproved = false
        statusText = "Checking name..."
        statusColor = Colors.textDim
        val race = races(selectedRace)
        val cls = availableClasses
        worldClient.approveName(name, race.id, cls(selectedClass)._1)

    ImGui.popStyleColor(3)

    // Enter to check name or advance
    if ImGui.isKeyPressed(ImGuiKey.Enter) then
      if nameApproved then
        step = 4
        statusText = ""
      else if !nameChecking then
        val name = nameInput.get().trim
        if name.nonEmpty then
          nameChecking = true
          nameApproved = false
          statusText = "Checking name..."
          statusColor = Colors.textDim
          val race = races(selectedRace)
          val cls = availableClasses
          worldClient.approveName(name, race.id, cls(selectedClass)._1)

    if nameApproved then
      renderNextButton(w, h, "Next: Confirm")

  // --- Step 4: Create ---
  private def renderCreate(w: Float, h: Float): Unit =
    val fieldX = (w - 400f) / 2f
    var y = 140f

    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val name = nameInput.get().trim
    val (str, sta, cha, dex, int_, agi, wis) = computeStats

    // Summary
    val lines = Vector(
      s"Name: $name",
      s"Race: ${race.name}  Class: ${cls._2}",
      s"Gender: ${genders(selectedGender)}",
      s"Deity: ${availableDeities.lift(selectedDeity).map(_._2).getOrElse("Unknown")}",
      s"Stats: STR $str  STA $sta  CHA $cha  DEX $dex  INT $int_  AGI $agi  WIS $wis",
    )
    for line <- lines do
      ImGui.setCursorPos(fieldX, y)
      pushColor(ImGuiCol.Text, Colors.text)
      ImGui.text(line)
      ImGui.popStyleColor()
      y += 30f

    // Create button
    val buttonW = 160f
    ImGui.setCursorPos((w - buttonW) / 2f, y + 40f)
    pushColor(ImGuiCol.Button, Colors.primary)
    pushColor(ImGuiCol.ButtonHovered, Colors.primary2)
    pushColor(ImGuiCol.ButtonActive, Colors.hex("C06820"))
    pushColor(ImGuiCol.Text, Colors.hex("1A1A1A"))
    val label = if creating then "Creating..." else "Create Character"
    if ImGui.button(label, buttonW, 40f) && !creating then
      doCreate()
    ImGui.popStyleColor(4)

    if ImGui.isKeyPressed(ImGuiKey.Enter) && !creating then
      doCreate()

  private def doCreate(): Unit =
    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val deityList = availableDeities
    val deityId = deityList.lift(selectedDeity).map(_._1).getOrElse(396)
    charCreateCombos.get((race.id, cls._1, deityId)) match
      case None =>
        statusText = "Invalid race/class/deity combination"
        statusColor = Colors.error
      case Some(zoneId) =>
        creating = true
        statusText = "Creating character..."
        statusColor = Colors.textDim
        val (str, sta, cha, dex, int_, agi, wis) = computeStats
        worldClient.createCharacter(
          name = nameInput.get().trim,
          gender = selectedGender,
          race = race.id,
          classId = cls._1,
          str = str, sta = sta, cha = cha, dex = dex, int_ = int_, agi = agi, wis = wis,
          startZone = zoneId,
          deity = deityId,
        )

  // --- Helpers ---
  private def availableClasses: Vector[(Int, String)] =
    val race = races(selectedRace)
    race.classes.map(id => (id, classNames.getOrElse(id, s"Class($id)")))

  // Distributable bonus points per class. The server validates that total stats equal
  // base + bonus. In the original game the player distributes these manually.
  // TODO: add a stat point allocation UI; for now we dump all bonus into STR.
  private val classBonusPoints: Map[Int, Int] = Map(
    1 -> 25, 2 -> 30, 3 -> 20, 4 -> 20, 5 -> 20, 6 -> 30, 7 -> 20, 8 -> 25,
    9 -> 30, 10 -> 30, 11 -> 30, 12 -> 30, 13 -> 30, 14 -> 30, 15 -> 20, 16 -> 25,
  )

  private def computeStats: (Int, Int, Int, Int, Int, Int, Int) =
    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val (str, sta, cha, dex, int_, agi, wis) =
      baseStats.getOrElse((race.id, cls._1), (75, 75, 75, 75, 75, 75, 75))
    val bonus = classBonusPoints.getOrElse(cls._1, 25)
    (str + bonus, sta, cha, dex, int_, agi, wis)

  private def availableDeities: Vector[(Int, String)] =
    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val validDeityIds = charCreateCombos.keys
      .filter(k => k._1 == race.id && k._2 == cls._1)
      .map(_._3).toSet
    allDeities.filter((id, _) => validDeityIds.contains(id))
      .toVector.sortBy((id, _) => if id == 396 then 0 else id) // Agnostic first

  private def renderNextButton(w: Float, h: Float, label: String): Unit =
    val buttonW = 160f
    ImGui.setCursorPos((w - buttonW) / 2f, h - 110f)
    pushColor(ImGuiCol.Button, Colors.primary)
    pushColor(ImGuiCol.ButtonHovered, Colors.primary2)
    pushColor(ImGuiCol.ButtonActive, Colors.hex("C06820"))
    pushColor(ImGuiCol.Text, Colors.hex("1A1A1A"))
    if ImGui.button(label, buttonW, 36f) then
      advanceStep()
    ImGui.popStyleColor(4)

  private def advanceStep(): Unit =
    step match
      case 1 =>
        step = 2
        statusText = ""
      case 2 =>
        step = 3
        focusName = true
        nameApproved = false
        statusText = ""
      case 3 if nameApproved =>
        step = 4
        statusText = ""
      case _ => ()

  private def pushColor(idx: Int, c: (Float, Float, Float, Float)): Unit =
    ImGui.pushStyleColor(idx, c._1, c._2, c._3, c._4)

private case class RaceInfo(id: Int, name: String, classes: Vector[Int])
