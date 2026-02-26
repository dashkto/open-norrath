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
  * Steps: 1) Race & Class → 2) Details (gender, deity, city) → 3) Name → 4) Create
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
  private var selectedCity = 0

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

  // Deity data — filtered by race
  private val deities = Vector(
    DeityInfo(396, "Agnostic"),
    DeityInfo(201, "Bertoxxulous"), DeityInfo(202, "Brell Serilis"),
    DeityInfo(203, "Cazic Thule"), DeityInfo(205, "Erollisi Marr"),
    DeityInfo(206, "Bristlebane"), DeityInfo(207, "Innoruuk"),
    DeityInfo(208, "Karana"), DeityInfo(209, "Mithaniel Marr"),
    DeityInfo(210, "Prexus"), DeityInfo(211, "Quellious"),
    DeityInfo(212, "Rallos Zek"), DeityInfo(213, "Rodcet Nife"),
    DeityInfo(214, "Solusek Ro"), DeityInfo(215, "The Tribunal"),
    DeityInfo(216, "Tunare"), DeityInfo(217, "Veeshan"),
  )

  // Start zones — indexed by ID
  private val startZones = Vector(
    StartZone(0, "Odus"), StartZone(1, "Qeynos"), StartZone(2, "Halas"),
    StartZone(3, "Rivervale"), StartZone(4, "Freeport"), StartZone(5, "Neriak"),
    StartZone(6, "Gukta/Grobb"), StartZone(7, "Ogguk"), StartZone(8, "Kaladim"),
    StartZone(9, "Gfay"), StartZone(10, "Felwithe"), StartZone(11, "Akanon"),
    StartZone(12, "Cabalis"), StartZone(13, "Shar Vahl"),
  )

  // Base stats per race: (STR, STA, CHA, DEX, INT, AGI, WIS)
  private val raceStats: Map[Int, (Int,Int,Int,Int,Int,Int,Int)] = Map(
    1   -> (75, 75, 75, 75, 75, 75, 75),  // Human
    2   -> (113, 80, 55, 70, 60, 82, 70), // Barbarian
    3   -> (60, 70, 70, 70, 107, 70, 83), // Erudite
    4   -> (65, 65, 75, 80, 75, 95, 80),  // Wood Elf
    5   -> (55, 65, 80, 70, 92, 85, 95),  // High Elf
    6   -> (60, 65, 60, 75, 99, 90, 83),  // Dark Elf
    7   -> (70, 70, 75, 85, 75, 90, 60),  // Half Elf
    8   -> (90, 90, 45, 90, 60, 70, 83),  // Dwarf
    9   -> (108, 109, 40, 75, 52, 83, 60),// Troll
    10  -> (130, 122, 37, 70, 60, 70, 67),// Ogre
    11  -> (70, 75, 50, 90, 67, 95, 80),  // Halfling
    12  -> (60, 70, 60, 85, 98, 85, 67),  // Gnome
    128 -> (70, 70, 55, 70, 75, 90, 80),  // Iksar
    130 -> (90, 75, 65, 70, 65, 90, 70),  // Vah Shir
  )

  // Class stat bonuses: (STR, STA, CHA, DEX, INT, AGI, WIS)
  private val classStats: Map[Int, (Int,Int,Int,Int,Int,Int,Int)] = Map(
    1  -> (10, 10, 0, 0, 0, 5, 0),   // Warrior
    2  -> (5, 5, 0, 0, 0, 0, 10),    // Cleric
    3  -> (10, 5, 10, 0, 0, 0, 5),   // Paladin
    4  -> (5, 10, 0, 0, 0, 10, 5),   // Ranger
    5  -> (10, 10, 5, 0, 10, 0, 0),  // Shadow Knight
    6  -> (0, 5, 0, 0, 0, 0, 10),    // Druid
    7  -> (5, 5, 0, 10, 0, 10, 0),   // Monk
    8  -> (5, 0, 10, 0, 0, 0, 0),    // Bard
    9  -> (0, 0, 0, 10, 0, 10, 0),   // Rogue
    10 -> (0, 5, 0, 0, 0, 0, 10),    // Shaman
    11 -> (0, 0, 0, 10, 10, 0, 0),   // Necromancer
    12 -> (0, 10, 0, 0, 10, 0, 0),   // Wizard
    13 -> (0, 10, 0, 0, 10, 0, 0),   // Magician
    14 -> (0, 0, 10, 0, 10, 0, 0),   // Enchanter
    15 -> (0, 10, 0, 0, 0, 5, 10),   // Beastlord
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
    ImGui.setCursorPosX(innerX)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Deity")
    ImGui.popStyleColor()
    for (deity, i) <- deities.zipWithIndex do
      ImGui.setCursorPosX(innerX)
      val selected = i == selectedDeity
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  ${deity.name}##deity$i", selected, 0, fieldW, 0f) then
        selectedDeity = i
      if selected then ImGui.popStyleColor()

    ImGui.spacing(); ImGui.spacing()
    val validCities = startZonesForRace
    ImGui.setCursorPosX(innerX)
    pushColor(ImGuiCol.Text, Colors.cream)
    ImGui.text("Starting City")
    ImGui.popStyleColor()
    for (city, i) <- validCities.zipWithIndex do
      ImGui.setCursorPosX(innerX)
      val selected = i == selectedCity
      if selected then pushColor(ImGuiCol.Text, Colors.gold)
      if ImGui.selectable(s"  ${city.name}##city$i", selected, 0, fieldW, 0f) then
        selectedCity = i
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
      s"Deity: ${deities(selectedDeity).name}",
      s"City: ${startZonesForRace.lift(selectedCity).map(_.name).getOrElse("Default")}",
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
    creating = true
    statusText = "Creating character..."
    statusColor = Colors.textDim
    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val (str, sta, cha, dex, int_, agi, wis) = computeStats
    val cities = startZonesForRace
    val cityId = cities.lift(selectedCity).map(_.id).getOrElse(0)
    worldClient.createCharacter(
      gender = selectedGender,
      race = race.id,
      classId = cls._1,
      str = str, sta = sta, cha = cha, dex = dex, int_ = int_, agi = agi, wis = wis,
      startZone = cityId,
      deity = deities(selectedDeity).id,
    )

  // --- Helpers ---
  private def availableClasses: Vector[(Int, String)] =
    val race = races(selectedRace)
    race.classes.map(id => (id, classNames.getOrElse(id, s"Class($id)")))

  private def computeStats: (Int, Int, Int, Int, Int, Int, Int) =
    val race = races(selectedRace)
    val cls = availableClasses(selectedClass)
    val (rs, rt, rc, rd, ri, ra, rw) = raceStats.getOrElse(race.id, (75,75,75,75,75,75,75))
    val (cs, ct, cc, cd, ci, ca, cw) = classStats.getOrElse(cls._1, (0,0,0,0,0,0,0))
    (rs+cs, rt+ct, rc+cc, rd+cd, ri+ci, ra+ca, rw+cw)

  private def startZonesForRace: Vector[StartZone] =
    val raceId = races(selectedRace).id
    raceId match
      case 1   => Vector(startZones(1), startZones(4))           // Human: Qeynos, Freeport
      case 2   => Vector(startZones(2))                          // Barbarian: Halas
      case 3   => Vector(startZones(0))                          // Erudite: Odus
      case 4   => Vector(startZones(9))                          // Wood Elf: Gfay
      case 5   => Vector(startZones(10))                         // High Elf: Felwithe
      case 6   => Vector(startZones(5))                          // Dark Elf: Neriak
      case 7   => Vector(startZones(1), startZones(9))           // Half Elf: Qeynos, Gfay
      case 8   => Vector(startZones(8))                          // Dwarf: Kaladim
      case 9   => Vector(startZones(6))                          // Troll: Gukta/Grobb
      case 10  => Vector(startZones(7))                          // Ogre: Ogguk
      case 11  => Vector(startZones(3))                          // Halfling: Rivervale
      case 12  => Vector(startZones(11))                         // Gnome: Akanon
      case 128 => Vector(startZones(12))                         // Iksar: Cabalis
      case 130 => Vector(startZones(13))                         // Vah Shir: Shar Vahl
      case _   => Vector(startZones(1))

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
        selectedCity = 0
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
private case class DeityInfo(id: Int, name: String)
private case class StartZone(id: Int, name: String)
