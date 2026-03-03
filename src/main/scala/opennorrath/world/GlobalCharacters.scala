package opennorrath.world

import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.render.Texture
import opennorrath.wld.{Fragment12_TrackDef, Fragment14_Actor, WldFile}
import org.lwjgl.opengl.GL11.glDeleteTextures
import java.nio.file.{Files, Path}

/** Persistent store for global character models and animation tracks.
  * Loads once at startup from global*_chr.s3d files.
  *
  * Also builds a lightweight index (actor name → chr file path) from classic/Kunark/Velious
  * zone chr files. Models are lazy-loaded from this index on first use.
  */
object GlobalCharacters:

  private var _trackDefs: List[Fragment12_TrackDef] = Nil
  private var _characterBuilds: Map[String, CharacterModel] = Map.empty
  private var _textures: Map[String, Int] = Map.empty

  // Actor name → zone chr file path, for models not in global files.
  // Built at startup by scanning WLD actor names (no model building or texture loading).
  private var _actorIndex: Map[String, Path] = Map.empty

  def trackDefs: List[Fragment12_TrackDef] = _trackDefs
  def characterBuilds: Map[String, CharacterModel] = _characterBuilds
  def textures: Map[String, Int] = _textures

  /** Classic (exp 1), Kunark (exp 2), Velious (exp 3) zone short names.
    * Creature models from these zones are indexed at startup for lazy loading.
    */
  private val ClassicZones: Set[String] = Set(
    // Classic (expansion 1)
    "airplane", "akanon", "arena", "arena2", "befallen", "beholder", "blackburrow", "butcher",
    "cauldron", "cazicthule", "codecay", "commons", "crushbone", "cshome", "eastkarana", "ecommons",
    "erudnext", "erudnint", "erudsxing", "everfrost", "fearplane", "feerrott", "felwithea",
    "felwitheb", "freporte", "freportn", "freportw", "gfaydark", "grobb", "gukbottom", "gukswitch",
    "guktop", "halas", "hateplane", "highkeep", "highpass", "hole", "innothule", "kaladima",
    "kaladimb", "kedge", "kedge_tryout", "kerraridge", "kithicor", "lakerathe", "lavastorm",
    "lfaydark", "mistmoore", "misty", "najena", "nektulos", "neriaka", "neriakb", "neriakc",
    "northkarana", "nro", "oasis", "oggok", "oot", "paineel", "paw", "permafrost", "perma_tryout",
    "qcat", "qey2hh1", "qeynos", "qeynos2", "qeytoqrg", "qrg", "rathemtn", "rivervale",
    "runnyeye", "sodecay", "soldunga", "soldungb", "soldungb_tryout", "soltemple", "southkarana",
    "sro", "steamfont", "towerbone", "tox", "unrest",
    // Kunark (expansion 2)
    "burningwood", "cabeast", "cabwest", "charasis", "chardok", "citymist", "dalnir", "dreadlands",
    "droga", "emeraldjungle", "fieldofbone", "firiona", "frontiermtns", "kaesora", "karnor", "kurn",
    "lakeofillomen", "nurga", "overthere", "sebilis", "skyfire", "stonebrunt", "swampofnohope",
    "timorous", "trakanon", "veeshan", "veksar", "warrens", "warslikswood",
    // Velious (expansion 3)
    "cobaltscar", "crystal", "eastwastes", "frozenshadow", "greatdivide", "growthplane", "iceclad",
    "kael", "mischiefplane", "necropolis", "sirens", "skyshrine", "sleeper", "templeveeshan",
    "thurgadina", "thurgadinb", "velketor", "wakening", "westwastes",
  )

  def init(assetsDir: String): Unit =
    val dir = Path.of(assetsDir)
    if !Files.isDirectory(dir) then return

    val globalFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted
    if globalFiles.isEmpty then return

    val texMap = scala.collection.mutable.Map[String, Int]()
    val trackDefsBuilder = List.newBuilder[Fragment12_TrackDef]
    val buildsMap = scala.collection.mutable.Map[String, CharacterModel]()

    // Collect all track defs (animations) and actors from every global file.
    case class FileData(wld: WldFile, actors: List[Fragment14_Actor])
    val fileDataBuilder = List.newBuilder[FileData]

    for file <- globalFiles do
      try
        val fileEntries = PfsArchive.load(file)
        loadTextures(fileEntries, texMap)
        fileEntries.find(_.extension == "wld").foreach { wldEntry =>
          val fileWld = WldFile(wldEntry.data)
          trackDefsBuilder ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
          val actors = fileWld.fragmentsOfType[Fragment14_Actor]
          if actors.nonEmpty then
            fileDataBuilder += FileData(fileWld, actors)
        }
      catch case e: Exception =>
        println(s"  Warning: failed to load ${file.getFileName}: ${e.getMessage}")

    val allTracks = trackDefsBuilder.result()
    println(s"  Global animations: ${allTracks.size} tracks from ${globalFiles.length} files")

    // Build character models from global files.
    for data <- fileDataBuilder.result() do
      val builds = ZoneRenderer.buildCharacters(data.wld, data.actors, allTracks, quiet = true)
      for b <- builds if !buildsMap.contains(b.key) do
        buildsMap(b.key) = b

    println(s"  Global character models: ${buildsMap.size}")

    // Log clip counts per model to help diagnose missing animations
    for key <- buildsMap.keys.toSeq.sorted do
      val clips = buildsMap(key).clips
      val combat = clips.keys.filter(_.startsWith("C")).toSeq.sorted
      val death = clips.keys.filter(_.startsWith("D")).toSeq.sorted
      if combat.isEmpty || death.isEmpty then
        println(s"    $key: ${clips.size} clips (combat=${combat.mkString(",")}, death=${death.mkString(",")})")

    _trackDefs = allTracks
    _characterBuilds = buildsMap.toMap
    _textures = texMap.toMap

    // Build actor index from classic/Kunark/Velious zone chr files.
    // Only parses WLD actor fragment names — no model building or texture loading.
    buildActorIndex(dir)

  /** Look up a model, lazy-loading from a zone chr file if needed.
    * Returns None only if the model doesn't exist anywhere.
    */
  def getModel(modelCode: String): Option[CharacterModel] =
    _characterBuilds.get(modelCode).orElse(loadFromIndex(modelCode))

  private def loadFromIndex(modelCode: String): Option[CharacterModel] =
    _actorIndex.get(modelCode).flatMap { chrFile =>
      try
        println(s"  Loading model '$modelCode' from ${chrFile.getFileName}")
        val entries = PfsArchive.load(chrFile)
        val texMap = scala.collection.mutable.Map[String, Int]()
        texMap ++= _textures
        loadTextures(entries, texMap)
        entries.find(_.extension == "wld").flatMap { wldEntry =>
          val wld = WldFile(wldEntry.data)
          val actors = wld.fragmentsOfType[Fragment14_Actor]
          // Combine global tracks with this chr file's own tracks so zone-only
          // creatures get their animations (e.g. minotaur in qeynos_chr.s3d).
          val localTracks = wld.fragmentsOfType[Fragment12_TrackDef]
          val combinedTracks = _trackDefs ++ localTracks
          val builds = ZoneRenderer.buildCharacters(wld, actors, combinedTracks, quiet = true)
          val buildsMap = scala.collection.mutable.Map[String, CharacterModel]()
          buildsMap ++= _characterBuilds
          for b <- builds if !buildsMap.contains(b.key) do
            buildsMap(b.key) = b
          _characterBuilds = buildsMap.toMap
          _textures = texMap.toMap
          // Remove all loaded actors from the index — no need to re-scan this file
          val loadedKeys = builds.map(_.key).toSet
          _actorIndex = _actorIndex.removedAll(loadedKeys)
          _characterBuilds.get(modelCode)
        }
      catch
        case e: Exception =>
          println(s"  Warning: failed to load '$modelCode' from ${chrFile.getFileName}: ${e.getMessage}")
          _actorIndex = _actorIndex.removed(modelCode)
          None
    }

  private def buildActorIndex(dir: Path): Unit =
    val index = scala.collection.mutable.Map[String, Path]()
    for zoneName <- ClassicZones do
      val chrPath = dir.resolve(s"${zoneName}_chr.s3d")
      if Files.exists(chrPath) then
        try
          val entries = PfsArchive.load(chrPath, extensionFilter = Some(Set("wld")))
          entries.find(_.extension == "wld").foreach { wldEntry =>
            val wld = WldFile(wldEntry.data)
            for actor <- wld.fragmentsOfType[Fragment14_Actor] do
              val key = actor.name.replace("_ACTORDEF", "").toLowerCase
              if !_characterBuilds.contains(key) && !index.contains(key) then
                index(key) = chrPath
          }
        catch case _: Exception => ()
    _actorIndex = index.toMap
    if index.nonEmpty then
      println(s"  Actor index: ${index.size} zone-only models indexed from ${ClassicZones.size} zones")

  def cleanup(): Unit =
    _textures.values.foreach(glDeleteTextures)
    _textures = Map.empty
    _trackDefs = Nil
    _characterBuilds = Map.empty
    _actorIndex = Map.empty

  private def loadTextures(entries: List[PfsEntry], texMap: scala.collection.mutable.Map[String, Int]): Unit =
    for entry <- entries if entry.extension == "bmp" do
      val key = entry.name.toLowerCase
      if !texMap.contains(key) then
        try texMap(key) = Texture.loadFromBytes(entry.data, applyColorKey = false)
        catch case _: Exception => ()
