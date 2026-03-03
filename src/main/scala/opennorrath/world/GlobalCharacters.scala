package opennorrath.world

import opennorrath.animation.TrackMap
import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.render.Texture
import opennorrath.wld.{Fragment12_TrackDef, Fragment14_Actor, WldFile}
import org.lwjgl.opengl.GL11.glDeleteTextures
import java.nio.file.{Files, Path}

/** Persistent store for global character models and animation tracks.
  * Loads once at startup from global*_chr.s3d files.
  */
object GlobalCharacters:

  private var _trackMap: TrackMap = TrackMap.empty
  private var _characterBuilds: Map[String, CharacterModel] = Map.empty
  private var _textures: Map[String, Int] = Map.empty

  def trackMap: TrackMap = _trackMap
  def trackDefs: List[Fragment12_TrackDef] = _trackMap.byName.values.toList
  def characterBuilds: Map[String, CharacterModel] = _characterBuilds
  def textures: Map[String, Int] = _textures

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

    _trackMap = TrackMap.from(trackDefsBuilder.result())
    println(s"  Global animations: ${_trackMap.byName.size} tracks from ${globalFiles.length} files")

    // Build character models from global files.
    for data <- fileDataBuilder.result() do
      val builds = ZoneRenderer.buildCharacters(data.wld, data.actors, trackDefs, quiet = true)
      for b <- builds if !buildsMap.contains(b.key) do
        buildsMap(b.key) = b

    println(s"  Global character models: ${buildsMap.keys.toSeq.sorted.mkString(", ")}")

    // Log clip counts per model to help diagnose missing animations
    for key <- buildsMap.keys.toSeq.sorted do
      val clips = buildsMap(key).clips
      val combat = clips.keys.filter(_.startsWith("C")).toSeq.sorted
      val death = clips.keys.filter(_.startsWith("D")).toSeq.sorted
      if combat.isEmpty || death.isEmpty then
        println(s"    $key: ${clips.size} clips (combat=${combat.mkString(",")}, death=${death.mkString(",")})")

    _characterBuilds = buildsMap.toMap
    _textures = texMap.toMap

  def getModel(modelCode: String): Option[CharacterModel] =
    _characterBuilds.get(modelCode)

  def cleanup(): Unit =
    _textures.values.foreach(glDeleteTextures)
    _textures = Map.empty
    _trackMap = TrackMap.empty
    _characterBuilds = Map.empty

  private def loadTextures(entries: List[PfsEntry], texMap: scala.collection.mutable.Map[String, Int]): Unit =
    for entry <- entries if entry.extension == "bmp" do
      val key = entry.name.toLowerCase
      if !texMap.contains(key) then
        try texMap(key) = Texture.loadFromBytes(entry.data, applyColorKey = false)
        catch case _: Exception => ()
