package opennorrath.world

import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.render.Texture
import opennorrath.wld.{Fragment12_TrackDef, Fragment14_Actor, WldFile}
import org.lwjgl.opengl.GL11.glDeleteTextures
import java.nio.file.{Files, Path}

/** Persistent store for global character models and animation tracks.
  * Loads once at startup from global*_chr.s3d files; shared by ZoneRenderer and CharacterPreview.
  */
object GlobalCharacters:

  private var _trackDefs: List[Fragment12_TrackDef] = Nil
  private var _characterBuilds: Map[String, CharacterModel] = Map.empty
  private var _textures: Map[String, Int] = Map.empty

  def trackDefs: List[Fragment12_TrackDef] = _trackDefs
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

    // First pass: collect all track defs (animations) from every file, and build
    // characters from global_chr.s3d (the classic all-in-one character archive).
    var classicWld: Option[WldFile] = None
    var classicActors: List[Fragment14_Actor] = Nil

    for file <- globalFiles do
      try
        val fileName = file.getFileName.toString.toLowerCase
        if fileName == "global_chr.s3d" then
          val fileEntries = PfsArchive.load(file)
          loadTextures(fileEntries, texMap)
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefsBuilder ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
            classicWld = Some(fileWld)
            classicActors = fileWld.fragmentsOfType[Fragment14_Actor]
          }
        else
          val fileEntries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefsBuilder ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
          }
      catch case e: Exception =>
        println(s"  Warning: failed to load ${file.getFileName}: ${e.getMessage}")

    val allTracks = trackDefsBuilder.result()
    println(s"  Global animations: ${allTracks.size} tracks from ${globalFiles.length} files")

    // Build classic models from global_chr.s3d
    classicWld.foreach { wld =>
      val builds = ZoneRenderer.buildCharacters(wld, classicActors, allTracks, quiet = true)
      for b <- builds do buildsMap(b.key) = b
    }

    // Second pass: load per-race files (e.g. globaldam_chr.s3d) for models not yet loaded.
    // These contain race-specific actors and textures that may use different naming than
    // the model code in EqData (e.g. file "globaldam" contains actor "dem").
    for file <- globalFiles do
      try
        val fileName = file.getFileName.toString.toLowerCase
        if fileName != "global_chr.s3d" && !fileName.matches("global\\d+_chr.*\\.s3d") then
          val fileEntries = PfsArchive.load(file)
          loadTextures(fileEntries, texMap)
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            val actors = fileWld.fragmentsOfType[Fragment14_Actor]
            if actors.nonEmpty then
              val builds = ZoneRenderer.buildCharacters(fileWld, actors, allTracks, quiet = true)
              for b <- builds if !buildsMap.contains(b.key) do
                buildsMap(b.key) = b
          }
      catch case e: Exception =>
        println(s"  Warning: failed to load per-race models from ${file.getFileName}: ${e.getMessage}")

    if buildsMap.nonEmpty then
      println(s"  Global character models: ${buildsMap.size} (${buildsMap.keys.toSeq.sorted.mkString(", ")})")
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

  def cleanup(): Unit =
    _textures.values.foreach(glDeleteTextures)
    _textures = Map.empty
    _trackDefs = Nil
    _characterBuilds = Map.empty

  private def loadTextures(entries: List[PfsEntry], texMap: scala.collection.mutable.Map[String, Int]): Unit =
    for entry <- entries if entry.extension == "bmp" do
      val key = entry.name.toLowerCase
      if !texMap.contains(key) then
        try texMap(key) = Texture.loadFromBytes(entry.data, applyColorKey = false)
        catch case _: Exception => ()
