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
  private var _characterBuilds: Map[String, ZoneRenderer.CharBuild] = Map.empty
  private var _textures: Map[String, Int] = Map.empty

  def trackDefs: List[Fragment12_TrackDef] = _trackDefs
  def characterBuilds: Map[String, ZoneRenderer.CharBuild] = _characterBuilds
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
    var classicData: Option[(WldFile, List[Fragment14_Actor])] = None

    for file <- globalFiles do
      try
        val fileName = file.getFileName.toString.toLowerCase
        if fileName == "global_chr.s3d" then
          val fileEntries = PfsArchive.load(file)
          loadTextures(fileEntries, texMap)
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefsBuilder ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
            val actors = fileWld.fragmentsOfType[Fragment14_Actor]
            if actors.nonEmpty then classicData = Some((fileWld, actors))
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

    val builds = classicData match
      case Some((wld, actors)) =>
        ZoneRenderer.buildCharacters(wld, actors, allTracks, quiet = true)
      case None => Nil

    if builds.nonEmpty then
      println(s"  Global character models: ${builds.size} (${builds.map(_.key).sorted.mkString(", ")})")

    _trackDefs = allTracks
    _characterBuilds = builds.map(b => b.key -> b).toMap
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
