package opennorrath

import opennorrath.animation.AnimatedCharacter
import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f
import java.nio.file.{Path, Files}

/** Loads global character models and renders a single character preview.
  * Used by the character select screen to show a 3D model of the selected character.
  */
class CharacterPreview(assetsDir: String):

  // Texture management
  private val textureMap = scala.collection.mutable.Map[String, Int]()
  private val fallbackTexture = Texture.createCheckerboard(64, 8)

  // Shader + matrices
  private val shader = Shader.fromResources("/shaders/default.vert", "/shaders/default.frag")
  private val projection = Matrix4f()
  private val viewMatrix = Matrix4f()
  private val modelMatrix = Matrix4f()

  // Current preview
  private var currentChar: Option[AnimatedCharacter] = None
  private var currentBuild: Option[ZoneRenderer.CharBuild] = None
  private var currentCode = ""
  private var rotation = 0f

  // Load global character models
  val characterBuilds: Map[String, ZoneRenderer.CharBuild] = loadGlobalCharacters()

  def setCharacter(modelCode: String): Unit =
    if modelCode == currentCode then return
    currentCode = modelCode
    rotation = 0f

    // Clean up previous
    currentChar.foreach(_.glMesh.cleanup())
    currentChar = None
    currentBuild = None

    characterBuilds.get(modelCode) match
      case None => ()
      case Some(build) =>
        val interleaved = ZoneRenderer.buildInterleaved(build.zm)
        val glMesh = Mesh(interleaved, build.zm.indices, dynamic = build.clips.nonEmpty)
        val mm = Matrix4f()
        mm.translate(-build.glCenterX, -build.glMinY, -build.glCenterZ)
        val char = AnimatedCharacter(build.skeleton, build.meshFragments, build.zm, glMesh, mm, build.clips, interleaved.clone())
        // Play idle animation
        val idleCode = if build.clips.contains("L01") then "L01"
          else if build.clips.contains("P01") then "P01"
          else build.clips.headOption.map(_._1).getOrElse("")
        if idleCode.nonEmpty then char.play(idleCode)
        currentChar = Some(char)
        currentBuild = Some(build)

  def draw(dt: Float, viewportX: Int, viewportY: Int, viewportW: Int, viewportH: Int, fullW: Int, fullH: Int): Unit =
    currentChar match
      case None => return
      case Some(char) =>
        val build = currentBuild.get

        // Set viewport to right half
        glViewport(viewportX, viewportY, viewportW, viewportH)
        glClear(GL_DEPTH_BUFFER_BIT)
        glEnable(GL_DEPTH_TEST)

        // Projection for this viewport
        val aspect = viewportW.toFloat / viewportH.toFloat
        projection.identity().perspective(Math.toRadians(45.0).toFloat, aspect, 0.1f, 1000f)

        // Camera: look at model center
        val modelHeight = build.glHeight
        val eyeHeight = modelHeight * 0.45f
        val distance = math.max(modelHeight * 1.8f, 15f)
        viewMatrix.identity().lookAt(
          Vector3f(0f, eyeHeight, distance),
          Vector3f(0f, eyeHeight, 0f),
          Vector3f(0f, 1f, 0f),
        )

        // Model matrix: auto-rotate
        rotation += dt * 30f // degrees per second
        modelMatrix.identity()
        modelMatrix.rotateY(Math.toRadians(rotation.toDouble).toFloat)
        modelMatrix.translate(-build.glCenterX, -build.glMinY, -build.glCenterZ)

        // Render
        shader.use()
        shader.setMatrix4f("projection", projection)
        shader.setMatrix4f("view", viewMatrix)
        shader.setMatrix4f("model", modelMatrix)
        glVertexAttrib3f(2, 1f, 1f, 1f) // white vertex color

        char.update(dt)
        for group <- char.zoneMesh.groups do
          if group.materialType != MaterialType.Invisible && group.materialType != MaterialType.Boundary then
            val texId = textureMap.getOrElse(group.textureName.toLowerCase, fallbackTexture)
            glBindTexture(GL_TEXTURE_2D, texId)
            char.glMesh.drawRange(group.startIndex, group.indexCount)

        // Restore full viewport
        glViewport(0, 0, fullW, fullH)

  def cleanup(): Unit =
    currentChar.foreach(_.glMesh.cleanup())
    shader.cleanup()
    glDeleteTextures(fallbackTexture)
    textureMap.values.foreach(glDeleteTextures)

  private def loadTextures(entries: List[opennorrath.archive.PfsEntry]): Unit =
    for entry <- entries if entry.extension == "bmp" do
      val key = entry.name.toLowerCase
      if !textureMap.contains(key) then
        try textureMap(key) = Texture.loadFromBytes(entry.data)
        catch case _: Exception => ()

  private def loadGlobalCharacters(): Map[String, ZoneRenderer.CharBuild] =
    val dir = Path.of(assetsDir)
    if !Files.isDirectory(dir) then return Map.empty

    val globalFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted
    if globalFiles.isEmpty then return Map.empty

    // Classic models + BMP textures live in global_chr.s3d.
    // Per-race files (globalhum_chr.s3d etc.) contain Luclin-era models with DDS textures
    // we can't load, so we only extract animation tracks from them.
    val trackDefs = List.newBuilder[Fragment12_TrackDef]
    var classicData: Option[(WldFile, List[Fragment14_Actor])] = None

    for file <- globalFiles do
      try
        val fileName = file.getFileName.toString.toLowerCase
        if fileName == "global_chr.s3d" then
          // Classic models: load fully with BMP textures
          val fileEntries = PfsArchive.load(file)
          loadTextures(fileEntries)
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefs ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
            val actors = fileWld.fragmentsOfType[Fragment14_Actor]
            if actors.nonEmpty then classicData = Some((fileWld, actors))
          }
        else
          // All other files: extract tracks only (WLD)
          val fileEntries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefs ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
          }
      catch case e: Exception =>
        println(s"  Warning: failed to load ${file.getFileName}: ${e.getMessage}")

    val allTracks = trackDefs.result()
    println(s"  CharacterPreview: ${allTracks.size} tracks from ${globalFiles.length} files")

    val builds = scala.collection.mutable.Map[String, ZoneRenderer.CharBuild]()
    classicData.foreach { (wld, actors) =>
      for build <- ZoneRenderer.buildCharacters(wld, actors, allTracks, quiet = true) do
        builds(build.key) = build
    }

    if builds.nonEmpty then
      println(s"  CharacterPreview: ${builds.size} models (${builds.keys.toSeq.sorted.mkString(", ")})")
    builds.toMap
