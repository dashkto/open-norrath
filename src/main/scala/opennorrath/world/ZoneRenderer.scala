package opennorrath.world

import opennorrath.Settings
import opennorrath.animation.{AnimCode, AnimatedCharacter}
import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.render.{Mesh, Shader, Texture}
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f
import java.nio.file.{Path, Files}

class ZoneRenderer(s3dPath: String, settings: Settings = Settings()):

  private val entries = PfsArchive.load(Path.of(s3dPath))
  private val zoneWld = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
    .getOrElse(throw RuntimeException(s"No zone WLD found in $s3dPath"))

  private val wld = WldFile(zoneWld.data)
  private val zoneMesh = ZoneGeometry.extract(wld)
  val collision = ZoneCollision(zoneMesh)

  // Load line lights from companion .txt file
  private val lights: List[LightBaker.LineLight] =
    val txtPath = s3dPath.replaceAll("\\.s3d$", ".txt")
    if Files.exists(Path.of(txtPath)) then
      val ls = LightBaker.parseLights(txtPath)
      println(s"  Loaded ${ls.size} line lights from $txtPath")
      ls
    else Nil

  // Build interleaved vertex buffer: position (3) + uv (2) [+ color (3) if lit]
  private val vertexCount = zoneMesh.vertices.length / 3
  private val zoneStride = if lights.nonEmpty then 8 else 5
  private val interleavedVertices: Array[Float] =
    val base = ZoneRenderer.buildInterleaved(zoneMesh)
    if lights.nonEmpty then LightBaker.bakeVertexColors(base, lights) else base

  // Load textures from S3D entries, keyed by lowercase filename
  private[opennorrath] val textureMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty

  loadTextures(entries)

  private[opennorrath] val fallbackTexture = Texture.createCheckerboard(64, 8)

  val mesh = Mesh(interleavedVertices, zoneMesh.indices, stride = zoneStride)

  println(s"Zone loaded: $vertexCount vertices, ${zoneMesh.indices.length / 3} triangles, ${textureMap.size} textures")

  // Load objects and collect brazier emitters for flame particles
  private val (objectInstances, brazierEmitters) = loadObjects()

  // Load character build templates (zone + global models) and global animation tracks
  private var cachedGlobalTrackDefs: List[Fragment12_TrackDef] = Nil
  private var cachedGlobalWld: Option[WldFile] = None
  val characterBuilds: Map[String, ZoneRenderer.CharBuild] = loadCharacterBuilds()

  /** Return cached global animation TrackDefs (loaded during character init). */
  def loadGlobalAnimationTracks(): List[Fragment12_TrackDef] = cachedGlobalTrackDefs

  // Live spawn characters — managed by addSpawn/removeSpawn/updateSpawnPosition
  private case class SpawnInstance(char: AnimatedCharacter, build: ZoneRenderer.CharBuild, size: Float, var moving: Boolean = false,
    var textureOverrides: Map[String, String] = Map.empty)
  private val _spawnCharacters = scala.collection.mutable.Map[Int, SpawnInstance]()

  /** Return (spawnId, modelMatrix, headHeight) for each live spawn — used for nameplates. */
  def spawnNameplateData: Iterable[(Int, Matrix4f, Float)] =
    _spawnCharacters.map { (id, inst) => (id, inst.char.modelMatrix, inst.build.glHeight * inst.size) }

  /** Return (spawnId, modelMatrix, height, width, depth) for each live spawn — used for click targeting. */
  def spawnHitData: Iterable[(Int, Matrix4f, Float, Float, Float)] =
    _spawnCharacters.map { (id, inst) =>
      val s = inst.size
      (id, inst.char.modelMatrix, inst.build.glHeight * s, inst.build.glWidth * s, inst.build.glDepth * s)
    }

  /** Create a spawn character from a CharBuild template. Returns false if model not found. */
  def addSpawn(spawnId: Int, modelCode: String, position: Vector3f, heading: Int, size: Float): Boolean =
    characterBuilds.get(modelCode) match
      case None => false
      case Some(build) =>
        val interleaved = ZoneRenderer.buildInterleaved(build.zm)
        val glMesh = Mesh(interleaved, build.zm.indices, dynamic = build.clips.nonEmpty)
        val effectiveSize = if size > 0f then size / 6f else 1f
        val modelMatrix = buildSpawnMatrix(build, position, heading, effectiveSize)
        val char = AnimatedCharacter(build.skeleton, build.meshFragments, build.zm, glMesh, modelMatrix, build.clips, interleaved.clone())
        _spawnCharacters(spawnId) = SpawnInstance(char, build, effectiveSize)
        true

  /** Remove a spawn character and clean up its GPU resources. */
  def removeSpawn(spawnId: Int): Unit =
    _spawnCharacters.remove(spawnId).foreach(_.char.glMesh.cleanup())

  /** Update a spawn's position and heading. */
  def updateSpawnPosition(spawnId: Int, position: Vector3f, heading: Int): Unit =
    _spawnCharacters.get(spawnId).foreach { inst =>
      buildSpawnMatrix(inst.build, position, heading, inst.size, inst.char.modelMatrix)
    }

  /** Switch a spawn's animation based on movement state. */
  def updateSpawnAnimation(spawnId: Int, moving: Boolean, animType: Int = 0): Unit =
    _spawnCharacters.get(spawnId).foreach { inst =>
      if inst.moving != moving then
        inst.moving = moving
        val code = if moving then
          if inst.char.clips.contains(AnimCode.Run.code) then AnimCode.Run.code else AnimCode.Walk.code
        else
          if inst.char.clips.contains(AnimCode.Idle.code) then AnimCode.Idle.code else AnimCode.Passive.code
        inst.char.play(code)
    }

  /** Update a spawn's equipment texture overrides.
    * Parses base texture names to determine body part, then constructs variant names
    * using the equipment material IDs.
    * Texture naming: {race:3}{bodyPart:2}{material:02d}{index:02d}.bmp
    */
  def updateSpawnEquipment(spawnId: Int, equipment: Array[Int], bodyTexture: Int = 0): Unit =
    _spawnCharacters.get(spawnId).foreach { inst =>
      val overrides = scala.collection.mutable.Map[String, String]()
      for group <- inst.char.zoneMesh.groups do
        val baseName = group.textureName.toLowerCase
        if baseName.length >= 9 && baseName.endsWith(".bmp") then
          val prefix = baseName.substring(0, 3) // race code
          if prefix == inst.build.key then
            val bodyPart = baseName.substring(3, 5)
            val slot = ZoneRenderer.bodyPartToSlot.getOrElse(bodyPart, -1)
            if slot >= 0 then
              val material = if equipment(slot) != 0 then equipment(slot)
                else if bodyTexture != 0 then bodyTexture
                else 0
              if material != 0 then
                val index = baseName.substring(7, 9) // preserve original texture index
                val variant = s"${prefix}${bodyPart}${"%02d".format(material)}${index}.bmp"
                if textureMap.contains(variant) then
                  overrides(group.textureName.toLowerCase) = variant
      inst.textureOverrides = overrides.toMap
    }

  /** Spawn a line of models using a given race's mesh, each with a different race/gender's walk animation.
    * Returns list of (spawnId, label) for nameplate display.
    */
  def spawnAnimationTestLine(modelCode: String, basePosition: Vector3f, spacing: Float = 15f): List[(Int, String)] =
    val globalWld = cachedGlobalWld.getOrElse { println("[AnimTest] No global WLD cached"); return Nil }
    val build = characterBuilds.get(modelCode).getOrElse {
      println(s"[AnimTest] No $modelCode model (available: ${characterBuilds.keys.toSeq.sorted.mkString(", ")})")
      return Nil
    }

    val trackDefsByName: Map[String, Fragment12_TrackDef] = cachedGlobalTrackDefs.map(td => td.cleanName -> td).toMap
    val animCodes: Set[String] = trackDefsByName.keysIterator.filter(_.length > 3).map(_.take(3)).toSet

    // All playable race/gender combos (uppercase model prefix)
    val candidates = List(
      "HUM", "HUF", "BAM", "BAF", "ERM", "ERF", "ELM", "ELF",
      "EHM", "EHF", "DEM", "DEF", "HAM", "HAF", "DWM", "DWF",
      "TRM", "TRF", "OGM", "OGF", "GNM", "GNF", "IKM", "IKF",
      "KEM", "KEF",
    )

    val results = List.newBuilder[(Int, String)]
    var spawnId = 90000 // high IDs to avoid collision with real spawns
    var col = 0

    for prefix <- candidates do
      val clips = AnimatedCharacter.discoverAnimationsWithPrefix(
        globalWld, build.skeleton, trackDefsByName, animCodes, prefix)
      val walkClip = clips.get(AnimCode.Walk.code).orElse(clips.get(AnimCode.Run.code))
      if walkClip.isDefined then
        val pos = Vector3f(basePosition.x + col * spacing, basePosition.y, basePosition.z)
        val interleaved = ZoneRenderer.buildInterleaved(build.zm)
        val glMesh = Mesh(interleaved, build.zm.indices, dynamic = true)
        val modelMatrix = buildSpawnMatrix(build, pos, 0, 1f)
        val char = AnimatedCharacter(build.skeleton, build.meshFragments, build.zm,
          glMesh, modelMatrix, clips, interleaved.clone())
        char.play(walkClip.get.code)
        _spawnCharacters(spawnId) = SpawnInstance(char, build, 1f, moving = true)
        val label = prefix.toLowerCase
        println(s"[AnimTest] Spawned $modelCode with $label walk at col $col (${clips.size} clips)")
        results += ((spawnId, label))
        spawnId += 1
        col += 1
      else
        println(s"[AnimTest] $prefix: no walk animation for $modelCode")

    results.result()

  // No recenter needed here — character mesh vertices are already in local space with
  // origin near feet. CharacterPreview uses recenter to frame the model in its viewport.
  private def buildSpawnMatrix(build: ZoneRenderer.CharBuild, position: Vector3f, heading: Int, size: Float, target: Matrix4f = Matrix4f()): Matrix4f =
    target.identity()
    target.translate(position)
    target.rotateY(EqCoords.spawnHeadingToRadians(heading))
    target.scale(size)
    target

  // Create particle emitters from EQG file only (S3D has no emitter data)
  private val particleSystem: Option[ParticleSystem] =
    if !settings.useEqg then None
    else
      val emitterPath = s3dPath.replaceAll("\\.s3d$", "_EnvironmentEmitters.txt.backup")
      val emitters = ParticleSystem.parseEmitters(emitterPath)
      if emitters.nonEmpty then
        println(s"  Flame emitters: ${emitters.size} from EQG file")
      if emitters.nonEmpty then Some(ParticleSystem(emitters)) else None

  def resolveTexture(name: String): Int =
    if name.nonEmpty then textureMap.getOrElse(name.toLowerCase, fallbackTexture)
    else fallbackTexture

  private var elapsedMs: Float = 0f

  private def resolveAnimatedTexture(group: ZoneMeshGroup): Int =
    group.anim match
      case Some(anim) =>
        val frameIndex = ((elapsedMs / anim.delayMs).toInt % anim.frames.size).abs
        resolveTexture(anim.frames(frameIndex))
      case None =>
        resolveTexture(group.textureName)

  /** Texture name prefixes that should be rendered with additive blending regardless of WLD material type. */
  private val additiveTexturePrefixes = List("fire", "bfire", "flame")

  private def isAdditiveTexture(name: String): Boolean =
    val lower = name.toLowerCase
    additiveTexturePrefixes.exists(p => lower.startsWith(p))

  private def effectiveMaterialType(group: ZoneMeshGroup): MaterialType =
    if isAdditiveTexture(group.textureName) then MaterialType.TransparentAdditive
    else group.materialType

  private def isOpaque(mt: MaterialType): Boolean = mt == MaterialType.Diffuse || mt == MaterialType.TransparentMasked
  private def isTransparent(mt: MaterialType): Boolean = mt match
    case MaterialType.Transparent25 | MaterialType.Transparent50 | MaterialType.Transparent75
       | MaterialType.TransparentAdditive => true
    case _ => false

  private def setBlendMode(shader: Shader, mt: MaterialType): Unit = mt match
    case MaterialType.TransparentAdditive =>
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      shader.setFloat("alphaMultiplier", 1.0f)
    case MaterialType.Transparent25 =>
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      shader.setFloat("alphaMultiplier", 0.25f)
    case MaterialType.Transparent50 =>
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      shader.setFloat("alphaMultiplier", 0.50f)
    case MaterialType.Transparent75 =>
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      shader.setFloat("alphaMultiplier", 0.75f)
    case _ => ()

  def draw(shader: Shader, deltaTime: Float, viewMatrix: Matrix4f): Unit =
    elapsedMs += deltaTime * 1000f
    shader.setInt("tex0", 0)

    val identity = Matrix4f()

    // --- Pass 1: Opaque + masked geometry (depth write ON, blend OFF) ---
    shader.setMatrix4f("model", identity)
    for group <- zoneMesh.groups do
      val emt = effectiveMaterialType(group)
      if isOpaque(emt) then
        shader.setInt("alphaTest", if emt == MaterialType.TransparentMasked then 1 else 0)
        glBindTexture(GL_TEXTURE_2D, resolveAnimatedTexture(group))
        mesh.drawRange(group.startIndex, group.indexCount)

    glVertexAttrib3f(2, 1f, 1f, 1f)

    for obj <- objectInstances do
      shader.setMatrix4f("model", obj.modelMatrix)
      for group <- obj.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isOpaque(emt) then
          shader.setInt("alphaTest", if emt == MaterialType.TransparentMasked then 1 else 0)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(group.textureName))
          obj.glMesh.drawRange(group.startIndex, group.indexCount)

    for inst <- _spawnCharacters.values do
      inst.char.update(deltaTime)
      shader.setMatrix4f("model", inst.char.modelMatrix)
      for group <- inst.char.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isOpaque(emt) then
          shader.setInt("alphaTest", if emt == MaterialType.TransparentMasked then 1 else 0)
          val texName = inst.textureOverrides.getOrElse(group.textureName.toLowerCase, group.textureName)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(texName))
          inst.char.glMesh.drawRange(group.startIndex, group.indexCount)

    // --- Pass 2: Transparent geometry (depth write OFF, blend ON) ---
    glEnable(GL_BLEND)
    glDepthMask(false)
    shader.setInt("alphaTest", 1)

    shader.setMatrix4f("model", identity)
    glVertexAttrib3f(2, 1f, 1f, 1f)
    for group <- zoneMesh.groups do
      val emt = effectiveMaterialType(group)
      if isTransparent(emt) then
        setBlendMode(shader, emt)
        glBindTexture(GL_TEXTURE_2D, resolveAnimatedTexture(group))
        mesh.drawRange(group.startIndex, group.indexCount)

    for obj <- objectInstances do
      shader.setMatrix4f("model", obj.modelMatrix)
      for group <- obj.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isTransparent(emt) then
          setBlendMode(shader, emt)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(group.textureName))
          obj.glMesh.drawRange(group.startIndex, group.indexCount)

    for inst <- _spawnCharacters.values do
      shader.setMatrix4f("model", inst.char.modelMatrix)
      for group <- inst.char.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isTransparent(emt) then
          setBlendMode(shader, emt)
          val texName = inst.textureOverrides.getOrElse(group.textureName.toLowerCase, group.textureName)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(texName))
          inst.char.glMesh.drawRange(group.startIndex, group.indexCount)

    glDepthMask(true)
    glDisable(GL_BLEND)
    shader.setInt("alphaTest", 0)
    shader.setFloat("alphaMultiplier", 1.0f)

    // Update and draw particles (last, for correct additive blending)
    particleSystem.foreach { ps =>
      ps.update(deltaTime, viewMatrix)
      ps.draw(shader)
    }

  def cleanup(): Unit =
    mesh.cleanup()
    objectInstances.foreach(_.glMesh.cleanup())
    _spawnCharacters.values.foreach(_.char.glMesh.cleanup())
    _spawnCharacters.clear()
    particleSystem.foreach(_.cleanup())
    textureMap.values.foreach(glDeleteTextures(_))
    glDeleteTextures(fallbackTexture)

  private[opennorrath] def loadTextures(s3dEntries: List[PfsEntry], applyColorKey: Boolean = true): Unit =
    val bmpEntries = s3dEntries.filter(_.extension == "bmp")
    for entry <- bmpEntries do
      val key = entry.name.toLowerCase
      if !textureMap.contains(key) then
        try
          val texId = Texture.loadFromBytes(entry.data, applyColorKey = applyColorKey)
          textureMap(key) = texId
        catch case _: Exception => ()

  private def loadObjects(): (List[ObjectRenderData], List[Emitter]) =
    // Find objects.wld in the zone S3D
    val objectsWldOpt = entries.find(e => e.name == "objects.wld")
    if objectsWldOpt.isEmpty then return (Nil, Nil)

    val objectsWld = WldFile(objectsWldOpt.get.data)
    val placements = objectsWld.fragmentsOfType[Fragment15_ObjectInstance]

    if placements.isEmpty then return (Nil, Nil)

    // Load the _obj.s3d for object meshes
    val objS3dPath = s3dPath.replace(".s3d", "_obj.s3d")
    if !Files.exists(Path.of(objS3dPath)) then
      println(s"  Object S3D not found: $objS3dPath")
      return (Nil, Nil)

    val objEntries = PfsArchive.load(Path.of(objS3dPath))
    loadTextures(objEntries)

    val objWldEntry = objEntries.find(_.extension == "wld")
    if objWldEntry.isEmpty then return (Nil, Nil)

    val objWld = WldFile(objWldEntry.get.data)

    // Build actor name → mesh map
    // Actor names are like "MISTTHRONE_ACTORDEF", we match by stripping suffix
    val actors = objWld.fragmentsOfType[Fragment14_Actor]
    val actorMeshes: Map[String, ZoneMesh] = actors.flatMap { actor =>
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      // Follow component refs → MeshReference → Mesh
      val meshFragments = actor.componentRefs.flatMap { ref =>
        try
          objWld.fragment(ref) match
            case mr: Fragment2D_MeshReference =>
              objWld.fragment(mr.meshRef) match
                case m: Fragment36_Mesh => Some(m)
                case _ => None
            case _ => None
        catch case _: Exception => None
      }
      if meshFragments.nonEmpty then
        val zm = ZoneRenderer.extractMeshGeometry(objWld, meshFragments)
        Some((actorKey, zm))
      else None
    }.toMap

    println(s"  Object models: ${actorMeshes.size}")

    // Compute brazier mesh dimensions (EQ space) for flame placement and sizing
    case class BrazierInfo(height: Float, width: Float)
    val brazierInfo: Map[String, BrazierInfo] = actorMeshes.collect {
      case (name, zm) if name.contains("brazier") =>
        val vc = zm.vertices.length / 3
        var maxZ = Float.MinValue; var minX = Float.MaxValue; var maxX = Float.MinValue
        for i <- 0 until vc do
          val x = zm.vertices(i * 3); val z = zm.vertices(i * 3 + 2)
          if z > maxZ then maxZ = z
          if x < minX then minX = x; if x > maxX then maxX = x
        name -> BrazierInfo(maxZ, maxX - minX)
    }

    // Create render data for each placement, collect brazier emitters
    val emitterBuilder = List.newBuilder[Emitter]
    val results = placements.flatMap { placement =>
      // If this is a brazier, create a flame emitter at the top of the mesh
      if placement.actorName.contains("brazier") then
        val pos = placement.position
        val scl = if placement.scale.y != 0 then placement.scale.y else 1f
        val info = brazierInfo.getOrElse(placement.actorName, BrazierInfo(27f, 20f))
        val (glX, glY, glZ) = EqCoords.s3dToGl(pos.x, pos.y, pos.z)
        val flamePos = Vector3f(glX, glY + info.height * scl, glZ)
        // Scale particles relative to brazier width (normalize to ~20 unit reference width)
        val sizeScale = (info.width * scl / 20f).max(0.3f).min(2f)
        emitterBuilder += Emitter(flamePos, sizeScale)

      actorMeshes.get(placement.actorName).map { zm =>
        val interleaved = ZoneRenderer.buildInterleaved(zm)
        val glMesh = Mesh(interleaved, zm.indices)
        val modelMatrix = buildModelMatrix(placement)
        ObjectRenderData(zm, glMesh, modelMatrix)
      }
    }

    println(s"  Placed objects: ${results.size}")
    (results, emitterBuilder.result())

  // Character models live in {zone}_chr.s3d and global{code}_chr.s3d files.
  // Fragment chain: Actor(0x14) → SkeletonHierarchyRef(0x11) → SkeletonHierarchy(0x10) → MeshReference(0x2D) → Mesh(0x36)
  // Each bone references a TrackRef(0x13) → TrackDef(0x12) containing transform keyframes.
  // We build CharBuild templates here; actual spawn instances are created by addSpawn().
  private def loadCharacterBuilds(): Map[String, ZoneRenderer.CharBuild] =
    val builds = scala.collection.mutable.Map[String, ZoneRenderer.CharBuild]()

    // Load global data first (tracks + per-race character models)
    val (globalTracks, globalBuilds) = loadGlobalData()
    cachedGlobalTrackDefs = globalTracks
    for build <- globalBuilds do builds(build.key) = build

    // Load zone-specific characters (may override globals with zone-specific variants)
    val chrS3dPath = s3dPath.replace(".s3d", "_chr.s3d")
    if Files.exists(Path.of(chrS3dPath)) then
      val chrEntries = PfsArchive.load(Path.of(chrS3dPath))
      loadTextures(chrEntries, applyColorKey = false)
      val chrWldEntry = chrEntries.find(_.extension == "wld")
      chrWldEntry.foreach { entry =>
        val chrWld = WldFile(entry.data)
        val actors = chrWld.fragmentsOfType[Fragment14_Actor]
        println(s"  Zone character actors: ${actors.size} (${actors.map(_.name.replace("_ACTORDEF", "").toLowerCase).mkString(", ")})")
        val zoneBuilds = ZoneRenderer.buildCharacters(chrWld, actors, cachedGlobalTrackDefs)
        for build <- zoneBuilds do builds(build.key) = build
      }

    println(s"  Character models: ${builds.size} (${builds.keys.toSeq.sorted.mkString(", ")})")
    builds.toMap

  /** Load animation tracks and character models from all global*_chr*.s3d files.
    *
    * EQ stores character animations across multiple global S3D archives:
    *   global_chr.s3d    — Old-style anims for many races (DWF, ELF, BAF, HOM, etc.)
    *   global{N}_chr.s3d — Additional old-style animation tracks (global2..global7)
    *   global{code}_chr.s3d — Per-race files with Luclin-era skeletons + animations
    *     e.g. globalbaf_chr.s3d has BAF rest-pose + C01ABAF* Luclin animations
    *
    * Per-race files (matching global{letters}_chr.s3d) are loaded fully including textures,
    * since they contain the skeleton + mesh for that race. Animation-only files (global_chr.s3d,
    * global{N}_chr.s3d) load only the WLD for track extraction.
    *
    * Returns (allTracks, charBuilds) where charBuilds are models from per-race global files.
    */
  private def loadGlobalData(): (List[Fragment12_TrackDef], List[ZoneRenderer.CharBuild]) =
    val assetsDir = Path.of(s3dPath).getParent
    if assetsDir == null then return (Nil, Nil)
    val globalFiles = Files.list(assetsDir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted
    if globalFiles.isEmpty then return (Nil, Nil)

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
          loadTextures(fileEntries, applyColorKey = false)
          fileEntries.find(_.extension == "wld").foreach { wldEntry =>
            val fileWld = WldFile(wldEntry.data)
            trackDefs ++= fileWld.fragmentsOfType[Fragment12_TrackDef]
            val actors = fileWld.fragmentsOfType[Fragment14_Actor]
            cachedGlobalWld = Some(fileWld)
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
    println(s"  Global animations: ${allTracks.size} tracks from ${globalFiles.length} files")

    // Build classic character models from global_chr.s3d actors
    val buildList = classicData match
      case Some((wld, actors)) =>
        ZoneRenderer.buildCharacters(wld, actors, allTracks, quiet = true)
      case None => Nil

    if buildList.nonEmpty then
      println(s"  Global character models: ${buildList.size} (${buildList.map(_.key).sorted.mkString(", ")})")
    (allTracks, buildList)

  private def buildModelMatrix(placement: Fragment15_ObjectInstance): Matrix4f =
    val pos = placement.position
    val rot = placement.rotation
    val scl = placement.scale

    val (glX, glY, glZ) = EqCoords.s3dToGl(pos.x, pos.y, pos.z)
    val mat = Matrix4f()
    mat.translate(glX, glY, glZ)
    // Apply rotations (EQ rotation order)
    mat.rotateY(Math.toRadians(-rot.z).toFloat)  // heading (Z rot in EQ → Y rot in GL)
    mat.rotateX(Math.toRadians(rot.y).toFloat)    // pitch
    mat.rotateZ(Math.toRadians(rot.x).toFloat)    // roll
    // Scale - use Y as uniform scale if others are 0
    val s = if scl.y != 0 then scl.y else 1f
    mat.scale(s)
    mat

object ZoneRenderer:
  private[opennorrath] val PerRacePattern = "^global([a-z]{2,3})_chr\\.s3d$".r

  /** Map texture body-part codes → equipment slot indices.
    * Slots: 0=Head, 1=Chest, 2=Arms, 3=Wrist, 4=Hands, 5=Legs, 6=Feet
    */
  val bodyPartToSlot: Map[String, Int] = Map(
    "he" -> 0, "ch" -> 1, "ua" -> 2, "fa" -> 3,
    "hn" -> 4, "lg" -> 5, "ft" -> 6,
  )

  case class CharBuild(
    key: String, skeleton: Fragment10_SkeletonHierarchy, meshFragments: List[Fragment36_Mesh],
    zm: ZoneMesh, glWidth: Float, glDepth: Float, glHeight: Float,
    glCenterX: Float, glCenterZ: Float, glMinY: Float,
    clips: Map[String, opennorrath.animation.AnimationClip],
  )

  def buildInterleaved(zm: ZoneMesh): Array[Float] =
    val vc = zm.vertices.length / 3
    val arr = new Array[Float](vc * 5)
    for i <- 0 until vc do
      val (glX, glY, glZ) = EqCoords.s3dToGl(
        zm.vertices(i * 3 + 0),
        zm.vertices(i * 3 + 1),
        zm.vertices(i * 3 + 2),
      )
      arr(i * 5 + 0) = glX
      arr(i * 5 + 1) = glY
      arr(i * 5 + 2) = glZ
      if i * 2 + 1 < zm.uvs.length then
        arr(i * 5 + 3) = zm.uvs(i * 2 + 0)
        arr(i * 5 + 4) = zm.uvs(i * 2 + 1)
    arr

  def extractMeshGeometry(objWld: WldFile, meshFragments: List[Fragment36_Mesh]): ZoneMesh =
    var allVertices = Array.empty[Float]
    var allUvs = Array.empty[Float]
    var allIndices = Array.empty[Int]
    var allGroups = List.empty[ZoneMeshGroup]

    for mesh <- meshFragments do
      val vertexOffset = allVertices.length / 3

      val verts = mesh.vertices.flatMap(v => Array(v.x, v.y, v.z))
      allVertices = allVertices ++ verts

      // Pad UVs to match vertex count so flat arrays stay aligned
      val paddedUvs = if mesh.uvs.length < mesh.vertices.length then
        mesh.uvs ++ Array.fill(mesh.vertices.length - mesh.uvs.length)((0f, 0f))
      else
        mesh.uvs.take(mesh.vertices.length)
      val uvs = paddedUvs.flatMap((u, v) => Array(u, v))
      allUvs = allUvs ++ uvs

      var polyIndex = 0
      for group <- mesh.renderGroups do
        val startIndex = allIndices.length
        val textureName = ZoneGeometry.resolveTextureName(objWld, mesh.materialListRef, group.materialIndex)
        val matType = ZoneGeometry.resolveMaterialType(objWld, mesh.materialListRef, group.materialIndex)

        for _ <- 0 until group.polyCount do
          if polyIndex < mesh.polygons.length then
            val poly = mesh.polygons(polyIndex)
            allIndices = allIndices ++ Array(
              poly.v1 + vertexOffset,
              poly.v2 + vertexOffset,
              poly.v3 + vertexOffset,
            )
            polyIndex += 1

        val indexCount = allIndices.length - startIndex
        if indexCount > 0 then
          allGroups = allGroups :+ ZoneMeshGroup(startIndex, indexCount, textureName, matType)

    ZoneMesh(allVertices, allUvs, allIndices, allGroups)

  def applyBoneTransforms(mesh: Fragment36_Mesh, boneTransforms: Array[Matrix4f]): Fragment36_Mesh =
    if mesh.vertexPieces.isEmpty then return mesh

    val newVertices = mesh.vertices.clone()
    var vertexIndex = 0
    for piece <- mesh.vertexPieces do
      if piece.boneIndex < boneTransforms.length then
        val transform = boneTransforms(piece.boneIndex)
        for _ <- 0 until piece.count do
          if vertexIndex < newVertices.length then
            val v = newVertices(vertexIndex)
            val transformed = Vector3f(v.x, v.y, v.z)
            transform.transformPosition(transformed)
            newVertices(vertexIndex) = transformed
            vertexIndex += 1
      else
        vertexIndex += piece.count

    mesh.copy(vertices = newVertices)

  def resolveActorMeshes(wld: WldFile, actor: Fragment14_Actor): (Option[Fragment10_SkeletonHierarchy], List[Fragment36_Mesh]) =
    var skeleton: Option[Fragment10_SkeletonHierarchy] = None
    val meshFragments = actor.componentRefs.flatMap { ref =>
      try
        wld.fragment(ref) match
          case sr: Fragment11_SkeletonHierarchyRef =>
            wld.fragment(sr.skeletonRef) match
              case sk: Fragment10_SkeletonHierarchy =>
                skeleton = Some(sk)
                sk.meshRefs.flatMap { mr =>
                  wld.fragment(mr) match
                    case m: Fragment2D_MeshReference =>
                      wld.fragment(m.meshRef) match
                        case mesh: Fragment36_Mesh => Some(mesh)
                        case _ => None
                    case _ => None
                }
              case _ => Nil
          case mr: Fragment2D_MeshReference =>
            wld.fragment(mr.meshRef) match
              case mesh: Fragment36_Mesh => Some(mesh)
              case _ => None
          case _ => Nil
      catch case _: Exception => Nil
    }
    (skeleton, meshFragments)

  /** Build character data from actors: resolve meshes, discover animations, compute bounding boxes. */
  def buildCharacters(chrWld: WldFile, actors: List[Fragment14_Actor], extraTrackDefs: List[Fragment12_TrackDef] = Nil, quiet: Boolean = false): List[CharBuild] =
    val allTrackDefs = chrWld.fragmentsOfType[Fragment12_TrackDef] ++ extraTrackDefs
    val trackDefsByName: Map[String, Fragment12_TrackDef] = allTrackDefs.map { td =>
      td.cleanName -> td
    }.toMap
    val animCodes: Set[String] = trackDefsByName.keysIterator
      .filter(_.length > 3).map(_.take(3)).toSet

    actors.flatMap { actor =>
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      val (skeletonOpt, meshFragments) = resolveActorMeshes(chrWld, actor)

      if meshFragments.isEmpty || skeletonOpt.isEmpty then None
      else
        val sk = skeletonOpt.get
        if sk.isLuclin(chrWld) then
          if !quiet then println(f"    $actorKey: skipped (Luclin skeleton)")
          None
        else
          val clips = AnimatedCharacter.discoverAnimations(chrWld, sk, trackDefsByName, animCodes)

          val defaultClip = clips.get(AnimCode.Idle.code).orElse(clips.get(AnimCode.Passive.code)).orElse(clips.headOption.map(_._2))
          val boneTransforms = defaultClip match
            case Some(clip) => AnimatedCharacter.computeBoneTransforms(sk, clip, 0)
            case None => sk.boneWorldTransforms(chrWld)
          val transformedMeshes = meshFragments.map(mesh => applyBoneTransforms(mesh, boneTransforms))
          val zm = extractMeshGeometry(chrWld, transformedMeshes)

          val vc = zm.vertices.length / 3
          var eqMinX = Float.MaxValue; var eqMaxX = Float.MinValue
          var eqMinY = Float.MaxValue; var eqMaxY = Float.MinValue
          var eqMinZ = Float.MaxValue; var eqMaxZ = Float.MinValue
          for i <- 0 until vc do
            val x = zm.vertices(i * 3); val y = zm.vertices(i * 3 + 1); val z = zm.vertices(i * 3 + 2)
            if x < eqMinX then eqMinX = x; if x > eqMaxX then eqMaxX = x
            if y < eqMinY then eqMinY = y; if y > eqMaxY then eqMaxY = y
            if z < eqMinZ then eqMinZ = z; if z > eqMaxZ then eqMaxZ = z

          if !quiet then
            val animInfo = if clips.nonEmpty then
              clips.map((code, clip) => s"$code(${clip.frameCount}f)").mkString(", ")
            else "none"
            println(f"    $actorKey: ${meshFragments.size} meshes, ${zm.vertices.length / 3} verts, size(${eqMaxX - eqMinX}%.1f x ${eqMaxY - eqMinY}%.1f x ${eqMaxZ - eqMinZ}%.1f), anims: $animInfo")
          Some(CharBuild(actorKey, sk, meshFragments, zm,
            glWidth = eqMaxX - eqMinX, glDepth = eqMaxY - eqMinY, glHeight = eqMaxZ - eqMinZ,
            glCenterX = (eqMinX + eqMaxX) / 2f, glCenterZ = -(eqMinY + eqMaxY) / 2f, glMinY = eqMinZ,
            clips))
    }

case class ObjectRenderData(zoneMesh: ZoneMesh, glMesh: Mesh, modelMatrix: Matrix4f)
