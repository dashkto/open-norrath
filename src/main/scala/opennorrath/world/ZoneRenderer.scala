package opennorrath.world

import opennorrath.Settings
import opennorrath.animation.{AnimCode, AnimatedCharacter}
import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.render.{Mesh, Shader, Texture}
import opennorrath.state.ZoneCharacter
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.{glEnableVertexAttribArray, glVertexAttrib3f, glVertexAttribPointer}
import org.lwjgl.opengl.GL30.{glBindVertexArray, glDeleteVertexArrays, glGenVertexArrays}
import java.nio.file.{Path, Files}

class ZoneRenderer(s3dPath: String, settings: Settings = Settings(),
    zoneCharacters: scala.collection.Map[Int, ZoneCharacter] = Map.empty):

  private val entries = PfsArchive.load(Path.of(s3dPath))
  private val zoneWld = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
    .getOrElse(throw RuntimeException(s"No zone WLD found in $s3dPath"))

  private val wld = WldFile(zoneWld.data)
  private val zoneMesh = ZoneGeometry.extract(wld)
  val collision = ZoneCollision(zoneMesh)
  val zoneLineBsp: ZoneLineBsp = ZoneGeometry.extractZoneLineBsp(wld)

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

  // Equipment models from gequip*.s3d (IT number → mesh data)
  private val equipmentModels: Map[Int, ZoneRenderer.EquipModel] = loadEquipmentModels()

  /** Return (spawnId, modelMatrix, headHeight) for each live spawn — used for nameplates. */
  def spawnNameplateData: Iterable[(Int, Matrix4f, Float)] =
    zoneCharacters.collect { case (id, zc) if zc.hasRendering => (id, zc.animChar.modelMatrix, zc.build.glHeight * zc.effectiveSize) }

  /** Return (spawnId, modelMatrix, height, width, depth) for each live spawn — used for click targeting. */
  def spawnHitData: Iterable[(Int, Matrix4f, Float, Float, Float)] =
    zoneCharacters.collect { case (id, zc) if zc.hasRendering =>
      val s = zc.effectiveSize
      (id, zc.animChar.modelMatrix, zc.build.glHeight * s, zc.build.glWidth * s, zc.build.glDepth * s)
    }

  /** Initialize rendering on a ZoneCharacter. Returns false if model not found. */
  def initSpawnRendering(zc: ZoneCharacter): Boolean =
    characterBuilds.get(zc.modelCode) match
      case None => false
      case Some(build) =>
        val interleaved = ZoneRenderer.buildInterleaved(build.zm)
        val glMesh = Mesh(interleaved, build.zm.indices, dynamic = build.clips.nonEmpty)
        zc.initRendering(build, null) // sets effectiveSize/flyOffset, animChar filled below
        val flyPos = if zc.flyOffset != 0f then Vector3f(zc.position.x, zc.position.y + zc.flyOffset, zc.position.z) else zc.position
        val modelMatrix = buildSpawnMatrix(build, flyPos, zc.heading, zc.effectiveSize)
        val char = AnimatedCharacter(build.skeleton, build.meshFragments, build.zm, glMesh, modelMatrix, build.clips, interleaved.clone(), build.attachBoneIndices)
        zc.animChar = char
        updateSpawnEquipment(zc)
        true

  /** Get the vertical offset from model origin to feet for a given model code and size.
    * Returns -glMinY * effectiveSize (positive if origin is above feet).
    */
  /** Get (feetOffset, modelHeight) for a given model code and size. */
  def modelMetrics(modelCode: String, size: Float): (Float, Float) =
    characterBuilds.get(modelCode) match
      case Some(build) =>
        val effectiveSize = if size > 0f then size / 6f else 1f
        (-build.glMinY * effectiveSize, build.glHeight * effectiveSize)
      case None => (0f, 0f)

  /** Clean up GPU resources for a spawn character. */
  def cleanupSpawn(zc: ZoneCharacter): Unit =
    if zc.hasRendering then zc.animChar.glMesh.cleanup()

  /** Update a spawn's position (feet-level GL space) and heading. */
  def updateSpawnPosition(zc: ZoneCharacter): Unit =
    if zc.hasRendering then
      if zc.flyOffset != 0f then
        val flyPos = Vector3f(zc.position.x, zc.position.y + zc.flyOffset, zc.position.z)
        buildSpawnMatrix(zc.build, flyPos, zc.facingHeading, zc.effectiveSize, zc.animChar.modelMatrix)
      else
        buildSpawnMatrix(zc.build, zc.position, zc.facingHeading, zc.effectiveSize, zc.animChar.modelMatrix)

  /** Force a specific animation clip on a spawn, ignoring movement state. */
  def playSpawnAnimation(zc: ZoneCharacter, animCode: String, reverse: Boolean = false): Unit =
    if zc.hasRendering && zc.animChar.clips.contains(animCode) then
      zc.animChar.play(animCode, playReverse = reverse)

  /** Update a spawn's equipment texture overrides.
    * Parses base texture names to determine body part, then constructs variant names
    * using the equipment material IDs.
    * Texture naming: {race:3}{bodyPart:2}{material:02d}{index:02d}.bmp
    */
  def updateSpawnEquipment(zc: ZoneCharacter): Unit =
    if zc.hasRendering then
      zc.textureOverrides = ZoneRenderer.computeTextureOverrides(
        zc.animChar.zoneMesh.groups, zc.build.key, zc.equipment, textureMap, zc.bodyTexture)
      if zc.equipment.length > 8 then
        zc.weaponPrimary = zc.equipment(7)
        zc.weaponSecondary = zc.equipment(8)

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

  private val equipMatrix = Matrix4f() // reusable scratch matrix for equipment positioning

  private val eqToGl = ZoneRenderer.eqToGl
  private val glToEq = ZoneRenderer.glToEq

  private def findAttachKey(zc: ZoneCharacter, suffixTarget: String): Option[String] =
    ZoneRenderer.findAttachKey(zc.animChar.attachBoneIndices, suffixTarget)

  /** Draw an equipment model at a bone attachment point.
    *
    * Coordinate pipeline: equipment mesh vertices are in GL space (from buildInterleaved).
    * Bone transforms are in EQ S3D space. So we compose:
    *   modelMatrix × eqToGl × boneTransform × glToEq
    * which converts GL vertices → EQ space, applies bone, converts back to GL,
    * then applies the spawn's world position/heading/scale.
    */
  private def drawEquipment(shader: Shader, zc: ZoneCharacter, weaponId: Int, suffixTarget: String,
      opaquePass: Boolean): Unit =
    if weaponId == 0 then return
    equipmentModels.get(weaponId).foreach { equip =>
      val attachKey = findAttachKey(zc, suffixTarget)
      attachKey.flatMap(zc.animChar.attachmentTransform).foreach { boneTransform =>
        ZoneRenderer.composeEquipmentMatrix(equipMatrix, zc.animChar.modelMatrix, boneTransform, equip)
        shader.setMatrix4f("model", equipMatrix)
        for group <- equip.zm.groups do
          val emt = effectiveMaterialType(group)
          if opaquePass then
            if isOpaque(emt) then
              shader.setInt("alphaTest", if emt == MaterialType.TransparentMasked then 1 else 0)
              glBindTexture(GL_TEXTURE_2D, resolveTexture(group.textureName))
              equip.glMesh.drawRange(group.startIndex, group.indexCount)
          else
            if isTransparent(emt) then
              setBlendMode(shader, emt)
              glBindTexture(GL_TEXTURE_2D, resolveTexture(group.textureName))
              equip.glMesh.drawRange(group.startIndex, group.indexCount)
      }
    }

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

    for zc <- zoneCharacters.values if zc.hasRendering do
      zc.animChar.update(deltaTime)
      shader.setMatrix4f("model", zc.animChar.modelMatrix)
      for group <- zc.animChar.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isOpaque(emt) then
          shader.setInt("alphaTest", if emt == MaterialType.TransparentMasked then 1 else 0)
          val texName = zc.textureOverrides.getOrElse(group.textureName.toLowerCase, group.textureName)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(texName))
          zc.animChar.glMesh.drawRange(group.startIndex, group.indexCount)
      // Draw weapons at attachment points
      drawEquipment(shader, zc, zc.weaponPrimary, "R_POINT", opaquePass = true)
      drawEquipment(shader, zc, zc.weaponSecondary, "L_POINT", opaquePass = true)

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

    for zc <- zoneCharacters.values if zc.hasRendering do
      shader.setMatrix4f("model", zc.animChar.modelMatrix)
      for group <- zc.animChar.zoneMesh.groups do
        val emt = effectiveMaterialType(group)
        if isTransparent(emt) then
          setBlendMode(shader, emt)
          val texName = zc.textureOverrides.getOrElse(group.textureName.toLowerCase, group.textureName)
          glBindTexture(GL_TEXTURE_2D, resolveTexture(texName))
          zc.animChar.glMesh.drawRange(group.startIndex, group.indexCount)
      drawEquipment(shader, zc, zc.weaponPrimary, "R_POINT", opaquePass = false)
      drawEquipment(shader, zc, zc.weaponSecondary, "L_POINT", opaquePass = false)

    glDepthMask(true)
    glDisable(GL_BLEND)
    shader.setInt("alphaTest", 0)
    shader.setFloat("alphaMultiplier", 1.0f)

    // Update and draw particles (last, for correct additive blending)
    particleSystem.foreach { ps =>
      ps.update(deltaTime, viewMatrix)
      ps.draw(shader)
    }

  // --- Zone line sphere debug rendering ---
  private var sphereVao = 0
  private var sphereVbo = 0
  private var sphereVertexCount = 0
  private var sphereInited = false

  private def initZoneLineSpheres(): Unit =
    if zoneLineBsp.spheres.isEmpty then return
    val Segments = 24
    // Each sphere: 3 filled discs (triangle fans as GL_TRIANGLES), Segments triangles × 3 verts each
    val vertsPerSphere = Segments * 3 * 3
    val totalVerts = zoneLineBsp.spheres.size * vertsPerSphere
    val buf = BufferUtils.createFloatBuffer(totalVerts * 3)

    for s <- zoneLineBsp.spheres do
      val (cx, cy, cz) = EqCoords.s3dToGl(s.cx, s.cy, s.cz)
      val r = s.radius
      for i <- 0 until Segments do
        val a0 = (2 * Math.PI * i / Segments).toFloat
        val a1 = (2 * Math.PI * ((i + 1) % Segments) / Segments).toFloat
        // Horizontal disc (XZ plane)
        buf.put(cx).put(cy).put(cz)
        buf.put(cx + r * Math.cos(a0).toFloat).put(cy).put(cz + r * Math.sin(a0).toFloat)
        buf.put(cx + r * Math.cos(a1).toFloat).put(cy).put(cz + r * Math.sin(a1).toFloat)
        // Vertical disc (XY plane)
        buf.put(cx).put(cy).put(cz)
        buf.put(cx + r * Math.cos(a0).toFloat).put(cy + r * Math.sin(a0).toFloat).put(cz)
        buf.put(cx + r * Math.cos(a1).toFloat).put(cy + r * Math.sin(a1).toFloat).put(cz)
        // Vertical disc (YZ plane)
        buf.put(cx).put(cy).put(cz)
        buf.put(cx).put(cy + r * Math.sin(a0).toFloat).put(cz + r * Math.cos(a0).toFloat)
        buf.put(cx).put(cy + r * Math.sin(a1).toFloat).put(cz + r * Math.cos(a1).toFloat)
    buf.flip()
    sphereVertexCount = totalVerts

    sphereVao = glGenVertexArrays()
    sphereVbo = glGenBuffers()
    glBindVertexArray(sphereVao)
    glBindBuffer(GL_ARRAY_BUFFER, sphereVbo)
    glBufferData(GL_ARRAY_BUFFER, buf, GL_STATIC_DRAW)
    glVertexAttribPointer(0, 3, GL_FLOAT, false, 3 * 4, 0)
    glEnableVertexAttribArray(0)
    glBindVertexArray(0)
    sphereInited = true

  def drawZoneLineSpheres(shader: Shader): Unit =
    if !sphereInited then initZoneLineSpheres()
    if sphereVertexCount == 0 then return

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    shader.setMatrix4f("model", Matrix4f())
    glVertexAttrib3f(2, 0.4f, 0.7f, 1f)  // light blue
    shader.setFloat("alphaMultiplier", 0.4f)

    glBindVertexArray(sphereVao)
    glDrawArrays(GL_TRIANGLES, 0, sphereVertexCount)
    glBindVertexArray(0)

    shader.setFloat("alphaMultiplier", 1.0f)
    glVertexAttrib3f(2, 1f, 1f, 1f)
    glDepthMask(true)
    glDisable(GL_BLEND)

  def cleanup(): Unit =
    mesh.cleanup()
    objectInstances.foreach(_.glMesh.cleanup())
    zoneCharacters.values.foreach(zc => if zc.hasRendering then zc.animChar.glMesh.cleanup())
    equipmentModels.values.foreach(_.glMesh.cleanup())
    particleSystem.foreach(_.cleanup())
    if sphereInited then
      glDeleteBuffers(sphereVbo)
      glDeleteVertexArrays(sphereVao)
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

  private def loadEquipmentModels(): Map[Int, ZoneRenderer.EquipModel] =
    val assetsDir = Path.of(s3dPath).getParent
    if assetsDir == null then return Map.empty
    ZoneRenderer.loadEquipmentModels(assetsDir, entries => loadTextures(entries, applyColorKey = false))

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
    attachBoneIndices: Map[String, Int] = Map.empty, // bone suffix → index for _POINT bones
  )

  /** Equipment model with optional root bone offset from skeleton rest pose.
    * meshCenter is the EQ-space center baked into vertices during WLD parse —
    * needed to undo center offset before applying root bone transform for skeletal equipment.
    * isShield: true for flat items centered at origin (shields/bucklers) that need
    * outward offset at attachment points to avoid clipping through the arm.
    */
  case class EquipModel(zm: ZoneMesh, glMesh: Mesh, rootBoneTransform: Option[Matrix4f] = None, meshCenter: Vector3f = Vector3f(), isShield: Boolean = false)

  // EQ S3D space → GL space conversion matrix: (x,y,z) → (x, z, -y)
  // JOML constructor is column-major: (col0, col1, col2, col3)
  val eqToGl = Matrix4f(
    1f, 0f, 0f, 0f,   // col 0
    0f, 0f, -1f, 0f,  // col 1
    0f, 1f, 0f, 0f,   // col 2
    0f, 0f, 0f, 1f,   // col 3
  )
  // GL space → EQ S3D space (inverse): (x,y,z) → (x, -z, y)
  val glToEq = Matrix4f(
    1f, 0f, 0f, 0f,   // col 0
    0f, 0f, 1f, 0f,   // col 1
    0f, -1f, 0f, 0f,  // col 2
    0f, 0f, 0f, 1f,   // col 3
  )

  /** Find the best matching attachment bone for a target suffix.
    * For primary weapon: look for R_POINT
    * For secondary weapon: look for L_POINT, then SHIELD_POINT as fallback
    */
  def findAttachKey(attachBoneIndices: Map[String, Int], suffixTarget: String): Option[String] =
    val direct = attachBoneIndices.keys.find(_.endsWith(suffixTarget))
    if direct.isDefined then direct
    else if suffixTarget == "L_POINT" then
      attachBoneIndices.keys.find(_.endsWith("SHIELD_POINT"))
    else None

  /** Compose the equipment model matrix for rendering at a bone attachment point.
    *
    * Coordinate pipeline: equipment mesh vertices are in GL space (from buildInterleaved).
    * Bone transforms are in EQ S3D space. So we compose:
    *   charModelMatrix × eqToGl × boneTransform × [rootBone × translate(-center)] × glToEq
    *
    * For shields, applies outward offset to reduce arm clipping.
    */
  def composeEquipmentMatrix(target: Matrix4f, charModelMatrix: Matrix4f,
      boneTransform: Matrix4f, equip: EquipModel): Matrix4f =
    // For shield-like equipment, push slightly outward from the body center
    val effectiveBone = if equip.isShield then
      val bx = boneTransform.m30(); val by = boneTransform.m31(); val bz = boneTransform.m32()
      val dist = math.sqrt((bx * bx + by * by + bz * bz).toDouble).toFloat
      if dist > 0.01f then
        val ShieldOutwardOffset = 0.5f // EQ units
        val adjusted = Matrix4f(boneTransform)
        adjusted.setTranslation(
          bx + bx / dist * ShieldOutwardOffset,
          by + by / dist * ShieldOutwardOffset,
          bz + bz / dist * ShieldOutwardOffset)
        adjusted
      else boneTransform
    else boneTransform
    target.set(charModelMatrix).mul(eqToGl).mul(effectiveBone)
    equip.rootBoneTransform.foreach { rootXform =>
      target.mul(rootXform)
      val c = equip.meshCenter
      if c.x != 0f || c.y != 0f || c.z != 0f then
        target.translate(-c.x, -c.y, -c.z)
    }
    target.mul(glToEq)

  /** Compute texture overrides for equipment.
    * Maps base texture names to variant names based on equipment material IDs.
    * Texture naming: {race:3}{bodyPart:2}{material:02d}{index:02d}.bmp
    */
  def computeTextureOverrides(groups: List[ZoneMeshGroup], buildKey: String,
      equipment: Array[Int], textureMap: scala.collection.Map[String, Int],
      bodyTexture: Int = 0): Map[String, String] =
    val overrides = scala.collection.mutable.Map[String, String]()
    for group <- groups do
      val baseName = group.textureName.toLowerCase
      if baseName.length >= 9 && baseName.endsWith(".bmp") then
        val prefix = baseName.substring(0, 3) // race code
        if prefix == buildKey then
          val bodyPart = baseName.substring(3, 5)
          val slot = bodyPartToSlot.getOrElse(bodyPart, -1)
          if slot >= 0 && slot < equipment.length then
            val material = if equipment(slot) != 0 then equipment(slot)
              else if bodyTexture != 0 then bodyTexture
              else 0
            if material != 0 then
              val index = baseName.substring(7, 9) // preserve original texture index
              val variant = s"${prefix}${bodyPart}${"%02d".format(material)}${index}.bmp"
              if textureMap.contains(variant) then
                overrides(group.textureName.toLowerCase) = variant
    overrides.toMap

  /** Load equipment 3D models from gequip*.s3d archives.
    * Returns IT number → EquipModel (mesh + GPU data).
    */
  def loadEquipmentModels(assetsDir: Path, loadTextures: List[PfsEntry] => Unit): Map[Int, EquipModel] =
    val gequipFiles = Files.list(assetsDir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("gequip") && name.endsWith(".s3d")
      }.sorted
    if gequipFiles.isEmpty then return Map.empty

    val models = scala.collection.mutable.Map[Int, EquipModel]()
    val itPattern = "^it(\\d+)$".r

    for file <- gequipFiles do
      try
        val fileEntries = PfsArchive.load(file)
        loadTextures(fileEntries)
        fileEntries.find(_.extension == "wld").foreach { wldEntry =>
          val eqWld = WldFile(wldEntry.data)
          val actors = eqWld.fragmentsOfType[Fragment14_Actor]
          for actor <- actors do
            val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
            actorKey match
              case itPattern(numStr) =>
                val itNum = numStr.toInt
                if !models.contains(itNum) then
                  var meshFragments = List.empty[Fragment36_Mesh]
                  var rootBoneXform: Option[Matrix4f] = None
                  for ref <- actor.componentRefs do
                    try
                      eqWld.fragment(ref) match
                        case mr: Fragment2D_MeshReference =>
                          eqWld.fragment(mr.meshRef) match
                            case m: Fragment36_Mesh => meshFragments = meshFragments :+ m
                            case _ =>
                        case skelRef: Fragment11_SkeletonHierarchyRef =>
                          eqWld.fragment(skelRef.skeletonRef) match
                            case skel: Fragment10_SkeletonHierarchy =>
                              for mr <- skel.meshRefs do
                                try
                                  eqWld.fragment(mr) match
                                    case meshRef: Fragment2D_MeshReference =>
                                      eqWld.fragment(meshRef.meshRef) match
                                        case m: Fragment36_Mesh => meshFragments = meshFragments :+ m
                                        case _ =>
                                    case _ =>
                                catch case _: Exception => ()
                              if skel.bones.nonEmpty then
                                rootBoneXform = Some(skel.restPoseBoneTransform(0, eqWld))
                            case _ =>
                        case _ =>
                    catch case _: Exception => ()
                  if meshFragments.nonEmpty then
                    val cx = meshFragments.map(_.center.x).sum / meshFragments.size
                    val cy = meshFragments.map(_.center.y).sum / meshFragments.size
                    val cz = meshFragments.map(_.center.z).sum / meshFragments.size
                    val center = Vector3f(cx, cy, cz)
                    val zm = extractMeshGeometry(eqWld, meshFragments)
                    val vc = zm.vertices.length / 3
                    var isShield = false
                    if vc > 0 then
                      var minX = Float.MaxValue; var maxX = Float.MinValue
                      var minY = Float.MaxValue; var maxY = Float.MinValue
                      var minZ = Float.MaxValue; var maxZ = Float.MinValue
                      for i <- 0 until vc do
                        val x = zm.vertices(i * 3); val y = zm.vertices(i * 3 + 1); val z = zm.vertices(i * 3 + 2)
                        if x < minX then minX = x; if x > maxX then maxX = x
                        if y < minY then minY = y; if y > maxY then maxY = y
                        if z < minZ then minZ = z; if z > maxZ then maxZ = z
                      val extX = maxX - minX; val extY = maxY - minY; val extZ = maxZ - minZ
                      val maxExt = math.max(extX, math.max(extY, extZ))
                      val minExt = math.min(extX, math.min(extY, extZ))
                      isShield = maxExt > 0f && minExt / maxExt < 0.25f && center.length() < 0.5f && rootBoneXform.isEmpty
                    val interleaved = buildInterleaved(zm)
                    val glMesh = Mesh(interleaved, zm.indices)
                    models(itNum) = EquipModel(zm, glMesh, rootBoneXform, center, isShield)
              case _ => ()
        }
      catch case e: Exception =>
        println(s"  Warning: failed to load ${file.getFileName}: ${e.getMessage}")

    if models.nonEmpty then
      println(s"  Equipment models: ${models.size} from ${gequipFiles.length} gequip files")
    models.toMap

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

          // Resolve attachment point bones (suffix ending in _POINT)
          val attachIndices = resolveAttachBoneIndices(chrWld, sk)

          if !quiet then
            val animInfo = if clips.nonEmpty then
              clips.map((code, clip) => s"$code(${clip.frameCount}f)").mkString(", ")
            else "none"
            val attachInfo = if attachIndices.nonEmpty then s", attach: ${attachIndices.keys.mkString(",")}" else ""
            println(f"    $actorKey: ${meshFragments.size} meshes, ${zm.vertices.length / 3} verts, size(${eqMaxX - eqMinX}%.1f x ${eqMaxY - eqMinY}%.1f x ${eqMaxZ - eqMinZ}%.1f), anims: $animInfo$attachInfo")
          Some(CharBuild(actorKey, sk, meshFragments, zm,
            glWidth = eqMaxX - eqMinX, glDepth = eqMaxY - eqMinY, glHeight = eqMaxZ - eqMinZ,
            glCenterX = (eqMinX + eqMaxX) / 2f, glCenterZ = -(eqMinY + eqMaxY) / 2f, glMinY = eqMinZ,
            clips, attachIndices))
    }

  /** Scan skeleton bones for attachment point names (suffix ending in _POINT). */
  def resolveAttachBoneIndices(wld: WldFile, skeleton: Fragment10_SkeletonHierarchy): Map[String, Int] =
    val result = scala.collection.mutable.Map[String, Int]()
    for (bone, idx) <- skeleton.bones.zipWithIndex do
      try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        val trackDef = wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
        val name = trackDef.cleanName
        // Extract the suffix after the model prefix (shortest bone name = model prefix)
        // Attachment points end in _POINT
        if name.endsWith("_POINT") then
          // Use the suffix portion (e.g., "R_POINT", "L_POINT", "SHIELD_POINT")
          // The model prefix is the shortest clean name in the skeleton
          result(name) = idx
      catch case _: Exception => ()
    result.toMap

case class ObjectRenderData(zoneMesh: ZoneMesh, glMesh: Mesh, modelMatrix: Matrix4f)
