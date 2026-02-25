package opennorrath

import opennorrath.animation.AnimatedCharacter
import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f
import java.nio.file.{Path, Files}

class ZoneRenderer(s3dPath: String):

  private val entries = PfsArchive.load(Path.of(s3dPath))
  private val zoneWld = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
    .getOrElse(throw RuntimeException(s"No zone WLD found in $s3dPath"))

  private val wld = WldFile(zoneWld.data)
  private val zoneMesh = ZoneGeometry.extract(wld)

  // Load line lights from companion .txt file
  private val lights: List[LightBaker.LineLight] =
    val txtPath = s3dPath.replaceAll("\\.s3d$", ".txt")
    if Files.exists(Path.of(txtPath)) then
      val ls = LightBaker.parseLights(txtPath)
      println(s"  Loaded ${ls.size} line lights from $txtPath")
      ls
    else Nil

  // Build interleaved vertex buffer: position (3) + uv (2) [+ color (3) if lit]
  // EQ uses Z-up; OpenGL/our camera uses Y-up. Swap: EQ(X,Y,Z) → GL(X,Z,-Y)
  private val vertexCount = zoneMesh.vertices.length / 3
  private val zoneStride = if lights.nonEmpty then 8 else 5
  private val interleavedVertices: Array[Float] =
    val base = buildInterleaved(zoneMesh)
    if lights.nonEmpty then LightBaker.bakeVertexColors(base, lights) else base

  // Load textures from S3D entries, keyed by lowercase filename
  private val textureMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty

  loadTextures(entries)

  private val fallbackTexture = Texture.createCheckerboard(64, 8)

  val mesh = Mesh(interleavedVertices, zoneMesh.indices, stride = zoneStride)

  println(s"Zone loaded: $vertexCount vertices, ${zoneMesh.indices.length / 3} triangles, ${textureMap.size} textures")

  // Load objects
  private val objectInstances: List[ObjectRenderData] = loadObjects()

  // Load character models
  private val characterInstances: List[AnimatedCharacter] = loadCharacters()

  def draw(shader: Shader, deltaTime: Float): Unit =
    shader.setInt("tex0", 0)

    // Draw zone geometry
    val identity = Matrix4f()
    shader.setMatrix4f("model", identity)
    for group <- zoneMesh.groups do
      if group.materialType != MaterialType.Invisible && group.materialType != MaterialType.Boundary then
        val texId = if group.textureName.nonEmpty then
          textureMap.getOrElse(group.textureName.toLowerCase, fallbackTexture)
        else
          fallbackTexture
        glBindTexture(GL_TEXTURE_2D, texId)
        mesh.drawRange(group.startIndex, group.indexCount)

    // Set default vertex color to white for unlit meshes (objects/characters use stride-5, no color attribute)
    glVertexAttrib3f(2, 1f, 1f, 1f)

    // Draw placed objects
    for obj <- objectInstances do
      shader.setMatrix4f("model", obj.modelMatrix)
      for group <- obj.zoneMesh.groups do
        if group.materialType != MaterialType.Invisible && group.materialType != MaterialType.Boundary then
          val texId = if group.textureName.nonEmpty then
            textureMap.getOrElse(group.textureName.toLowerCase, fallbackTexture)
          else
            fallbackTexture
          glBindTexture(GL_TEXTURE_2D, texId)
          obj.glMesh.drawRange(group.startIndex, group.indexCount)

    // Update and draw animated characters
    for char <- characterInstances do
      char.update(deltaTime)
      shader.setMatrix4f("model", char.modelMatrix)
      for group <- char.zoneMesh.groups do
        if group.materialType != MaterialType.Invisible && group.materialType != MaterialType.Boundary then
          val texId = if group.textureName.nonEmpty then
            textureMap.getOrElse(group.textureName.toLowerCase, fallbackTexture)
          else
            fallbackTexture
          glBindTexture(GL_TEXTURE_2D, texId)
          char.glMesh.drawRange(group.startIndex, group.indexCount)

  def cleanup(): Unit =
    mesh.cleanup()
    objectInstances.foreach(_.glMesh.cleanup())
    characterInstances.foreach(_.glMesh.cleanup())
    textureMap.values.foreach(glDeleteTextures(_))
    glDeleteTextures(fallbackTexture)

  private def buildInterleaved(zm: ZoneMesh): Array[Float] =
    val vc = zm.vertices.length / 3
    val arr = new Array[Float](vc * 5)
    for i <- 0 until vc do
      val eqX = zm.vertices(i * 3 + 0)
      val eqY = zm.vertices(i * 3 + 1)
      val eqZ = zm.vertices(i * 3 + 2)
      arr(i * 5 + 0) = eqX
      arr(i * 5 + 1) = eqZ
      arr(i * 5 + 2) = -eqY
      if i * 2 + 1 < zm.uvs.length then
        arr(i * 5 + 3) = zm.uvs(i * 2 + 0)
        arr(i * 5 + 4) = zm.uvs(i * 2 + 1)
    arr

  private def loadTextures(s3dEntries: List[PfsEntry]): Unit =
    val bmpEntries = s3dEntries.filter(_.extension == "bmp")
    for entry <- bmpEntries do
      val key = entry.name.toLowerCase
      if !textureMap.contains(key) then
        try
          val texId = Texture.loadFromBytes(entry.data)
          textureMap(key) = texId
        catch case _: Exception => ()

  private def loadObjects(): List[ObjectRenderData] =
    // Find objects.wld in the zone S3D
    val objectsWldOpt = entries.find(e => e.name == "objects.wld")
    if objectsWldOpt.isEmpty then return Nil

    val objectsWld = WldFile(objectsWldOpt.get.data)
    val placements = objectsWld.fragmentsOfType[Fragment15_ObjectInstance]

    if placements.isEmpty then return Nil

    // Load the _obj.s3d for object meshes
    val objS3dPath = s3dPath.replace(".s3d", "_obj.s3d")
    if !Files.exists(Path.of(objS3dPath)) then
      println(s"  Object S3D not found: $objS3dPath")
      return Nil

    val objEntries = PfsArchive.load(Path.of(objS3dPath))
    loadTextures(objEntries)

    val objWldEntry = objEntries.find(_.extension == "wld")
    if objWldEntry.isEmpty then return Nil

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
        // Create a temporary WLD-like context for ZoneGeometry extraction
        // We need the material chain from objWld
        val zm = extractMeshGeometry(objWld, meshFragments)
        Some((actorKey, zm))
      else None
    }.toMap

    println(s"  Object models: ${actorMeshes.size} (${actorMeshes.keys.mkString(", ")})")

    // Create render data for each placement
    val results = placements.flatMap { placement =>
      actorMeshes.get(placement.actorName).map { zm =>
        val interleaved = buildInterleaved(zm)
        val glMesh = Mesh(interleaved, zm.indices)
        val modelMatrix = buildModelMatrix(placement)
        ObjectRenderData(zm, glMesh, modelMatrix)
      }
    }

    println(s"  Placed objects: ${results.size}")
    results

  // Character models live in {zone}_chr.s3d. Unlike zone objects which have pre-placed instances,
  // characters are spawned dynamically by the server. The fragment chain is:
  // Actor(0x14) → SkeletonHierarchyRef(0x11) → SkeletonHierarchy(0x10) → MeshReference(0x2D) → Mesh(0x36)
  // Each bone references a TrackRef(0x13) → TrackDef(0x12) containing transform keyframes.
  // Frame 0 is the rest pose. We compute bone world transforms by walking the parent chain
  // and apply them to vertices (which are stored in bone-local space) to assemble the model.
  private def loadCharacters(): List[AnimatedCharacter] =
    val chrS3dPath = s3dPath.replace(".s3d", "_chr.s3d")
    if !Files.exists(Path.of(chrS3dPath)) then return Nil

    val chrEntries = PfsArchive.load(Path.of(chrS3dPath))
    loadTextures(chrEntries)

    val chrWldEntry = chrEntries.find(_.extension == "wld")
    if chrWldEntry.isEmpty then return Nil

    val chrWld = WldFile(chrWldEntry.get.data)
    val actors = chrWld.fragmentsOfType[Fragment14_Actor]

    println(s"  Character actors: ${actors.size} (${actors.map(_.name.replace("_ACTORDEF", "").toLowerCase).mkString(", ")})")

    case class CharBuild(key: String, skeleton: Fragment10_SkeletonHierarchy, meshFragments: List[Fragment36_Mesh],
                         zm: ZoneMesh, glWidth: Float, glDepth: Float, glHeight: Float,
                         glCenterX: Float, glCenterZ: Float, glMinY: Float, clips: Map[String, opennorrath.animation.AnimationClip])

    val builds = actors.flatMap { actor =>
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      val (skeletonOpt, meshFragments) = resolveActorMeshes(chrWld, actor)

      if meshFragments.isEmpty || skeletonOpt.isEmpty then
        println(s"    $actorKey: no meshes or skeleton found")
        None
      else
        val sk = skeletonOpt.get

        // Discover animations
        val clips = AnimatedCharacter.discoverAnimations(chrWld, sk)

        // Use the default animation's frame 0 for initial pose and bounding box,
        // so placement matches what the player actually sees during animation.
        // Fall back to rest pose (base tracks) if no animations found.
        val defaultClip = clips.get("L01").orElse(clips.get("P01")).orElse(clips.headOption.map(_._2))
        val boneTransforms = defaultClip match
          case Some(clip) => AnimatedCharacter.computeBoneTransforms(sk, clip, 0)
          case None => sk.boneWorldTransforms(chrWld)
        val transformedMeshes = meshFragments.map(mesh => applyBoneTransforms(mesh, boneTransforms))
        val zm = extractMeshGeometry(chrWld, transformedMeshes)

        // Compute bounding box in EQ space then convert to GL
        val vc = zm.vertices.length / 3
        var eqMinX = Float.MaxValue; var eqMaxX = Float.MinValue
        var eqMinY = Float.MaxValue; var eqMaxY = Float.MinValue
        var eqMinZ = Float.MaxValue; var eqMaxZ = Float.MinValue
        for i <- 0 until vc do
          val x = zm.vertices(i * 3); val y = zm.vertices(i * 3 + 1); val z = zm.vertices(i * 3 + 2)
          if x < eqMinX then eqMinX = x; if x > eqMaxX then eqMaxX = x
          if y < eqMinY then eqMinY = y; if y > eqMaxY then eqMaxY = y
          if z < eqMinZ then eqMinZ = z; if z > eqMaxZ then eqMaxZ = z
        val glWidth = eqMaxX - eqMinX
        val glHeight = eqMaxZ - eqMinZ
        val glDepth = eqMaxY - eqMinY
        val glCenterX = (eqMinX + eqMaxX) / 2f
        val glCenterZ = -(eqMinY + eqMaxY) / 2f
        val glMinY = eqMinZ

        val animInfo = if clips.nonEmpty then
          clips.map((code, clip) => s"$code(${clip.frameCount}f)").mkString(", ")
        else "none"
        println(s"    $actorKey: ${meshFragments.size} meshes, ${zm.vertices.length / 3} verts, anims: $animInfo")
        Some(CharBuild(actorKey, sk, meshFragments, zm, glWidth, glDepth, glHeight, glCenterX, glCenterZ, glMinY, clips))
    }

    // Place characters in a line, normalized to similar display height
    val targetHeight = 50f
    var xCursor = -370f
    val results = builds.map { build =>
      val interleaved = buildInterleaved(build.zm)
      val glMesh = Mesh(interleaved, build.zm.indices, dynamic = build.clips.nonEmpty)

      val scale = if build.glHeight > 0 then targetHeight / build.glHeight else 10f
      val halfExtent = math.max(build.glWidth, build.glDepth) * scale / 2f

      xCursor += halfExtent
      val modelMatrix = Matrix4f()
      modelMatrix.translate(xCursor, 0f, -270f)
      modelMatrix.scale(scale)
      modelMatrix.translate(-build.glCenterX, -build.glMinY, -build.glCenterZ)
      xCursor += halfExtent + 15f

      AnimatedCharacter(build.skeleton, build.meshFragments, build.zm, glMesh, modelMatrix, build.clips, interleaved.clone())
    }

    println(s"  Characters placed: ${results.size}")
    results

  private def resolveActorMeshes(wld: WldFile, actor: Fragment14_Actor): (Option[Fragment10_SkeletonHierarchy], List[Fragment36_Mesh]) =
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

  private def extractMeshGeometry(objWld: WldFile, meshFragments: List[Fragment36_Mesh]): ZoneMesh =
    var allVertices = Array.empty[Float]
    var allUvs = Array.empty[Float]
    var allIndices = Array.empty[Int]
    var allGroups = List.empty[ZoneMeshGroup]

    for mesh <- meshFragments do
      val vertexOffset = allVertices.length / 3

      val verts = mesh.vertices.flatMap(v => Array(v.x, v.y, v.z))
      allVertices = allVertices ++ verts

      val uvs = mesh.uvs.flatMap((u, v) => Array(u, v))
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

  /** Transform mesh vertices from bone-local space to model space using skeleton rest pose */
  private def applyBoneTransforms(mesh: Fragment36_Mesh, boneTransforms: Array[Matrix4f]): Fragment36_Mesh =
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

  private def buildModelMatrix(placement: Fragment15_ObjectInstance): Matrix4f =
    val pos = placement.position
    val rot = placement.rotation
    val scl = placement.scale

    // EQ coordinates → GL coordinates: (X, Z, -Y)
    val mat = Matrix4f()
    mat.translate(pos.x, pos.z, -pos.y)
    // Apply rotations (EQ rotation order)
    mat.rotateY(Math.toRadians(-rot.z).toFloat)  // heading (Z rot in EQ → Y rot in GL)
    mat.rotateX(Math.toRadians(rot.y).toFloat)    // pitch
    mat.rotateZ(Math.toRadians(rot.x).toFloat)    // roll
    // Scale - use Y as uniform scale if others are 0
    val s = if scl.y != 0 then scl.y else 1f
    mat.scale(s)
    mat

case class ObjectRenderData(zoneMesh: ZoneMesh, glMesh: Mesh, modelMatrix: Matrix4f)
