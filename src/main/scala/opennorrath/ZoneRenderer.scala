package opennorrath

import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import java.nio.file.{Path, Files}

class ZoneRenderer(s3dPath: String):

  private val entries = PfsArchive.load(Path.of(s3dPath))
  private val zoneWld = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
    .getOrElse(throw RuntimeException(s"No zone WLD found in $s3dPath"))

  private val wld = WldFile(zoneWld.data)
  private val zoneMesh = ZoneGeometry.extract(wld)

  // Build interleaved vertex buffer: position (3) + uv (2)
  // EQ uses Z-up; OpenGL/our camera uses Y-up. Swap: EQ(X,Y,Z) → GL(X,Z,-Y)
  private val vertexCount = zoneMesh.vertices.length / 3
  private val interleavedVertices: Array[Float] = buildInterleaved(zoneMesh)

  // Load textures from S3D entries, keyed by lowercase filename
  private val textureMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty

  loadTextures(entries)

  private val fallbackTexture = Texture.createCheckerboard(64, 8)

  val mesh = Mesh(interleavedVertices, zoneMesh.indices)

  println(s"Zone loaded: $vertexCount vertices, ${zoneMesh.indices.length / 3} triangles, ${textureMap.size} textures")

  // Load objects
  private val objectInstances: List[ObjectRenderData] = loadObjects()

  def draw(shader: Shader): Unit =
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

  def cleanup(): Unit =
    mesh.cleanup()
    objectInstances.foreach(_.glMesh.cleanup())
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
