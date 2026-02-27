package opennorrath.wld

import org.joml.Vector3f

case class TextureAnim(frames: Vector[String], delayMs: Int)

case class ZoneMeshGroup(startIndex: Int, indexCount: Int, textureName: String, materialType: MaterialType, anim: Option[TextureAnim] = None)

case class ZoneMesh(
    vertices: Array[Float],  // x, y, z interleaved (S3D space)
    uvs: Array[Float],       // u, v interleaved
    normals: Array[Float],   // nx, ny, nz interleaved (S3D space, parallel to vertices)
    indices: Array[Int],
    groups: List[ZoneMeshGroup],
)

// --- Zone line detection via BSP ---
//
// The S3D WLD file contains a BSP tree that partitions the zone into regions.
// Three fragment types work together:
//
//   0x21 BspTree — flat array of BspNode. Each node is either:
//     - internal: has a split plane (normal + distance) and left/right children
//     - leaf: has regionId > 0, meaning the point is in that region
//     Traversal: dot(normal, point) >= splitDistance → go left, else right.
//     Children are 1-based indices into the node array.
//
//   0x22 BspRegion — one per leaf region, ordered. The Nth 0x22 fragment
//     corresponds to regionId N+1 in the BSP tree (regions are 1-based).
//     Contains bounding geometry for the convex volume.
//
//   0x29 BspRegionType — tags groups of regions with a type string.
//     Known prefixes (from EQEmu azone2/wld.cpp):
//       WT    = 1 Water
//       LA    = 2 Lava
//       DRNTP = 3 Zone Line
//       DRP_  = 4 PVP
//       SL    = 5 Slippery/Slime
//       DRN   = 6 Ice/Ice Water (checked after DRNTP so it won't match zone lines)
//       VWN   = 7 Ice/Ice Water (variant?)
//     The type is determined from both the fragment name (string hash) and from
//     an encoded string inside the fragment body; the body string takes priority.
//     NOTE: our assumption that digits after "DRNTP" encode a destination zone ID
//     (e.g. "drntp00032" → zone 32) is unverified — azone2 only checks the prefix.
//     regionIndices are 0-based indices into the ordered list of 0x22 fragments,
//     so they map to BSP regionId = index + 1.
//
// The client is responsible for zone line detection: traverse the BSP tree with
// the player's S3D-space position, check if the leaf region is tagged as a zone
// line by 0x29, and if so send OP_ZoneChange with the target zone ID. The server
// validates using trigger coordinates from its zone_points DB table (which are
// never sent to the client).

/** Zone line info from a 0x29 DRNTP region type string (e.g. "DRNTP00255000001_ZONE"). */
case class ZoneLineInfo(
    regionName: String,    // raw 0x29 string
    param1: Int,           // 5-digit group after DRNTP (e.g. 255), -1 if absent
    param2: Int,           // 6-digit group (e.g. 1), -1 if absent
)

/** Bounding sphere from a 0x22 BSP region, used as a fallback zone line trigger. */
case class ZoneLineSphere(
    info: ZoneLineInfo,
    cx: Float, cy: Float, cz: Float,  // center in S3D coordinates
    radius: Float,
)

/** Traverses the 0x21 BSP tree to find which 0x22 region a point is in,
  * then checks if that region is a zone line (tagged by 0x29).
  * All coordinates are in S3D space.
  */
class ZoneLineBsp(
    val nodes: Array[BspNode],
    val zoneLineRegions: Map[Int, ZoneLineInfo],  // regionId (1-based) → zone line info
    val spheres: Vector[ZoneLineSphere] = Vector.empty,
):
  /** Check if an S3D-space point is inside a zone line region.
    * Returns the zone line info if so.
    */
  def check(s3dX: Float, s3dY: Float, s3dZ: Float): Option[ZoneLineInfo] =
    if nodes.isEmpty then return None
    var idx = 0
    var depth = 0
    while depth < 1000 do  // safety limit
      if idx < 0 || idx >= nodes.length then return None
      val node = nodes(idx)
      if node.regionId > 0 then
        // Leaf node — check if this region is a zone line
        return zoneLineRegions.get(node.regionId)
      // Internal node — test which side of the split plane
      val dot = node.normalX * s3dX + node.normalY * s3dY + node.normalZ * s3dZ
      if dot >= node.splitDistance then
        idx = node.leftNode - 1   // 1-based → 0-based
      else
        idx = node.rightNode - 1
      depth += 1
    None

  /** Return the BSP leaf region ID for a point (for debug display). */
  def regionAt(s3dX: Float, s3dY: Float, s3dZ: Float): Int =
    if nodes.isEmpty then return -1
    var idx = 0
    var depth = 0
    while depth < 1000 do
      if idx < 0 || idx >= nodes.length then return -1
      val node = nodes(idx)
      if node.regionId > 0 then return node.regionId
      val dot = node.normalX * s3dX + node.normalY * s3dY + node.normalZ * s3dZ
      if dot >= node.splitDistance then
        idx = node.leftNode - 1
      else
        idx = node.rightNode - 1
      depth += 1
    -1

  /** Trace the BSP traversal for debugging. */
  def traceAt(s3dX: Float, s3dY: Float, s3dZ: Float): Unit =
    if nodes.isEmpty then { println("[BSP trace] empty"); return }
    var idx = 0
    var depth = 0
    while depth < 20 do  // limit trace depth
      if idx < 0 || idx >= nodes.length then
        println(f"[BSP trace] OOB at depth=$depth idx=$idx (max=${nodes.length})")
        return
      val node = nodes(idx)
      if node.regionId > 0 then
        println(f"[BSP trace] LEAF depth=$depth idx=$idx regionId=${node.regionId}")
        return
      val dot = node.normalX * s3dX + node.normalY * s3dY + node.normalZ * s3dZ
      val side = if dot >= node.splitDistance then "L" else "R"
      val nextIdx = if dot >= node.splitDistance then node.leftNode - 1 else node.rightNode - 1
      println(f"[BSP trace] depth=$depth idx=$idx n=(${node.normalX}%.2f,${node.normalY}%.2f,${node.normalZ}%.2f) d=${node.splitDistance}%.2f dot=${dot}%.2f → $side next=$nextIdx")
      idx = nextIdx
      depth += 1

  /** Check if an S3D-space point is inside any zone line sphere. */
  def checkSphere(s3dX: Float, s3dY: Float, s3dZ: Float): Option[ZoneLineInfo] =
    var i = 0
    while i < spheres.length do
      val s = spheres(i)
      val dx = s3dX - s.cx; val dy = s3dY - s.cy; val dz = s3dZ - s.cz
      if dx * dx + dy * dy + dz * dz <= s.radius * s.radius then
        return Some(s.info)
      i += 1
    None

  def isEmpty: Boolean = (nodes.isEmpty || zoneLineRegions.isEmpty) && spheres.isEmpty

object ZoneGeometry:

  def extract(wld: WldFile): ZoneMesh =
    val meshes = wld.fragmentsOfType[Fragment36_Mesh]

    var allVertices = Array.empty[Float]
    var allUvs = Array.empty[Float]
    var allNormals = Array.empty[Float]
    var allIndices = Array.empty[Int]
    var allGroups = List.empty[ZoneMeshGroup]

    for mesh <- meshes do
      val vertexOffset = allVertices.length / 3

      // Append vertices
      val verts = mesh.vertices.flatMap(v => Array(v.x, v.y, v.z))
      allVertices = allVertices ++ verts

      // Append UVs — pad to match vertex count so flat arrays stay aligned
      val paddedUvs = if mesh.uvs.length < mesh.vertices.length then
        mesh.uvs ++ Array.fill(mesh.vertices.length - mesh.uvs.length)((0f, 0f))
      else
        mesh.uvs.take(mesh.vertices.length)
      val uvs = paddedUvs.flatMap((u, v) => Array(u, v))
      allUvs = allUvs ++ uvs

      // Append normals — stored in S3D space like vertices; pad with up (0,0,1) if missing
      val paddedNormals = if mesh.normals.length >= mesh.vertices.length then
        mesh.normals.take(mesh.vertices.length)
      else
        mesh.normals ++ Array.fill(mesh.vertices.length - mesh.normals.length)(Vector3f(0f, 0f, 1f))
      val norms = paddedNormals.flatMap(n => Array(n.x, n.y, n.z))
      allNormals = allNormals ++ norms

      // Process render groups to get material-grouped triangles
      var polyIndex = 0
      for group <- mesh.renderGroups do
        val startIndex = allIndices.length
        val textureName = resolveTextureName(wld, mesh.materialListRef, group.materialIndex)
        val matType = resolveMaterialType(wld, mesh.materialListRef, group.materialIndex)
        val anim = resolveTextureAnim(wld, mesh.materialListRef, group.materialIndex)

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
          allGroups = allGroups :+ ZoneMeshGroup(startIndex, indexCount, textureName, matType, anim)

    ZoneMesh(allVertices, allUvs, allNormals, allIndices, allGroups)

  def resolveTextureName(wld: WldFile, materialListRef: Int, materialIndex: Int): String =
    try
      val matList = wld.fragment(materialListRef).asInstanceOf[Fragment31_MaterialList]
      val matRef = matList.materialRefs(materialIndex)
      val material = wld.fragment(matRef).asInstanceOf[Fragment30_Material]

      if material.bitmapInfoRefIndex == 0 then return ""

      val bitmapInfoRef = wld.fragment(material.bitmapInfoRefIndex).asInstanceOf[Fragment05_BitmapInfoRef]
      val bitmapInfo = wld.fragment(bitmapInfoRef.refIndex).asInstanceOf[Fragment04_BitmapInfo]

      if bitmapInfo.textureRefs.isEmpty then return ""

      val bitmapName = wld.fragment(bitmapInfo.textureRefs.head).asInstanceOf[Fragment03_BitmapName]
      bitmapName.filename
    catch case _: Exception => ""

  def resolveTextureAnim(wld: WldFile, materialListRef: Int, materialIndex: Int): Option[TextureAnim] =
    try
      val matList = wld.fragment(materialListRef).asInstanceOf[Fragment31_MaterialList]
      val matRef = matList.materialRefs(materialIndex)
      val material = wld.fragment(matRef).asInstanceOf[Fragment30_Material]
      if material.bitmapInfoRefIndex == 0 then return None
      val bitmapInfoRef = wld.fragment(material.bitmapInfoRefIndex).asInstanceOf[Fragment05_BitmapInfoRef]
      val bitmapInfo = wld.fragment(bitmapInfoRef.refIndex).asInstanceOf[Fragment04_BitmapInfo]
      if !bitmapInfo.isAnimated || bitmapInfo.textureRefs.size < 2 then return None
      val frames = bitmapInfo.textureRefs.flatMap { ref =>
        try Some(wld.fragment(ref).asInstanceOf[Fragment03_BitmapName].filename)
        catch case _: Exception => None
      }.toVector
      if frames.size < 2 then None
      else Some(TextureAnim(frames, bitmapInfo.animDelay))
    catch case _: Exception => None

  def resolveMaterialType(wld: WldFile, materialListRef: Int, materialIndex: Int): MaterialType =
    try
      val matList = wld.fragment(materialListRef).asInstanceOf[Fragment31_MaterialList]
      val matRef = matList.materialRefs(materialIndex)
      val material = wld.fragment(matRef).asInstanceOf[Fragment30_Material]
      material.shaderType
    catch case _: Exception => MaterialType.Diffuse

  /** Build a BSP-based zone line detector from the WLD file.
    * Uses 0x21 (BSP tree), 0x22 (BSP regions), and 0x29 (region types).
    */
  def extractZoneLineBsp(wld: WldFile): ZoneLineBsp =
    val bspTrees = wld.fragmentsOfType[Fragment21_BspTree]
    val regionTypes = wld.fragmentsOfType[Fragment29_BspRegionType]

    val bspRegions = wld.fragmentsOfType[Fragment22_BspRegion]
    // Log unknown fragment types to see what we're not parsing
    val unknowns = wld.fragmentsOfType[UnknownFragment].groupBy(_.fragmentType).map((t, fs) => f"0x$t%02x(${fs.size})").mkString(", ")
    println(s"  BSP fragments: ${bspTrees.size} trees (0x21), ${bspRegions.size} regions (0x22), ${regionTypes.size} types (0x29)")
    println(s"  Unknown fragment types: $unknowns")
    for rt <- regionTypes do
      println(s"    0x29 '${rt.name}' str='${rt.regionString}' indices=${rt.regionIndices.mkString(",")}")

    if bspTrees.isEmpty then return ZoneLineBsp(Array.empty, Map.empty)
    val tree = bspTrees.head

    // Build mapping: regionId (1-based index into 0x22 list) → zone line info
    val zoneLineMap = scala.collection.mutable.Map[Int, ZoneLineInfo]()
    for rt <- regionTypes do
      if rt.regionType == BspRegionKind.ZoneLine then
        val info = ZoneLineInfo(rt.regionString, rt.zoneLineParam1, rt.zoneLineParam2)
        for idx <- rt.regionIndices do
          // 0x29 indices are 0-based, BSP tree regionIds are 1-based
          zoneLineMap(idx + 1) = info
        println(s"  Zone line: '${rt.regionString}' → param1=${rt.zoneLineParam1} param2=${rt.zoneLineParam2} regions=${rt.regionIndices.mkString(",")}")

    // Build zone line spheres from 0x22 bounding spheres
    val zoneLineSpheres = scala.collection.mutable.ArrayBuffer[ZoneLineSphere]()
    for rt <- regionTypes do
      if rt.regionType == BspRegionKind.ZoneLine then
        val info = ZoneLineInfo(rt.regionString, rt.zoneLineParam1, rt.zoneLineParam2)
        for idx <- rt.regionIndices do
          if idx >= 0 && idx < bspRegions.size then
            val region = bspRegions(idx)
            if region.sphereRadius > 0 then
              zoneLineSpheres += ZoneLineSphere(info, region.sphereX, region.sphereY, region.sphereZ, region.sphereRadius)
              println(f"  Zone line sphere: '${rt.regionString}' idx=$idx center=(${region.sphereX}%.1f,${region.sphereY}%.1f,${region.sphereZ}%.1f) r=${region.sphereRadius}%.1f")
            else
              println(s"  Zone line region $idx has no sphere (flags=0x${region.flags.toHexString})")

    println(s"  BSP: ${tree.nodes.length} nodes, ${zoneLineMap.size} BSP zone lines, ${zoneLineSpheres.size} sphere zone lines")
    ZoneLineBsp(tree.nodes, zoneLineMap.toMap, zoneLineSpheres.toVector)

