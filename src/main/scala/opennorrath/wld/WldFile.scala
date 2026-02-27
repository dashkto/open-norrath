package opennorrath.wld

import org.joml.{Quaternionf, Vector3f}
import java.nio.{ByteBuffer, ByteOrder}

class WldFile(data: Array[Byte]):

  private val Magic = 0x54503D02
  private val OldVersion = 0x00015500

  private val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)

  private val XorKey = Array[Byte](
    0x95.toByte, 0x3A.toByte, 0xC5.toByte, 0x2A.toByte,
    0x95.toByte, 0x7A.toByte, 0x95.toByte, 0x6A.toByte,
  )

  val (isOldFormat: Boolean, stringHash: WldStringHash, fragments: Array[WldFragment]) = parse()

  def fragment(index: Int): WldFragment = fragments(index - 1) // 1-based

  def fragmentsOfType[T <: WldFragment](using ct: reflect.ClassTag[T]): List[T] =
    fragments.collect { case f: T => f }.toList

  private def parse(): (Boolean, WldStringHash, Array[WldFragment]) =
    val magic = buf.getInt()
    if magic != Magic then
      throw RuntimeException(f"Invalid WLD magic: 0x$magic%08X (expected 0x$Magic%08X)")

    val version = buf.getInt()
    val oldFormat = version == OldVersion
    val fragmentCount = buf.getInt()
    val _bspRegionCount = buf.getInt()
    val _unknown = buf.getInt()
    val hashSize = buf.getInt()
    val _unknown2 = buf.getInt()

    val hashBytes = new Array[Byte](hashSize)
    buf.get(hashBytes)
    val hash = WldStringHash(hashBytes)

    val frags = new Array[WldFragment](fragmentCount)
    for i <- 0 until fragmentCount do
      val size = buf.getInt()
      val fragType = buf.getInt()
      val startPos = buf.position()

      frags(i) = parseFragment(fragType, size, hash, oldFormat)

      // Ensure we advance past the full fragment regardless of how much we read
      buf.position(startPos + size)

    (oldFormat, hash, frags)

  private def parseFragment(fragType: Int, size: Int, hash: WldStringHash, oldFormat: Boolean): WldFragment =
    val nameRef = buf.getInt()
    val name = hash.lookup(nameRef)

    fragType match
      case 0x03 => parseFragment03(name)
      case 0x04 => parseFragment04(name)
      case 0x05 => parseFragment05(name)
      case 0x08 => parseFragment08(name)
      case 0x09 => parseFragment09(name)
      case 0x10 => parseFragment10(name)
      case 0x11 => parseFragment11(name)
      case 0x12 => parseFragment12(name)
      case 0x13 => parseFragment13(name)
      case 0x14 => parseFragment14(name)
      case 0x15 => parseFragment15(name, hash)
      case 0x16 => parseFragment16(name)
      case 0x1B => parseFragment1B(name)
      case 0x1C => parseFragment1C(name)
      case 0x21 => parseFragment21(name)
      case 0x22 => parseFragment22(name)
      case 0x29 => parseFragment29(name)
      case 0x2A => parseFragment2A(name)
      case 0x2D => parseFragment2D(name)
      case 0x30 => parseFragment30(name)
      case 0x31 => parseFragment31(name)
      case 0x35 => parseFragment35(name)
      case 0x36 => parseFragment36(name, oldFormat)
      case _    => UnknownFragment(name, fragType, size)

  private def parseFragment08(name: String): Fragment08_Camera =
    // 26 mixed int32/float parameters, purpose unknown. Data skipped by outer loop.
    Fragment08_Camera(name)

  private def parseFragment09(name: String): Fragment09_CameraRef =
    val cameraRef = buf.getInt()
    val _flags = buf.getInt()
    Fragment09_CameraRef(name, cameraRef)

  private def parseFragment16(name: String): Fragment16_Unknown =
    val value = buf.getFloat()
    Fragment16_Unknown(name, value)

  private def parseFragment1B(name: String): Fragment1B_LightSource =
    val flags = buf.getInt()
    val frameCount = buf.getInt()
    val isPlaced = (flags & 0x02) != 0
    val isColored = (flags & 0x10) != 0
    if (flags & 0x01) != 0 then buf.position(buf.position() + 12)
    if isPlaced then buf.position(buf.position() + 4)
    var (r, g, b, a) = (1f, 1f, 1f, 1f)
    var attenuation = 0
    if isColored && frameCount > 0 then
      attenuation = buf.getInt()
      r = buf.getFloat()
      g = buf.getFloat()
      b = buf.getFloat()
      a = buf.getFloat()
    Fragment1B_LightSource(name, flags, isPlaced, isColored, attenuation, r, g, b, a)

  private def parseFragment1C(name: String): Fragment1C_LightSourceRef =
    val lightRef = buf.getInt()
    val _flags = buf.getInt()
    Fragment1C_LightSourceRef(name, lightRef)

  private def parseFragment2A(name: String): Fragment2A_AmbientLight =
    val lightRef = buf.getInt()
    val _flags = buf.getInt()
    val regionCount = buf.getInt()
    val regionIds = (0 until regionCount).map(_ => buf.getInt()).toArray
    Fragment2A_AmbientLight(name, lightRef, regionIds)

  private def parseFragment35(name: String): Fragment35_GlobalAmbientLight =
    val color = buf.getInt()
    val b = ((color >> 0) & 0xFF) / 255f
    val g = ((color >> 8) & 0xFF) / 255f
    val r = ((color >> 16) & 0xFF) / 255f
    val a = ((color >> 24) & 0xFF) / 255f
    Fragment35_GlobalAmbientLight(name, r, g, b, a)

  private def parseFragment03(name: String): Fragment03_BitmapName =
    val count = buf.getInt()
    val nameLen = java.lang.Short.toUnsignedInt(buf.getShort())
    val encoded = new Array[Byte](nameLen)
    buf.get(encoded)
    val hashKey = Array[Byte](
      0x95.toByte, 0x3A.toByte, 0xC5.toByte, 0x2A.toByte,
      0x95.toByte, 0x7A.toByte, 0x95.toByte, 0x6A.toByte,
    )
    val decoded = encoded.zipWithIndex.map { (b, i) => (b ^ hashKey(i % hashKey.length)).toByte }
    // Strip null terminator
    val end = decoded.indexOf(0.toByte)
    val len = if end < 0 then decoded.length else end
    val filename = String(decoded, 0, len, "US-ASCII")
    Fragment03_BitmapName(name, filename)

  private def parseFragment04(name: String): Fragment04_BitmapInfo =
    val flags = buf.getInt()
    val count = buf.getInt()
    val isAnimated = (flags & 0x08) != 0
    val animDelay = if isAnimated then buf.getInt() else 0
    val refs = (0 until count).map(_ => buf.getInt()).toList
    Fragment04_BitmapInfo(name, flags, isAnimated, animDelay, refs)

  private def parseFragment05(name: String): Fragment05_BitmapInfoRef =
    val ref = buf.getInt()
    val _flags = buf.getInt()
    Fragment05_BitmapInfoRef(name, ref)

  private def parseFragment10(name: String): Fragment10_SkeletonHierarchy =
    val flags = buf.getInt()
    val hasBit0 = (flags & 0x01) != 0
    val hasBoundingRadius = (flags & 0x02) != 0
    val hasMeshReferences = (flags & 0x200) != 0

    val boneCount = buf.getInt()
    val _fragment18Ref = buf.getInt()

    if hasBit0 then buf.position(buf.position() + 12)
    if hasBoundingRadius then buf.position(buf.position() + 4)

    // Read bones with hierarchy info
    val boneData = new Array[(Int, Int, List[Int])](boneCount) // (trackRef, meshRef, children)
    for i <- 0 until boneCount do
      val _boneNameRef = buf.getInt()
      val _boneFlags = buf.getInt()
      val trackRef = buf.getInt()
      val meshRef = buf.getInt()
      val childCount = buf.getInt()
      val children = (0 until childCount).map(_ => buf.getInt()).toList
      boneData(i) = (trackRef, meshRef, children)

    // Build parent index lookup from children
    val parentIndex = new Array[Int](boneCount)
    java.util.Arrays.fill(parentIndex, -1)
    for i <- 0 until boneCount do
      for child <- boneData(i)._3 do
        if child >= 0 && child < boneCount then
          parentIndex(child) = i

    val bones = (0 until boneCount).map { i =>
      val (trackRef, meshRef, children) = boneData(i)
      SkeletonBone(trackRef, meshRef, parentIndex(i), children)
    }.toArray

    val boneMeshRefs = bones.filter(_.meshRef > 0).map(_.meshRef).toList

    // Trailing mesh reference list
    val trailingMeshRefs = if hasMeshReferences then
      val meshCount = buf.getInt()
      val refs = (0 until meshCount).map(_ => buf.getInt()).toList
      buf.position(buf.position() + meshCount * 4)
      refs
    else Nil

    Fragment10_SkeletonHierarchy(name, (boneMeshRefs ++ trailingMeshRefs).distinct, bones)

  private def parseFragment11(name: String): Fragment11_SkeletonHierarchyRef =
    val skeletonRef = buf.getInt()
    Fragment11_SkeletonHierarchyRef(name, skeletonRef)

  private def parseFragment12(name: String): Fragment12_TrackDef =
    val flags = buf.getInt()
    val isCompressed = (flags & 0x08) != 0
    val frameCount = buf.getInt()

    // Compressed: 8 × int16 per frame (rotation as raw quaternion, translation/scale as fixed-point / 256)
    // Uncompressed: 8 × float32 per frame (scale, translation xyz, rotation wxyz)
    val frames = if isCompressed then
      (0 until frameCount).map { _ =>
        val rotW = buf.getShort()
        val rotX = buf.getShort()
        val rotY = buf.getShort()
        val rotZ = buf.getShort()
        val shiftX = buf.getShort()
        val shiftY = buf.getShort()
        val shiftZ = buf.getShort()
        val shiftDenom = buf.getShort()
        val scale = if shiftDenom != 0 then shiftDenom.toFloat / 256f else 1f
        val translation = if shiftDenom != 0 then
          Vector3f(shiftX.toFloat / 256f, shiftY.toFloat / 256f, shiftZ.toFloat / 256f)
        else Vector3f(0f, 0f, 0f)
        val rotation = Quaternionf(rotX.toFloat, rotY.toFloat, rotZ.toFloat, rotW.toFloat).normalize()
        BoneTransform(translation, rotation, scale)
      }.toArray
    else
      (0 until frameCount).map { _ =>
        val shiftDenom = buf.getFloat()
        val shiftX = buf.getFloat()
        val shiftY = buf.getFloat()
        val shiftZ = buf.getFloat()
        val rotW = buf.getFloat()
        val rotX = buf.getFloat()
        val rotY = buf.getFloat()
        val rotZ = buf.getFloat()
        val rotation = Quaternionf(rotX, rotY, rotZ, rotW).normalize()
        BoneTransform(Vector3f(shiftX, shiftY, shiftZ), rotation, shiftDenom)
      }.toArray

    Fragment12_TrackDef(name, frames)

  private def parseFragment13(name: String): Fragment13_TrackRef =
    val trackDefRef = buf.getInt()
    val _flags = buf.getInt()
    Fragment13_TrackRef(name, trackDefRef)

  private def parseFragment14(name: String): Fragment14_Actor =
    val flags = buf.getInt()
    val _callbackNameRef = buf.getInt()
    val size1 = buf.getInt()
    val componentCount = buf.getInt()
    val _fragment2 = buf.getInt()

    // Optional fields based on flags
    if (flags & 0x01) != 0 then buf.position(buf.position() + 4) // params1
    if (flags & 0x02) != 0 then buf.position(buf.position() + 28) // params2 (7 ints)

    // Size1 entries: each has a dataPairCount, then that many (int32 + int16 + int16) pairs
    for _ <- 0 until size1 do
      val dataPairCount = buf.getInt()
      for _ <- 0 until dataPairCount do
        buf.getInt()   // value
        buf.getShort() // value2
        buf.getShort() // value3

    // Read component references
    val componentRefs = (0 until componentCount).map(_ => buf.getInt()).toList

    Fragment14_Actor(name, componentRefs)

  private def parseFragment15(name: String, hash: WldStringHash): Fragment15_ObjectInstance =
    val actorNameRef = buf.getInt()
    // Actor name is a negative reference into string hash
    val rawActorName = hash.lookup(actorNameRef)
    // Strip _ACTORDEF suffix and lowercase
    val actorName = rawActorName.replace("_ACTORDEF", "").toLowerCase

    val _flags = buf.getInt()
    val _fragment1 = buf.getInt()

    val posX = buf.getFloat()
    val posY = buf.getFloat()
    val posZ = buf.getFloat()

    val rotZ = buf.getFloat() * (360f / 512f)
    val rotY = buf.getFloat() * (360f / 512f)
    val rotX = buf.getFloat() * (360f / 512f)

    val scaleY = buf.getFloat()
    val scaleX = buf.getFloat()
    val scaleZ = buf.getFloat()

    Fragment15_ObjectInstance(
      name,
      actorName,
      Vector3f(posX, posY, posZ),
      Vector3f(rotX, rotY, rotZ),
      Vector3f(scaleX, scaleY, scaleZ),
    )

  private def parseFragment21(name: String): Fragment21_BspTree =
    val nodeCount = buf.getInt()
    val nodes = (0 until nodeCount).map { _ =>
      val nx = buf.getFloat()
      val ny = buf.getFloat()
      val nz = buf.getFloat()
      val dist = buf.getFloat()
      val regionId = buf.getInt()
      val left = buf.getInt()
      val right = buf.getInt()
      BspNode(nx, ny, nz, dist, regionId, left, right)
    }.toArray
    Fragment21_BspTree(name, nodes)

  private def parseFragment22(name: String): Fragment22_BspRegion =
    // Walk forward through struct_Data22 like azone2/wld.cpp Data22 handler.
    // Fixed header: flags, fragment1, size1..size6 = 10 x int32 = 40 bytes
    val flags = buf.getInt()
    val _fragment1 = buf.getInt()
    val size1 = buf.getInt()
    val size2 = buf.getInt()
    val _params1 = buf.getInt()
    val size3 = buf.getInt()
    val size4 = buf.getInt()
    val _params2 = buf.getInt()
    val size5 = buf.getInt()
    val _size6 = buf.getInt()

    // Skip variable-length arrays
    buf.position(buf.position() + 12 * size1)  // array1: 12 bytes per entry
    buf.position(buf.position() + 8 * size2)   // array2: 8 bytes per entry
    // azone2 bails if size3 or size4 != 0; skip them if present
    buf.position(buf.position() + size3 * 4)
    buf.position(buf.position() + size4 * 4)
    buf.position(buf.position() + size5 * 7 * 4)  // array5: 28 bytes per entry
    // size6 data: uint16 length + that many bytes of RLE data
    val d6size = java.lang.Short.toUnsignedInt(buf.getShort())
    buf.position(buf.position() + d6size)

    // Sphere: 4 floats (x, y, z, radius) immediately after the RLE data
    val sx = buf.getFloat()
    val sy = buf.getFloat()
    val sz = buf.getFloat()
    val sr = buf.getFloat()
    Fragment22_BspRegion(name, flags, sx, sy, sz, sr)

  private def parseFragment29(name: String): Fragment29_BspRegionType =
    val _flags = buf.getInt()
    val regionCount = buf.getInt()
    val regionIndices = (0 until regionCount).map(_ => buf.getInt()).toArray
    val stringSize = buf.getInt()
    val regionString = if stringSize > 0 then
      val encoded = new Array[Byte](stringSize)
      buf.get(encoded)
      val decoded = encoded.zipWithIndex.map { (b, i) => (b ^ XorKey(i % XorKey.length)).toByte }
      val end = decoded.indexOf(0.toByte)
      val len = if end < 0 then decoded.length else end
      String(decoded, 0, len, "US-ASCII")
    else name

    Fragment29_BspRegionType(name, regionIndices, regionString)

  private def parseFragment2D(name: String): Fragment2D_MeshReference =
    val meshRef = buf.getInt()
    Fragment2D_MeshReference(name, meshRef)

  private def parseFragment30(name: String): Fragment30_Material =
    val flags = buf.getInt()
    val params = buf.getInt()
    val shaderType = MaterialType.fromParams(params)
    // color (4 bytes) + brightness (float) + scaledAmbient (float)
    buf.position(buf.position() + 12)
    // Bitmap reference is always present; 0 means no texture
    val bitmapRef = buf.getInt()
    Fragment30_Material(name, flags, shaderType, bitmapRef)

  private def parseFragment31(name: String): Fragment31_MaterialList =
    val _flags = buf.getInt()
    val count = buf.getInt()
    val refs = (0 until count).map(_ => buf.getInt()).toList
    Fragment31_MaterialList(name, refs)

  private def parseFragment36(name: String, oldFormat: Boolean): Fragment36_Mesh =
    val flags = buf.getInt()
    val materialListRef = buf.getInt()
    val _animRef = buf.getInt()
    buf.position(buf.position() + 8) // skip 2 unknowns

    val centerX = buf.getFloat()
    val centerY = buf.getFloat()
    val centerZ = buf.getFloat()
    val center = Vector3f(centerX, centerY, centerZ)

    buf.position(buf.position() + 12) // skip 3 unknown ints
    val _maxDist = buf.getFloat()
    val _minX = buf.getFloat(); val _minY = buf.getFloat(); val _minZ = buf.getFloat()
    val _maxX = buf.getFloat(); val _maxY = buf.getFloat(); val _maxZ = buf.getFloat()

    val vertexCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val texCoordCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val normalCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val colorCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val polyCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val vertexPieceCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val polyTexCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val vertexTexCount = java.lang.Short.toUnsignedInt(buf.getShort())
    val size9 = java.lang.Short.toUnsignedInt(buf.getShort())
    val scaleShift = buf.getShort().toInt

    val scale = 1.0f / (1 << scaleShift)

    // Vertices
    val vertices = (0 until vertexCount).map { _ =>
      val x = buf.getShort().toFloat * scale + centerX
      val y = buf.getShort().toFloat * scale + centerY
      val z = buf.getShort().toFloat * scale + centerZ
      Vector3f(x, y, z)
    }.toArray

    // Texture coordinates
    val uvs = if oldFormat then
      (0 until texCoordCount).map { _ =>
        val u = buf.getShort().toFloat / 256f
        val v = buf.getShort().toFloat / 256f
        (u, v)
      }.toArray
    else
      (0 until texCoordCount).map { _ =>
        val u = buf.getFloat()
        val v = buf.getFloat()
        (u, v)
      }.toArray

    // Normals
    val normals = (0 until normalCount).map { _ =>
      val x = buf.get().toFloat / 128f
      val y = buf.get().toFloat / 128f
      val z = buf.get().toFloat / 128f
      Vector3f(x, y, z)
    }.toArray

    // Colors (skip)
    buf.position(buf.position() + colorCount * 4)

    // Polygons
    val polygons = (0 until polyCount).map { _ =>
      val solid = buf.getShort() == 0
      val v1 = java.lang.Short.toUnsignedInt(buf.getShort())
      val v2 = java.lang.Short.toUnsignedInt(buf.getShort())
      val v3 = java.lang.Short.toUnsignedInt(buf.getShort())
      MeshPolygon(solid, v1, v2, v3)
    }.toArray

    // Vertex pieces: maps consecutive vertex ranges to skeleton bones.
    // Each piece says "the next N vertices belong to bone B".
    // Character mesh vertices are in bone-local space and must be transformed
    // by their bone's world matrix to assemble the model.
    val vertexPieces = (0 until vertexPieceCount).map { _ =>
      val count = java.lang.Short.toUnsignedInt(buf.getShort())
      val boneIndex = java.lang.Short.toUnsignedInt(buf.getShort())
      VertexPiece(count, boneIndex)
    }.toArray

    // Render groups (polygon texture mapping)
    val renderGroups = (0 until polyTexCount).map { _ =>
      val count = java.lang.Short.toUnsignedInt(buf.getShort())
      val matIdx = java.lang.Short.toUnsignedInt(buf.getShort())
      RenderGroup(count, matIdx)
    }.toArray

    Fragment36_Mesh(name, materialListRef, center, vertices, uvs, normals, polygons, renderGroups, vertexPieces)
