package opennorrath.wld

import org.joml.{Matrix4f, Quaternionf, Vector3f}

sealed trait WldFragment:
  def name: String

case class UnknownFragment(name: String, fragmentType: Int, size: Int) extends WldFragment

// 0x03 - Texture filename
case class Fragment03_BitmapName(name: String, filename: String) extends WldFragment

// 0x04 - Texture info (possibly animated)
case class Fragment04_BitmapInfo(
    name: String,
    flags: Int,
    isAnimated: Boolean,
    animDelay: Int,
    textureRefs: List[Int], // 1-based fragment indices to 0x03
) extends WldFragment

// 0x05 - Reference to BitmapInfo
case class Fragment05_BitmapInfoRef(name: String, refIndex: Int) extends WldFragment

// 0x30 - Material definition
case class Fragment30_Material(
    name: String,
    flags: Int,
    shaderType: MaterialType,
    bitmapInfoRefIndex: Int, // 1-based fragment index to 0x05, or 0
) extends WldFragment

// 0x31 - Material list
case class Fragment31_MaterialList(
    name: String,
    materialRefs: List[Int], // 1-based fragment indices to 0x30
) extends WldFragment

// 0x36 - Mesh geometry
case class Fragment36_Mesh(
    name: String,
    materialListRef: Int,
    center: Vector3f,
    vertices: Array[Vector3f],
    uvs: Array[(Float, Float)],
    normals: Array[Vector3f],
    polygons: Array[MeshPolygon],
    renderGroups: Array[RenderGroup],
    vertexPieces: Array[VertexPiece], // bone assignments for character meshes
) extends WldFragment

// 0x14 - Actor definition (object model)
case class Fragment14_Actor(
    name: String,
    componentRefs: List[Int], // 1-based fragment indices to 0x2D or 0x11
) extends WldFragment

// 0x15 - Object instance (placement)
case class Fragment15_ObjectInstance(
    name: String,
    actorName: String,
    position: Vector3f,
    rotation: Vector3f, // degrees
    scale: Vector3f,
) extends WldFragment

// 0x10 - Skeleton hierarchy
case class Fragment10_SkeletonHierarchy(
    name: String,
    meshRefs: List[Int],
    bones: Array[SkeletonBone],
) extends WldFragment:
  /** Whether this skeleton uses Luclin-era bone naming (incompatible with old-style animations). */
  def isLuclin(wld: WldFile): Boolean =
    bones.exists { bone =>
      try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef].isLuclin
      catch case _: Exception => false
    }

  /** Compute world-space transform for each bone by walking parent chain */
  def boneWorldTransforms(wld: WldFile): Array[Matrix4f] =
    val transforms = new Array[Matrix4f](bones.length)
    for i <- bones.indices do
      transforms(i) = computeBoneWorld(i, wld, transforms)
    transforms

  /** Compute the world-space rest-pose transform for a single bone (walking up parent chain). */
  def restPoseBoneTransform(index: Int, wld: WldFile): Matrix4f =
    val cache = new Array[Matrix4f](bones.length)
    computeBoneWorld(index, wld, cache)

  private def computeBoneWorld(index: Int, wld: WldFile, cache: Array[Matrix4f]): Matrix4f =
    if cache(index) != null then return cache(index)
    val bone = bones(index)
    val local = bone.restTransform(wld)
    val parent = bone.parentIndex
    val world = if parent < 0 then local
    else
      val parentWorld = computeBoneWorld(parent, wld, cache)
      Matrix4f(parentWorld).mul(local)
    cache(index) = world
    world

// 0x11 - Skeleton hierarchy reference
case class Fragment11_SkeletonHierarchyRef(name: String, skeletonRef: Int) extends WldFragment

// 0x12 - Track definition (bone transforms per frame)
// Each bone in a skeleton (0x10) references a 0x13 TrackRef → 0x12 TrackDef.
// Naming convention (after stripping _TRACKDEF and uppercasing):
//   Rest pose:  {MODEL}{BONE}          e.g. HUMPE, BAFBI_L
//   Old-style:  {CODE}{MODEL}{BONE}    e.g. C01HUMPE, L01BAFBI_L
//   Luclin-era: {CODE}{A|B}{MODEL}{BONE} e.g. C01AHOFBIBICEPL
// where MODEL is a 3-char race/gender code (HUM, BAF, HOF, etc.),
// CODE is a 3-char animation code (C01=combat, L01=idle, P01=passive, etc.),
// A/B is a Luclin skeleton variant, and BONE is the bone suffix.
// Old-style bones use short names (PE, CH, BI_L). Luclin uses long names (BIBICEPL, CHCHEST).
// Race codes: HUM/HUF=Human, BAM/BAF=Barbarian, ELM/ELF=Wood Elf, HAM/HAF=Half Elf,
//   DWM/DWF=Dwarf, TRM/TRF=Troll, OGM/OGF=Ogre, HOM/HOF=Halfling, GNM/GNF=Gnome,
//   IKM/IKF=Iksar, KEM/KEF=Vah Shir, ERM/ERF=Erudite.
case class Fragment12_TrackDef(name: String, frames: Array[BoneTransform]) extends WldFragment:
  /** Track name without _TRACKDEF suffix, uppercased. */
  lazy val cleanName: String = name.toUpperCase.replace("_TRACKDEF", "")

  /** Whether this track has animation (more than 1 frame). */
  def isAnimated: Boolean = frames.length > 1

  /** Whether this track uses Luclin-era bone naming (long compound suffixes like BIBICEPL).
    * Old-style bone suffixes are short (2-5 chars): PE, BI_L, FI_R2.
    * Luclin suffixes are long (7+ chars): BIBICEPL, BICLAVL, CHCHEST.
    */
  def isLuclin: Boolean =
    // Strip model prefix (3 chars) and any animation code prefix (3 chars + optional variant letter)
    // from cleanName to get the bone suffix, then check length.
    // For rest-pose tracks like "ERMBIBICEPL", suffix = "BIBICEPL" (8 chars) → Luclin.
    // For rest-pose tracks like "HOFBI_L", suffix = "BI_L" (4 chars) → old-style.
    val cn = cleanName
    if cn.length <= 3 then false // root bone (just model prefix)
    else
      val suffix = cn.drop(3) // strip 3-char model prefix
      !suffix.endsWith("_POINT") && suffix.length > 5

  /** Rest-pose translation (frame 0). Safe even for empty frames. */
  def restTranslation: Vector3f =
    if frames.nonEmpty then frames(0).translation else Vector3f()

// 0x13 - Track reference
case class Fragment13_TrackRef(name: String, trackDefRef: Int) extends WldFragment

// 0x21 - BSP tree: flat array of nodes partitioning the zone into regions.
// See ZoneGeometry.scala for how 0x21, 0x22, and 0x29 work together for zone line detection.
case class Fragment21_BspTree(
    name: String,
    nodes: Array[BspNode],
) extends WldFragment

// 28 bytes per node. Internal nodes split space with a plane; leaves identify a region.
case class BspNode(
    normalX: Float, normalY: Float, normalZ: Float, // split plane normal (S3D coords)
    splitDistance: Float,                             // dot(normal, point) >= this → left
    regionId: Int,     // 0 = internal node; >0 = leaf, 1-based index into 0x22 list
    leftNode: Int,     // 1-based index into node array (for internal nodes)
    rightNode: Int,    // 1-based index into node array (for internal nodes)
)

// 0x22 - BSP region: one per leaf. The Nth 0x22 fragment = BSP regionId N+1.
case class Fragment22_BspRegion(
    name: String,
    flags: Int,
    sphereX: Float = 0, sphereY: Float = 0, sphereZ: Float = 0, sphereRadius: Float = 0,
) extends WldFragment

// 0x29 - BSP region type: tags groups of 0x22 regions with a type string.
// Known prefixes (from EQEmu azone2/wld.cpp, checked in this order):
//   WT    = Water        LA    = Lava         DRNTP = ZoneLine
//   DRP_  = PVP          SL    = Slippery     DRN   = Ice
//   VWN   = VWater
// Zone line strings follow the pattern: DRNTP{5-digit}{6-digit}_ZONE
//   e.g. "DRNTP00255000001_ZONE" — the meaning of the digit groups is unverified.
case class Fragment29_BspRegionType(
    name: String,
    regionIndices: Array[Int],   // 0-based indices into 0x22 list (= BSP regionId - 1)
    regionString: String,        // decoded type string from fragment body, or name if empty
) extends WldFragment:

  /** Region type derived from the type string prefix, matching azone2 priority order. */
  def regionType: BspRegionKind =
    val s = regionString.toUpperCase
    if s.startsWith("DRNTP") then BspRegionKind.ZoneLine
    else if s.startsWith("DRP_") then BspRegionKind.PVP
    else if s.startsWith("DRN") then BspRegionKind.Ice
    else if s.startsWith("WT") then BspRegionKind.Water
    else if s.startsWith("LA") then BspRegionKind.Lava
    else if s.startsWith("SL") then BspRegionKind.Slippery
    else if s.startsWith("VWN") then BspRegionKind.VWater
    else BspRegionKind.Unknown

  /** For zone line regions: the 5-digit group after "DRNTP" (e.g. 255 from "DRNTP00255000001_ZONE"). */
  def zoneLineParam1: Int =
    val s = regionString.toUpperCase
    if !s.startsWith("DRNTP") || s.length < 10 then -1
    else try s.substring(5, 10).trim.toInt catch case _: NumberFormatException => -1

  /** For zone line regions: the 6-digit group (e.g. 1 from "DRNTP00255000001_ZONE"). */
  def zoneLineParam2: Int =
    val s = regionString.toUpperCase
    if !s.startsWith("DRNTP") || s.length < 16 then -1
    else try s.substring(10, 16).trim.toInt catch case _: NumberFormatException => -1

enum BspRegionKind:
  case Water, Lava, ZoneLine, PVP, Slippery, Ice, VWater, Unknown

// 0x08 - Camera (unknown purpose, found in zone files)
case class Fragment08_Camera(name: String) extends WldFragment

// 0x09 - Camera reference
case class Fragment09_CameraRef(name: String, cameraRef: Int) extends WldFragment

// 0x16 - Unknown (single float parameter, typically 0.1)
case class Fragment16_Unknown(name: String, value: Float) extends WldFragment

// 0x1B - Light source definition
case class Fragment1B_LightSource(
    name: String,
    flags: Int,
    isPlaced: Boolean,
    isColored: Boolean,
    attenuation: Int,
    red: Float, green: Float, blue: Float, alpha: Float,
) extends WldFragment

// 0x1C - Light source reference
case class Fragment1C_LightSourceRef(name: String, lightSourceRef: Int) extends WldFragment

// 0x2A - Ambient light (references light source + region list, unused in Trilogy)
case class Fragment2A_AmbientLight(
    name: String,
    lightRef: Int,
    regionIds: Array[Int],
) extends WldFragment

// 0x35 - Global ambient light color (BGRA)
case class Fragment35_GlobalAmbientLight(
    name: String,
    red: Float, green: Float, blue: Float, alpha: Float,
) extends WldFragment

// 0x2D - Mesh reference
case class Fragment2D_MeshReference(name: String, meshRef: Int) extends WldFragment

case class BoneTransform(translation: Vector3f, rotation: Quaternionf, scale: Float)

case class SkeletonBone(trackRef: Int, meshRef: Int, parentIndex: Int, children: List[Int]):
  def restTransform(wld: WldFile): Matrix4f =
    try
      val trackFragment = wld.fragment(trackRef).asInstanceOf[Fragment13_TrackRef]
      val trackDef = wld.fragment(trackFragment.trackDefRef).asInstanceOf[Fragment12_TrackDef]
      val frame = trackDef.frames(0) // rest pose = frame 0
      val mat = Matrix4f()
      mat.translate(frame.translation)
      mat.rotate(frame.rotation)
      if frame.scale != 0f && frame.scale != 1f then mat.scale(frame.scale)
      mat
    catch case _: Exception => Matrix4f()

case class VertexPiece(count: Int, boneIndex: Int)

case class MeshPolygon(solid: Boolean, v1: Int, v2: Int, v3: Int)

case class RenderGroup(polyCount: Int, materialIndex: Int)

enum MaterialType:
  case Diffuse
  case Transparent25
  case Transparent50
  case Transparent75
  case TransparentMasked
  case TransparentAdditive
  case Boundary
  case Invisible
  case Unknown

object MaterialType:
  def fromParams(params: Int): MaterialType =
    val value = params & ~0x80000000
    value match
      case 0          => MaterialType.Boundary
      case 0x00000001 => MaterialType.Diffuse
      case 0x00000002 => MaterialType.TransparentMasked
      case 0x00000004 => MaterialType.TransparentAdditive
      case 0x00000005 => MaterialType.Transparent25
      case 0x00000007 => MaterialType.TransparentMasked   // masked passable (vegetation)
      case 0x00000009 => MaterialType.Transparent75
      case 0x0000000A => MaterialType.Diffuse
      case 0x0000000B => MaterialType.TransparentAdditive  // additive unlit
      case 0x00000013 => MaterialType.TransparentMasked    // masked (trees/vegetation)
      case 0x00000014 => MaterialType.Diffuse
      case 0x00000015 => MaterialType.Diffuse
      case 0x00000017 => MaterialType.TransparentAdditive  // additive
      case 0x00000053 => MaterialType.Transparent50
      case _ => MaterialType.Diffuse
