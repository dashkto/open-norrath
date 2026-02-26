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
  /** Compute world-space transform for each bone by walking parent chain */
  def boneWorldTransforms(wld: WldFile): Array[Matrix4f] =
    val transforms = new Array[Matrix4f](bones.length)
    for i <- bones.indices do
      transforms(i) = computeBoneWorld(i, wld, transforms)
    transforms

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

  /** Rest-pose translation (frame 0). Safe even for empty frames. */
  def restTranslation: Vector3f =
    if frames.nonEmpty then frames(0).translation else Vector3f()

// 0x13 - Track reference
case class Fragment13_TrackRef(name: String, trackDefRef: Int) extends WldFragment

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
      case 0x00000009 => MaterialType.Transparent75
      case 0x0000000A => MaterialType.Diffuse
      case 0x0000000B => MaterialType.Diffuse
      case 0x00000013 => MaterialType.Diffuse
      case 0x00000014 => MaterialType.Diffuse
      case 0x00000015 => MaterialType.Diffuse
      case 0x00000053 => MaterialType.Transparent50
      case _ => MaterialType.Diffuse
