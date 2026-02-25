package opennorrath.wld

import org.joml.Vector3f

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

// 0x2D - Mesh reference
case class Fragment2D_MeshReference(name: String, meshRef: Int) extends WldFragment

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
      case _          => MaterialType.Diffuse
