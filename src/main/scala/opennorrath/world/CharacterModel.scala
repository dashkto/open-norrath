package opennorrath.world

import opennorrath.animation.AnimationClip
import opennorrath.wld.{Fragment10_SkeletonHierarchy, Fragment36_Mesh, ZoneMesh}

/** Template for rendering a character model — skeleton, geometry, animations, and bounding box.
  *
  * Built once per model code (e.g. "hum", "elf") by GlobalCharacters or ZoneRenderer,
  * then shared by all spawns of that model. Actual per-spawn state lives in AnimatedCharacter.
  */
case class CharacterModel(
    key: String,                                     // model code (e.g. "hum", "elf", "bat")
    skeleton: Fragment10_SkeletonHierarchy,           // bone hierarchy with rest-pose tracks
    meshFragments: List[Fragment36_Mesh],             // raw mesh data in bone-local S3D space
    zm: ZoneMesh,                                     // combined geometry (rest-pose applied)
    glWidth: Float, glDepth: Float, glHeight: Float,  // bounding box dimensions in GL space
    glCenterX: Float, glCenterZ: Float, glMinY: Float, // centering offsets
    clips: Map[String, AnimationClip],                // animation code → clip (e.g. "C01" → attack)
    attachBoneIndices: Map[String, Int] = Map.empty,  // bone suffix → index for _POINT bones
)
