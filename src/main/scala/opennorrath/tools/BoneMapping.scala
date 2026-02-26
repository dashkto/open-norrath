package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.Path

/** Show bone mapping between old-style and new-style skeletons for a model.
  * Usage: runMain opennorrath.tools.BoneMapping <dir> <model>
  * e.g.: runMain opennorrath.tools.BoneMapping assets/EverQuest BAF
  */
object BoneMapping:
  def main(args: Array[String]): Unit =
    val dir = args(0)
    val model = args(1).toUpperCase

    // Load old-style tracks (from global_chr.s3d and global{model}_chr.s3d where model = haf/baf etc)
    // Load new-style tracks (from global{model}_chr.s3d)
    val oldFile = Path.of(dir, s"global${model.toLowerCase}_chr.s3d")
    val newFile = Path.of(dir, s"global${model.take(1).toLowerCase}${if model.endsWith("F") then "o" else "o"}${model.drop(1).take(1).toLowerCase}_chr.s3d")

    // Load all tracks from the model's global file
    val mainEntries = PfsArchive.load(Path.of(dir, s"global${model.toLowerCase}_chr.s3d"), extensionFilter = Some(Set("wld")))
    val mainTracks = mainEntries.find(_.extension == "wld").map { we =>
      WldFile(we.data).fragmentsOfType[Fragment12_TrackDef]
    }.getOrElse(Nil)

    println(s"=== ${model} tracks from global${model.toLowerCase}_chr.s3d ===")
    println(s"Total: ${mainTracks.size}, Multi-frame: ${mainTracks.count(_.isAnimated)}")

    // Show rest-pose (1-frame) tracks grouped by prefix pattern
    val restPose = mainTracks.filter(_.frames.length == 1)
    println(s"\nRest-pose tracks (${restPose.size}):")
    for td <- restPose.sortBy(_.cleanName).take(40) do
      val t = td.frames(0).translation
      println(f"  ${td.cleanName}%-40s (${t.x}%.2f, ${t.y}%.2f, ${t.z}%.2f)")
    if restPose.size > 40 then println(s"  ... and ${restPose.size - 40} more")

    // Extract unique bone suffixes from a single animation code
    val sampleCode = mainTracks.filter(_.isAnimated).map(_.cleanName.take(3)).distinct.sorted.headOption
    sampleCode.foreach { code =>
      println(s"\nNew-style bones for anim code $code:")
      val animTracks = mainTracks.filter(td => td.isAnimated && td.cleanName.startsWith(code))
      // The model prefix in new-style is: variant(1) + model(3) = 4 chars after the 3-char code
      val prefixLen = 3 + 4 // code(3) + variant(1) + model(3)
      val bones = animTracks.map(_.cleanName.drop(prefixLen)).sorted
      for b <- bones.take(40) do
        println(s"  $b")
      if bones.size > 40 then println(s"  ... and ${bones.size - 40} more")
    }

    // Compare with old-style bone suffixes from global_chr.s3d
    val globalEntries = PfsArchive.load(Path.of(dir, "global_chr.s3d"), extensionFilter = Some(Set("wld")))
    val globalTracks = globalEntries.find(_.extension == "wld").map { we =>
      WldFile(we.data).fragmentsOfType[Fragment12_TrackDef]
    }.getOrElse(Nil)
    val oldBones = globalTracks.filter(_.frames.length == 1)
      .map(_.cleanName).filter(_.startsWith(model)).map(_.drop(model.length)).sorted
    println(s"\nOld-style bone suffixes for $model (${oldBones.size}):")
    for b <- oldBones do
      println(s"  $b")
