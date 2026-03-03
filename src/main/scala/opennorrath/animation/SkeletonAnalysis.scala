package opennorrath.animation

import opennorrath.wld.{Fragment10_SkeletonHierarchy, Fragment12_TrackDef, Fragment13_TrackRef, WldFile}

/** Derived bone analysis for a skeleton: model prefix, bone suffixes, and attachment point flags.
  *
  * Computed once from a WLD + skeleton. Centralizes the "extract baseSuffixes,
  * find modelPrefix, partition _POINT bones" pattern that was previously inlined
  * in discoverAnimations, discoverAnimationsWithPrefix, and MissingAnimSurvey.analyzeModel.
  */
class SkeletonAnalysis private (
    val modelPrefix: String,                          // shortest non-empty bone name, e.g. "HUM"
    val boneSuffixes: Array[String],                  // per-bone suffix after stripping modelPrefix
    val nonPointSuffixes: Set[String],                // distinct suffixes excluding _POINT and empty
    val isAttachmentPoint: Array[Boolean],            // per-bone flag for _POINT bones
    val baseTrackDefs: Array[Fragment12_TrackDef],    // rest-pose track defs; null for unresolvable bones
):

  /** True if at least one bone was successfully named (modelPrefix is non-empty). */
  def isValid: Boolean = modelPrefix.nonEmpty

  /** Discover all animation clips for this skeleton's own model prefix. */
  def discoverClips(trackMap: TrackMap): Map[String, AnimationClip] =
    if !isValid then return Map.empty
    val validPrefixes = trackMap.animCodes.flatMap(code =>
      trackMap.findPrefix(code, modelPrefix, nonPointSuffixes))
    validPrefixes.flatMap(prefix => buildClip(trackMap, prefix, modelPrefix)).toMap

  /** Discover clips using a foreign model's animation prefix.
    *
    * Looks for {code}{foreignPrefix} tracks and maps them onto this skeleton's
    * bone suffixes. Attachment-point bones always keep their rest-pose track.
    */
  def discoverClipsWithForeignPrefix(trackMap: TrackMap, foreignPrefix: String): Map[String, AnimationClip] =
    if !isValid then return Map.empty
    val validPrefixes = trackMap.animCodes.flatMap(code =>
      trackMap.findPrefix(code, foreignPrefix, nonPointSuffixes))
    validPrefixes.flatMap(prefix => buildClip(trackMap, prefix, foreignPrefix)).toMap

  /** Count distinct animation clips without building AnimationClip objects.
    * Used by MissingAnimSurvey for lightweight analysis.
    */
  def countClips(trackMap: TrackMap): Int =
    if !isValid then 0
    else trackMap.animCodes.count(code =>
      trackMap.findPrefix(code, modelPrefix, nonPointSuffixes).isDefined)

  private def buildClip(
      trackMap: TrackMap,
      prefix: String,
      model: String,
  ): Option[(String, AnimationClip)] =
    val boneTracks: Array[Fragment12_TrackDef] = boneSuffixes.zipWithIndex.map { (suffix, i) =>
      if isAttachmentPoint(i) then baseTrackDefs(i)
      else trackMap.byName.getOrElse(prefix + suffix, baseTrackDefs(i))
    }
    val frameCount = boneTracks.filter(_ != null).map(_.frames.length).maxOption.getOrElse(0)
    if frameCount > 1 then
      val animCode = prefix.dropRight(model.length)
      Some(animCode -> AnimationClip(animCode, frameCount, boneTracks))
    else None

object SkeletonAnalysis:

  /** Build from a WLD and skeleton. Returns an invalid analysis if no bones can be resolved. */
  def apply(wld: WldFile, skeleton: Fragment10_SkeletonHierarchy): SkeletonAnalysis =
    val baseTrackDefs: Array[Fragment12_TrackDef] = skeleton.bones.map { bone =>
      try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
      catch case _: Exception => null
    }
    val baseNames: Array[String] = baseTrackDefs.map(td => if td != null then td.cleanName else "")
    val modelPrefix = baseNames.filter(_.nonEmpty).minByOption(_.length).getOrElse("")
    val boneSuffixes = baseNames.map { name =>
      if name.startsWith(modelPrefix) then name.drop(modelPrefix.length) else name
    }
    val isAttachmentPoint = boneSuffixes.map(_.endsWith("_POINT"))
    val nonPointSuffixes = boneSuffixes.distinct.filter(s => s.nonEmpty && !s.endsWith("_POINT")).toSet
    new SkeletonAnalysis(modelPrefix, boneSuffixes, nonPointSuffixes, isAttachmentPoint, baseTrackDefs)
