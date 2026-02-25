package opennorrath.animation

import opennorrath.Mesh
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}

case class AnimationClip(code: String, frameCount: Int, boneTrackDefs: Array[Fragment12_TrackDef])

class AnimatedCharacter(
    val skeleton: Fragment10_SkeletonHierarchy,
    val originalMeshes: List[Fragment36_Mesh], // bone-local space
    val zoneMesh: ZoneMesh,
    val glMesh: Mesh,
    val modelMatrix: Matrix4f,
    val clips: Map[String, AnimationClip],
    private val interleavedBuffer: Array[Float], // mutable working buffer
):
  private var currentClip: Option[AnimationClip] = selectDefaultClip()
  private var currentFrame: Int = 0
  private var frameTime: Float = 0f
  private val fps: Float = 15f // EQ animation speed

  def clipNames: Seq[String] = clips.keys.toSeq.sorted

  def play(code: String): Unit =
    clips.get(code).foreach { clip =>
      currentClip = Some(clip)
      currentFrame = 0
      frameTime = 0f
    }

  def update(deltaTime: Float): Unit =
    currentClip match
      case None => return
      case Some(clip) =>
        frameTime += deltaTime
        val frameDuration = 1f / fps
        if frameTime >= frameDuration then
          frameTime -= frameDuration
          val prevFrame = currentFrame
          currentFrame = (currentFrame + 1) % clip.frameCount
          if currentFrame != prevFrame then
            updateVertices(clip, currentFrame)

  private def updateVertices(clip: AnimationClip, frame: Int): Unit =
    val boneTransforms = computeBoneTransforms(clip, frame)
    var globalVertexIdx = 0

    for mesh <- originalMeshes do
      var vertexIndex = 0
      for piece <- mesh.vertexPieces do
        if piece.boneIndex < boneTransforms.length then
          val transform = boneTransforms(piece.boneIndex)
          for _ <- 0 until piece.count do
            if vertexIndex < mesh.vertices.length then
              val v = mesh.vertices(vertexIndex)
              val transformed = Vector3f(v.x, v.y, v.z)
              transform.transformPosition(transformed)
              // Write to interleaved buffer with EQ→GL swizzle: (X, Z, -Y)
              val bufIdx = globalVertexIdx * 5
              interleavedBuffer(bufIdx + 0) = transformed.x
              interleavedBuffer(bufIdx + 1) = transformed.z
              interleavedBuffer(bufIdx + 2) = -transformed.y
              // UV slots (bufIdx+3, bufIdx+4) unchanged
              globalVertexIdx += 1
              vertexIndex += 1
        else
          globalVertexIdx += piece.count
          vertexIndex += piece.count

    glMesh.updateVertices(interleavedBuffer)

  private def computeBoneTransforms(clip: AnimationClip, frame: Int): Array[Matrix4f] =
    AnimatedCharacter.computeBoneTransforms(skeleton, clip, frame)

  private def selectDefaultClip(): Option[AnimationClip] =
    clips.get("L01") // idle
      .orElse(clips.get("P01")) // idle variant
      .orElse(clips.headOption.map(_._2))

object AnimatedCharacter:

  /** Compute world-space bone transforms for a given animation clip and frame. */
  def computeBoneTransforms(
      skeleton: Fragment10_SkeletonHierarchy,
      clip: AnimationClip,
      frame: Int,
  ): Array[Matrix4f] =
    val cache = new Array[Matrix4f](skeleton.bones.length)
    def computeWorld(index: Int): Matrix4f =
      if cache(index) != null then return cache(index)
      val bone = skeleton.bones(index)
      val trackDef = clip.boneTrackDefs(index)
      val f = trackDef.frames(if frame < trackDef.frames.length then frame else 0)
      val local = Matrix4f()
      local.translate(f.translation)
      local.rotate(f.rotation)
      if f.scale != 0f && f.scale != 1f then local.scale(f.scale)
      val world = if bone.parentIndex < 0 then local
      else Matrix4f(computeWorld(bone.parentIndex)).mul(local)
      cache(index) = world
      world
    for i <- skeleton.bones.indices do computeWorld(i)
    cache

  /** Discover animations by matching track names against skeleton bone suffixes.
    * Base tracks: {MODEL}{BONE_SUFFIX}_TRACKDEF (1 frame)
    * Animation tracks: {ANIM_CODE}{ANIM_MODEL}{BONE_SUFFIX}_TRACKDEF (N frames)
    * The animation model prefix may differ from the base model prefix.
    */
  def discoverAnimations(
      wld: WldFile,
      skeleton: Fragment10_SkeletonHierarchy,
  ): Map[String, AnimationClip] =
    // Step 1: Get base track names and TrackDefs for each bone
    val baseTrackDefs: Array[Fragment12_TrackDef] = skeleton.bones.map { bone =>
      try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
      catch case _: Exception => null
    }
    val baseSuffixes: Array[String] = baseTrackDefs.map { td =>
      if td != null then td.name.toUpperCase.replace("_TRACKDEF", "") else ""
    }

    // Find common prefix (the model name) by finding the shortest base track name
    // which should be the root bone (just the model prefix with no bone suffix)
    val modelPrefix = baseSuffixes.filter(_.nonEmpty).minByOption(_.length).getOrElse("")
    if modelPrefix.isEmpty then return Map.empty

    // Bone suffixes are what's left after stripping the model prefix
    val boneSuffixes = baseSuffixes.map { name =>
      if name.startsWith(modelPrefix) then name.drop(modelPrefix.length) else name
    }

    // Attachment point bones (suffix ends with _POINT) never have animation tracks —
    // they inherit their parent bone's transform. Use the base track for these.
    val isAttachmentPoint = boneSuffixes.map(_.endsWith("_POINT"))

    // Step 2: Scan all TrackDefs for animation tracks
    val allTrackDefs = wld.fragmentsOfType[Fragment12_TrackDef]
    val trackDefByName = allTrackDefs.map { td =>
      td.name.toUpperCase.replace("_TRACKDEF", "") -> td
    }.toMap

    // Step 3: Find animation prefixes by matching multi-frame tracks to bone suffixes
    val baseNames = baseSuffixes.toSet
    val candidatePrefixes = scala.collection.mutable.Set[String]()
    for (name, td) <- trackDefByName do
      if !baseNames.contains(name) && td.frames.length > 1 then
        for suffix <- boneSuffixes.distinct if suffix.nonEmpty && !suffix.endsWith("_POINT") do
          if name.endsWith(suffix) then
            candidatePrefixes += name.dropRight(suffix.length)

    // Also check prefixes from 1-frame root candidates (root bone often has 1 frame)
    for (name, td) <- trackDefByName do
      if !baseNames.contains(name) && td.frames.length == 1 then
        val firstNonRootSuffix = boneSuffixes.find(s => s.nonEmpty && !s.endsWith("_POINT"))
        firstNonRootSuffix.foreach { suffix =>
          if trackDefByName.contains(name + suffix) then
            candidatePrefixes += name
        }

    // Step 4: For each candidate prefix, require ALL non-attachment-point bones to have
    // a matching track. Attachment points use the base track (rest pose).
    val clips = candidatePrefixes.flatMap { prefix =>
      val allNonPointBonesMatch = boneSuffixes.zipWithIndex.forall { (suffix, i) =>
        isAttachmentPoint(i) || trackDefByName.contains(prefix + suffix)
      }
      if !allNonPointBonesMatch then None
      else
        val boneTracks: Array[Fragment12_TrackDef] = boneSuffixes.zipWithIndex.map { (suffix, i) =>
          if isAttachmentPoint(i) then baseTrackDefs(i)
          else trackDefByName(prefix + suffix)
        }
        val frameCount = boneTracks.filter(_ != null).map(_.frames.length).max
        if frameCount > 1 then // 1-frame = rest pose, not an animation
          val animCode = prefix.take(3)
          Some(animCode -> AnimationClip(animCode, frameCount, boneTracks))
        else None
    }.toMap

    clips
