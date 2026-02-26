package opennorrath.animation

import opennorrath.Mesh
import opennorrath.wld.*
import org.joml.{Matrix4f, Quaternionf, Vector3f}

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

  // Vertex-level interpolation: pre-compute vertices at frame boundaries,
  // then cheaply lerp positions each render frame.
  private val frameAVertices = interleavedBuffer.clone()
  private val frameBVertices = interleavedBuffer.clone()
  private var needsInit = true

  def clipNames: Seq[String] = clips.keys.toSeq.sorted

  def play(code: String): Unit =
    clips.get(code).foreach { clip =>
      currentClip = Some(clip)
      currentFrame = 0
      frameTime = 0f
      needsInit = true
    }

  def update(deltaTime: Float): Unit =
    currentClip match
      case None => return
      case Some(clip) =>
        frameTime += deltaTime
        val frameDuration = 1f / fps
        var frameChanged = false
        while frameTime >= frameDuration do
          frameTime -= frameDuration
          currentFrame = (currentFrame + 1) % clip.frameCount
          frameChanged = true

        if frameChanged || needsInit then
          val nextFrame = (currentFrame + 1) % clip.frameCount
          computeFrameVertices(clip, currentFrame, frameAVertices)
          computeFrameVertices(clip, nextFrame, frameBVertices)
          needsInit = false

        // Lerp vertex positions between frame A and frame B
        val t = frameTime / frameDuration
        val vertexCount = interleavedBuffer.length / 5
        var i = 0
        while i < vertexCount do
          val idx = i * 5
          interleavedBuffer(idx) = frameAVertices(idx) + (frameBVertices(idx) - frameAVertices(idx)) * t
          interleavedBuffer(idx + 1) = frameAVertices(idx + 1) + (frameBVertices(idx + 1) - frameAVertices(idx + 1)) * t
          interleavedBuffer(idx + 2) = frameAVertices(idx + 2) + (frameBVertices(idx + 2) - frameAVertices(idx + 2)) * t
          i += 1
        glMesh.updateVertices(interleavedBuffer)

  /** Compute bone-transformed vertices for a single frame into the target buffer. */
  private def computeFrameVertices(clip: AnimationClip, frame: Int, buffer: Array[Float]): Unit =
    val boneTransforms = AnimatedCharacter.computeBoneTransforms(skeleton, clip, frame)
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
              // Write with EQ→GL swizzle: (X, Z, -Y)
              val bufIdx = globalVertexIdx * 5
              buffer(bufIdx + 0) = transformed.x
              buffer(bufIdx + 1) = transformed.z
              buffer(bufIdx + 2) = -transformed.y
              globalVertexIdx += 1
              vertexIndex += 1
        else
          globalVertexIdx += piece.count
          vertexIndex += piece.count

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

  /** Compute world-space bone transforms with linear interpolation between two frames. */
  def computeBoneTransformsLerp(
      skeleton: Fragment10_SkeletonHierarchy,
      clip: AnimationClip,
      frameA: Int,
      frameB: Int,
      t: Float,
  ): Array[Matrix4f] =
    val cache = new Array[Matrix4f](skeleton.bones.length)
    def computeWorld(index: Int): Matrix4f =
      if cache(index) != null then return cache(index)
      val bone = skeleton.bones(index)
      val trackDef = clip.boneTrackDefs(index)
      val fa = trackDef.frames(if frameA < trackDef.frames.length then frameA else 0)
      val fb = trackDef.frames(if frameB < trackDef.frames.length then frameB else 0)
      // Lerp translation and scale, slerp rotation
      val translation = Vector3f(fa.translation).lerp(fb.translation, t)
      val rotation = Quaternionf(fa.rotation).slerp(fb.rotation, t)
      // EQ uses scale=0 as sentinel for "no scale" (= 1.0); normalize before lerping
      val sa = if fa.scale == 0f then 1f else fa.scale
      val sb = if fb.scale == 0f then 1f else fb.scale
      val scale = sa + (sb - sa) * t
      val local = Matrix4f()
      local.translate(translation)
      local.rotate(rotation)
      if scale != 1f then local.scale(scale)
      val world = if bone.parentIndex < 0 then local
      else Matrix4f(computeWorld(bone.parentIndex)).mul(local)
      cache(index) = world
      world
    for i <- skeleton.bones.indices do computeWorld(i)
    cache

  /** Discover animations by matching track names against skeleton bone suffixes.
    * @param trackDefsByName Pre-built map of track name → TrackDef (built once in buildCharacters).
    * @param animCodes Pre-computed set of unique 3-char animation codes from all tracks.
    */
  def discoverAnimations(
      wld: WldFile,
      skeleton: Fragment10_SkeletonHierarchy,
      trackDefsByName: Map[String, Fragment12_TrackDef],
      animCodes: Set[String],
  ): Map[String, AnimationClip] =
    // Step 1: Get base track names and TrackDefs for each bone
    val baseTrackDefs: Array[Fragment12_TrackDef] = skeleton.bones.map { bone =>
      try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
      catch case _: Exception => null
    }
    val baseSuffixes: Array[String] = baseTrackDefs.map { td =>
      if td != null then td.cleanName else ""
    }

    val modelPrefix = baseSuffixes.filter(_.nonEmpty).minByOption(_.length).getOrElse("")
    if modelPrefix.isEmpty then return Map.empty

    val boneSuffixes = baseSuffixes.map { name =>
      if name.startsWith(modelPrefix) then name.drop(modelPrefix.length) else name
    }
    val isAttachmentPoint = boneSuffixes.map(_.endsWith("_POINT"))
    val nonPointSuffixes = boneSuffixes.distinct.filter(s => s.nonEmpty && !s.endsWith("_POINT"))

    // Step 2: Find animation prefixes via direct map lookups (O(animCodes × suffixes))
    // instead of scanning all 335K tracks. For each 3-char anim code, check if
    // {code}{modelPrefix}{boneSuffix} exists in the map.
    val validPrefixes = animCodes.filter { code =>
      val prefix = code + modelPrefix
      // Root bone (empty suffix) or any non-point bone must have a track
      trackDefsByName.contains(prefix) ||
        nonPointSuffixes.exists(s => trackDefsByName.contains(prefix + s))
    }.map(_ + modelPrefix)

    // Don't fallback to a different model's animations — only use own-prefix tracks.
    // Build clips from own-prefix animations, using base track (rest pose) for missing bones.
    val clips = validPrefixes.flatMap { prefix =>
      val boneTracks: Array[Fragment12_TrackDef] = boneSuffixes.zipWithIndex.map { (suffix, i) =>
        if isAttachmentPoint(i) then baseTrackDefs(i)
        else trackDefsByName.getOrElse(prefix + suffix, baseTrackDefs(i))
      }
      val frameCount = boneTracks.filter(_ != null).map(_.frames.length).max
      if frameCount > 1 then
        val animCode = prefix.dropRight(modelPrefix.length)
        Some(animCode -> AnimationClip(animCode, frameCount, boneTracks))
      else None
    }.toMap

    clips
