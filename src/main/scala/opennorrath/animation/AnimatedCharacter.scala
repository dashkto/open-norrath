package opennorrath.animation

import opennorrath.render.Mesh
import opennorrath.world.EqCoords
import opennorrath.wld.*
import org.joml.{Matrix4f, Quaternionf, Vector3f}

/** Standard EQ 3-char animation codes. */
enum AnimCode(val code: String):
  case Idle    extends AnimCode("P01")
  case Walk    extends AnimCode("L01")
  case Run     extends AnimCode("L02")
  case Passive extends AnimCode("P02")
  case Sit     extends AnimCode("L05")
  case Crouch  extends AnimCode("L06")
  case Death   extends AnimCode("D01")
  case Combat  extends AnimCode("C01")

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
  def currentClipCode: String = currentClip.map(_.code).getOrElse("none")

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
              val (glX, glY, glZ) = EqCoords.s3dToGl(transformed.x, transformed.y, transformed.z)
              val bufIdx = globalVertexIdx * 5
              buffer(bufIdx + 0) = glX
              buffer(bufIdx + 1) = glY
              buffer(bufIdx + 2) = glZ
              globalVertexIdx += 1
              vertexIndex += 1
        else
          globalVertexIdx += piece.count
          vertexIndex += piece.count

  private def selectDefaultClip(): Option[AnimationClip] =
    clips.get(AnimCode.Idle.code)
      .orElse(clips.get(AnimCode.Passive.code))
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

  /** Manual animation fallback map: model code → fallback model code (uppercase).
    * When a model is missing animation clips, tracks from the fallback model are used.
    */
  val animFallbacks: Map[String, String] = Map(
    "TIG" -> "LIM",
    "HOF" -> "DWF",
    "HOM" -> "DWM",
    "GNF" -> "DWF",
    "GNM" -> "DWM",
    "HUF" -> "ELF",
    "ERM" -> "HUM",
    "ERF" -> "ELF",
    "HAM" -> "ELM",
    "HAF" -> "ELF",
    "OGM" -> "TRM",
  )

  /** Discover animations for a skeleton using a foreign model's track prefix.
    * Uses the skeleton's bone suffixes but searches for tracks named {code}{foreignPrefix}{suffix}.
    * Useful for borrowing animations from one race and applying them to another's mesh.
    */
  def discoverAnimationsWithPrefix(
      wld: WldFile,
      skeleton: Fragment10_SkeletonHierarchy,
      trackDefsByName: Map[String, Fragment12_TrackDef],
      animCodes: Set[String],
      foreignPrefix: String,
  ): Map[String, AnimationClip] =
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

    def hasTracksForPrefix(prefix: String): Boolean =
      trackDefsByName.contains(prefix) ||
        nonPointSuffixes.exists(s => trackDefsByName.contains(prefix + s))

    def findPrefix(code: String): Option[String] =
      val direct = code + foreignPrefix
      if hasTracksForPrefix(direct) then Some(direct)
      else Iterator("A", "B").map(v => code + v + foreignPrefix).find(hasTracksForPrefix)

    val validPrefixes = animCodes.flatMap(findPrefix)
    validPrefixes.flatMap { prefix =>
      val boneTracks: Array[Fragment12_TrackDef] = boneSuffixes.zipWithIndex.map { (suffix, i) =>
        if isAttachmentPoint(i) then baseTrackDefs(i)
        else trackDefsByName.getOrElse(prefix + suffix, baseTrackDefs(i))
      }
      val frameCount = boneTracks.filter(_ != null).map(_.frames.length).max
      if frameCount > 1 then
        val animCode = prefix.dropRight(foreignPrefix.length)
        Some(animCode -> AnimationClip(animCode, frameCount, boneTracks))
      else None
    }.toMap

  /** Discover animations by matching track names against skeleton bone suffixes.
    *
    * Track naming:
    *   Old-style:  {CODE}{MODEL}{BONE}      e.g. C01HUMPE
    *   Luclin-era: {CODE}{A|B}{MODEL}{BONE}  e.g. C01AHOFBIBICEPL
    */
  def discoverAnimations(
      wld: WldFile,
      skeleton: Fragment10_SkeletonHierarchy,
      trackDefsByName: Map[String, Fragment12_TrackDef],
      animCodes: Set[String],
  ): Map[String, AnimationClip] =
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

    def hasTracksForPrefix(prefix: String): Boolean =
      trackDefsByName.contains(prefix) ||
        nonPointSuffixes.exists(s => trackDefsByName.contains(prefix + s))

    def findPrefix(code: String, model: String): Option[String] =
      val direct = code + model
      if hasTracksForPrefix(direct) then Some(direct)
      else Iterator("A", "B").map(v => code + v + model).find(hasTracksForPrefix)

    def buildClip(prefix: String, model: String): Option[(String, AnimationClip)] =
      val boneTracks: Array[Fragment12_TrackDef] = boneSuffixes.zipWithIndex.map { (suffix, i) =>
        if isAttachmentPoint(i) then baseTrackDefs(i)
        else trackDefsByName.getOrElse(prefix + suffix, baseTrackDefs(i))
      }
      val frameCount = boneTracks.filter(_ != null).map(_.frames.length).max
      if frameCount > 1 then
        val animCode = prefix.dropRight(model.length)
        Some(animCode -> AnimationClip(animCode, frameCount, boneTracks))
      else None

    // Discover clips for the primary model
    val validPrefixes = animCodes.flatMap(code => findPrefix(code, modelPrefix))
    var clips = validPrefixes.flatMap(p => buildClip(p, modelPrefix)).toMap

    // Fill in missing animations from manual fallback map
    animFallbacks.get(modelPrefix).foreach { fallback =>
      val haveCodes = clips.keys.flatMap(k => if k.length >= 3 then Some(k.take(3)) else None).toSet
      val missingCodes = animCodes -- haveCodes
      if missingCodes.nonEmpty then
        val fbPrefixes = missingCodes.flatMap(code => findPrefix(code, fallback))
        val fbClips = fbPrefixes.flatMap(p => buildClip(p, fallback))
        if fbClips.nonEmpty then clips = clips ++ fbClips
    }

    clips
