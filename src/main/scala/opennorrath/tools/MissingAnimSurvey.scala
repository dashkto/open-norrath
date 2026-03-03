package opennorrath.tools

import opennorrath.animation.{AnimatedCharacter, SkeletonAnalysis, TrackMap}
import opennorrath.archive.PfsArchive
import opennorrath.wld.{Fragment10_SkeletonHierarchy, Fragment11_SkeletonHierarchyRef, Fragment14_Actor, WldFile}
import opennorrath.world.GlobalCharacters
import java.nio.file.{Path, Files}

/** Scan all global and zone chr files to find character models with no animations.
  *
  * For each model with 0 clips, prints its bone suffixes and lists candidate models
  * that have animations AND share the same bone suffix set — these are valid fallback
  * candidates for the animFallbacks map in AnimatedCharacter.
  *
  * Usage: sbt "runMain opennorrath.tools.MissingAnimSurvey [assetsDir]"
  */
object MissingAnimSurvey:

  case class ModelInfo(
      key: String,            // e.g. "brm"
      modelPrefix: String,    // e.g. "BRM" (from skeleton bone names)
      boneSuffixes: Set[String],
      clipCount: Int,
      source: String,         // which file it came from
  )

  def main(args: Array[String]): Unit =
    val dir = Path.of(if args.nonEmpty then args(0) else "assets/EverQuest")
    if !Files.isDirectory(dir) then
      println(s"Directory not found: $dir")
      return

    // Step 1: Load global track defs via GlobalCharacters (headless — no OpenGL)
    println("Loading global track defs...")
    GlobalCharacters.initHeadless(dir.toString)
    val globalTrackMap = GlobalCharacters.trackMap

    // Step 2: Scan all chr files for actors
    val chrFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.contains("_chr") && name.endsWith(".s3d")
      }.sorted

    println(s"\nScanning ${chrFiles.length} chr files for actors...")
    val models = List.newBuilder[ModelInfo]

    for file <- chrFiles do
      try
        val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
        entries.find(_.extension == "wld").foreach { wldEntry =>
          val wld = WldFile(wldEntry.data)
          val localTrackMap = TrackMap.merge(globalTrackMap, wld)

          val actors = wld.fragmentsOfType[Fragment14_Actor]
          for actor <- actors do
            val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
            val skeletonOpt = resolveActorSkeleton(wld, actor)
            skeletonOpt.foreach { sk =>
              if !sk.isLuclin(wld) then
                val info = analyzeModel(wld, sk, actorKey, localTrackMap, file.getFileName.toString)
                models += info
            }
        }
      catch case e: Exception =>
        println(s"  Warning: failed to scan ${file.getFileName}: ${e.getMessage}")

    val allModels = models.result()

    // Deduplicate: keep the first occurrence of each model key
    val seen = scala.collection.mutable.Set[String]()
    val uniqueModels = allModels.filter { m =>
      if seen.contains(m.key) then false
      else { seen += m.key; true }
    }

    // Exclude models already covered by animFallbacks — they have a known solution.
    val coveredKeys = AnimatedCharacter.animFallbacks.keys.map(_.toLowerCase).toSet
    val uncovered = uniqueModels.filterNot(m => coveredKeys.contains(m.key))

    val (animated, unanimated) = uncovered.partition(_.clipCount > 0)

    // Step 3: For each unanimated model, find candidates with matching bone suffixes
    println(s"\n${"=" * 70}")
    println(s"RESULTS: ${uniqueModels.size} unique models, ${coveredKeys.size} covered by fallbacks, ${unanimated.size} missing animations")
    println(s"${"=" * 70}")

    if unanimated.nonEmpty then
      println(s"\nModels with NO animations and no fallback (${unanimated.size}):")
      println(s"${"-" * 70}")
      for m <- unanimated.sortBy(_.key) do
        println(s"\n  ${m.key.toUpperCase} (from ${m.source})")
        println(s"    boneSuffixes: ${m.boneSuffixes.toSeq.sorted.mkString(", ")}")

        // Find animated models with the same bone suffix set
        val candidates = animated.filter(_.boneSuffixes == m.boneSuffixes).map(_.modelPrefix).sorted
        if candidates.nonEmpty then
          println(s"    EXACT suffix match → ${candidates.mkString(", ")}")
        else
          // Find models with a superset of our suffixes
          val supersets = animated.filter(a => m.boneSuffixes.subsetOf(a.boneSuffixes))
            .map(a => s"${a.modelPrefix}(+${(a.boneSuffixes -- m.boneSuffixes).size})").sorted
          if supersets.nonEmpty then
            println(s"    superset match → ${supersets.mkString(", ")}")
          else
            println(s"    NO compatible models found")

    println(s"\n\nAnimated models (${animated.map(_.key)})")

  private def resolveActorSkeleton(wld: WldFile, actor: Fragment14_Actor): Option[Fragment10_SkeletonHierarchy] =
    actor.componentRefs.iterator.flatMap { ref =>
      try
        wld.fragment(ref) match
          case sr: Fragment11_SkeletonHierarchyRef =>
            wld.fragment(sr.skeletonRef) match
              case sk: Fragment10_SkeletonHierarchy => Some(sk)
              case _ => None
          case _ => None
      catch case _: Exception => None
    }.nextOption()

  private def analyzeModel(
      wld: WldFile,
      skeleton: Fragment10_SkeletonHierarchy,
      actorKey: String,
      trackMap: TrackMap,
      source: String,
  ): ModelInfo =
    val analysis = SkeletonAnalysis(wld, skeleton)
    if !analysis.isValid then
      return ModelInfo(actorKey, "???", Set.empty, 0, source)
    ModelInfo(actorKey, analysis.modelPrefix, analysis.nonPointSuffixes, analysis.countClips(trackMap), source)
