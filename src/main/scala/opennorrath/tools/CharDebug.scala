package opennorrath.tools

import opennorrath.animation.AnimatedCharacter
import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.{Path, Files}

/** Debug _chr.s3d: actors, skeletons, animations.
  * Usage: runMain opennorrath.tools.CharDebug <dir> <zone>
  *   e.g. runMain opennorrath.tools.CharDebug assets/EverQuest rivervale
  */
object CharDebug:
  def main(args: Array[String]): Unit =
    val dir = if args.length > 0 then args(0) else "assets/EverQuest"
    val zone = if args.length > 1 then args(1) else "arena"
    val chrS3dPath = Path.of(dir, s"${zone}_chr.s3d")

    if !Files.exists(chrS3dPath) then
      println(s"Character S3D not found: $chrS3dPath")
      return

    // Load global animation tracks
    val globalTracks = loadGlobalTracks(Path.of(dir))
    println(s"Global tracks loaded: ${globalTracks.size}")

    val entries = PfsArchive.load(chrS3dPath)
    println(s"=== Character S3D: $chrS3dPath ===")
    println(s"  Entries: ${entries.size}")
    for e <- entries do
      println(s"    ${e.name} (${e.data.length} bytes)")

    val wldEntry = entries.find(_.extension == "wld")
    if wldEntry.isEmpty then
      println("  No WLD found")
      return

    val wld = WldFile(wldEntry.get.data)
    println(s"\n  Total fragments: ${wld.fragments.size}")
    val byType = wld.fragments.groupBy(_.getClass.getSimpleName)
    for (typeName, frags) <- byType.toList.sortBy(_._1) do
      println(s"    $typeName: ${frags.size}")

    // Build combined track map (zone + global)
    val allTrackDefs = wld.fragmentsOfType[Fragment12_TrackDef] ++ globalTracks
    val trackDefsByName: Map[String, Fragment12_TrackDef] = allTrackDefs.map { td =>
      td.cleanName -> td
    }.toMap
    val animCodes: Set[String] = trackDefsByName.keysIterator
      .filter(_.length > 3).map(_.take(3)).toSet
    println(s"  Track name map: ${trackDefsByName.size} unique entries, ${animCodes.size} anim codes")

    // Actors and their skeletons/animations
    val actors = wld.fragmentsOfType[Fragment14_Actor]
    println(s"\n=== Actors: ${actors.size} ===")
    for actor <- actors do
      val actorKey = actor.name.replace("_ACTORDEF", "").toLowerCase
      println(s"\n  $actorKey (refs: ${actor.componentRefs.mkString(", ")})")

      // Find skeleton
      val skeletonOpt = actor.componentRefs.flatMap { ref =>
        try wld.fragment(ref) match
          case sr: Fragment11_SkeletonHierarchyRef =>
            wld.fragment(sr.skeletonRef) match
              case sk: Fragment10_SkeletonHierarchy => Some(sk)
              case _ => None
          case _ => None
        catch case _: Exception => None
      }.headOption

      skeletonOpt match
        case Some(sk) =>
          println(s"    Skeleton: ${sk.name}, ${sk.bones.length} bones")

          // Show base track names (model prefix + bone suffixes)
          val baseNames = sk.bones.map { bone =>
            try
              val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
              wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef].cleanName
            catch case _: Exception => "?"
          }
          val modelPrefix = baseNames.filter(_.nonEmpty).minByOption(_.length).getOrElse("")
          println(s"    Model prefix: '$modelPrefix', bones: ${baseNames.take(8).mkString(", ")}${if baseNames.length > 8 then "..." else ""}")

          // Animations
          val clips = AnimatedCharacter.discoverAnimations(wld, sk, trackDefsByName, animCodes)
          if clips.nonEmpty then
            println(s"    Animations: ${clips.size} clips")
            for (code, clip) <- clips.toList.sortBy(_._1).take(10) do
              println(s"      $code: ${clip.frameCount} frames")
            if clips.size > 10 then println(s"      ... and ${clips.size - 10} more")
          else
            println(s"    Animations: none")

        case None =>
          println(s"    No skeleton found")

  private def loadGlobalTracks(dir: Path): List[Fragment12_TrackDef] =
    val globalFiles = Files.list(dir).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted
    val trackDefs = scala.collection.mutable.ListBuffer[Fragment12_TrackDef]()
    for file <- globalFiles do
      try
        val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
        entries.find(_.extension == "wld").foreach { wldEntry =>
          val wld = WldFile(wldEntry.data)
          trackDefs ++= wld.fragmentsOfType[Fragment12_TrackDef]
        }
      catch case _: Exception => ()
    trackDefs.toList
