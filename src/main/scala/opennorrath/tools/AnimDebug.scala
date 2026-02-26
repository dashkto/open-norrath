package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.*
import java.nio.file.Path

/** Debug skeleton bone ordering for TIG (tiger) model across archives.
  *
  * Compares the TIG skeleton in arena_chr.s3d vs global6_chr.s3d to identify
  * bone ordering differences.
  *
  * Usage: runMain opennorrath.tools.AnimDebug
  */
object AnimDebug:
  val AssetsDir = "assets/EverQuest"

  case class BoneInfo(index: Int, parentIndex: Int, trackName: String)

  def loadSkeleton(archiveName: String, actorFilter: String = "TIG"): (WldFile, Fragment10_SkeletonHierarchy, Array[BoneInfo]) =
    val path = Path.of(AssetsDir, archiveName)
    val entries = PfsArchive.load(path)
    val wldEntry = entries.find(_.extension == "wld").get
    val wld = WldFile(wldEntry.data)

    val actors = wld.fragmentsOfType[Fragment14_Actor]
    val tigActor = actors.find(_.name.toUpperCase.contains(actorFilter.toUpperCase)).getOrElse {
      throw RuntimeException(s"No $actorFilter actor found in $archiveName")
    }

    val skeleton = tigActor.componentRefs.flatMap { ref =>
      try
        wld.fragment(ref) match
          case sr: Fragment11_SkeletonHierarchyRef =>
            Some(wld.fragment(sr.skeletonRef).asInstanceOf[Fragment10_SkeletonHierarchy])
          case _ => None
      catch case _: Exception => None
    }.headOption.getOrElse {
      throw RuntimeException(s"No skeleton found for TIG in $archiveName")
    }

    val boneInfos = skeleton.bones.zipWithIndex.map { (bone, i) =>
      val trackName = try
        val trackRef = wld.fragment(bone.trackRef).asInstanceOf[Fragment13_TrackRef]
        val trackDef = wld.fragment(trackRef.trackDefRef).asInstanceOf[Fragment12_TrackDef]
        trackDef.name
      catch case e: Exception => s"<error: ${e.getMessage}>"
      BoneInfo(i, bone.parentIndex, trackName)
    }

    (wld, skeleton, boneInfos)

  def printSkeleton(label: String, bones: Array[BoneInfo]): Unit =
    println(s"\n=== $label ===")
    println(f"  ${"Idx"}%4s  ${"Parent"}%6s  Track Name")
    println("  " + "-" * 60)
    for b <- bones do
      println(f"  [${b.index}%2d]  ${b.parentIndex}%6d  ${b.trackName}")

  def main(args: Array[String]): Unit =
    val actor = if args.length > 0 then args(0) else "TIG"
    val archives = if args.length > 1 then args.drop(1).toList else List("arena_chr.s3d", "global6_chr.s3d")

    val loaded = archives.flatMap { arch =>
      try
        val (wld, skel, bones) = loadSkeleton(arch, actor)
        printSkeleton(s"$arch - $actor Skeleton", bones)
        Some((arch, bones))
      catch case e: Exception =>
        println(s"\n$arch: ${e.getMessage}")
        None
    }

    if loaded.size < 2 then return

    val (arch1, bones1) = loaded(0)
    val (arch2, bones2) = loaded(1)

    // ---- Compare ----
    println(s"\n=== Comparison ===")
    println(s"  $arch1 bones: ${bones1.length}")
    println(s"  $arch2 bones: ${bones2.length}")

    if bones1.length != bones2.length then
      println(s"  DIFFERENT bone count!")

    val parents1 = bones1.map(_.parentIndex).toSeq
    val parents2 = bones2.map(_.parentIndex).toSeq

    if parents1 == parents2 then
      println("\n  SAME ORDER - index mapping is correct")
      println(f"\n  ${"Idx"}%4s  ${"Parent"}%6s  ${"Track 1"}%-40s  Track 2")
      println("  " + "-" * 100)
      for i <- bones1.indices do
        val a = bones1(i)
        val g = if i < bones2.length then bones2(i) else BoneInfo(i, -999, "<missing>")
        val marker = if a.trackName != g.trackName then " <-- DIFF" else ""
        println(f"  [${a.index}%2d]  ${a.parentIndex}%6d  ${a.trackName}%-40s  ${g.trackName}$marker")
    else
      println("\n  DIFFERENT parent hierarchy! Building bone mapping by track name...")

      val byTrack2 = bones2.map(b => b.trackName.toUpperCase -> b).toMap
      println(f"\n  ${"Idx1"}%5s  ${"Parent1"}%8s  ${"Track"}%-40s  ${"Idx2"}%5s  ${"Parent2"}%8s")
      println("  " + "-" * 110)
      for a <- bones1 do
        val key = a.trackName.toUpperCase
        val g = byTrack2.get(key)
        val gIdx = g.map(_.index.toString).getOrElse("???")
        val gPar = g.map(_.parentIndex.toString).getOrElse("???")
        val marker = g match
          case Some(gb) if gb.index != a.index => " <-- REORDERED"
          case None => " <-- NOT FOUND"
          case _ => ""
        println(f"  [${a.index}%2d]  ${a.parentIndex}%8d  ${a.trackName}%-40s  ${gIdx}%5s  ${gPar}%8s$marker")

      // Print explicit index mapping array
      println(s"\n  Index mapping ($arch1 -> $arch2):")
      val mapping = bones1.map { a =>
        val key = a.trackName.toUpperCase
        byTrack2.get(key).map(_.index).getOrElse(-1)
      }
      println(s"  ${mapping.mkString(", ")}")
