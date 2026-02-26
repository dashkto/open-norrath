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

  def loadTigSkeleton(archiveName: String): (WldFile, Fragment10_SkeletonHierarchy, Array[BoneInfo]) =
    val path = Path.of(AssetsDir, archiveName)
    val entries = PfsArchive.load(path)
    val wldEntry = entries.find(_.extension == "wld").get
    val wld = WldFile(wldEntry.data)

    val actors = wld.fragmentsOfType[Fragment14_Actor]
    val tigActor = actors.find(_.name.toUpperCase.contains("TIG")).getOrElse {
      throw RuntimeException(s"No TIG actor found in $archiveName")
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
    // ---- Load arena_chr.s3d TIG skeleton ----
    val (arenaWld, arenaSkel, arenaBones) = loadTigSkeleton("arena_chr.s3d")
    printSkeleton("arena_chr.s3d - TIG Skeleton", arenaBones)

    // ---- Load global6_chr.s3d TIG skeleton ----
    val (global6Wld, global6Skel, global6Bones) = loadTigSkeleton("global6_chr.s3d")
    printSkeleton("global6_chr.s3d - TIG Skeleton", global6Bones)

    // ---- Compare ----
    println(s"\n=== Comparison ===")
    println(s"  arena_chr  bones: ${arenaBones.length}")
    println(s"  global6_chr bones: ${global6Bones.length}")

    if arenaBones.length != global6Bones.length then
      println(s"  DIFFERENT bone count!")

    val arenaParents = arenaBones.map(_.parentIndex).toSeq
    val global6Parents = global6Bones.map(_.parentIndex).toSeq

    if arenaParents == global6Parents then
      println("\n  SAME ORDER - index mapping is correct")
      // Still show side-by-side for track name comparison
      println(f"\n  ${"Idx"}%4s  ${"Parent"}%6s  ${"Arena Track"}%-40s  Global6 Track")
      println("  " + "-" * 100)
      for i <- arenaBones.indices do
        val a = arenaBones(i)
        val g = if i < global6Bones.length then global6Bones(i) else BoneInfo(i, -999, "<missing>")
        val marker = if a.trackName != g.trackName then " <-- DIFF" else ""
        println(f"  [${a.index}%2d]  ${a.parentIndex}%6d  ${a.trackName}%-40s  ${g.trackName}$marker")
    else
      println("\n  DIFFERENT parent hierarchy! Building bone mapping by track name...")

      // Build mapping: for each arena bone, find the global6 bone with the same track name
      val global6ByTrack = global6Bones.map(b => b.trackName.toUpperCase -> b).toMap
      println(f"\n  ${"Arena"}%5s  ${"ArenaParent"}%11s  ${"Track"}%-40s  ${"Global6"}%7s  ${"G6Parent"}%8s")
      println("  " + "-" * 110)
      for a <- arenaBones do
        val key = a.trackName.toUpperCase
        val g = global6ByTrack.get(key)
        val gIdx = g.map(_.index.toString).getOrElse("???")
        val gPar = g.map(_.parentIndex.toString).getOrElse("???")
        val marker = g match
          case Some(gb) if gb.index != a.index => " <-- REORDERED"
          case None => " <-- NOT FOUND"
          case _ => ""
        println(f"  [${a.index}%2d]  ${a.parentIndex}%11d  ${a.trackName}%-40s  ${gIdx}%7s  ${gPar}%8s$marker")

      // Print explicit index mapping array
      println("\n  Index mapping (arena -> global6):")
      val mapping = arenaBones.map { a =>
        val key = a.trackName.toUpperCase
        global6ByTrack.get(key).map(_.index).getOrElse(-1)
      }
      println(s"  ${mapping.mkString(", ")}")
