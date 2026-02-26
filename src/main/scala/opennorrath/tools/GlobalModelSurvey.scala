package opennorrath.tools

import opennorrath.archive.PfsArchive
import opennorrath.wld.{WldFile, Fragment12_TrackDef}
import java.nio.file.{Path, Files}

/** Show all unique model prefixes with multi-frame animation tracks across all global files. */
object GlobalModelSurvey:
  def main(args: Array[String]): Unit =
    val dir = if args.nonEmpty then args(0) else "assets/EverQuest"
    val globalFiles = Files.list(Path.of(dir)).toArray.map(_.asInstanceOf[Path])
      .filter { p =>
        val name = p.getFileName.toString.toLowerCase
        name.startsWith("global") && name.contains("_chr") && name.endsWith(".s3d")
      }.sorted

    // Collect (model prefix → set of source files) for multi-frame animation tracks
    val modelSources = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
    val modelAnimCodes = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()

    for file <- globalFiles do
      try
        val entries = PfsArchive.load(file, extensionFilter = Some(Set("wld")))
        entries.find(_.extension == "wld").foreach { wldEntry =>
          val wld = WldFile(wldEntry.data)
          val multiFrame = wld.fragmentsOfType[Fragment12_TrackDef].filter(_.frames.length > 1)
          for td <- multiFrame do
            val name = td.name.toUpperCase.replace("_TRACKDEF", "")
            if name.length > 3 then
              val code = name.take(3)
              // Try to extract model prefix: strip the anim code (3 chars) and find the model
              // by looking for common bone suffixes at the end
              val rest = name.drop(3)
              // Model prefix is everything up to the first bone suffix
              // Common suffixes: PE, CH, HE, BI_R, BI_L, etc.
              // Use the shortest token that matches a known model (3-4 chars usually)
              for len <- 3 to math.min(5, rest.length) do
                val model = rest.take(len)
                modelSources.getOrElseUpdate(model, scala.collection.mutable.Set()) += file.getFileName.toString
                modelAnimCodes.getOrElseUpdate(model, scala.collection.mutable.Set()) += code
        }
      catch case _: Exception => ()

    // Filter to likely real model prefixes (have >= 5 anim codes = real character)
    val realModels = modelSources.filter { (model, _) =>
      modelAnimCodes.getOrElse(model, Set()).size >= 5
    }.toList.sortBy(_._1)

    println(s"Models with >=5 animation codes:")
    for (model, files) <- realModels do
      val codes = modelAnimCodes(model).toList.sorted
      println(f"  $model%-8s ${codes.size}%3d anim codes, from: ${files.toList.sorted.mkString(", ")}")
