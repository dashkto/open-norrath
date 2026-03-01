val scala3Version = "3.6.3"
val lwjglVersion = "3.3.6"
val jomlVersion = "1.10.8"
val imguiVersion = "1.87.6"

// Natives for the current build machine (used by `sbt run`)
val lwjglNatives = {
  val os = System.getProperty("os.name").toLowerCase
  val arch = System.getProperty("os.arch")
  if (os.contains("mac")) {
    if (arch == "aarch64" || arch == "arm64") "natives-macos-arm64"
    else "natives-macos"
  } else if (os.contains("linux")) {
    if (arch == "aarch64" || arch == "arm64") "natives-linux-arm64"
    else "natives-linux"
  } else if (os.contains("win")) {
    "natives-windows"
  } else {
    throw new RuntimeException(s"Unsupported OS: $os")
  }
}

// All platform natives (bundled into the fat jar for cross-platform distribution)
val allLwjglNatives = Seq(
  "natives-macos", "natives-macos-arm64",
  "natives-linux", "natives-linux-arm64",
  "natives-windows",
)

val lwjglModules = Seq("lwjgl", "lwjgl-glfw", "lwjgl-opengl", "lwjgl-openal", "lwjgl-stb")

lazy val root = project
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "open-norrath",
    version := "0.2.0",
    scalaVersion := scala3Version,
    fork := true,
    Compile / mainClass := Some("opennorrath.Main"),
    javaOptions += "-XstartOnFirstThread",
    buildInfoKeys := Seq[BuildInfoKey](
      version,
      BuildInfoKey.action("gitCommit") { scala.sys.process.Process("git rev-parse --short HEAD").!!.trim },
      BuildInfoKey.action("buildTime") { java.time.Instant.now().toString },
    ),
    buildInfoPackage := "opennorrath",

    // Assembly configuration for fat jar
    assembly / assemblyJarName := "open-norrath.jar",
    assembly / mainClass := Some("opennorrath.Main"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "versions", _*) => MergeStrategy.first
      case PathList("META-INF", xs @ _*) =>
        xs.map(_.toLowerCase) match {
          case "manifest.mf" :: Nil => MergeStrategy.discard
          case "services" :: _      => MergeStrategy.filterDistinctLines
          case _                    => MergeStrategy.discard
        }
      case _ => MergeStrategy.first
    },

    libraryDependencies ++= Seq(
      // LWJGL core modules
      lwjglModules.map(m => "org.lwjgl" % m % lwjglVersion),
      // Natives for current platform (sbt run)
      lwjglModules.map(m => "org.lwjgl" % m % lwjglVersion classifier lwjglNatives),
      // Natives for all platforms (fat jar)
      for {
        m <- lwjglModules
        n <- allLwjglNatives
      } yield "org.lwjgl" % m % lwjglVersion classifier n,
      Seq(
        // Math
        "org.joml" % "joml" % jomlVersion,
        // ImGui — binding + all platform natives
        "io.github.spair" % "imgui-java-binding"         % imguiVersion,
        "io.github.spair" % "imgui-java-lwjgl3"           % imguiVersion,
        "io.github.spair" % "imgui-java-natives-macos"     % imguiVersion,
        "io.github.spair" % "imgui-java-natives-linux"     % imguiVersion,
        "io.github.spair" % "imgui-java-natives-windows"   % imguiVersion,
        // YAML settings
        "org.yaml" % "snakeyaml" % "2.3",
      ),
    ).flatten,
  )
