val scala3Version = "3.6.3"
val lwjglVersion = "3.3.6"
val jomlVersion = "1.10.8"
val imguiVersion = "1.87.6"

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

lazy val root = project
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "open-norrath",
    version := "0.1.0-SNAPSHOT",
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
    libraryDependencies ++= Seq(
      // LWJGL core
      "org.lwjgl" % "lwjgl"         % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw"    % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl"  % lwjglVersion,
      "org.lwjgl" % "lwjgl-openal"  % lwjglVersion,
      "org.lwjgl" % "lwjgl-stb"     % lwjglVersion,
      // Natives
      "org.lwjgl" % "lwjgl"         % lwjglVersion classifier lwjglNatives,
      "org.lwjgl" % "lwjgl-glfw"    % lwjglVersion classifier lwjglNatives,
      "org.lwjgl" % "lwjgl-opengl"  % lwjglVersion classifier lwjglNatives,
      "org.lwjgl" % "lwjgl-openal"  % lwjglVersion classifier lwjglNatives,
      "org.lwjgl" % "lwjgl-stb"     % lwjglVersion classifier lwjglNatives,
      // Math
      "org.joml" % "joml" % jomlVersion,
      // YAML settings
      "org.yaml" % "snakeyaml" % "2.3",
    ),
  )
