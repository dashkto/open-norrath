package opennorrath

import org.yaml.snakeyaml.Yaml
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

case class DebugSettings(
  animationModel: String = "",
)

case class LoginSettings(
  host: String = "127.0.0.1",
  port: Int = 6000,
  worldPort: Int = 9000,
)

case class WindowSettings(
  width: Int = 1600,
  height: Int = 900,
)

case class Settings(
  useEqg: Boolean = false,
  window: WindowSettings = WindowSettings(),
  debug: DebugSettings = DebugSettings(),
  login: LoginSettings = LoginSettings(),
)

/** Typed wrapper around SnakeYAML's raw Map[String, Any] output. */
private class YamlMap(data: java.util.Map[String, Any]):
  private val map = data.asScala

  def string(key: String, default: String = ""): String =
    map.get(key).map(_.toString).getOrElse(default)

  def boolean(key: String, default: Boolean = false): Boolean =
    map.get(key).map(_.toString.toBoolean).getOrElse(default)

  def int(key: String, default: Int = 0): Int =
    map.get(key).map(_.toString.toInt).getOrElse(default)

  def nested(key: String): Option[YamlMap] =
    map.get(key).collect { case m: java.util.Map[?, ?] =>
      YamlMap(m.asInstanceOf[java.util.Map[String, Any]])
    }

object Settings:

  def load(path: String = "settings.yml"): Settings =
    val file = Path.of(path)
    if !Files.exists(file) then
      println(s"Settings file not found: $path, using defaults")
      return Settings()

    val yaml = Yaml()
    val raw = yaml.load[java.util.Map[String, Any]](Files.newInputStream(file))
    if raw == null then return Settings()

    val root = YamlMap(raw)
    val debug = root.nested("debug").map { d =>
      DebugSettings(animationModel = d.string("animation_model"))
    }.getOrElse(DebugSettings())

    val windowSettings = root.nested("window").map { w =>
      WindowSettings(
        width = w.int("width", 1280),
        height = w.int("height", 720),
      )
    }.getOrElse(WindowSettings())

    val login = root.nested("login").map { l =>
      LoginSettings(
        host = l.string("host", "127.0.0.1"),
        port = l.int("port", 6000),
        worldPort = l.int("world_port", 9000),
      )
    }.getOrElse(LoginSettings())

    Settings(
      useEqg = root.boolean("use_eqg"),
      window = windowSettings,
      debug = debug,
      login = login,
    )
