package opennorrath

import org.yaml.snakeyaml.Yaml
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

case class LoginSettings(
  host: String = "127.0.0.1",
  port: Int = 6000,
  worldPort: Int = 9000,
)

case class WindowSettings(
  width: Int = 1600,
  height: Int = 900,
)

case class RenderSettings(
  characterDistance: Float = 1000f, // Max distance (EQ units) to render zone characters
)

/** A named server profile. Combines connection info with protocol selection. */
case class ServerConfig(
  name: String,
  host: String = "127.0.0.1",
  port: Int = 6000,
  worldPort: Int = 9000,
  macMode: Boolean = false,
)

case class Settings(
  useEqg: Boolean = false,
  macMode: Boolean = true, // true = Mac protocol, false = Titanium (PC) protocol
  window: WindowSettings = WindowSettings(),
  render: RenderSettings = RenderSettings(),
  login: LoginSettings = LoginSettings(),
  servers: Map[String, ServerConfig] = Map.empty,
  activeServer: String = "",
):
  /** Resolved server: if activeServer points to a valid servers entry, use it.
    * Otherwise fall back to the top-level login/macMode fields.
    */
  lazy val resolvedServer: Option[ServerConfig] = servers.get(activeServer)

  /** Login settings resolved from the active server profile, or the top-level login block. */
  lazy val resolvedLogin: LoginSettings = resolvedServer match
    case Some(s) => LoginSettings(s.host, s.port, s.worldPort)
    case None => login

  /** Mac mode resolved from the active server profile, or the top-level mac_mode field. */
  lazy val resolvedMacMode: Boolean = resolvedServer.map(_.macMode).getOrElse(macMode)

/** Typed wrapper around SnakeYAML's raw Map[String, Any] output. */
private class YamlMap(data: java.util.Map[String, Any]):
  private val map = data.asScala

  def string(key: String, default: String = ""): String =
    map.get(key).map(_.toString).getOrElse(default)

  def boolean(key: String, default: Boolean = false): Boolean =
    map.get(key).map(_.toString.toBoolean).getOrElse(default)

  def int(key: String, default: Int = 0): Int =
    map.get(key).map(_.toString.toInt).getOrElse(default)

  def float(key: String, default: Float = 0f): Float =
    map.get(key).map(_.toString.toFloat).getOrElse(default)

  def nested(key: String): Option[YamlMap] =
    map.get(key).collect { case m: java.util.Map[?, ?] =>
      YamlMap(m.asInstanceOf[java.util.Map[String, Any]])
    }

  /** Iterate a map of named nested objects (e.g. servers: { takp: {...}, p99: {...} }). */
  def nestedMap(key: String): Map[String, YamlMap] =
    map.get(key) match
      case Some(m: java.util.Map[?, ?]) =>
        m.asInstanceOf[java.util.Map[String, Any]].asScala.collect {
          case (k, v: java.util.Map[?, ?]) =>
            k -> YamlMap(v.asInstanceOf[java.util.Map[String, Any]])
        }.toMap
      case _ => Map.empty

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
    val windowSettings = root.nested("window").map { w =>
      WindowSettings(
        width = w.int("width", 1280),
        height = w.int("height", 720),
      )
    }.getOrElse(WindowSettings())

    val renderSettings = root.nested("render").map { r =>
      RenderSettings(
        characterDistance = r.float("character_distance", 200f),
      )
    }.getOrElse(RenderSettings())

    val login = root.nested("login").map { l =>
      LoginSettings(
        host = l.string("host", "127.0.0.1"),
        port = l.int("port", 6000),
        worldPort = l.int("world_port", 9000),
      )
    }.getOrElse(LoginSettings())

    val servers = root.nestedMap("servers").map { (name, s) =>
      name -> ServerConfig(
        name = name,
        host = s.string("host", "127.0.0.1"),
        port = s.int("port", 6000),
        worldPort = s.int("world_port", 9000),
        macMode = s.boolean("mac_mode", default = false),
      )
    }
    val activeServer = root.string("active_server")

    Settings(
      useEqg = root.boolean("use_eqg"),
      macMode = root.boolean("mac_mode", default = true),
      window = windowSettings,
      render = renderSettings,
      login = login,
      servers = servers,
      activeServer = activeServer,
    )
