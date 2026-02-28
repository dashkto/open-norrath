package opennorrath.state

import org.joml.Vector3f
import opennorrath.network.GameTime

/** Tracks EQ game time, advancing locally from a server-provided anchor.
  *
  * EQ time runs at 1 EQ minute = 3 real seconds (20x speed).
  * The server sends OP_TimeOfDay once during zone entry; after that the client
  * advances the clock locally. A fresh anchor resyncs on every zone.
  *
  * Calendar: hours 1-24, minutes 0-59, days 1-28, months 1-12.
  */
object GameClock:

  private val EqMinuteMs = 3000L // 1 EQ minute = 3 real seconds

  /** Default when not synced — noon gives good lighting without needing server data. */
  private val Noon = GameTime(hour = 12, minute = 0, day = 1, month = 1, year = 0)

  // Anchor: server-reported EQ time + the real time when we received it
  @volatile private var anchorTotalMinutes: Long = 0
  @volatile private var anchorRealMs: Long = 0
  @volatile private var synced: Boolean = false

  /** Set the time anchor from an OP_TimeOfDay packet. */
  def sync(time: GameTime): Unit =
    anchorTotalMinutes = toTotalMinutes(time)
    anchorRealMs = System.currentTimeMillis()
    synced = true

  /** Override the clock with a specific hour and minute. Treated as a local anchor
    * that advances normally — will be overwritten by the next server OP_TimeOfDay.
    */
  def setTime(hour: Int, minute: Int): Unit =
    val t = if synced then now else Noon
    val overridden = GameTime(hour = hour, minute = minute, day = t.day, month = t.month, year = t.year)
    anchorTotalMinutes = toTotalMinutes(overridden)
    anchorRealMs = System.currentTimeMillis()
    synced = true

  /** Whether we've received at least one time sync from the server. */
  def isSynced: Boolean = synced

  /** Current EQ game time, advanced from the anchor by elapsed real time. Defaults to noon. */
  def now: GameTime =
    if !synced then return Noon
    val elapsedMs = System.currentTimeMillis() - anchorRealMs
    val elapsedEqMinutes = elapsedMs / EqMinuteMs
    fromTotalMinutes(anchorTotalMinutes + elapsedEqMinutes)

  /** Current hour (1-24) for quick day/night checks. */
  def hour: Int = now.hour

  /** True if it's nighttime in Norrath (roughly 8pm-6am, hours 20-24 and 1-6). */
  def isNight: Boolean =
    val h = hour
    h >= 20 || h <= 6

  // ---------------------------------------------------------------------------
  // Lighting — sun direction, ambient, and light color derived from game time
  // ---------------------------------------------------------------------------

  /** Sun direction in GL space, pointing FROM sun TOWARD scene.
    * Sweeps east→overhead→west during the day; fixed dim moonlight at night.
    */
  def sunDirection: Vector3f =
    val t = now
    sunDirectionForTime(t.hour, t.minute)

  /** Ambient light strength: brighter at noon, dimmer at night. */
  def ambientStrength: Float =
    val t = now
    ambientForTime(t.hour, t.minute)

  /** Light color tint: warm at sunrise/sunset, cool at night, white at midday. */
  def lightColor: Vector3f =
    val t = now
    lightColorForTime(t.hour, t.minute)

  /** Continuous hour 0-23.99 from EQ hours (1-24) + minutes. */
  private def hourFloat(hour: Int, minute: Int): Float =
    (hour % 24).toFloat + minute / 60f

  /** Sun elevation angle: 0 at dawn/dusk, 1 at noon, negative at night. */
  private def sunElevation(hf: Float): Float =
    val sunAngle = ((hf - 6.0) / 12.0 * math.Pi).toFloat
    math.sin(sunAngle).toFloat

  def sunDirectionForTime(hour: Int, minute: Int): Vector3f =
    val hf = hourFloat(hour, minute)
    val sunAngle = ((hf - 6.0) / 12.0 * math.Pi).toFloat
    val elevation = math.sin(sunAngle).toFloat

    if elevation > 0.05f then
      // Daytime: sun sweeps east→overhead→west
      val sweep = math.cos(sunAngle).toFloat
      Vector3f(sweep, -elevation, 0.3f).normalize()
    else
      // Night: fixed soft moonlight from above
      Vector3f(0.2f, -0.9f, 0.3f).normalize()

  def ambientForTime(hour: Int, minute: Int): Float =
    val hf = hourFloat(hour, minute)
    val elevation = sunElevation(hf)

    if elevation > 0 then
      // Day: 0.25 at horizon → 0.40 at noon
      0.25f + 0.15f * elevation
    else
      // Night: 0.12 at dusk/dawn → 0.06 at midnight
      0.06f + 0.06f * (1f + elevation)

  def lightColorForTime(hour: Int, minute: Int): Vector3f =
    val hf = hourFloat(hour, minute)
    val elevation = sunElevation(hf)

    if elevation > 0.3f then
      // Full daylight — white
      Vector3f(1f, 1f, 1f)
    else if elevation > 0f then
      // Dawn/dusk transition — warm orange, lerp toward white as sun rises
      val t = elevation / 0.3f // 0 at horizon → 1 at full day
      Vector3f(1f, 0.7f + 0.3f * t, 0.4f + 0.6f * t)
    else if elevation > -0.3f then
      // Twilight — transition from warm to cool
      val t = (elevation + 0.3f) / 0.3f // 0 at deep twilight → 1 at horizon
      Vector3f(0.2f + 0.8f * t, 0.25f + 0.45f * t, 0.4f + 0f * t)
    else
      // Deep night — dim blue moonlight
      Vector3f(0.2f, 0.25f, 0.4f)

  /** Sky clear color: bright blue at midday, warm at dawn/dusk, dark at night. */
  def skyColor: Vector3f =
    val t = now
    skyColorForTime(t.hour, t.minute)

  def skyColorForTime(hour: Int, minute: Int): Vector3f =
    val hf = hourFloat(hour, minute)
    val elevation = sunElevation(hf)

    if elevation > 0.3f then
      // Full daylight — bright sky blue
      Vector3f(0.3f, 0.5f, 0.7f)
    else if elevation > 0f then
      // Dawn/dusk — lerp from warm horizon to day blue
      val t = elevation / 0.3f
      Vector3f(0.4f + -0.1f * t, 0.25f + 0.25f * t, 0.3f + 0.4f * t)
    else if elevation > -0.3f then
      // Twilight — transition from warm horizon glow to dark
      val t = (elevation + 0.3f) / 0.3f // 0 at deep twilight → 1 at horizon
      Vector3f(0.05f + 0.35f * t, 0.05f + 0.20f * t, 0.1f + 0.2f * t)
    else
      // Deep night — near black with faint blue
      Vector3f(0.05f, 0.05f, 0.1f)

  // ---------------------------------------------------------------------------
  // Time arithmetic
  // ---------------------------------------------------------------------------

  /** Convert GameTime to total minutes since year 0 for easy arithmetic. */
  private def toTotalMinutes(t: GameTime): Long =
    val years = t.year.toLong
    val months = t.month - 1       // 0-based for math
    val days = t.day - 1           // 0-based for math
    val hours = t.hour - 1         // 0-based for math
    val minutes = t.minute
    minutes + hours * 60L + days * 24L * 60L + months * 28L * 24L * 60L + years * 12L * 28L * 24L * 60L

  /** Convert total minutes back to GameTime (1-based calendar fields). */
  private def fromTotalMinutes(total: Long): GameTime =
    var remaining = total
    val minute = (remaining % 60).toInt
    remaining /= 60
    val hour = (remaining % 24).toInt + 1     // 1-24
    remaining /= 24
    val day = (remaining % 28).toInt + 1      // 1-28
    remaining /= 28
    val month = (remaining % 12).toInt + 1    // 1-12
    remaining /= 12
    val year = remaining.toInt
    GameTime(hour = hour, minute = minute, day = day, month = month, year = year)
