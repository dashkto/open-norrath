package opennorrath.world

import java.nio.file.Path

import opennorrath.archive.{PfsArchive, PfsEntry}
import opennorrath.wld.{WldFile, ZoneGeometry, ZoneLineBsp, ZoneMesh}

/** Consolidated zone data: identity + parsed S3D content + zone lines.
  *
  * This is a pure data class — no GPU resources. ZoneRenderer takes a Zone
  * and builds the GL-side meshes, textures, and shadow maps.
  */
case class Zone(
    shortName: String,         // e.g. "qeynos"
    id: Int,                   // zone ID from server (0 if offline/unknown)
    s3dPath: String,           // filesystem path to the .s3d archive
    entries: List[PfsEntry],   // all files inside the S3D archive
    wld: WldFile,              // parsed zone WLD (excludes objects/lights WLDs)
    zoneMesh: ZoneMesh,        // extracted geometry
    zoneLineBsp: ZoneLineBsp,  // BSP-based zone line detection
)

object Zone:
  /** Load a zone from its short name (e.g. "arena", "qeynos"). */
  def load(shortName: String, id: Int = 0): Zone =
    val s3dPath = s"assets/EverQuest/$shortName.s3d"
    val entries = PfsArchive.load(Path.of(s3dPath))
    val zoneWld = entries.find(e => e.extension == "wld" && !e.name.contains("objects") && !e.name.contains("lights"))
      .getOrElse(throw RuntimeException(s"No zone WLD found in $s3dPath"))
    val wld = WldFile(zoneWld.data)
    val zoneMesh = ZoneGeometry.extract(wld)
    val zoneLineBsp = ZoneGeometry.extractZoneLineBsp(wld)
    Zone(shortName, id, s3dPath, entries, wld, zoneMesh, zoneLineBsp)
