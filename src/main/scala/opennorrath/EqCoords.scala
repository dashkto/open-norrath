package opennorrath

import org.joml.Vector3f

/** Coordinate conversion between EQ systems and OpenGL.
  *
  * EQ has two coordinate systems that differ by an X/Y swap:
  *
  *   1. '''S3D files''' (zone mesh, object placements): X=east, Y=north, Z=up
  *   2. '''Server''' (spawn positions, /loc output):    X=north, Y=east, Z=up
  *
  * The swap happens in the server's map generation tool (map.cpp line 772)
  * which swaps vertex X and Y when building collision geometry from S3D data.
  * All server coordinates (player profile, spawn structs, zone safe points)
  * use the swapped system.
  *
  * OpenGL uses Y-up, so the final mapping is:
  *   - S3D  → GL:  (eqX, eqZ, -eqY)
  *   - Server → GL: (serverY, serverZ, -serverX)
  */
object EqCoords:

  // ---------------------------------------------------------------------------
  // S3D file coordinates → GL
  // ---------------------------------------------------------------------------

  /** Convert S3D vertex/placement coordinates to GL.
    * Used by zone mesh vertices, object placements, character animation output.
    */
  inline def s3dToGl(eqX: Float, eqY: Float, eqZ: Float): (Float, Float, Float) =
    (eqX, eqZ, -eqY)

  // ---------------------------------------------------------------------------
  // Server coordinates → GL
  // ---------------------------------------------------------------------------

  /** Convert server coordinates (spawn, profile, safe point) to a GL position.
    * Applies the X/Y swap relative to S3D, then the Z-up → Y-up transform.
    */
  def serverToGl(serverY: Float, serverX: Float, serverZ: Float): Vector3f =
    Vector3f(serverY, serverZ, -serverX)

  // ---------------------------------------------------------------------------
  // Heading conversion
  // ---------------------------------------------------------------------------

  /** Convert a spawn heading byte (0-255, 0=north, clockwise) to camera yaw degrees.
    * EQ north (+Y server / -Z GL) corresponds to camera yaw -90 degrees.
    */
  inline def spawnHeadingToYaw(heading: Int): Float =
    (heading * 360f / 256f) - 90f

  /** Convert a profile heading float (0-512, 0=north, clockwise) to camera yaw degrees. */
  inline def profileHeadingToYaw(heading: Float): Float =
    (heading * 360f / 512f) - 90f

  /** Convert a spawn heading byte (0-255, 0=north, clockwise) to model rotation radians.
    * EQ models face south in their rest pose, so add π to flip them to face north at heading 0.
    */
  inline def spawnHeadingToRadians(heading: Int): Float =
    Math.PI.toFloat - (heading * 2f * Math.PI.toFloat / 256f)
