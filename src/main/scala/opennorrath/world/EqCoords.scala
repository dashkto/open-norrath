package opennorrath.world

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

  /** Convert GL position back to server coordinates (inverse of serverToGl).
    * GL(x,y,z) → server(y=x, x=-z, z=y)
    */
  inline def glToServer(glX: Float, glY: Float, glZ: Float): (Float, Float, Float) =
    (glX, -glZ, glY) // (serverY, serverX, serverZ)

  // ---------------------------------------------------------------------------
  // Heading conversion
  // ---------------------------------------------------------------------------

  /** Convert a spawn heading byte (0-255, 0=east, CCW) to camera yaw degrees.
    * Camera yaw uses CW convention, so we negate.
    */
  inline def spawnHeadingToYaw(heading: Int): Float =
    -(heading * 360f / 256f)

  /** Convert a profile heading float (0-512, 0=east, CCW) to camera yaw degrees. */
  inline def profileHeadingToYaw(heading: Float): Float =
    -(heading * 360f / 512f)

  /** Convert a spawn heading byte (0-255, 0=east, CCW) to model rotation radians.
    * EQ models face east (+X) in their rest pose after s3dToGl conversion,
    * so heading 0 = no rotation. Heading increases CCW when viewed from above.
    */
  inline def spawnHeadingToRadians(heading: Int): Float =
    heading * 2f * Math.PI.toFloat / 256f
