package opennorrath.world

object LightBaker:

  case class LineLight(
    x1: Float, y1: Float, z1: Float,
    x2: Float, y2: Float, z2: Float,
    r: Float, g: Float, b: Float,
  )

  val DefaultRadius: Float = 300f
  val Ambient: (Float, Float, Float) = (0.08f, 0.08f, 0.10f)

  def parseLights(path: String): List[LineLight] =
    val source = scala.io.Source.fromFile(path)
    try
      source.getLines().flatMap { line =>
        val trimmed = line.trim
        if trimmed.startsWith("L ") then
          val parts = trimmed.drop(2).split(",").map(_.trim.toFloat)
          if parts.length == 9 then
            // Convert EQ coords to GL: GL(X,Y,Z) = EQ(X,Z,-Y)
            Some(LineLight(
              parts(0), parts(2), -parts(1),
              parts(3), parts(5), -parts(4),
              parts(6) / 255f, parts(7) / 255f, parts(8) / 255f,
            ))
          else None
        else None
      }.toList
    finally source.close()

  /** Closest-point-on-line-segment distance squared. */
  private def distSqToSegment(
    px: Float, py: Float, pz: Float,
    ax: Float, ay: Float, az: Float,
    bx: Float, by: Float, bz: Float,
  ): Float =
    val abx = bx - ax; val aby = by - ay; val abz = bz - az
    val apx = px - ax; val apy = py - ay; val apz = pz - az
    val dot = apx * abx + apy * aby + apz * abz
    val lenSq = abx * abx + aby * aby + abz * abz
    val t = if lenSq == 0f then 0f else (dot / lenSq).max(0f).min(1f)
    val cx = ax + t * abx - px
    val cy = ay + t * aby - py
    val cz = az + t * abz - pz
    cx * cx + cy * cy + cz * cz

  /** Bake vertex colors from line lights. Takes stride-5 buffer, returns stride-8 (pos3+uv2+color3). */
  def bakeVertexColors(
    vertices: Array[Float],
    lights: List[LineLight],
    radius: Float = DefaultRadius,
    ambient: (Float, Float, Float) = Ambient,
  ): Array[Float] =
    val vertexCount = vertices.length / 5
    val result = new Array[Float](vertexCount * 8)
    val radiusSq = radius * radius

    for i <- 0 until vertexCount do
      val px = vertices(i * 5 + 0)
      val py = vertices(i * 5 + 1)
      val pz = vertices(i * 5 + 2)

      var r = ambient._1
      var g = ambient._2
      var b = ambient._3

      for light <- lights do
        val dSq = distSqToSegment(px, py, pz,
          light.x1, light.y1, light.z1,
          light.x2, light.y2, light.z2)
        if dSq < radiusSq then
          val d = math.sqrt(dSq.toDouble).toFloat
          val atten = (1f - d / radius) * (1f - d / radius) // quadratic falloff with hard cutoff
          r += light.r * atten
          g += light.g * atten
          b += light.b * atten

      result(i * 8 + 0) = px
      result(i * 8 + 1) = py
      result(i * 8 + 2) = pz
      result(i * 8 + 3) = vertices(i * 5 + 3)
      result(i * 8 + 4) = vertices(i * 5 + 4)
      result(i * 8 + 5) = r.min(1f)
      result(i * 8 + 6) = g.min(1f)
      result(i * 8 + 7) = b.min(1f)

    result
