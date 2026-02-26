package opennorrath

import opennorrath.animation.AnimatedCharacter
import opennorrath.wld.*
import org.joml.{Matrix4f, Vector3f}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glVertexAttrib3f

/** Debug renderer that wraps ZoneRenderer and adds an animation showcase grid.
  * All animations for `animationModel` are displayed simultaneously in a grid above the arena.
  * Delegates spawn management to the inner ZoneRenderer.
  */
class ZoneRenderDebug(s3dPath: String, settings: Settings, animationModel: String):

  private val zone = ZoneRenderer(s3dPath, settings)

  /** Expose character build templates for spawn creation. */
  def characterBuilds: Map[String, ZoneRenderer.CharBuild] = zone.characterBuilds

  // Spawn management — delegate to ZoneRenderer
  def addSpawn(spawnId: Int, modelCode: String, position: Vector3f, heading: Int, size: Float): Boolean =
    zone.addSpawn(spawnId, modelCode, position, heading, size)

  def removeSpawn(spawnId: Int): Unit = zone.removeSpawn(spawnId)

  def updateSpawnPosition(spawnId: Int, position: Vector3f, heading: Int): Unit =
    zone.updateSpawnPosition(spawnId, position, heading)

  private val showcaseChars: List[AnimatedCharacter] =
    if animationModel.nonEmpty then loadShowcase() else Nil

  def draw(shader: Shader, deltaTime: Float, viewMatrix: Matrix4f): Unit =
    zone.draw(shader, deltaTime, viewMatrix)

    if showcaseChars.nonEmpty then
      glVertexAttrib3f(2, 1f, 1f, 1f)
      for char <- showcaseChars do
        char.update(deltaTime)
        shader.setMatrix4f("model", char.modelMatrix)
        for group <- char.zoneMesh.groups do
          if group.materialType != MaterialType.Invisible && group.materialType != MaterialType.Boundary then
            glBindTexture(GL_TEXTURE_2D, zone.resolveTexture(group.textureName))
            char.glMesh.drawRange(group.startIndex, group.indexCount)

  def cleanup(): Unit =
    zone.cleanup()
    showcaseChars.foreach(_.glMesh.cleanup())

  private def loadShowcase(): List[AnimatedCharacter] =
    val target = animationModel.toLowerCase
    val buildOpt = zone.characterBuilds.get(target)
    if buildOpt.isEmpty then
      println(s"  Animation model '$target' not found, available: ${zone.characterBuilds.keys.toSeq.sorted.mkString(", ")}")
      return Nil

    val build = buildOpt.get
    val sortedClips = build.clips.toList.sortBy(_._1)
    val count = sortedClips.size
    val cols = math.ceil(math.sqrt(count.toDouble)).toInt
    val debugScale = 5f
    val spacing = (math.max(build.glWidth, build.glDepth) + 20f) * debugScale

    // Center grid above the arena plinth: GL(-38.6, 19.1, -393.9)
    val centerX = -38.6f
    val centerY = 80f
    val centerZ = -393.9f
    val rows = math.ceil(count.toDouble / cols).toInt

    val results = sortedClips.zipWithIndex.map { case ((code, _), i) =>
      val col = i % cols
      val row = i / cols
      val x = centerX + (col - (cols - 1) / 2f) * spacing
      val z = centerZ + (row - (rows - 1) / 2f) * -spacing

      val interleaved = ZoneRenderer.buildInterleaved(build.zm)
      val glMesh = Mesh(interleaved, build.zm.indices, dynamic = true)
      val modelMatrix = Matrix4f()
      modelMatrix.translate(x, centerY, z)
      modelMatrix.scale(debugScale)
      modelMatrix.translate(-build.glCenterX, -build.glMinY, -build.glCenterZ)

      val char = AnimatedCharacter(build.skeleton, build.meshFragments, build.zm, glMesh, modelMatrix, build.clips, interleaved.clone())
      char.play(code)
      println(s"    $code at grid($col, $row)")
      char
    }

    println(s"  Animation showcase: ${results.size} clips for '$target' at y=300")
    results
