package opennorrath.world

import opennorrath.archive.PfsEntry
import opennorrath.render.Texture
import org.lwjgl.opengl.GL11.glDeleteTextures
import java.nio.file.{Files, Path}

/** Persistent store for equipment models from gequip*.s3d archives.
  * Loads once at startup; shared by ZoneRenderer and CharacterPreview.
  */
object EquipmentModels:

  private var _models: Map[Int, ZoneRenderer.EquipModel] = Map.empty
  private var _textures: Map[String, Int] = Map.empty

  def models: Map[Int, ZoneRenderer.EquipModel] = _models
  def textures: Map[String, Int] = _textures
  def get(itNum: Int): Option[ZoneRenderer.EquipModel] = _models.get(itNum)

  def init(assetsDir: String): Unit =
    val dir = Path.of(assetsDir)
    if !Files.isDirectory(dir) then return

    val texMap = scala.collection.mutable.Map[String, Int]()
    val loadTex: List[PfsEntry] => Unit = entries =>
      for entry <- entries if entry.extension == "bmp" do
        val key = entry.name.toLowerCase
        if !texMap.contains(key) then
          try texMap(key) = Texture.loadFromBytes(entry.data, applyColorKey = false)
          catch case _: Exception => ()

    _models = ZoneRenderer.loadEquipmentModels(dir, loadTex)
    _textures = texMap.toMap

  def cleanup(): Unit =
    _models.values.foreach(_.glMesh.cleanup())
    _textures.values.foreach(glDeleteTextures)
    _models = Map.empty
    _textures = Map.empty
