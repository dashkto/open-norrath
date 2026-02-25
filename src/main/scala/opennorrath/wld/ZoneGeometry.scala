package opennorrath.wld

case class ZoneMeshGroup(startIndex: Int, indexCount: Int, textureName: String, materialType: MaterialType)

case class ZoneMesh(
    vertices: Array[Float],  // x, y, z interleaved
    uvs: Array[Float],       // u, v interleaved
    indices: Array[Int],
    groups: List[ZoneMeshGroup],
)

object ZoneGeometry:

  def extract(wld: WldFile): ZoneMesh =
    val meshes = wld.fragmentsOfType[Fragment36_Mesh]

    var allVertices = Array.empty[Float]
    var allUvs = Array.empty[Float]
    var allIndices = Array.empty[Int]
    var allGroups = List.empty[ZoneMeshGroup]

    for mesh <- meshes do
      val vertexOffset = allVertices.length / 3

      // Append vertices
      val verts = mesh.vertices.flatMap(v => Array(v.x, v.y, v.z))
      allVertices = allVertices ++ verts

      // Append UVs
      val uvs = mesh.uvs.flatMap((u, v) => Array(u, v))
      allUvs = allUvs ++ uvs

      // Process render groups to get material-grouped triangles
      var polyIndex = 0
      for group <- mesh.renderGroups do
        val startIndex = allIndices.length
        val textureName = resolveTextureName(wld, mesh.materialListRef, group.materialIndex)
        val matType = resolveMaterialType(wld, mesh.materialListRef, group.materialIndex)

        for _ <- 0 until group.polyCount do
          if polyIndex < mesh.polygons.length then
            val poly = mesh.polygons(polyIndex)
            allIndices = allIndices ++ Array(
              poly.v1 + vertexOffset,
              poly.v2 + vertexOffset,
              poly.v3 + vertexOffset,
            )
            polyIndex += 1

        val indexCount = allIndices.length - startIndex
        if indexCount > 0 then
          allGroups = allGroups :+ ZoneMeshGroup(startIndex, indexCount, textureName, matType)

    ZoneMesh(allVertices, allUvs, allIndices, allGroups)

  def resolveTextureName(wld: WldFile, materialListRef: Int, materialIndex: Int): String =
    try
      val matList = wld.fragment(materialListRef).asInstanceOf[Fragment31_MaterialList]
      val matRef = matList.materialRefs(materialIndex)
      val material = wld.fragment(matRef).asInstanceOf[Fragment30_Material]

      if material.bitmapInfoRefIndex == 0 then return ""

      val bitmapInfoRef = wld.fragment(material.bitmapInfoRefIndex).asInstanceOf[Fragment05_BitmapInfoRef]
      val bitmapInfo = wld.fragment(bitmapInfoRef.refIndex).asInstanceOf[Fragment04_BitmapInfo]

      if bitmapInfo.textureRefs.isEmpty then return ""

      val bitmapName = wld.fragment(bitmapInfo.textureRefs.head).asInstanceOf[Fragment03_BitmapName]
      bitmapName.filename
    catch case _: Exception => ""

  def resolveMaterialType(wld: WldFile, materialListRef: Int, materialIndex: Int): MaterialType =
    try
      val matList = wld.fragment(materialListRef).asInstanceOf[Fragment31_MaterialList]
      val matRef = matList.materialRefs(materialIndex)
      val material = wld.fragment(matRef).asInstanceOf[Fragment30_Material]
      material.shaderType
    catch case _: Exception => MaterialType.Diffuse
