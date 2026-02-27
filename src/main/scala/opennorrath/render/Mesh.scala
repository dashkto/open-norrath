package opennorrath.render

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.BufferUtils

/** OpenGL mesh with configurable vertex layout.
  * Stride 5: [x, y, z, u, v]             — zone mesh, objects, static models (GL-space positions)
  * Stride 6: [x, y, z, u, v, boneIndex]  — skinned character meshes (bone-local S3D positions)
  * Stride 8: [x, y, z, u, v, r, g, b]    — zone mesh with baked vertex lighting
  *
  * Optional separate normal VBO: when normalsOpt is provided, a second VBO is created
  * with normals at attribute location 4. This avoids changing any existing stride/layout.
  */
class Mesh(vertices: Array[Float], indices: Array[Int], dynamic: Boolean = false, stride: Int = 5,
           normalsOpt: Option[Array[Float]] = None):

  private val vao = glGenVertexArrays()
  private val vbo = glGenBuffers()
  private val ebo = glGenBuffers()
  private val indexCount = indices.length
  private val vertexBufferSize = vertices.length

  glBindVertexArray(vao)

  val vbuf = BufferUtils.createFloatBuffer(vertices.length)
  vbuf.put(vertices).flip()
  glBindBuffer(GL_ARRAY_BUFFER, vbo)
  glBufferData(GL_ARRAY_BUFFER, vbuf, if dynamic then GL_DYNAMIC_DRAW else GL_STATIC_DRAW)

  val ibuf = BufferUtils.createIntBuffer(indices.length)
  ibuf.put(indices).flip()
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, ibuf, GL_STATIC_DRAW)

  private val strideBytes = stride * 4
  // position: 3 floats
  glVertexAttribPointer(0, 3, GL_FLOAT, false, strideBytes, 0)
  glEnableVertexAttribArray(0)
  // uv: 2 floats
  glVertexAttribPointer(1, 2, GL_FLOAT, false, strideBytes, 3 * 4)
  glEnableVertexAttribArray(1)
  // bone index: 1 float (skinned meshes, stride == 6)
  if stride == 6 then
    glVertexAttribPointer(3, 1, GL_FLOAT, false, strideBytes, 5 * 4)
    glEnableVertexAttribArray(3)
  // vertex color: 3 floats (optional, stride >= 8)
  if stride >= 8 then
    glVertexAttribPointer(2, 3, GL_FLOAT, false, strideBytes, 5 * 4)
    glEnableVertexAttribArray(2)

  // Normal vector: 3 floats at attrib location 4 (optional separate VBO).
  // Kept in a separate buffer so existing stride/layout code is untouched.
  private val normVbo: Int = normalsOpt match
    case Some(normals) =>
      val nid = glGenBuffers()
      val nbuf = BufferUtils.createFloatBuffer(normals.length)
      nbuf.put(normals).flip()
      glBindBuffer(GL_ARRAY_BUFFER, nid)
      glBufferData(GL_ARRAY_BUFFER, nbuf, GL_STATIC_DRAW)
      glVertexAttribPointer(4, 3, GL_FLOAT, false, 3 * 4, 0)
      glEnableVertexAttribArray(4)
      nid
    case None => 0

  glBindVertexArray(0)

  def draw(): Unit =
    glBindVertexArray(vao)
    glDrawElements(GL_TRIANGLES, indexCount, GL_UNSIGNED_INT, 0)
    glBindVertexArray(0)

  def drawRange(startIndex: Int, count: Int): Unit =
    glBindVertexArray(vao)
    glDrawElements(GL_TRIANGLES, count, GL_UNSIGNED_INT, startIndex.toLong * 4)
    glBindVertexArray(0)

  /** Bind this mesh's VAO. Pair with unbind() for batched draw calls. */
  def bind(): Unit = glBindVertexArray(vao)

  /** Unbind VAO after batched draw calls. */
  def unbind(): Unit = glBindVertexArray(0)

  /** Issue a draw call without binding/unbinding VAO — caller must call bind() first. */
  def drawRangeNoBind(startIndex: Int, count: Int): Unit =
    glDrawElements(GL_TRIANGLES, count, GL_UNSIGNED_INT, startIndex.toLong * 4)

  def updateVertices(newVertices: Array[Float]): Unit =
    val buf = BufferUtils.createFloatBuffer(newVertices.length)
    buf.put(newVertices).flip()
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferSubData(GL_ARRAY_BUFFER, 0, buf)

  def cleanup(): Unit =
    glDeleteBuffers(vbo)
    glDeleteBuffers(ebo)
    if normVbo != 0 then glDeleteBuffers(normVbo)
    glDeleteVertexArrays(vao)
