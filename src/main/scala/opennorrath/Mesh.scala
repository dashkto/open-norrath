package opennorrath

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.BufferUtils

class Mesh(vertices: Array[Float], indices: Array[Int], dynamic: Boolean = false, stride: Int = 5):

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
  // vertex color: 3 floats (optional, stride >= 8)
  if stride >= 8 then
    glVertexAttribPointer(2, 3, GL_FLOAT, false, strideBytes, 5 * 4)
    glEnableVertexAttribArray(2)

  glBindVertexArray(0)

  def draw(): Unit =
    glBindVertexArray(vao)
    glDrawElements(GL_TRIANGLES, indexCount, GL_UNSIGNED_INT, 0)
    glBindVertexArray(0)

  def drawRange(startIndex: Int, count: Int): Unit =
    glBindVertexArray(vao)
    glDrawElements(GL_TRIANGLES, count, GL_UNSIGNED_INT, startIndex.toLong * 4)
    glBindVertexArray(0)

  def updateVertices(newVertices: Array[Float]): Unit =
    val buf = BufferUtils.createFloatBuffer(newVertices.length)
    buf.put(newVertices).flip()
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferSubData(GL_ARRAY_BUFFER, 0, buf)

  def cleanup(): Unit =
    glDeleteBuffers(vbo)
    glDeleteBuffers(ebo)
    glDeleteVertexArrays(vao)
