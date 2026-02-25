package opennorrath

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.BufferUtils

class Mesh(vertices: Array[Float], indices: Array[Int]):

  private val vao = glGenVertexArrays()
  private val vbo = glGenBuffers()
  private val ebo = glGenBuffers()
  private val indexCount = indices.length

  glBindVertexArray(vao)

  val vbuf = BufferUtils.createFloatBuffer(vertices.length)
  vbuf.put(vertices).flip()
  glBindBuffer(GL_ARRAY_BUFFER, vbo)
  glBufferData(GL_ARRAY_BUFFER, vbuf, GL_STATIC_DRAW)

  val ibuf = BufferUtils.createIntBuffer(indices.length)
  ibuf.put(indices).flip()
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo)
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, ibuf, GL_STATIC_DRAW)

  // position: 3 floats
  glVertexAttribPointer(0, 3, GL_FLOAT, false, 5 * 4, 0)
  glEnableVertexAttribArray(0)
  // uv: 2 floats
  glVertexAttribPointer(1, 2, GL_FLOAT, false, 5 * 4, 3 * 4)
  glEnableVertexAttribArray(1)

  glBindVertexArray(0)

  def draw(): Unit =
    glBindVertexArray(vao)
    glDrawElements(GL_TRIANGLES, indexCount, GL_UNSIGNED_INT, 0)
    glBindVertexArray(0)

  def cleanup(): Unit =
    glDeleteBuffers(vbo)
    glDeleteBuffers(ebo)
    glDeleteVertexArrays(vao)
