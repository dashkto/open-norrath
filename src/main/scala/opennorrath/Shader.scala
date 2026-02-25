package opennorrath

import org.joml.Matrix4f
import org.lwjgl.opengl.GL20.*
import org.lwjgl.system.MemoryStack

class Shader(vertexSource: String, fragmentSource: String):

  val program: Int = {
    val vertexId = compileShader(vertexSource, GL_VERTEX_SHADER)
    val fragmentId = compileShader(fragmentSource, GL_FRAGMENT_SHADER)

    val programId = glCreateProgram()
    glAttachShader(programId, vertexId)
    glAttachShader(programId, fragmentId)
    glLinkProgram(programId)

    if glGetProgrami(programId, GL_LINK_STATUS) == 0 then
      val log = glGetProgramInfoLog(programId)
      throw RuntimeException(s"Shader link error:\n$log")

    glDeleteShader(vertexId)
    glDeleteShader(fragmentId)
    programId
  }

  def use(): Unit = glUseProgram(program)

  def setInt(name: String, value: Int): Unit =
    glUniform1i(glGetUniformLocation(program, name), value)

  def setVec4(name: String, x: Float, y: Float, z: Float, w: Float): Unit =
    glUniform4f(glGetUniformLocation(program, name), x, y, z, w)

  def setMatrix4f(name: String, matrix: Matrix4f): Unit =
    val stack = MemoryStack.stackPush()
    try
      val buf = stack.mallocFloat(16)
      matrix.get(buf)
      glUniformMatrix4fv(glGetUniformLocation(program, name), false, buf)
    finally stack.pop()

  def cleanup(): Unit = glDeleteProgram(program)

  private def compileShader(source: String, shaderType: Int): Int =
    val id = glCreateShader(shaderType)
    glShaderSource(id, source)
    glCompileShader(id)
    if glGetShaderi(id, GL_COMPILE_STATUS) == 0 then
      val log = glGetShaderInfoLog(id)
      val typeName = if shaderType == GL_VERTEX_SHADER then "vertex" else "fragment"
      throw RuntimeException(s"$typeName shader compile error:\n$log")
    id

object Shader:
  def loadResource(path: String): String =
    val stream = getClass.getResourceAsStream(path)
    if stream == null then throw RuntimeException(s"Shader resource not found: $path")
    try String(stream.readAllBytes()) finally stream.close()

  def fromResources(vertPath: String, fragPath: String): Shader =
    Shader(loadResource(vertPath), loadResource(fragPath))
