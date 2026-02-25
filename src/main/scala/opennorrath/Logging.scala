package opennorrath

import java.io.{FileOutputStream, OutputStream, PrintStream}
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Logging:

  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

  def init(dir: String = "logs"): Unit =
    Files.createDirectories(Path.of(dir))
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"))
    val logFile = Path.of(dir).resolve(s"session_$timestamp.log")
    val fos = FileOutputStream(logFile.toFile)
    val ps = TimestampPrintStream(TeeOutputStream(fos))
    System.setOut(ps)
    System.setErr(ps)
    println(s"Logging to $logFile")

  /** PrintStream that prepends a timestamp to each println call. */
  private class TimestampPrintStream(out: OutputStream) extends PrintStream(out, true):
    override def println(x: String): Unit =
      val ts = LocalDateTime.now().format(timeFmt)
      super.println(s"[$ts] $x")

    override def println(x: Any): Unit =
      println(String.valueOf(x))

  private class TeeOutputStream(file: FileOutputStream) extends OutputStream:
    private val original = System.out

    override def write(b: Int): Unit =
      original.write(b)
      file.write(b)

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      original.write(b, off, len)
      file.write(b, off, len)

    override def flush(): Unit =
      original.flush()
      file.flush()

    override def close(): Unit =
      flush()
      file.close()
