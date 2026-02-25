package opennorrath.archive

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}
import java.util.zip.Inflater

object PfsArchive:

  private val Magic = 0x20534650 // "PFS " little-endian
  private val VersionV1 = 0x10000
  private val VersionV2 = 0x20000
  private val DirectoryCrc = 0x61580AC9

  def load(path: Path): List[PfsEntry] =
    val channel = FileChannel.open(path, StandardOpenOption.READ)
    try
      val fileSize = channel.size()
      val buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, fileSize)
      buf.order(ByteOrder.LITTLE_ENDIAN)
      parse(buf)
    finally channel.close()

  private def parse(buf: ByteBuffer): List[PfsEntry] =
    // Header
    val indexOffset = buf.getInt()
    val magic = buf.getInt()
    if magic != Magic then
      throw RuntimeException(f"Invalid PFS magic: 0x$magic%08X (expected 0x$Magic%08X)")

    val version = buf.getInt()
    if version != VersionV1 && version != VersionV2 then
      throw RuntimeException(f"Unsupported PFS version: 0x$version%08X")

    // Read index
    buf.position(indexOffset)
    val entryCount = buf.getInt()

    case class DirEntry(crc: Int, offset: Int, uncompressedSize: Int)

    val entries = (0 until entryCount).map { _ =>
      DirEntry(buf.getInt(), buf.getInt(), buf.getInt())
    }.toList

    // Separate the filename directory entry from file entries
    val (dirEntries, fileEntries) = if version == VersionV2 then
      entries.partition(e => e.crc == DirectoryCrc || e.crc == 0xFFFFFFFF.toInt)
    else
      (Nil, entries)

    // Decompress each file entry
    val fileData = fileEntries.map { entry =>
      val data = decompressFile(buf, entry.offset, entry.uncompressedSize)
      (entry, data)
    }

    // Parse filenames from directory (v2)
    val filenames: Map[Int, String] = if dirEntries.nonEmpty then
      val dirEntry = dirEntries.head
      val dirData = decompressFile(buf, dirEntry.offset, dirEntry.uncompressedSize)
      parseFilenameDirectory(dirData)
    else
      Map.empty

    // Match filenames to entries
    fileData.map { (entry, data) =>
      val name = filenames.getOrElse(entry.crc, f"${entry.crc}%08X.bin")
      PfsEntry(name, data)
    }

  private def decompressFile(buf: ByteBuffer, offset: Int, uncompressedSize: Int): Array[Byte] =
    val result = new Array[Byte](uncompressedSize)
    var totalRead = 0
    buf.position(offset)

    while totalRead < uncompressedSize do
      val deflatedLen = buf.getInt()
      val inflatedLen = buf.getInt()

      val compressed = new Array[Byte](deflatedLen)
      buf.get(compressed)

      // Try zlib-wrapped first, fall back to raw deflate
      val bytesInflated = tryInflate(compressed, result, totalRead, inflatedLen, raw = false)
        .getOrElse(tryInflate(compressed, result, totalRead, inflatedLen, raw = true)
          .getOrElse(throw RuntimeException(
            s"Failed to decompress chunk at offset ${buf.position() - deflatedLen - 8}")))

      totalRead += bytesInflated

    result

  private def tryInflate(
      compressed: Array[Byte],
      output: Array[Byte],
      outputOffset: Int,
      expectedSize: Int,
      raw: Boolean,
  ): Option[Int] =
    val inflater = Inflater(raw)
    try
      inflater.setInput(compressed)
      val n = inflater.inflate(output, outputOffset, expectedSize)
      if n == expectedSize then Some(n) else None
    catch case _: Exception => None
    finally inflater.end()

  private def parseFilenameDirectory(data: Array[Byte]): Map[Int, String] =
    val buf = ByteBuffer.wrap(data).order(ByteOrder.LITTLE_ENDIAN)
    val count = buf.getInt()

    val names = (0 until count).map { _ =>
      val len = buf.getInt()
      val nameBytes = new Array[Byte](len)
      buf.get(nameBytes)
      // Strip null terminator
      val name = String(nameBytes, 0, len - 1, "US-ASCII")
      val crc = PfsCrc.compute(name)
      (crc, name)
    }.toMap

    names
