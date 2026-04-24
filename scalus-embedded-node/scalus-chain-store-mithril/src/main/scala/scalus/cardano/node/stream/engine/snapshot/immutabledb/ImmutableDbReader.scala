package scalus.cardano.node.stream.engine.snapshot.immutabledb

import java.io.RandomAccessFile
import java.nio.file.{Files, Path}
import scala.util.Using

/** Streaming reader over an entire `immutable/` directory extracted from a Mithril Cardano Database
  * V2 snapshot (or a local cardano-node `db/immutable/`).
  *
  * Chunks are visited in ascending numeric order. Block bytes are read on demand via a
  * `RandomAccessFile` positioned at the secondary-derived offset, so the caller can restore blocks
  * into a `ChainStore` without ever holding a whole chunk in memory.
  *
  * Byron/EBB chunks are not supported — will throw from [[ImmutableDb.parseSecondary]] if it hits a
  * 52-byte-entry secondary file (28-byte hash).
  */
final class ImmutableDbReader(immutableDir: Path) {

    /** Numerically-sorted chunk numbers present in `immutableDir`. A chunk is only included when
      * the full `.chunk`/`.primary`/`.secondary` trio is present; lone files are ignored.
      */
    lazy val chunkNumbers: IndexedSeq[Int] = {
        require(Files.isDirectory(immutableDir), s"not a directory: $immutableDir")
        import scala.jdk.CollectionConverters.*
        val byNumber = collection.mutable.Map.empty[Int, Int]
        Using.resource(Files.list(immutableDir)) { stream =>
            stream.iterator.asScala.foreach { p =>
                val name = p.getFileName.toString
                val dot = name.lastIndexOf('.')
                if dot > 0 then {
                    val base = name.substring(0, dot)
                    val ext = name.substring(dot + 1)
                    if ext == "chunk" || ext == "primary" || ext == "secondary" then {
                        base.toIntOption.foreach(n => byNumber(n) = byNumber.getOrElse(n, 0) + 1)
                    }
                }
            }
        }
        byNumber.iterator.collect { case (n, 3) => n }.toArray.sorted.toIndexedSeq
    }

    /** Lazy iterator over every block in the directory, chunk-ordered, slot-ordered within a chunk.
      * Each yielded block's `blockBytes` is a freshly-allocated array that the caller owns.
      */
    def blocks(): Iterator[ImmutableDb.ImmutableBlock] =
        chunkNumbers.iterator.flatMap(readChunk)

    /** Parse the index pair, then iterate blocks of one chunk. Keeps the `.chunk` handle open for
      * the duration of this iterator — callers that only scan headers can stop early and the handle
      * is closed by the iterator's `onClose`-analog (we drain eagerly to avoid that, see note
      * below).
      *
      * Note: we currently eagerly materialise block bytes per chunk before yielding, which trades
      * ~chunk-size of RAM (~2 MB uncompressed) for simpler resource cleanup. Streaming-with-defer
      * is a straightforward follow-up if profiling shows it matters.
      */
    def readChunk(n: Int): Iterator[ImmutableDb.ImmutableBlock] = {
        // `chunkNumbers` already filtered for trios where all three files exist; `Files.readAllBytes`
        // / `RandomAccessFile` here throw `NoSuchFileException` with a full path if a file vanishes
        // concurrently — same information we'd get from an `exists` pre-check, without the extra
        // `stat` calls × thousands of chunks.
        val primaryPath = immutableDir.resolve(f"$n%05d.primary")
        val secondaryPath = immutableDir.resolve(f"$n%05d.secondary")
        val chunkPath = immutableDir.resolve(f"$n%05d.chunk")

        val primary = ImmutableDb.parsePrimary(Files.readAllBytes(primaryPath))
        val secondary = ImmutableDb.parseSecondary(Files.readAllBytes(secondaryPath))
        // Cross-check: the last primary offset (past-the-end in secondary) must equal the
        // secondary's total byte length. Catches truncated indexes early.
        val expectedLast = secondary.size * ImmutableDb.ShelleySecondaryEntrySize
        require(
          primary.last == expectedLast,
          s"chunk $n: primary last offset ${primary.last} != $expectedLast " +
              s"(secondary has ${secondary.size} entries)"
        )

        val chunkSize = Files.size(chunkPath)
        val ranges = ImmutableDb.blockRanges(secondary, chunkSize)

        val blocks = Array.ofDim[ImmutableDb.ImmutableBlock](secondary.size)
        Using.resource(new RandomAccessFile(chunkPath.toFile, "r")) { raf =>
            var i = 0
            while i < secondary.size do {
                val (off, size) = ranges(i)
                raf.seek(off)
                val buf = Array.ofDim[Byte](size)
                raf.readFully(buf)
                blocks(i) =
                    ImmutableDb.ImmutableBlock(n, secondary(i).slot, secondary(i).headerHash, buf)
                i += 1
            }
        }
        blocks.iterator
    }
}
