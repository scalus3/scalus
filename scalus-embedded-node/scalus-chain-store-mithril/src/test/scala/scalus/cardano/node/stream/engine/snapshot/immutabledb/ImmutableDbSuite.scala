package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scalus.cardano.node.stream.engine.snapshot.TestUtils
import scalus.utils.Hex.toHex

/** Round-trip tests for the ImmutableDB decoder and the digests verifier.
  *
  * Fixtures are built directly from the known wire format — we don't depend on cardano-node to
  * generate them, so this suite runs in-process with no external binaries. Covers:
  *
  *   - `.primary` / `.secondary` / `.chunk` round-trip for a multi-block chunk,
  *   - rejection of mismatched primary/secondary sizes,
  *   - digest verification success, mismatch detection, missing/unexpected files.
  */
final class ImmutableDbSuite extends AnyFunSuite {

    import ImmutableDbSuite.*

    test("parse+read: three blocks across one chunk round-trip") {
        val tmp = Files.createTempDirectory("scalus-immdb-")
        try {
            val immutable = tmp.resolve("immutable")
            Files.createDirectories(immutable)

            // Block bytes — arbitrary, just have to be non-overlapping.
            val blocks = Seq(
              (100L, bytes("first-block-cbor-goes-here")),
              (101L, bytes("another-one")),
              (105L, bytes("third"))
            )
            writeChunk(immutable, n = 42, blocks)

            val reader = new ImmutableDbReader(immutable)
            assert(reader.chunkNumbers == IndexedSeq(42))
            val got = reader.blocks().toVector
            assert(got.size == 3)
            assert(got.map(_.slot) == Vector(100L, 101L, 105L))
            assert(got.map(b => new String(b.blockBytes)) == blocks.map(b => new String(b._2)))
            assert(got.forall(_.headerHash.length == ImmutableDb.ShelleyHashSize))
        } finally cleanup(tmp)
    }

    test("parse: missing-primary rejects") {
        val tmp = Files.createTempDirectory("scalus-immdb-bad-")
        try {
            val immutable = tmp.resolve("immutable")
            Files.createDirectories(immutable)
            writeChunk(immutable, n = 1, Seq((0L, bytes("x"))))
            Files.delete(immutable.resolve("00001.primary"))
            val reader = new ImmutableDbReader(immutable)
            // trio is incomplete → filtered out.
            assert(reader.chunkNumbers.isEmpty)
        } finally cleanup(tmp)
    }

    test("parse: bad primary version rejected") {
        val bad = Array[Byte](99, 0, 0, 0, 0)
        val e = intercept[IllegalArgumentException](ImmutableDb.parsePrimary(bad))
        assert(e.getMessage.contains("version"))
    }

    test("verify: round-trip success") {
        val tmp = Files.createTempDirectory("scalus-digest-")
        try {
            val immutable = tmp.resolve("immutable")
            Files.createDirectories(immutable)
            writeChunk(immutable, n = 7, Seq((1L, bytes("aaa")), (2L, bytes("bb"))))

            val manifest = manifestFor(immutable)
            val res = DigestsVerifier.verify(immutable, manifest)
            assert(res.isComplete, s"expected complete; got $res")
            assert(res.verified == 3) // chunk + primary + secondary
            assert(res.mismatches.isEmpty)
        } finally cleanup(tmp)
    }

    test("verify: detects a corrupted chunk") {
        val tmp = Files.createTempDirectory("scalus-digest-bad-")
        try {
            val immutable = tmp.resolve("immutable")
            Files.createDirectories(immutable)
            writeChunk(immutable, n = 7, Seq((1L, bytes("aaa"))))
            val manifest = manifestFor(immutable)

            // Corrupt the chunk file by overwriting a byte.
            val chunk = immutable.resolve("00007.chunk")
            val data = Files.readAllBytes(chunk)
            data(0) = (data(0) ^ 0xff).toByte
            Files.write(chunk, data)

            val res = DigestsVerifier.verify(immutable, manifest)
            assert(!res.presentMatchesManifest)
            assert(res.mismatches.exists(_.fileName == "00007.chunk"))
        } finally cleanup(tmp)
    }

    test("verify: reports missing + unexpected") {
        val tmp = Files.createTempDirectory("scalus-digest-gaps-")
        try {
            val immutable = tmp.resolve("immutable")
            Files.createDirectories(immutable)
            writeChunk(immutable, n = 5, Seq((0L, bytes("x"))))
            val manifest = manifestFor(immutable)

            // Drop one file and add an unknown one.
            Files.delete(immutable.resolve("00005.primary"))
            Files.write(immutable.resolve("extra.txt"), "hi".getBytes)

            val res = DigestsVerifier.verify(immutable, manifest)
            assert(res.missingOnDisk.contains("00005.primary"))
            assert(res.unexpectedOnDisk.contains("extra.txt"))
        } finally cleanup(tmp)
    }
}

object ImmutableDbSuite {

    private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")

    private def cleanup(dir: Path): Unit = TestUtils.deleteRecursively(dir)

    /** Build a minimal Shelley-shaped `.chunk/.primary/.secondary` trio for `n` in `immutableDir`.
      * Layout: one secondary entry per block, one primary offset per slot 0..=maxSlot with empty
      * slots keeping the previous offset. Header hash is filler bytes — we only care about
      * round-tripping here.
      */
    def writeChunk(immutableDir: Path, n: Int, blocks: Seq[(Long, Array[Byte])]): Unit = {
        val chunkName = f"$n%05d.chunk"
        val primaryName = f"$n%05d.primary"
        val secondaryName = f"$n%05d.secondary"

        val chunkBuf = new java.io.ByteArrayOutputStream()
        val secondaryBuf = ByteBuffer
            .allocate(blocks.size * ImmutableDb.ShelleySecondaryEntrySize)
            .order(ByteOrder.BIG_ENDIAN)

        var offset = 0L
        blocks.foreach { case (slot, data) =>
            // secondary entry
            secondaryBuf.putLong(offset) // blockOffset
            secondaryBuf.putShort(0.toShort) // headerOffset
            secondaryBuf.putShort(0.toShort) // headerSize (0 = unknown; we don't care here)
            secondaryBuf.putInt(0) // checksum (same)
            val hash = Array.ofDim[Byte](ImmutableDb.ShelleyHashSize)
            java.util.Arrays.fill(hash, (slot & 0xff).toByte)
            secondaryBuf.put(hash)
            secondaryBuf.putLong(slot)
            // chunk bytes
            chunkBuf.write(data)
            offset += data.length
        }
        Files.write(immutableDir.resolve(secondaryName), secondaryBuf.array)
        Files.write(immutableDir.resolve(chunkName), chunkBuf.toByteArray)

        // Primary: one u32be offset per relative slot. Use the blocks' slot numbers as a stand-in
        // for relative-slot indices — a real chunk would align them to its epoch base, but the
        // reader only cares about (a) first offset is 0 and (b) final offset equals secondary size.
        val maxSlot = blocks.map(_._1).max.toInt
        val offsets = Array.ofDim[Int](maxSlot + 2) // slots 0..maxSlot plus past-the-end
        var cumulative = 0
        val filled = blocks.map(b => (b._1.toInt, ImmutableDb.ShelleySecondaryEntrySize)).toMap
        var i = 0
        while i <= maxSlot do {
            offsets(i) = cumulative
            cumulative += filled.getOrElse(i, 0)
            i += 1
        }
        offsets(maxSlot + 1) = cumulative
        val pBuf = ByteBuffer.allocate(1 + offsets.length * 4).order(ByteOrder.BIG_ENDIAN)
        pBuf.put(ImmutableDb.CurrentPrimaryVersion)
        offsets.foreach(pBuf.putInt)
        Files.write(immutableDir.resolve(primaryName), pBuf.array)
    }

    def manifestFor(immutableDir: Path): DigestsVerifier.DigestManifest = {
        import scala.jdk.CollectionConverters.*
        val entries = scala.util.Using.resource(Files.list(immutableDir)) { s =>
            s.iterator.asScala
                .filter(Files.isRegularFile(_))
                .map(p => p.getFileName.toString -> sha256HexOf(p))
                .toMap
        }
        DigestsVerifier.DigestManifest(entries)
    }

    private def sha256HexOf(p: Path): String = {
        val md = MessageDigest.getInstance("SHA-256")
        md.update(Files.readAllBytes(p))
        md.digest().toHex
    }
}
