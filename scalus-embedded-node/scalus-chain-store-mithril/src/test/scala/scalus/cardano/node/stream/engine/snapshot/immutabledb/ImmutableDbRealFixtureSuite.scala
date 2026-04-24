package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.snapshot.TestUtils

import java.nio.file.{Files, Path, StandardCopyOption}

/** Runs the parser + verifier against three real (but tiny) immutable-chunk trios copied from
  * `testing-preview` — `22449`, `22477`, `22481`. Each trio is <24 KB uncompressed, so committing
  * them costs ~83 KB of test-resources weight in exchange for end-to-end coverage against
  * byte-for-byte data the Mithril aggregator actually signed.
  *
  * The companion `digests.partial.json` carries only the 9 entries that cover these files; the
  * verifier's `loadManifest` doesn't care that the manifest is partial.
  *
  * Keeping the fixture small on purpose: the parser is size-agnostic, so a bigger chunk wouldn't
  * exercise new code paths — it'd just slow checkouts.
  */
final class ImmutableDbRealFixtureSuite extends AnyFunSuite {

    import ImmutableDbRealFixtureSuite.*

    test("real fixture: parse 3 chunks, count blocks, check header hash length") {
        val (immutableDir, _) = stageFixture()
        try {
            val reader = new ImmutableDbReader(immutableDir)
            assert(reader.chunkNumbers == IndexedSeq(22449, 22477, 22481))

            val blocks = reader.blocks().toVector
            // Preview at this epoch was post-Conway; chunks 22477/22481 have 1 block each
            // (6048 B chunk = single block), 22449 has 1 block in 20337 B (one fat block).
            assert(blocks.nonEmpty, "expected at least one block")
            assert(blocks.forall(_.headerHash.length == ImmutableDb.ShelleyHashSize))

            // slot numbers must be strictly increasing within a chunk and across chunks (preview
            // slot leaders produce blocks monotonically; an OBO bug in the decoder would break
            // this invariant).
            val slots = blocks.map(_.slot)
            assert(slots.sliding(2).forall { case Seq(a, b) => a < b; case _ => true })

            // Block bytes must be non-empty and sum to exactly the chunk-file total (three
            // .chunk files), i.e. nothing was missed by offset arithmetic.
            val perChunkExpected = reader.chunkNumbers.map { n =>
                Files.size(immutableDir.resolve(f"$n%05d.chunk"))
            }.sum
            val perChunkActual = blocks.map(_.blockBytes.length.toLong).sum
            assert(
              perChunkActual == perChunkExpected,
              s"byte accounting: $perChunkActual vs $perChunkExpected"
            )
        } finally cleanup(immutableDir.getParent)
    }

    test("real fixture: digest verification against sliced manifest") {
        val (immutableDir, manifestPath) = stageFixture()
        try {
            val manifest = DigestsVerifier.loadManifest(manifestPath)
            assert(
              manifest.size == 9,
              s"sliced manifest should have 9 entries; got ${manifest.size}"
            )

            val res = DigestsVerifier.verify(immutableDir, manifest)
            assert(res.presentMatchesManifest, s"integrity check failed: $res")
            assert(
              res.isComplete,
              s"manifest should be fully covered (sliced to fixture); got $res"
            )
            assert(res.verified == 9)
            assert(res.mismatches.isEmpty)
            assert(res.missingOnDisk.isEmpty)
            assert(res.unexpectedOnDisk.isEmpty)
        } finally cleanup(immutableDir.getParent)
    }

    test("real fixture: corrupting a byte trips the verifier") {
        val (immutableDir, manifestPath) = stageFixture()
        try {
            val chunk = immutableDir.resolve("22477.chunk")
            val data = Files.readAllBytes(chunk)
            data(0) = (data(0) ^ 0x01).toByte
            Files.write(chunk, data)

            val res =
                DigestsVerifier.verify(immutableDir, DigestsVerifier.loadManifest(manifestPath))
            assert(!res.presentMatchesManifest)
            assert(res.mismatches.exists(_.fileName == "22477.chunk"))
        } finally cleanup(immutableDir.getParent)
    }
}

object ImmutableDbRealFixtureSuite {

    /** Copy the classpath fixture into a temp dir. We can't read from `src/test/resources` in place
      * because `ImmutableDbReader` needs an `immutable/` directory on disk, and classpath URLs can
      * be inside a JAR when the tests run against a packaged artifact.
      */
    def stageFixture(): (Path, Path) = {
        val tmp = Files.createTempDirectory("scalus-immdb-real-")
        val immutable = tmp.resolve("immutable")
        Files.createDirectories(immutable)
        Seq(
          "22449.chunk",
          "22449.primary",
          "22449.secondary",
          "22477.chunk",
          "22477.primary",
          "22477.secondary",
          "22481.chunk",
          "22481.primary",
          "22481.secondary"
        ).foreach { name =>
            val url = Option(getClass.getResource(s"/immutabledb/fixture/immutable/$name"))
                .getOrElse(fail(s"missing fixture resource: immutable/$name"))
            scala.util.Using.resource(url.openStream()) { in =>
                Files.copy(in, immutable.resolve(name), StandardCopyOption.REPLACE_EXISTING)
            }
        }
        val manifestOut = tmp.resolve("digests.partial.json")
        scala.util.Using.resource(
          Option(getClass.getResource("/immutabledb/fixture/digests.partial.json"))
              .getOrElse(fail("missing fixture resource: digests.partial.json"))
              .openStream()
        )(in => Files.copy(in, manifestOut, StandardCopyOption.REPLACE_EXISTING))
        (immutable, manifestOut)
    }

    private def fail(msg: String): Nothing = throw new AssertionError(msg)

    def cleanup(dir: Path): Unit = TestUtils.deleteRecursively(dir)
}
