package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.snapshot.TestUtils.humanBytes

import java.nio.file.{Files, Path}

/** Manual probe: points at an already-extracted `immutable/` directory on disk (e.g. the one left
  * behind by `scripts/download_preview.sh`) and:
  *
  *   1. Verifies each chunk/primary/secondary file's SHA-256 against `digests.json`,
  *   2. Walks every block in every chunk, tallies counts + bytes + slot range.
  *
  * Gated on `SCALUS_IMMUTABLEDB_SRC=<path>` so `sbt test` doesn't invoke it. The given path should
  * be the root of the extracted V2 layout (contains `immutable/`, `digests/`, `ledger/`).
  *
  * Invoke:
  * {{{
  *   SCALUS_IMMUTABLEDB_SRC=/tmp/mithrill-preview \
  *     sbt 'scalusChainStoreMithril/testOnly *ImmutableDbReadProbe'
  * }}}
  */
final class ImmutableDbReadProbe extends AnyFunSuite {

    test("[manual] verify + enumerate every block in an on-disk Cardano DB V2 snapshot") {
        val src = sys.env.get("SCALUS_IMMUTABLEDB_SRC").map(Path.of(_))
        assume(src.isDefined, "set SCALUS_IMMUTABLEDB_SRC=<extracted-snapshot-dir>")
        val root = src.get
        val immutable = root.resolve("immutable")
        assume(Files.isDirectory(immutable), s"need $immutable")

        val t0 = System.nanoTime()
        // Mithril's digests archive extracts a single `*digests*.json` at the snapshot root (not
        // under `digests/`). Allow the caller to override via SCALUS_IMMUTABLEDB_DIGESTS.
        val manifestPath = sys.env
            .get("SCALUS_IMMUTABLEDB_DIGESTS")
            .map(Path.of(_))
            .getOrElse(DigestsVerifier.loadManifestFromDir(root))
        info(s"using manifest: $manifestPath")
        val manifest = DigestsVerifier.loadManifest(manifestPath)
        info(s"loaded manifest: ${manifest.size} entries")

        val vr = DigestsVerifier.verify(immutable, manifest)
        val vElapsed = (System.nanoTime() - t0) / 1_000_000L
        info(
          s"verification: verified=${vr.verified} mismatches=${vr.mismatches.size} " +
              s"missing=${vr.missingOnDisk.size} unexpected=${vr.unexpectedOnDisk.size} " +
              s"(${vElapsed}ms)"
        )
        if vr.mismatches.nonEmpty then
            vr.mismatches.take(5).foreach(m => info(s"  MISMATCH ${m.fileName}"))
        if vr.missingOnDisk.nonEmpty then
            info(s"  missing: ${vr.missingOnDisk.take(10).mkString(", ")}")

        val reader = new ImmutableDbReader(immutable)
        info(
          s"chunks present: ${reader.chunkNumbers.size} " +
              s"(first=${reader.chunkNumbers.head}, last=${reader.chunkNumbers.last})"
        )

        val t1 = System.nanoTime()
        var blockCount = 0L
        var byteCount = 0L
        var minSlot = Long.MaxValue
        var maxSlot = Long.MinValue
        val erasHisto = scala.collection.mutable.Map.empty[Int, Long]
        var decodeOk = 0L
        var decodeErr = 0L
        val sampleErrors = scala.collection.mutable.ArrayBuffer.empty[String]
        val decodeBlocks = sys.env.get("SCALUS_IMMUTABLEDB_DECODE").forall(_ != "0")

        reader.blocks().foreach { b =>
            blockCount += 1
            byteCount += b.blockBytes.length
            if b.slot < minSlot then minSlot = b.slot
            if b.slot > maxSlot then maxSlot = b.slot

            if decodeBlocks then {
                HfcDiskBlockDecoder.decode(b) match {
                    case Right(d) =>
                        decodeOk += 1
                        erasHisto(d.era) = erasHisto.getOrElse(d.era, 0L) + 1
                    case Left(err) =>
                        decodeErr += 1
                        if sampleErrors.size < 5 then
                            sampleErrors += s"chunk=${b.chunkNo} slot=${b.slot}: $err"
                }
            }

            if blockCount % 100_000L == 0L then
                info(s"  scanned $blockCount blocks so far (last slot=${b.slot})")
        }
        val pElapsed = (System.nanoTime() - t1) / 1_000_000L

        info(
          s"=== SUMMARY ===\n" +
              s"  present-matches-manifest = ${vr.presentMatchesManifest}\n" +
              s"  complete (full manifest covered) = ${vr.isComplete}\n" +
              s"  blocks = $blockCount\n" +
              s"  total block bytes = ${humanBytes(byteCount)} ($byteCount)\n" +
              s"  slot range = [$minSlot..$maxSlot]\n" +
              s"  parse elapsed = ${pElapsed}ms\n" +
              s"  parse throughput = ${humanBytes((byteCount * 1000L) / math.max(pElapsed, 1L))}/s\n" +
              (if decodeBlocks then
                   s"  decode ok = $decodeOk, errors = $decodeErr\n" +
                       s"  era histogram = ${erasHisto.toSeq.sortBy(_._1).map((e, c) => s"era$e=$c").mkString(", ")}\n" +
                       (if sampleErrors.nonEmpty then
                            s"  sample errors:\n    " + sampleErrors.mkString("\n    ") + "\n"
                        else "")
               else "  decode skipped (set SCALUS_IMMUTABLEDB_DECODE=1 to enable)\n")
        )
    }

}
