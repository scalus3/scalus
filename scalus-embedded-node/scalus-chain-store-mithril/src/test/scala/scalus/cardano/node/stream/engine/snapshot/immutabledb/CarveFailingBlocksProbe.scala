package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.toHex

import java.nio.file.{Files, Path}

/** Manual probe that walks the full extracted preview snapshot, runs every block through
  * `HfcDiskBlockDecoder`, collects the blocks that fail, and dumps a hex preview + header metadata
  * for each. Gated on `SCALUS_IMMUTABLEDB_SRC` plus `SCALUS_CARVE_FAILURES=1`.
  *
  * Useful when narrowing down schema edge cases like "key 6 in TransactionBody" on a recent preview
  * snapshot that shouldn't have pre-Conway fields.
  */
final class CarveFailingBlocksProbe extends AnyFunSuite {

    test("[manual] carve failing blocks, dump hex") {
        val src = sys.env.get("SCALUS_IMMUTABLEDB_SRC").map(Path.of(_))
        val enabled = sys.env.get("SCALUS_CARVE_FAILURES").contains("1")
        assume(src.isDefined && enabled, "set SCALUS_IMMUTABLEDB_SRC and SCALUS_CARVE_FAILURES=1")

        val immutableDir = src.get.resolve("immutable")
        assume(Files.isDirectory(immutableDir), s"need $immutableDir")

        val reader = new ImmutableDbReader(immutableDir)
        val errorPattern = sys.env.getOrElse("SCALUS_CARVE_PATTERN", "Unknown key 6")
        val maxDump = sys.env.get("SCALUS_CARVE_MAX").map(_.toInt).getOrElse(3)
        val dumpBytes = sys.env.get("SCALUS_CARVE_BYTES").map(_.toInt).getOrElse(80)

        var examined = 0L
        var found = 0
        reader.blocks().foreach { b =>
            examined += 1
            HfcDiskBlockDecoder.decode(b) match {
                case Left(err: HfcDiskBlockDecoder.Error.LedgerDecode)
                    if messageContains(err, errorPattern) =>
                    if found < maxDump then {
                        info(
                          s"[${found + 1}/$maxDump] chunk=${b.chunkNo} slot=${b.slot} " +
                              s"blockSize=${b.blockBytes.length} headerHash=${b.headerHash.toHex}"
                        )
                        info(s"  cause: ${err.cause.getMessage}")
                        info(
                          s"  first $dumpBytes bytes:\n    ${b.blockBytes.take(dumpBytes).toHex}"
                        )
                        val dest =
                            Files.createTempFile(s"scalus-carve-${b.chunkNo}-${b.slot}-", ".bin")
                        Files.write(dest, b.blockBytes)
                        info(s"  wrote full block to $dest")
                    }
                    found += 1
                case _ => // skip
            }
            if examined % 500_000L == 0L then info(s"  scanned $examined blocks so far")
        }
        info(s"=== matched $found / $examined blocks for pattern '$errorPattern' ===")
    }

    private def messageContains(
        err: HfcDiskBlockDecoder.Error.LedgerDecode,
        pattern: String
    ): Boolean =
        Option(err.cause.getMessage).exists(_.contains(pattern))
}
