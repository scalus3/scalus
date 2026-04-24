package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.snapshot.TestUtils

import java.nio.file.{Files, Path}

/** Manual probe against a real mithril-downloaded preview (or mainnet) snapshot.
  *
  * '''Not a real test.''' Gated on `SCALUS_LEDGER_STATE_PROBE=1` and `SCALUS_MITHRIL_DEST=<dir>`
  * (or default fallback) pointing at an already-extracted snapshot parent. Skips otherwise.
  *
  * Reports tag distribution and aborts on the first MemPack decode failure, emitting a hex
  * dump of the offending entry so we can eyeball the mis-aligned decoder.
  *
  * Invoke:
  * {{{
  *   SCALUS_LEDGER_STATE_PROBE=1 \
  *   SCALUS_MITHRIL_DEST=/tmp/mithrill-preview \
  *     sbt 'scalusChainStoreMithril/testOnly *LedgerStateRealFixtureProbe'
  * }}}
  */
final class LedgerStateRealFixtureProbe extends AnyFunSuite {

    private val DefaultDest = "/tmp/mithrill-preview"

    test("[manual] stream-decode real preview tables file, report tag distribution") {
        val enabled = sys.env.get("SCALUS_LEDGER_STATE_PROBE").contains("1")
        assume(enabled, "set SCALUS_LEDGER_STATE_PROBE=1 to run")

        val root = Path.of(sys.env.getOrElse("SCALUS_MITHRIL_DEST", DefaultDest))
        val ledgerDir = root.resolve("ledger")
        assume(Files.isDirectory(ledgerDir), s"ledger dir not found at $ledgerDir")

        val layout = LedgerStateLayout.highestSlotIn(ledgerDir).getOrElse(fail("no ledger entries"))
        info(s"layout = $layout")
        layout match {
            case LedgerStateLayout.InMemoryV2(dir, slot) =>
                info(s"slot=$slot dir=$dir")
                probeTables(dir.resolve("tables"))
            case other => fail(s"unsupported layout on real fixture: $other")
        }
    }

    private def probeTables(tablesFile: Path): Unit = {
        val size = Files.size(tablesFile)
        info(f"tables file size: $size%,d bytes (${TestUtils.humanBytes(size)})")

        import io.bullet.borer.Cbor
        val started = System.nanoTime()
        val input = new java.io.BufferedInputStream(
          new java.io.FileInputStream(tablesFile.toFile),
          64 * 1024
        )
        try {
            val reader = Cbor.reader(input)
            reader.readArrayHeader(1L)
            val finiteLen =
                if reader.tryReadMapStart() then -1L else reader.readMapHeader()
            info(
              s"map header: ${if finiteLen < 0 then "indefinite" else s"finite($finiteLen)"}"
            )

            val byTxOutTag = scala.collection.mutable.Map.empty[Int, Long].withDefaultValue(0L)
            val byShape = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
            var total = 0L
            var done = false
            var dumpRemaining = sys.env.get("SCALUS_LEDGER_STATE_DUMP").map(_.toInt).getOrElse(0)
            while !done do {
                val atBreak =
                    if finiteLen >= 0 then total >= finiteLen
                    else reader.tryReadBreak()
                if atBreak then done = true
                else {
                    val keyBytes = reader.readByteArray()
                    val valueBytes = reader.readByteArray()
                    val txOutTag =
                        if valueBytes.isEmpty then -1 else valueBytes(0) & 0xff
                    byTxOutTag(txOutTag) += 1L
                    // For tags that carry a CompactValue (0, 1, 4, 5), the CV is right
                    // after the CompactAddr. Peek the CV tag (ada-only=0 vs multi-asset=1)
                    // so we can see the (TxOut tag, CV tag) cross-distribution.
                    val cvTag = if (txOutTag == 0 || txOutTag == 1 || txOutTag == 4 || txOutTag == 5) then {
                        // CompactAddr = Length (1-byte varlen for small) + N bytes.
                        // Assume small address (<128 bytes; always true in practice).
                        val addrLen = valueBytes(1) & 0xff
                        val cvTagPos = 2 + addrLen
                        if cvTagPos < valueBytes.length then valueBytes(cvTagPos) & 0xff else -2
                    } else -1
                    val shapeKey = s"tag$txOutTag,cv$cvTag"
                    byShape(shapeKey) += 1L

                    // One-off dump of the FIRST tag-4 ada-only entry so we can hand-decode it
                    // without the multi-asset rep distraction.
                    if txOutTag == 4 && cvTag == 0 && byShape("dumped_tag4_cv0") == 0 then {
                        info(
                          f"[DUMP] first tag-4 cv-0 at entry #$total%,d  valueLen=${valueBytes.length}"
                        )
                        info(
                          s"  value: " + valueBytes.map(b => f"${b & 0xff}%02x").mkString
                        )
                        byShape("dumped_tag4_cv0") += 1L
                    }
                    if txOutTag == 5 && cvTag == 0 && byShape("dumped_tag5_cv0") == 0 then {
                        info(
                          f"[DUMP] first tag-5 cv-0 at entry #$total%,d  valueLen=${valueBytes.length}"
                        )
                        info(
                          s"  value: " + valueBytes.map(b => f"${b & 0xff}%02x").mkString
                        )
                        byShape("dumped_tag5_cv0") += 1L
                    }
                    if dumpRemaining > 0 then {
                        dumpRemaining -= 1
                        info(
                          f"entry $total%,d  txOutTag=$txOutTag  keyLen=${keyBytes.length}  valueLen=${valueBytes.length}"
                        )
                        info(
                          s"  key:   " + keyBytes.map(b => f"${b & 0xff}%02x").mkString
                        )
                        info(
                          s"  value: " + valueBytes.map(b => f"${b & 0xff}%02x").mkString
                        )
                    }
                    // Set SCALUS_LEDGER_STATE_KEEP_GOING=1 to bucket errors and keep
                    // processing (useful while bringing up a new tag); otherwise fail fast on
                    // the first decode error.
                    val continueOnError = sys.env.contains("SCALUS_LEDGER_STATE_KEEP_GOING")
                    try MemPackReaders.readTxOut(MemPack.Reader(valueBytes))
                    catch {
                        case ex: MemPack.DecodeError =>
                            byShape(s"ERROR-tag$txOutTag") += 1L
                            if !continueOnError then {
                                info(
                                  f"decode error at entry #$total%,d (txOutTag=$txOutTag): ${ex.getMessage}"
                                )
                                info(
                                  s"  key bytes (hex, ${keyBytes.length}B): " +
                                      keyBytes.take(64).map(b => f"${b & 0xff}%02x").mkString
                                )
                                info(
                                  s"  value bytes (hex, ${valueBytes.length}B): " +
                                      valueBytes.take(128).map(b => f"${b & 0xff}%02x").mkString
                                )
                                throw ex
                            }
                    }
                    total += 1L
                    if (total & 0xffff) == 0 then info(s"…decoded $total entries so far")
                }
            }

            val elapsed = (System.nanoTime() - started) / 1_000_000L
            info(
              f"decoded $total%,d entries in ${elapsed}%d ms " +
                  f"(${total.toDouble / math.max(elapsed, 1L) * 1000.0}%.0f entries/s)"
            )
            info("TxOut tag distribution:")
            byTxOutTag.toSeq.sortBy(-_._2).foreach { case (k, v) =>
                info(f"  tag $k%3d:  $v%,d")
            }
            info("shape distribution:")
            byShape.toSeq.sortBy(-_._2).foreach { case (k, v) =>
                info(f"  $k%-40s $v%,d")
            }
            assert(total > 0L, "expected at least one UTxO entry on a real fixture")
        } finally input.close()
    }
}
