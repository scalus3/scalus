package scalus.cardano.node.stream.engine.snapshot

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.KvChainStore
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore

import java.nio.file.{Files, Path}

/** Manual end-to-end probe: runs [[SnapshotDirRestorer]] against a real mithril-downloaded
  * preview snapshot. Gated on `SCALUS_SNAPSHOT_DIR_PROBE=1` and `SCALUS_MITHRIL_DEST=<dir>` (or
  * the `/tmp/mithrill-preview` fallback).
  *
  * {{{
  *   SCALUS_SNAPSHOT_DIR_PROBE=1 \
  *   SCALUS_MITHRIL_DEST=/tmp/mithrill-preview \
  *     sbt 'scalusChainStoreMithril/testOnly *SnapshotDirRealFixtureProbe'
  * }}}
  */
final class SnapshotDirRealFixtureProbe extends AnyFunSuite {

    private val DefaultDest = "/tmp/mithrill-preview"

    test("[manual] end-to-end SnapshotDirRestorer against preview fixture") {
        val enabled = sys.env.get("SCALUS_SNAPSHOT_DIR_PROBE").contains("1")
        assume(enabled, "set SCALUS_SNAPSHOT_DIR_PROBE=1 to run")

        val root = Path.of(sys.env.getOrElse("SCALUS_MITHRIL_DEST", DefaultDest))
        assume(
          Files.isDirectory(root.resolve("immutable")) &&
              Files.isDirectory(root.resolve("ledger")),
          s"expected immutable/ and ledger/ under $root"
        )

        val store = new KvChainStore(InMemoryKvStore())
        try {
            val blockProgress = new java.util.concurrent.atomic.AtomicLong(0L)
            val utxoProgress = new java.util.concurrent.atomic.AtomicLong(0L)
            val stats = new SnapshotDirRestorer(store).restore(
              root,
              onBlockProgress = p => {
                  val chunks = p.chunkIndex + 1
                  if chunks % 100 == 0 || chunks == p.totalChunks then
                      blockProgress.set(p.cumulativeBlocks)
                      info(
                        f"blocks: chunk $chunks%,d / ${p.totalChunks}%,d  cumBlocks=${p.cumulativeBlocks}%,d  cumBytes=${TestUtils.humanBytes(p.cumulativeBytes)}"
                      )
              },
              onUtxoProgress = p => {
                  utxoProgress.set(p.utxosApplied)
                  if (p.utxosApplied & 0xffff) == 0 then
                      info(f"utxos: ${p.utxosApplied}%,d applied")
              }
            )
            info(
              f"=== DONE in ${stats.elapsedMillis / 1000}%,ds ===" +
                  f"  blocks=${stats.blocks.blocksApplied}%,d  utxos=${stats.utxos.utxosRestored}%,d" +
                  f"  skippedByron=${stats.blocks.skippedByron}%,d"
            )
            info(s"final tip: ${stats.blocks.tip}")
            assert(stats.blocks.blocksApplied > 0)
            assert(stats.utxos.utxosRestored > 0)
            assert(stats.utxos.tip == stats.blocks.tip.get)
            assert(store.tip == stats.blocks.tip)
        } finally store.close()
    }
}
