package scalus.cardano.node.stream.engine.snapshot.immutabledb

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.node.stream.engine.KvChainStore
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore

/** End-to-end: committed preview fixture → ImmutableDbReader → HfcDiskBlockDecoder → AppliedBlock
  * → `KvChainStore.appendBlock`. Asserts the store's tip, block count, and round-trip of the
  * appended blocks via `blocksBetween`.
  */
final class ImmutableDbRestorerSuite extends AnyFunSuite {

    test("real fixture: restore 3 chunks into KvChainStore, read back via blocksBetween") {
        val (immutableDir, _) = ImmutableDbRealFixtureSuite.stageFixture()
        try {
            val store = new KvChainStore(InMemoryKvStore())
            try {
                assert(store.tip.isEmpty, "fresh store should have no tip")

                val progress = scala.collection.mutable.ArrayBuffer.empty[ImmutableDbRestorer.Progress]
                val stats = new ImmutableDbRestorer(store).restore(
                  immutableDir,
                  onProgress = p => progress += p
                )

                assert(stats.chunksProcessed == 3)
                assert(stats.blocksApplied > 0, s"expected >0 blocks applied, got $stats")
                assert(stats.skippedByron == 0, "preview fixture has no Byron chunks")
                assert(stats.tip.isDefined)
                assert(progress.size == 3)
                assert(progress.last.cumulativeBlocks == stats.blocksApplied)

                // Store tip must match the restorer-reported tip (KvChainStore persists .tip from
                // each appendBlock).
                assert(store.tip.contains(stats.tip.get), s"tip mismatch: ${store.tip} vs ${stats.tip}")

                // Round-trip: read every applied block back via blocksBetween. We use
                // `ChainPoint.origin` as the lower bound so we see everything; upper bound is the
                // current tip.
                val readBack = store
                    .blocksBetween(ChainPoint.origin, stats.tip.get.point)
                    .getOrElse(fail("blocksBetween should not return Exhausted after restore"))
                    .toVector
                assert(
                  readBack.size.toLong == stats.blocksApplied,
                  s"read back ${readBack.size} blocks, restored ${stats.blocksApplied}"
                )
                // Slots must be strictly increasing — proves block ordering was preserved.
                val slots = readBack.map(_.point.slot)
                assert(slots == slots.sorted.distinct, s"blocks not monotonic: $slots")
            } finally store.close()
        } finally ImmutableDbRealFixtureSuite.cleanup(immutableDir.getParent)
    }
}
