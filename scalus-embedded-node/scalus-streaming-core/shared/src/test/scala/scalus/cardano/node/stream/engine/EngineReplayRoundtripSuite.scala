package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore
import scalus.cardano.node.stream.{StartFrom, UtxoEvent}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

import EngineTestFixtures.*

/** Phase 2a end-to-end: the canonical Checkpoint-driven restart flow.
  *
  *   1. Engine A starts cold, a subscription acquires the bucket for `FromAddress(addressA)`, 20
  *      blocks are applied. The persistence journal captures each `onRollForward` delta.
  *   2. Engine A takes a snapshot, compacts it, and shuts down.
  *   3. Engine B is rebuilt from the persisted state. The rollback buffer is re-hydrated from the
  *      compacted snapshot's `volatileTail`; buckets are restored with `current` + `history`
  *      populated from the snapshot's per-block deltas.
  *   4. A new subscription on B registers with `StartFrom.At(block_5.point)`. The per-subscription
  *      replay emits `Created` events for blocks 6..20 — sourcing deltas from the bucket's restored
  *      `history` (block.transactions are synthetic-empty post-restore).
  *
  * This is the round-trip the M7 `checkpoint-restart-m7.md` doc advertises.
  */
class EngineReplayRoundtripSuite extends AnyFunSuite {

    private val ci: CardanoInfo = CardanoInfo.preview
    private val timeout: FiniteDuration = 5.seconds
    private val query: UtxoQuery = UtxoQuery(UtxoSource.FromAddress(addressA))

    test("persist + restart + At(checkpoint) replays events 6..20 for the same query") {
        val store = EnginePersistenceStore.inMemory()

        // --- Engine A: run 20 blocks with an active subscription so the bucket for
        //     FromAddress(addressA) exists at snapshot time.
        val engineA = new Engine(ci, None, Engine.DefaultSecurityParam, store)
        val subA = engineA.nextSubscriptionId()
        val mailboxA = Mailbox.delta[UtxoEvent]()
        Await.result(
          engineA.registerUtxoSubscription(subA, query, includeExistingUtxos = false, mailboxA),
          timeout
        )

        (1L to 20L).foreach { n =>
            Await.result(
              engineA.onRollForward(
                block(n, tx(idN = 100L + n, producing = IndexedSeq(output(addressA, n))))
              ),
              timeout
            )
        }

        // Ignore Engine A's live events — we're testing the restart replay on Engine B.
        val liveEvents = drain(mailboxA, 20)
        assert(liveEvents.size == 20)

        // Seal the in-memory store: take snapshot, compact, close.
        val snap = Await.result(
          engineA.takeSnapshot(appId = "round-trip-test", networkMagic = 2L),
          timeout
        )
        Await.result(store.compact(snap), timeout)
        Await.result(store.close(), timeout)
        Await.result(engineA.shutdown(), timeout)

        // --- Engine B: rebuild from the sealed store.
        val state = Await.result(store.load(), timeout).get
        val engineB = Await.result(
          Engine.rebuildFrom(state, ci, None, Engine.DefaultSecurityParam, store),
          timeout
        )

        // Sanity: the restored tip matches where Engine A stopped.
        assert(engineB.currentTip.map(_.point) == Some(point(20)))

        // --- Checkpoint-driven re-subscribe at block 5.
        val subB = engineB.nextSubscriptionId()
        val mailboxB = Mailbox.delta[UtxoEvent]()
        Await.result(
          engineB.registerUtxoSubscription(
            subB,
            query,
            includeExistingUtxos = false,
            mailboxB,
            StartFrom.At(point(5))
          ),
          timeout
        )

        val replayed = drain(mailboxB, 15).collect { case c: UtxoEvent.Created => c }
        assert(replayed.size == 15)
        assert(replayed.map(_.at) == (6L to 20L).map(point))
        assert(replayed.map(_.producedBy) == (6L to 20L).map(n => txHash(100L + n)))
    }
}
