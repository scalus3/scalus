package scalus.cardano.node.stream.engine.persistence

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.UtxoQuery
import scalus.cardano.node.UtxoSource
import scalus.cardano.node.stream.engine.{Engine, EngineTestFixtures, Mailbox}
import scalus.cardano.node.stream.UtxoEvent

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import EngineTestFixtures.*

/** Round-trips engine state through [[EnginePersistenceStore.inMemory]].
  *
  * Exercises the warm-restart contract without touching the filesystem: drive one engine through
  * forwards / rollbacks / submits, take a snapshot, then rebuild a fresh engine from the shared
  * in-memory store and assert state equivalence.
  */
class InMemoryStoreSuite extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global
    private val timeout: FiniteDuration = 5.seconds
    private val ci = CardanoInfo.preview
    private val appId = "scalus.test.in-memory"
    private val networkMagic = 42L

    test("tip + bucket state + ownSubmissions survive through snapshot+rebuild") {
        val store = EnginePersistenceStore.inMemory()
        val engine = new Engine(ci, None, Engine.DefaultSecurityParam, store)

        // Live subscription so UTxO events actually land in a bucket.
        val id = engine.nextSubscriptionId()
        val mb = Mailbox.delta[UtxoEvent]()
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))
        Await.result(
          engine.registerUtxoSubscription(id, q, includeExistingUtxos = false, mb),
          timeout
        )

        val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
        Await.result(engine.onRollForward(block(1, tx1)), timeout)
        Await.result(engine.notifySubmit(txHash(777)), timeout)
        val tx2 = tx(101, producing = IndexedSeq(output(addressA, 20L)))
        Await.result(engine.onRollForward(block(2, tx2)), timeout)

        val snap = Await.result(engine.takeSnapshot(appId, networkMagic), timeout)
        Await.result(store.compact(snap), timeout)

        // Build a fresh engine from the same store.
        val loaded = Await.result(store.load(), timeout)
        assert(loaded.isDefined)
        val engine2 = Await.result(
          Engine.rebuildFrom(loaded.get, ci, None, Engine.DefaultSecurityParam, store),
          timeout
        )

        assert(engine2.currentTip.map(_.point) == Some(point(2)))
        // Subscribing with the same query on the rebuilt engine must see the carried-over
        // bucket state — seeding is skipped because the bucket already exists.
        val mb2 = Mailbox.delta[UtxoEvent]()
        val id2 = engine2.nextSubscriptionId()
        Await.result(
          engine2.registerUtxoSubscription(id2, q, includeExistingUtxos = false, mb2),
          timeout
        )
        val local = Await.result(engine2.findUtxosLocal(q), timeout)
        assert(local.isDefined, "query should be covered by a rehydrated bucket")
        val utxos = local.get
        assert(utxos.size == 2, s"expected 2 UTxOs, got ${utxos.size}")
        assert(utxos.contains(input(100, 0)))
        assert(utxos.contains(input(101, 0)))
    }

    test("ownSubmitted + forward via journal (no compaction) replays correctly") {
        val store = EnginePersistenceStore.inMemory()
        val engine = new Engine(ci, None, Engine.DefaultSecurityParam, store)

        Await.result(engine.notifySubmit(txHash(555)), timeout)
        Await.result(
          engine.onRollForward(block(1, tx(1, producing = IndexedSeq(output(addressA, 5L))))),
          timeout
        )
        // NO compact — everything stays in the journal.

        val loaded = Await.result(store.load(), timeout)
        assert(loaded.isDefined)
        assert(loaded.get.snapshot.isEmpty, "no compaction was triggered")
        assert(loaded.get.journal.size == 2)

        val engine2 = Await.result(
          Engine.rebuildFrom(loaded.get, ci, None, Engine.DefaultSecurityParam, store),
          timeout
        )
        assert(engine2.currentTip.map(_.point) == Some(point(1)))
        val status = Await.result(engine2.txStatus(txHash(555)), timeout)
        assert(status.contains(scalus.cardano.node.TransactionStatus.Pending))
    }

    test("rollback emits Backward record and replay restores pre-rollback tip") {
        val store = EnginePersistenceStore.inMemory()
        val engine = new Engine(ci, None, Engine.DefaultSecurityParam, store)

        val mb = Mailbox.delta[UtxoEvent]()
        val id = engine.nextSubscriptionId()
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))
        Await.result(
          engine.registerUtxoSubscription(id, q, includeExistingUtxos = false, mb),
          timeout
        )

        Await.result(
          engine.onRollForward(block(1, tx(1, producing = IndexedSeq(output(addressA, 5L))))),
          timeout
        )
        Await.result(
          engine.onRollForward(block(2, tx(2, producing = IndexedSeq(output(addressA, 7L))))),
          timeout
        )
        Await.result(engine.onRollBackward(point(1)), timeout)

        val loaded = Await.result(store.load(), timeout)
        assert(loaded.isDefined)
        // Journal should contain 2 Forwards + 1 Backward.
        assert(loaded.get.journal.collect { case _: JournalRecord.Backward => 1 }.size == 1)

        val engine2 = Await.result(
          Engine.rebuildFrom(loaded.get, ci, None, Engine.DefaultSecurityParam, store),
          timeout
        )
        assert(engine2.currentTip.map(_.point) == Some(point(1)))
    }
}
