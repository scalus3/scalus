package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.{ChainPoint, StartFrom, UtxoEvent}
import scalus.cardano.node.stream.engine.replay.ReplayError
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.Await
import scala.concurrent.duration.*

import EngineTestFixtures.*

class EngineReplaySuite extends AnyFunSuite {

    private val ci: CardanoInfo = CardanoInfo.preview
    private val timeout: FiniteDuration = 5.seconds

    private def mkEngine: Engine = new Engine(ci, None, securityParam = 2160)

    test("StartFrom.At(point) replays matching UTxO events between checkpoint and current tip") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        // Seed 4 blocks, each creating a UTxO at addressA before any subscription exists.
        // When a subscription registers at checkpoint point(1), replay should emit Created events
        // for the UTxOs created in blocks 2, 3 and 4 (strictly after the checkpoint, up to tip).
        val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
        val tx2 = tx(200, producing = IndexedSeq(output(addressA, 20L)))
        val tx3 = tx(300, producing = IndexedSeq(output(addressA, 30L)))
        val tx4 = tx(400, producing = IndexedSeq(output(addressA, 40L)))
        Await.result(engine.onRollForward(block(1, tx1)), timeout)
        Await.result(engine.onRollForward(block(2, tx2)), timeout)
        Await.result(engine.onRollForward(block(3, tx3)), timeout)
        Await.result(engine.onRollForward(block(4, tx4)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            q,
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(point(1))
          ),
          timeout
        )

        val events = drain(mailbox, 3)
        val created = events.collect { case c: UtxoEvent.Created => c }
        assert(created.map(_.at) == Seq(point(2), point(3), point(4)))
    }

    test("StartFrom.At(currentTip) produces an empty replay and transitions to live") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        val tx1 = tx(100, producing = IndexedSeq(output(addressA, 1L)))
        Await.result(engine.onRollForward(block(1, tx1)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            q,
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(point(1))
          ),
          timeout
        )

        // Replay window is empty. Next block should flow as live.
        val tx2 = tx(200, producing = IndexedSeq(output(addressA, 2L)))
        Await.result(engine.onRollForward(block(2, tx2)), timeout)

        val events = drain(mailbox, 1)
        assert(events.size == 1)
        val c = events.head.asInstanceOf[UtxoEvent.Created]
        assert(c.at == point(2))
    }

    test("StartFrom.At(point ahead of tip) fails with ReplayCheckpointAhead") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        Await.result(engine.onRollForward(block(1, tx(100))), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val fut = engine.registerUtxoSubscription(
          id,
          q,
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(99))
        )
        val err = Await.ready(fut, timeout).value.get.failed.get
        assert(err.isInstanceOf[ReplayError.ReplayCheckpointAhead])

        // Mailbox also surfaces the same error.
        val pulled = Await.ready(mailbox.pull(), timeout).value.get
        assert(pulled.failed.get.isInstanceOf[ReplayError.ReplayCheckpointAhead])
    }

    test("StartFrom.At(uncovered point) fails with ReplaySourceExhausted") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        Await.result(engine.onRollForward(block(5, tx(100))), timeout)
        Await.result(engine.onRollForward(block(6, tx(200))), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        // point(3) is not in the rollback buffer (we only have blocks 5,6), and there's no
        // ChainStore or peer source configured.
        val fut = engine.registerUtxoSubscription(
          id,
          q,
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(3))
        )
        val err = Await.ready(fut, timeout).value.get.failed.get
        assert(err.isInstanceOf[ReplayError.ReplaySourceExhausted])
    }

    test("live events arriving after a replay land in order after the replay events") {
        // The replay runs as a single worker task; any live onRollForward queues behind it, so
        // its events appear on the mailbox strictly after the replay output.
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        Await.result(
          engine.onRollForward(block(1, tx(100, producing = IndexedSeq(output(addressA, 1L))))),
          timeout
        )
        Await.result(
          engine.onRollForward(block(2, tx(200, producing = IndexedSeq(output(addressA, 2L))))),
          timeout
        )

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        // Kick off a replay from origin; the worker will emit events for blocks 1 and 2.
        val regFut = engine.registerUtxoSubscription(
          id,
          q,
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(ChainPoint.origin)
        )
        Await.result(regFut, timeout)

        // Now a live block lands.
        Await.result(
          engine.onRollForward(block(3, tx(300, producing = IndexedSeq(output(addressA, 3L))))),
          timeout
        )

        val events = drain(mailbox, 3).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.at) == Seq(point(1), point(2), point(3)))
    }

    test("unregister after replay completes is a no-op for the next onRollForward") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        Await.result(
          engine.onRollForward(block(1, tx(100, producing = IndexedSeq(output(addressA, 1L))))),
          timeout
        )

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            q,
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(ChainPoint.origin)
          ),
          timeout
        )
        Await.result(engine.unregisterUtxo(id), timeout)

        // Subsequent onRollForward must not throw or stall.
        Await.result(
          engine.onRollForward(block(2, tx(200, producing = IndexedSeq(output(addressA, 2L))))),
          timeout
        )
    }

    test("UtxoSource.FromAddress replay tracks Spent events for intra-window chains") {
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        val tx1 = tx(100, producing = IndexedSeq(output(addressA, 1L)))
        val tx2 = tx(200, spending = Set(input(100, 0)))
        Await.result(engine.onRollForward(block(1, tx1)), timeout)
        Await.result(engine.onRollForward(block(2, tx2)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            q,
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(ChainPoint.origin)
          ),
          timeout
        )

        val events = drain(mailbox, 2)
        assert(events.head.isInstanceOf[UtxoEvent.Created])
        assert(events.last.isInstanceOf[UtxoEvent.Spent])
    }

    test("rollback during replay emits RolledBack after the replay events") {
        // The engine worker is single-threaded; a rollback enqueued before the replay's
        // submit finishes executes after it. The subscriber therefore sees the full
        // replay first and `RolledBack(to)` after, even when `to` is inside the replay
        // window. Consumers treat `RolledBack` as "discard everything past this point"
        // — consistent with the live-fan-out contract that replay events are real past
        // events.
        val engine = mkEngine
        val q = UtxoQuery(UtxoSource.FromAddress(addressA))

        (1L to 5L).foreach { n =>
            Await.result(
              engine.onRollForward(
                block(n, tx(100 + n, producing = IndexedSeq(output(addressA, n))))
              ),
              timeout
            )
        }

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()

        // Do NOT await registration before enqueueing the rollback; both land on the
        // worker queue in FIFO order.
        val regFut = engine.registerUtxoSubscription(
          id,
          q,
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )
        val rbFut = engine.onRollBackward(point(3))
        Await.result(regFut, timeout)
        Await.result(rbFut, timeout)

        val events = drain(mailbox, 5)
        val created = events.init.collect { case c: UtxoEvent.Created => c }
        assert(created.map(_.at) == Seq(point(2), point(3), point(4), point(5)))
        assert(events.last == UtxoEvent.RolledBack(point(3)))
    }

    test("multi-subscription same checkpoint: each sub runs its own replay") {
        // Three subscribers registering with the same `At(point)` simultaneously each
        // pay the replay cost independently — M7 makes no replay-sharing promise. Each
        // must see the full replay window for its own query.
        val engine = mkEngine
        val qA = UtxoQuery(UtxoSource.FromAddress(addressA))

        (1L to 4L).foreach { n =>
            Await.result(
              engine.onRollForward(
                block(n, tx(100 + n, producing = IndexedSeq(output(addressA, n))))
              ),
              timeout
            )
        }

        val mailboxes = (1 to 3).map(_ => Mailbox.delta[UtxoEvent]())
        // Enqueue all three registrations without awaiting — they serialise on the
        // worker but register concurrently from the caller's perspective.
        val futures = mailboxes.map { mb =>
            engine.registerUtxoSubscription(
              engine.nextSubscriptionId(),
              qA,
              includeExistingUtxos = false,
              mb,
              StartFrom.At(point(1))
            )
        }
        futures.foreach(Await.result(_, timeout))

        mailboxes.foreach { mb =>
            val created = drain(mb, 3).collect { case c: UtxoEvent.Created => c }
            assert(created.map(_.at) == Seq(point(2), point(3), point(4)))
        }
    }
}
