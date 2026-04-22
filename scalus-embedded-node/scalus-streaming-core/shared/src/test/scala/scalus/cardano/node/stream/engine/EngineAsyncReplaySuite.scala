package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.engine.replay.{AsyncReplaySourceStub, ExhaustedAsyncReplaySource, ImmediateAsyncReplaySource, InterruptedAsyncReplaySource, ReplayError}
import scalus.cardano.node.stream.{StartFrom, UtxoEvent}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

import EngineTestFixtures.*

class EngineAsyncReplaySuite extends AnyFunSuite {

    private val ci: CardanoInfo = CardanoInfo.preview
    private val timeout: FiniteDuration = 5.seconds
    private given ExecutionContext = ExecutionContext.global

    test("async fallback is tried only when every sync source misses the window") {
        // Rollback buffer holds blocks 5..6; subscription asks for point(2), which sync sources
        // cannot reach. The async source prefetches blocks 3..6 and the sub sees Created for each.
        val prefetch = (3L to 6L).map(n =>
            block(n, tx(idN = 100L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(new ImmediateAsyncReplaySource(prefetch))
        )
        Await.result(
          engine.onRollForward(
            block(5, tx(idN = 500L, producing = IndexedSeq(output(addressA, 5))))
          ),
          timeout
        )
        Await.result(
          engine.onRollForward(
            block(6, tx(idN = 600L, producing = IndexedSeq(output(addressA, 6))))
          ),
          timeout
        )

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            UtxoQuery(UtxoSource.FromAddress(addressA)),
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(point(2))
          ),
          timeout
        )

        val events = drain(mailbox, 4).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.at) == Seq(point(3), point(4), point(5), point(6)))
    }

    test("sync source wins over async source when both cover the window") {
        // With the rollback buffer already covering the checkpoint the engine must NOT call
        // prefetch on the async source. Observable via the distinct producedBy hashes.
        val syncHist = (1L to 3L).map(n =>
            block(n, tx(idN = 100L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val asyncOverlap = (1L to 3L).map(n =>
            block(n, tx(idN = 9000L + n, producing = IndexedSeq(output(addressA, n * 10))))
        )
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(new ImmediateAsyncReplaySource(asyncOverlap))
        )
        syncHist.foreach(b => Await.result(engine.onRollForward(b), timeout))

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            UtxoQuery(UtxoSource.FromAddress(addressA)),
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(point(1))
          ),
          timeout
        )

        val events = drain(mailbox, 2).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.producedBy) == Seq(txHash(102L), txHash(103L)))
    }

    test("live events during async prefetch are buffered and drain after the replay") {
        // The stub gates prefetch on an external signal. While prefetch is waiting we apply a
        // live block; its events land in the subscription's pending buffer. Phase B then emits
        // prefetched events first, then the buffered event, then subsequent live events directly.
        val prefetch = (2L to 3L).map(n =>
            block(n, tx(idN = 2000L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val stub = new AsyncReplaySourceStub(prefetch)
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(stub)
        )

        // Engine tip at registration = point(3).
        Await.result(
          engine.onRollForward(
            block(3, tx(idN = 300L, producing = IndexedSeq(output(addressA, 99))))
          ),
          timeout
        )

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val regFut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )

        // While prefetch is blocked, deliver a new live block at point(4). Its event must go to
        // the sub's buffer, not the mailbox.
        Await.result(
          engine.onRollForward(
            block(4, tx(idN = 400L, producing = IndexedSeq(output(addressA, 4))))
          ),
          timeout
        )
        stub.complete()
        Await.result(regFut, timeout)

        val events = drain(mailbox, 3).collect { case c: UtxoEvent.Created => c }
        // Two prefetched (2, 3) then the buffered live event (4), in that exact order.
        assert(events.map(_.at) == Seq(point(2), point(3), point(4)))
    }

    test("async prefetch returning ReplaySourceExhausted cascades to the next async source") {
        val prefetch = (2L to 3L).map(n =>
            block(n, tx(idN = 2000L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(
            new ExhaustedAsyncReplaySource,
            new ImmediateAsyncReplaySource(prefetch)
          )
        )
        Await.result(
          engine.onRollForward(
            block(3, tx(idN = 30L, producing = IndexedSeq(output(addressA, 3))))
          ),
          timeout
        )

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        Await.result(
          engine.registerUtxoSubscription(
            id,
            UtxoQuery(UtxoSource.FromAddress(addressA)),
            includeExistingUtxos = false,
            mailbox,
            StartFrom.At(point(1))
          ),
          timeout
        )

        val events = drain(mailbox, 2).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.at) == Seq(point(2), point(3)))
    }

    test("async prefetch returning a non-exhaustion error fails the sub and does NOT cascade") {
        // InterruptedAsyncReplaySource always returns ReplayInterrupted. Even though a covering
        // source is listed after it, the engine must stop on the first non-exhaustion error so
        // the caller learns the actual cause.
        val covering = (2L to 3L).map(n => block(n, tx(2000L + n)))
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(
            new InterruptedAsyncReplaySource("simulated peer crash"),
            new ImmediateAsyncReplaySource(covering)
          )
        )
        Await.result(engine.onRollForward(block(3)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val fut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )
        val err = Await.ready(fut, timeout).value.get.failed.get
        assert(err.isInstanceOf[ReplayError.ReplayInterrupted])

        val pulled = Await.ready(mailbox.pull(), timeout).value.get
        assert(pulled.failed.get.isInstanceOf[ReplayError.ReplayInterrupted])
    }

    test("failed prefetch Future is surfaced as ReplayInterrupted") {
        val stub = new AsyncReplaySourceStub(Seq.empty)
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(stub)
        )
        Await.result(engine.onRollForward(block(2)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val fut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )
        stub.fail(new RuntimeException("socket closed"))
        val err = Await.ready(fut, timeout).value.get.failed.get
        assert(err.isInstanceOf[ReplayError.ReplayInterrupted])
    }

    test("unregister during prefetch stops the replay cleanly — Phase B does not leak events") {
        val prefetch = (2L to 3L).map(n =>
            block(n, tx(idN = 2000L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val stub = new AsyncReplaySourceStub(prefetch)
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(stub)
        )
        Await.result(engine.onRollForward(block(3)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val regFut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )

        Await.result(engine.unregisterUtxo(id), timeout)
        stub.complete()
        Await.result(regFut, timeout)

        // A subsequent live block must not throw and must not be routed to the departed sub.
        Await.result(engine.onRollForward(block(4)), timeout)
    }

    test("rollback during prefetch goes to the buffer and emerges after the replay") {
        // A rollback that fires while prefetch is in flight is captured in the sub's pending
        // buffer. Phase B emits the prefetched replay first, then the buffered RolledBack, same
        // as the sync-replay contract from EngineReplaySuite.
        val prefetch = (2L to 3L).map(n =>
            block(n, tx(idN = 2000L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val stub = new AsyncReplaySourceStub(prefetch)
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(stub)
        )
        (1L to 3L).foreach { n =>
            Await.result(
              engine.onRollForward(
                block(n, tx(idN = 100L + n, producing = IndexedSeq(output(addressA, n))))
              ),
              timeout
            )
        }

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val regFut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(1))
        )

        Await.result(engine.onRollBackward(point(2)), timeout)
        stub.complete()
        Await.result(regFut, timeout)

        val events = drain(mailbox, 3)
        val created = events.init.collect { case c: UtxoEvent.Created => c }
        assert(created.map(_.at) == Seq(point(2), point(3)))
        assert(events.last == UtxoEvent.RolledBack(point(2)))
    }
}
