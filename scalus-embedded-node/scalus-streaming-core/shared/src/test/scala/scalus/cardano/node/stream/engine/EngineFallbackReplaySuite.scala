package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.engine.replay.{ReplayError, RollbackBufferReplaySource}
import scalus.cardano.node.stream.{StartFrom, UtxoEvent}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.Await
import scala.concurrent.duration.*

import EngineTestFixtures.*

class EngineFallbackReplaySuite extends AnyFunSuite {

    private val ci: CardanoInfo = CardanoInfo.preview
    private val timeout: FiniteDuration = 5.seconds

    test("rollback buffer wins when it covers the checkpoint") {
        // Rollback buffer and fallback cover the same slots with DIFFERENT tx ids so the
        // observed producedBy reveals which source produced the replay events.
        val hist = (1L to 3L).map(n =>
            block(n, tx(idN = 100L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val stubBlocks = (1L to 3L).map(n =>
            block(n, tx(idN = 9000L + n, producing = IndexedSeq(output(addressA, n * 10))))
        )
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(new RollbackBufferReplaySource(stubBlocks))
        )
        hist.foreach(b => Await.result(engine.onRollForward(b), timeout))

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

    test("fallback source covers when rollback buffer doesn't") {
        val stubBlocks = (2L to 6L).map(n =>
            block(n, tx(idN = 2000L + n, producing = IndexedSeq(output(addressA, n))))
        )
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(new RollbackBufferReplaySource(stubBlocks))
        )
        Await.result(
          engine.onRollForward(
            block(5, tx(idN = 5555L, producing = IndexedSeq(output(addressA, 5))))
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

        val events = drain(mailbox, 3).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.at) == Seq(point(3), point(4), point(5)))
    }

    test("every source exhausted fails with ReplaySourceExhausted") {
        val stubBlocks = (5L to 6L).map(block(_))
        val engine = new Engine(
          ci,
          None,
          securityParam = 2160,
          fallbackReplaySources = List(new RollbackBufferReplaySource(stubBlocks))
        )
        Await.result(engine.onRollForward(block(5)), timeout)
        Await.result(engine.onRollForward(block(6)), timeout)

        val id = engine.nextSubscriptionId()
        val mailbox = Mailbox.delta[UtxoEvent]()
        val fut = engine.registerUtxoSubscription(
          id,
          UtxoQuery(UtxoSource.FromAddress(addressA)),
          includeExistingUtxos = false,
          mailbox,
          StartFrom.At(point(2))
        )
        val err = Await.ready(fut, timeout).value.get.failed.get
        assert(err.isInstanceOf[ReplayError.ReplaySourceExhausted])
        assert(err.asInstanceOf[ReplayError.ReplaySourceExhausted].point == point(2))
    }
}
