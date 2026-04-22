package scalus.cardano.node.stream.engine

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.engine.kvstore.InMemoryKvStore
import scalus.cardano.node.stream.engine.replay.ChainStoreReplaySource
import scalus.cardano.node.stream.{ChainPoint, StartFrom, UtxoEvent}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import scala.concurrent.Await
import scala.concurrent.duration.*

import EngineTestFixtures.*

/** End-to-end: configure the engine with a [[KvChainStore]] backed by [[InMemoryKvStore]], apply
  * blocks past the rollback-buffer horizon, subscribe at a point only the ChainStore still covers,
  * observe the replay landing from the store instead of falling through to a peer source.
  */
class EngineChainStoreSuite extends AnyFunSuite {

    private val ci: CardanoInfo = CardanoInfo.preview
    private val timeout: FiniteDuration = 5.seconds

    test("engine populates the ChainStore on every onRollForward") {
        val store = new KvChainStore(InMemoryKvStore())
        val engine =
            new Engine(ci, None, securityParam = 2160, chainStore = Some(store))
        val blocks =
            (1L to 3L).map(n => block(n, tx(100 + n, producing = IndexedSeq(output(addressA, n)))))
        blocks.foreach(b => Await.result(engine.onRollForward(b), timeout))

        assert(store.tip.map(_.point).contains(blocks.last.point))
        val stored = store.blocksBetween(ChainPoint.origin, blocks.last.point).toOption.get.toList
        assert(stored.map(_.point) == blocks.map(_.point))
    }

    test("engine trims the ChainStore on onRollBackward") {
        val store = new KvChainStore(InMemoryKvStore())
        val engine =
            new Engine(ci, None, securityParam = 2160, chainStore = Some(store))
        (1L to 5L).foreach(n => Await.result(engine.onRollForward(block(n, tx(100 + n))), timeout))

        Await.result(engine.onRollBackward(point(3)), timeout)
        assert(store.tip.map(_.point).contains(point(3)))
        val stored = store.blocksBetween(ChainPoint.origin, point(5)).toOption.get.toList
        assert(stored.map(_.point) == Seq(point(1), point(2), point(3)))
    }

    test("ChainStoreReplaySource serves checkpoint replay from the populated store") {
        // Small securityParam so the rollback buffer ages blocks out; ChainStore picks up the
        // slack for `At(old_point)` resubscribes.
        val store = new KvChainStore(InMemoryKvStore())
        val engine = new Engine(
          ci,
          None,
          securityParam = 2,
          fallbackReplaySources = List(new ChainStoreReplaySource(store)),
          chainStore = Some(store)
        )

        // Apply 5 blocks with matching UTxO creations. The rollback buffer (k=2) holds only
        // the last two; blocks 1..3 are only in the ChainStore now.
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

        val events = drain(mailbox, 4).collect { case c: UtxoEvent.Created => c }
        assert(events.map(_.at) == Seq(point(2), point(3), point(4), point(5)))
    }
}
