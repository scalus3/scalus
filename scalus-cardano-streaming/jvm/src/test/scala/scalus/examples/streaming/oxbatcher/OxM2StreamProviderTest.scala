package scalus.examples.streaming.oxbatcher

import org.scalatest.funsuite.AnyFunSuite
import ox.*
import ox.flow.Flow
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.stream.{ChainPoint, StartFrom, SubscriptionOptions, SyntheticEventSource, UtxoEvent, UtxoEventQuery}
import scalus.cardano.node.stream.engine.{Engine, EngineTestFixtures}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import OxScalusAsyncStream.Id
import EngineTestFixtures.*

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** M2 round-trip tests for the ox adapter. Mirrors
  * [[scalus.examples.streaming.fs2batcher.Fs2M2StreamProviderTest]].
  */
class OxM2StreamProviderTest extends AnyFunSuite {

    private given ExecutionContext = ExecutionContext.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val awaitTimeout: FiniteDuration = 5.seconds

    private def newProvider: OxBlockchainStreamProvider & SyntheticEventSource[Id, Flow] =
        new OxBlockchainStreamProvider(
          new Engine(ci, None, Engine.DefaultSecurityParam)
        ) with SyntheticEventSource[Id, Flow]

    test("subscribeUtxoQuery: Created then Spent as blocks are applied") {
        supervised {
            val provider = newProvider
            val q = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false
            )
            val flow = provider.subscribeUtxoQuery(q, opts)
            val pusher = forkUser {
                val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
                val tx2 = tx(200, spending = Set(input(100, 0)))
                Await.result(provider.applyBlock(block(1, tx1)), awaitTimeout)
                Await.result(provider.applyBlock(block(2, tx2)), awaitTimeout)
            }
            val events = flow.take(2).runToList()
            pusher.join()
            val kinds = events.map {
                case _: UtxoEvent.Created    => "C"
                case _: UtxoEvent.Spent      => "S"
                case _: UtxoEvent.RolledBack => "R"
            }
            assert(kinds == List("C", "S"))
        }
    }

    test("subscribeUtxoQuery: RolledBack after rollbackTo") {
        supervised {
            val provider = newProvider
            val q = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false
            )
            val flow = provider.subscribeUtxoQuery(q, opts)
            val pusher = forkUser {
                val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
                Await.result(provider.applyBlock(block(1, tx1)), awaitTimeout)
                Await.result(provider.rollbackTo(ChainPoint(0L, blockHash(0))), awaitTimeout)
            }
            val events = flow.take(2).runToList()
            pusher.join()
            assert(events.last.isInstanceOf[UtxoEvent.RolledBack])
        }
    }

    test("subscribeProtocolParams emits current value immediately") {
        supervised {
            val provider = newProvider
            val flow = provider.subscribeProtocolParams()
            val params = flow.take(1).runToList()
            assert(params == List(ci.protocolParams))
        }
    }

    test("subscribeTransactionStatus: latest-value sees a current value after submit + block") {
        supervised {
            val provider = newProvider
            val h = txHash(999)
            val flow = provider.subscribeTransactionStatus(h)
            val pusher = forkUser {
                Await.result(provider.engine.notifySubmit(h), awaitTimeout)
                Await.result(provider.applyBlock(block(1, tx(999))), awaitTimeout)
            }
            // LatestValueMailbox coalesces — assert one status arrives.
            val statuses = flow.take(1).runToList()
            pusher.join()
            assert(statuses.size == 1)
        }
    }
}
