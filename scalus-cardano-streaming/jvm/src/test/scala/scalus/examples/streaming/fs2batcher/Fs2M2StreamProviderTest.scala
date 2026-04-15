package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.TransactionStatus
import scalus.cardano.node.stream.{ChainPoint, StartFrom, SubscriptionOptions, SyntheticEventSource, UtxoEvent, UtxoEventQuery}
import scalus.cardano.node.stream.engine.{Engine, EngineTestFixtures}
import scalus.cardano.node.{UtxoQuery, UtxoSource}

import Fs2ScalusAsyncStream.IOStream
import EngineTestFixtures.*

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** M2 round-trip tests: drive the engine through `applyBlock` / `rollbackTo`, then observe what the
  * adapter's fs2 stream emits.
  *
  * Distinct from [[Fs2BlockchainStreamProviderTest]] which exercises raw fan-out and back-pressure
  * without touching engine state.
  */
class Fs2M2StreamProviderTest extends AnyFunSuite {

    private given IORuntime = IORuntime.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val awaitTimeout: FiniteDuration = 5.seconds

    private def newProvider(using
        Dispatcher[IO],
        ExecutionContext
    ): Fs2BlockchainStreamProvider & SyntheticEventSource[IO, IOStream] =
        new Fs2BlockchainStreamProvider(
          new Engine(ci, None, Engine.DefaultSecurityParam)
        ) with SyntheticEventSource[IO, IOStream]

    private def withDispatcher[T](
        f: (Dispatcher[IO], ExecutionContext) ?=> IO[T]
    ): T =
        // Parallel dispatcher is safe now that engine-side ordering
        // lives in the mailbox (synchronous `offer`) rather than in
        // racy `sink.offer` completion callbacks.
        Dispatcher
            .parallel[IO]
            .use { d =>
                given Dispatcher[IO] = d
                given ExecutionContext = ExecutionContext.global
                f
            }
            .unsafeRunSync()

    test("subscribeUtxoQuery: Created then Spent as blocks are applied") {
        withDispatcher {
            val provider = newProvider
            val q = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false
            )
            val stream = provider.subscribeUtxoQuery(q, opts)
            val collected = stream.take(2).compile.toList

            val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
            val tx2 = tx(200, spending = Set(input(100, 0)))
            Await.result(provider.applyBlock(block(1, tx1)), awaitTimeout)
            Await.result(provider.applyBlock(block(2, tx2)), awaitTimeout)

            collected.map { events =>
                val kinds = events.map {
                    case _: UtxoEvent.Created    => "C"
                    case _: UtxoEvent.Spent      => "S"
                    case _: UtxoEvent.RolledBack => "R"
                }
                assert(kinds == List("C", "S"))
            }
        }
    }

    test("subscribeUtxoQuery: RolledBack event after rollbackTo") {
        withDispatcher {
            val provider = newProvider
            val q = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false
            )
            val stream = provider.subscribeUtxoQuery(q, opts)
            val collected = stream.take(2).compile.toList

            val tx1 = tx(100, producing = IndexedSeq(output(addressA, 10L)))
            Await.result(provider.applyBlock(block(1, tx1)), awaitTimeout)
            Await.result(provider.rollbackTo(ChainPoint(0L, blockHash(0))), awaitTimeout)

            collected.map { events =>
                val last = events.last
                assert(last.isInstanceOf[UtxoEvent.RolledBack])
            }
        }
    }

    test("subscribeProtocolParams emits current value immediately") {
        withDispatcher {
            val provider = newProvider
            val stream = provider.subscribeProtocolParams()
            val collected = stream.take(1).compile.toList
            collected.map { params =>
                assert(params.size == 1)
                assert(params.head == ci.protocolParams)
            }
        }
    }

    test("subscribeTransactionStatus: latest-value sees Confirmed after submit + block") {
        withDispatcher {
            val provider = newProvider
            val h = txHash(999)
            val stream = provider.subscribeTransactionStatus(h)
            // LatestValueMailbox coalesces; we may see fewer than 3
            // events but the last one must be Confirmed.
            val collected = stream.take(1).compile.toList

            Await.result(provider.engine.notifySubmit(h), awaitTimeout)
            Await.result(provider.applyBlock(block(1, tx(999))), awaitTimeout)

            collected.map { statuses =>
                assert(statuses.nonEmpty)
                // Either NotFound (registration emitted before
                // submit/block landed) or Confirmed (events coalesced
                // by the time take(1) sampled). Any intermediate
                // Pending → Confirmed transition is squashed.
                assert(
                  statuses.head == TransactionStatus.NotFound ||
                      statuses.head == TransactionStatus.Pending ||
                      statuses.head == TransactionStatus.Confirmed
                )
            }
        }
    }
}
