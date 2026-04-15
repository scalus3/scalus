package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.infra.ScalusBufferOverflowException
import scalus.cardano.ledger.{BlockHash, CardanoInfo}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, DeltaBufferPolicy, StartFrom, SubscriptionOptions, SyntheticEventSource}
import scalus.cardano.node.stream.fs2.{Fs2BlockchainStreamProvider, Fs2ScalusAsyncStream}

import Fs2ScalusAsyncStream.IOStream

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Round-trip tests for the fs2 [[Fs2BlockchainStreamProvider]] driven by [[SyntheticEventSource]].
  * Exercises the adapter-stream + mailbox plumbing in isolation from the engine's full pipeline.
  *
  * Mailbox semantics differ by stream kind:
  *   - tip / params / tx-status use a [[scalus.cardano.node.stream.engine.LatestValueMailbox]] —
  *     newer-wins, intermediate values may be coalesced.
  *   - UTxO uses a [[scalus.cardano.node.stream.engine.DeltaMailbox]] — strict FIFO; overflow on a
  *     bounded mailbox terminates with [[ScalusBufferOverflowException]].
  */
class Fs2BlockchainStreamProviderTest extends AnyFunSuite {

    private given IORuntime = IORuntime.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val zeroBlockHash: BlockHash = BlockHash.fromHex("0" * 64)
    private def pt(slot: Long): ChainPoint = ChainPoint(slot, zeroBlockHash)
    private def chainTip(slot: Long): ChainTip = ChainTip(pt(slot), slot)
    private val awaitTimeout: FiniteDuration = 5.seconds

    private def newProvider(using
        Dispatcher[IO],
        scala.concurrent.ExecutionContext
    ): Fs2BlockchainStreamProvider & SyntheticEventSource[IO, IOStream] =
        new Fs2BlockchainStreamProvider(
          new scalus.cardano.node.stream.engine.Engine(
            ci,
            None,
            scalus.cardano.node.stream.engine.Engine.DefaultSecurityParam
          )
        ) with SyntheticEventSource[IO, IOStream]

    private def withDispatcher[T](
        f: (Dispatcher[IO], scala.concurrent.ExecutionContext) ?=> IO[T]
    ): T =
        Dispatcher
            .parallel[IO]
            .use { d =>
                given Dispatcher[IO] = d
                given scala.concurrent.ExecutionContext = global
                f
            }
            .unsafeRunSync()

    test("subscribeTip: latest-value semantics — coalesces, then terminates on close") {
        withDispatcher {
            val provider = newProvider
            val stream = provider.subscribeTip()
            val collector = stream.compile.toList
            (1 to 10).foreach(i => Await.result(provider.pushTip(chainTip(i.toLong)), awaitTimeout))
            Await.result(provider.closeAllSubs(), awaitTimeout)
            collector.map { tips =>
                // LatestValueMailbox coalesces — assert we get *some*
                // values ending with the most recent and that the
                // stream terminates cleanly.
                assert(tips.nonEmpty)
                assert(tips.last.point.slot == 10L)
            }
        }
    }

    test("subscribeTip cancellation: stream.take(1) shrinks the registry") {
        withDispatcher {
            val provider = newProvider
            val stream = provider.subscribeTip()
            val take1 = stream.take(1).compile.toList
            Await.result(provider.pushTip(chainTip(1L)), awaitTimeout)
            take1.map { tips =>
                assert(tips.size == 1)
                Await.result(provider.pushTip(chainTip(99L)), awaitTimeout)
                assert(provider.activeSubscriptionCount == 0)
            }
        }
    }

    test("subscribeTip: failAllSubs terminates the stream with the supplied error") {
        withDispatcher {
            val provider = newProvider
            val stream = provider.subscribeTip()
            val attempted = stream.attempt.compile.toList
            Await.result(provider.pushTip(chainTip(1L)), awaitTimeout)
            val boom = new RuntimeException("boom")
            Await.result(provider.failAllSubs(boom), awaitTimeout)
            attempted.map { results =>
                val (_, lefts) = results.partition(_.isRight)
                assert(lefts.size == 1)
                assert(lefts.head.swap.toOption.exists(_.getMessage == "boom"))
            }
        }
    }

    test("DeltaMailbox overflow: bounded utxo subscription fails after exceeding capacity") {
        withDispatcher {
            val provider = newProvider
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = false,
              bufferPolicy = DeltaBufferPolicy.Bounded(2)
            )
            val q = scalus.cardano.node.stream.UtxoEventQuery(
              scalus.cardano.node.UtxoQuery(
                scalus.cardano.node.UtxoSource.FromInputs(Set.empty)
              )
            )
            val stream = provider.subscribeUtxoQuery(q, opts)
            // Push 3 events into a Bounded(2) mailbox, then drain.
            // The first two arrive; the third overflows so the
            // consumer's pull eventually fails.
            (1 to 3).foreach { i =>
                Await.result(provider.pushUtxo(testEvent(i.toLong)), awaitTimeout)
            }
            val attempted = stream.attempt.compile.toList
            attempted.map { results =>
                val (rights, lefts) = results.partition(_.isRight)
                assert(rights.size == 2)
                assert(lefts.size == 1)
                assert(
                  lefts.head.swap.toOption.exists(_.isInstanceOf[ScalusBufferOverflowException])
                )
            }
        }
    }

    private def testEvent(slot: Long): scalus.cardano.node.stream.UtxoEvent.RolledBack =
        scalus.cardano.node.stream.UtxoEvent.RolledBack(pt(slot))
}
