package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.infra.ScalusBufferOverflowException
import scalus.cardano.ledger.{BlockHash, CardanoInfo}
import scalus.cardano.node.stream.{
    BufferPolicy,
    ChainPoint,
    Overflow,
    StartFrom,
    SubscriptionOptions,
    SyntheticEventSource
}

import Fs2ScalusAsyncStream.IOStream

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Round-trip tests for the fs2 [[Fs2BlockchainStreamProvider]] driven
  * by [[SyntheticEventSource]].
  *
  * Each test:
  *   1. allocates a `Dispatcher[IO]` (the user-supplied resource the
  *      adapter requires);
  *   2. subscribes — registers a sink in the provider's registry and
  *      returns the lazy fs2 stream;
  *   3. drives the synthetic source synchronously, awaiting each push
  *      so ordering is deterministic;
  *   4. consumes the stream and asserts on the result.
  */
class Fs2BlockchainStreamProviderTest extends AnyFunSuite {

    private given IORuntime = IORuntime.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val zeroBlockHash: BlockHash =
        BlockHash.fromHex("0" * 64)
    private def pt(slot: Long): ChainPoint = ChainPoint(slot, zeroBlockHash)
    private val awaitTimeout: FiniteDuration = 5.seconds

    private final class TestProvider(using Dispatcher[IO], scala.concurrent.ExecutionContext)
        extends Fs2BlockchainStreamProvider(ci)
        with SyntheticEventSource[IO, IOStream]

    /** Run a Dispatcher-bound action synchronously. */
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

    test("subscribeTip + push N points: order preserved, stream completes on closeAllSubs") {
        withDispatcher {
            val provider = new TestProvider
            val stream = provider.subscribeTip()
            val collector = stream.compile.toList
            // Subscribe first, push next, then close — so the stream
            // sees all 10 points and a clean end-of-stream.
            (1 to 10).foreach(i => Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout))
            Await.result(provider.closeAllSubs(), awaitTimeout)
            collector.map { points =>
                assert(points.map(_.slot) == (1L to 10L).toList)
            }
        }
    }

    test("cancellation: dropping the stream unregisters the sink") {
        withDispatcher {
            val provider = new TestProvider
            val stream = provider.subscribeTip()
            // Take only 5 points, then the stream cancels.
            val take5 = stream.take(5).compile.toList
            // Push 5 points so the stream can finish.
            (1 to 5).foreach(i => Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout))
            take5.map { points =>
                assert(points.map(_.slot) == (1L to 5L).toList)
                // After consumer cancels, onCancel fires and the
                // registry shrinks. The push below should observe
                // zero subscribers — it won't deliver anywhere, but
                // also won't fail.
                Await.result(provider.pushTip(pt(99L)), awaitTimeout)
                // Verify the registry is empty post-cancellation.
                assert(provider.activeSubscriptionCount == 0)
            }
        }
    }

    test("error propagation: failAllSubs terminates the stream with the supplied error") {
        withDispatcher {
            val provider = new TestProvider
            val stream = provider.subscribeTip()
            val attempted = stream.attempt.compile.toList
            (1 to 3).foreach(i => Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout))
            val boom = new RuntimeException("boom")
            Await.result(provider.failAllSubs(boom), awaitTimeout)
            attempted.map { results =>
                val (rights, lefts) = results.partition(_.isRight)
                assert(rights.flatMap(_.toOption).map(_.slot) == List(1L, 2L, 3L))
                assert(lefts.size == 1)
                assert(lefts.head.swap.toOption.exists(_.getMessage == "boom"))
            }
        }
    }

    test("Fail overflow policy: producer offer future fails after capacity exceeded") {
        withDispatcher {
            val provider = new TestProvider
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              bufferPolicy = BufferPolicy.Bounded(2, Overflow.Fail)
            )
            val q = scalus.cardano.node.stream.UtxoEventQuery(
              scalus.cardano.node.UtxoQuery(
                scalus.cardano.node.UtxoSource.FromInputs(Set.empty)
              )
            )
            // Allocate the subscription stream but don't consume it,
            // so the buffer fills. The producer's third offer fails;
            // cross-tier overflow signalling to the consumer stream is
            // best-effort and not asserted here.
            val _stream = provider.subscribeUtxoQuery(q, opts)
            IO.fromFuture(IO {
                for {
                    _ <- provider.pushUtxo(testEvent(1L))
                    _ <- provider.pushUtxo(testEvent(2L))
                    failed <- provider.pushUtxo(testEvent(3L)).failed
                } yield assert(failed.isInstanceOf[ScalusBufferOverflowException])
            })
        }
    }

    private def testEvent(slot: Long): scalus.cardano.node.stream.UtxoEvent.RolledBack =
        // RolledBack carries no UTxO data — perfect lightweight fixture.
        scalus.cardano.node.stream.UtxoEvent.RolledBack(pt(slot))
}
