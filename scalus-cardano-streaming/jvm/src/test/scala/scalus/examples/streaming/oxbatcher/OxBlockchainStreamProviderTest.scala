package scalus.examples.streaming.oxbatcher

import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import ox.*
import ox.flow.Flow
import scalus.cardano.ledger.{BlockHash, CardanoInfo}
import scalus.cardano.node.stream.{ChainPoint, SyntheticEventSource}

import OxScalusAsyncStream.Id

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** Round-trip tests for [[OxBlockchainStreamProvider]] driven by
  * [[SyntheticEventSource]].
  *
  * Each test runs inside a `supervised` scope. A user fork pushes
  * events through the synthetic source while the main thread consumes
  * the resulting `Flow`. `closeAllSubs` terminates the underlying
  * channel, which in turn ends the flow cleanly.
  */
class OxBlockchainStreamProviderTest extends AnyFunSuite with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(20, Millis))

    private given ExecutionContext = ExecutionContext.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val zeroBlockHash: BlockHash = BlockHash.fromHex("0" * 64)
    private def pt(slot: Long): ChainPoint = ChainPoint(slot, zeroBlockHash)
    private val awaitTimeout: FiniteDuration = 5.seconds

    private final class TestProvider
        extends OxBlockchainStreamProvider(ci)
        with SyntheticEventSource[Id, Flow]

    test("subscribeTip + push N points: order preserved, flow completes on closeAllSubs") {
        supervised {
            val provider = new TestProvider
            val flow = provider.subscribeTip()
            val pusher = forkUser {
                (1 to 10).foreach { i =>
                    Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout)
                }
                Await.result(provider.closeAllSubs(), awaitTimeout)
            }
            val collected = flow.runToList()
            pusher.join()
            assert(collected.map(_.slot) == (1L to 10L).toList)
        }
    }

    test("cancellation: completing the flow consumer unregisters the sink") {
        supervised {
            val provider = new TestProvider
            val flow = provider.subscribeTip()
            // The pusher feeds 5 events, then waits for the consumer
            // to complete its take(5) and free the subscription.
            val pusher = forkUser {
                (1 to 5).foreach { i =>
                    Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout)
                }
            }
            val collected = flow.take(5).runToList()
            pusher.join()
            assert(collected.map(_.slot) == (1L to 5L).toList)
            eventually {
                assert(provider.activeSubscriptionCount == 0)
            }
        }
    }

    test("error propagation: failAllSubs terminates the flow with the supplied error") {
        supervised {
            val provider = new TestProvider
            val flow = provider.subscribeTip()
            val pusher = forkUser {
                (1 to 3).foreach { i =>
                    Await.result(provider.pushTip(pt(i.toLong)), awaitTimeout)
                }
                Await.result(provider.failAllSubs(new RuntimeException("boom")), awaitTimeout)
            }
            val attempt =
                try {
                    val xs = flow.runToList()
                    Right(xs)
                } catch {
                    case t: Throwable => Left(t)
                }
            pusher.join()
            attempt match {
                case Left(t) =>
                    assert(t.getMessage == "boom" || Option(t.getCause).exists(_.getMessage == "boom"))
                case Right(xs) =>
                    fail(s"expected the flow to fail; got $xs")
            }
        }
    }
}
