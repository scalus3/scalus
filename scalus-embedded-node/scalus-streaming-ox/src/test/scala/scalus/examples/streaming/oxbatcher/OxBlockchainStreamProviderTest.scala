package scalus.examples.streaming.oxbatcher

import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import ox.*
import ox.flow.Flow
import scalus.cardano.ledger.{BlockHash, CardanoInfo}
import scalus.cardano.node.stream.{ChainPoint, ChainTip, SyntheticEventSource}
import scalus.cardano.node.stream.ox.{OxBlockchainStreamProvider, OxScalusAsyncStream}

import OxScalusAsyncStream.Id

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** Round-trip tests for [[OxBlockchainStreamProvider]] driven by [[SyntheticEventSource]]. Mirrors
  * `Fs2BlockchainStreamProviderTest` — same semantics, ox idioms.
  */
class OxBlockchainStreamProviderTest extends AnyFunSuite with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(20, Millis))

    private given ExecutionContext = ExecutionContext.global

    private val ci: CardanoInfo = CardanoInfo.preview
    private val zeroBlockHash: BlockHash = BlockHash.fromHex("0" * 64)
    private def pt(slot: Long): ChainPoint = ChainPoint(slot, zeroBlockHash)
    private def chainTip(slot: Long): ChainTip = ChainTip(pt(slot), slot)
    private val awaitTimeout: FiniteDuration = 5.seconds

    private def newProvider: OxBlockchainStreamProvider & SyntheticEventSource[Id, Flow] =
        new OxBlockchainStreamProvider(
          new scalus.cardano.node.stream.engine.Engine(
            ci,
            None,
            scalus.cardano.node.stream.engine.Engine.DefaultSecurityParam
          )
        ) with SyntheticEventSource[Id, Flow]

    test("subscribeTip: latest-value coalesces, flow completes on closeAllSubs") {
        supervised {
            val provider = newProvider
            val flow = provider.subscribeTip()
            val pusher = forkUser {
                (1 to 10).foreach { i =>
                    Await.result(provider.pushTip(chainTip(i.toLong)), awaitTimeout)
                }
                Await.result(provider.closeAllSubs(), awaitTimeout)
            }
            val collected = flow.runToList()
            pusher.join()
            assert(collected.nonEmpty)
            assert(collected.last.point.slot == 10L)
        }
    }

    test("subscribeTip cancellation: take(1) shrinks the registry") {
        supervised {
            val provider = newProvider
            val flow = provider.subscribeTip()
            val pusher = forkUser {
                Await.result(provider.pushTip(chainTip(1L)), awaitTimeout)
            }
            val collected = flow.take(1).runToList()
            pusher.join()
            assert(collected.size == 1)
            eventually {
                assert(provider.activeSubscriptionCount == 0)
            }
        }
    }

    test("subscribeTip: failAllSubs terminates the flow with the supplied error") {
        supervised {
            val provider = newProvider
            val flow = provider.subscribeTip()
            val pusher = forkUser {
                Await.result(provider.pushTip(chainTip(1L)), awaitTimeout)
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
                    assert(
                      t.getMessage == "boom" || Option(t.getCause).exists(_.getMessage == "boom")
                    )
                case Right(xs) =>
                    fail(s"expected the flow to fail; got $xs")
            }
        }
    }
}
