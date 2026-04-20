package scalus.cardano.network.it

import ox.*
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.NetworkMagic
import scalus.cardano.node.stream.ox.OxBlockchainStreamProvider
import scalus.cardano.node.stream.{BackupSource, ChainSyncSource, StreamProviderConfig}
import scalus.testing.yaci.YaciDevKit

import scala.concurrent.ExecutionContext

/** Ox counterpart to [[YaciN2nChainSyncSuite]] — same end-to-end sanity check, different
  * stream library. Direct-style ox `Flow` consumed in the caller's `supervised` scope.
  */
class YaciN2nChainSyncOxSuite
    extends AnyFunSuite
    with YaciDevKit
    with ScalaFutures
    with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    test("ox provider: tip advances via ChainSyncSource.N2N against yaci-devkit") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        val config = StreamProviderConfig(
          cardanoInfo = CardanoInfo.preview,
          chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
          backup = BackupSource.NoBackup
        )

        supervised {
            val provider = OxBlockchainStreamProvider.create(config)
            try {
                // Take the first 2 tips with a 20s deadline. `runToList` pulls from the mailbox
                // sequentially; ox's virtual thread blocks cheaply while we wait for blocks.
                //
                // If the applier fails (decode error, NoIntersection, unsupported era, …) the
                // tip stream fails with the typed cause via `engine.failAllSubscribers(cause)`,
                // and `runToList` rethrows it — the test surfaces the real cause rather than a
                // misleading "got 0 tips" message.
                val tips =
                    try
                        provider
                            .subscribeTip()
                            .take(2)
                            .runToList()
                    catch {
                        case t: Throwable =>
                            fail(
                              s"tip stream failed before 2 tips arrived: ${t.getClass.getSimpleName}: ${t.getMessage}",
                              t
                            )
                    }

                assert(tips.size == 2, s"expected 2 tips in 20s window, got ${tips.size}")
                assert(tips.last.point.slot > 0L, "last tip should have a concrete slot")
                assert(
                  tips.last.point.slot >= tips.head.point.slot,
                  s"slot should advance or stay equal; first=${tips.head.point.slot}, last=${tips.last.point.slot}"
                )
            } finally provider.close()
        }
    }
}
