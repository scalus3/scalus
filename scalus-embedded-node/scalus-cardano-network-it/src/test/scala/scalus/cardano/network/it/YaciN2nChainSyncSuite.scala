package scalus.cardano.network.it

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.NetworkMagic
import scalus.cardano.node.stream.fs2.Fs2BlockchainStreamProvider
import scalus.cardano.node.stream.{BackupSource, ChainSyncSource, StreamProviderConfig}
import scalus.testing.yaci.YaciDevKit

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

/** End-to-end yaci-devkit chain-sync IT for the fs2 provider.
  *
  * Spins up yaci, connects via `Fs2BlockchainStreamProvider.create(ChainSyncSource.N2N(...))`,
  * subscribes to the tip stream, and verifies that the tip advances — i.e. the full
  * `NodeToNodeClient → Multiplexer → ChainSyncDriver → BlockFetchDriver → BlockEnvelope →
  * Engine` path is wired correctly against a real Cardano node.
  *
  * Runs under `sbt it`. Requires Docker.
  */
class YaciN2nChainSyncSuite
    extends AnyFunSuite
    with YaciDevKit
    with ScalaFutures
    with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    test("tip advances while connected via ChainSyncSource.N2N against yaci-devkit") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            val config = StreamProviderConfig(
              cardanoInfo = CardanoInfo.preview, // protocol-params shape; slot config unused here
              chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
              backup = BackupSource.NoBackup
            )

            // Capture tips into an atomic list so we can assert advancement after the stream
            // has run for a while. `subscribeTip` pulls from the engine's tip mailbox — first
            // tip typically arrives within ~1s once ChainApplier delivers its first block.
            val tipCount = new AtomicLong(0L)
            val lastSlot = new AtomicReference[Long](-1L)

            for {
                provider <- Fs2BlockchainStreamProvider.create(config)
                // Run the tip stream; on applier failure the stream fails with the typed
                // cause (thanks to `engine.failAllSubscribers(cause)` in the provider wiring),
                // so `handleErrorWith` reaches the original error instead of seeing a silent
                // EOS.
                streamOutcome <- provider
                    .subscribeTip()
                    .evalMap(tip =>
                        IO {
                            tipCount.incrementAndGet()
                            lastSlot.set(tip.point.slot)
                        }
                    )
                    .interruptAfter(20.seconds)
                    .compile
                    .drain
                    .attempt
                _ <- provider.close()
            } yield {
                // Surface any underlying cause loudly before asserting the count — so the
                // test output tells us WHY it failed rather than just "expected ≥ 2 tips".
                streamOutcome match {
                    case Left(t) =>
                        fail(
                          s"tip stream terminated with error before threshold: ${t.getClass.getSimpleName}: ${t.getMessage} " +
                              s"(tipCount=${tipCount.get}, lastSlot=${lastSlot.get})",
                          t
                        )
                    case Right(_) => ()
                }
                // Yaci ticks a slot per second by default; 20s should yield multiple tips.
                assert(
                  tipCount.get >= 2L,
                  s"expected ≥ 2 tip updates in 20s, got ${tipCount.get} (lastSlot=${lastSlot.get})"
                )
                assert(
                  lastSlot.get > 0L,
                  s"expected a concrete slot, got ${lastSlot.get}"
                )
            }
        }.unsafeRunSync()
    }
}
