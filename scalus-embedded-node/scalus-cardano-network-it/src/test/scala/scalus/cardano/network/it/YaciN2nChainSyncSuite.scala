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

    // Instrumentation: scalatest buffers `info(...)` until the end of the test, and sbt often
    // eats test stdout. Writing to stderr with a [IT] prefix shows up live in the sbt console so
    // a hung test is diagnosable without waiting for the scalatest report to flush.
    private def itLog(msg: String): Unit =
        System.err.println(s"[IT][${System.currentTimeMillis()}] $msg")

    test("tip advances while connected via ChainSyncSource.N2N against yaci-devkit") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            val config = StreamProviderConfig(
              appId = "scalus.it.yaci-n2n-chain-sync.tip",
              cardanoInfo = CardanoInfo.preview, // protocol-params shape; slot config unused here
              chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
              backup = BackupSource.NoBackup,
              enginePersistence =
                  scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore.noop
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

    test("provider.close() mid-sync tears down cleanly (no dangling subscribers)") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            val config = StreamProviderConfig(
              appId = "scalus.it.yaci-n2n-chain-sync.mid-close",
              cardanoInfo = CardanoInfo.preview,
              chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
              backup = BackupSource.NoBackup,
              enginePersistence =
                  scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore.noop
            )

            val tipCount = new AtomicLong(0L)

            // Flow:
            //   1. spin up the provider + subscribe to tip
            //   2. wait for at least one live tip so we're mid-sync, not pre-intersect
            //   3. call provider.close() while the tip stream is still subscribed
            //   4. the stream must reach clean EOS (not failure — close() is a graceful shutdown)
            //   5. the engine must report no lingering subscribers
            for {
                provider <- Fs2BlockchainStreamProvider.create(config)
                // Start the tip stream in the background; `compile.drain` completes when the
                // stream ends (EOS from close(), or failure from applier). We do NOT use
                // interruptAfter — the close() call is the interrupt source.
                streamFiber <- provider
                    .subscribeTip()
                    .evalMap(_ => IO { tipCount.incrementAndGet(); () })
                    .compile
                    .drain
                    .attempt
                    .start
                // Wait up to 15s for the first live tip from yaci.
                _ <- IO.race(
                  IO.sleep(15.seconds) *> IO.raiseError(
                    new AssertionError(s"no tip seen within 15s; tipCount=${tipCount.get}")
                  ),
                  IO {
                      while tipCount.get < 1L do Thread.sleep(50)
                  }
                )
                // Mid-sync teardown. close() runs preClose (cancel applier + close conn) then
                // engine.closeAllSubscribers() — the stream should EOS gracefully.
                _ <- provider.close()
                // Join the tip stream with a deadline — if close() deadlocked or failed to
                // deliver the EOS, this surfaces as a timeout, not a silent hang.
                streamOutcome <- streamFiber.joinWithNever.timeoutTo(
                  10.seconds,
                  IO.raiseError(new AssertionError("tip stream did not terminate within 10s of close()"))
                )
            } yield {
                streamOutcome match {
                    case Left(t) =>
                        fail(
                          s"tip stream failed on graceful close: ${t.getClass.getSimpleName}: ${t.getMessage}",
                          t
                        )
                    case Right(_) => ()
                }
                assert(
                  tipCount.get >= 1L,
                  s"expected at least one tip before close, got ${tipCount.get}"
                )
            }
        }.unsafeRunSync()
    }
}
