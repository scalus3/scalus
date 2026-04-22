package scalus.cardano.network.it

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.{ClientConfig, NetworkMagic, NodeToNodeClient}
import scalus.cardano.node.stream.fs2.Fs2BlockchainStreamProvider
import scalus.cardano.node.stream.{BackupSource, ChainSyncSource, StreamProviderConfig}

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Smoke tests against a real Cardano Preview relay. Env-gated via `SCALUS_N2N_PREVIEW_IT=1` so
  * regular `sbt it` doesn't take outbound TCP to a public relay — maintainers opt in.
  *
  * Two tests only. Everything else is covered by [[YaciN2nHandshakeSuite]] and
  * [[YaciN2nKeepAliveSuite]] against the local yaci container.
  */
class PreviewRelaySmokeSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    private val relayHost = "preview-node.play.dev.cardano.org"
    private val relayPort = 3001

    private def requireEnabled(): Unit =
        assume(sys.env.get("SCALUS_N2N_PREVIEW_IT").contains("1"), "SCALUS_N2N_PREVIEW_IT=1 not set")

    test("handshake against preview relay negotiates a supported version") {
        requireEnabled()
        val conn = NodeToNodeClient
            .connect(relayHost, relayPort, NetworkMagic.Preview)
            .futureValue
        try {
            val v = conn.negotiatedVersion.version
            assert(v == 14 || v == 15 || v == 16, s"unexpected version $v")
        } finally conn.close().futureValue
    }

    test("stay-alive for 90 seconds with at least two keep-alive beats") {
        requireEnabled()
        val conn = NodeToNodeClient
            .connect(
              relayHost,
              relayPort,
              NetworkMagic.Preview,
              ClientConfig.default.copy(
                keepAliveInterval = 20.seconds,
                keepAliveTimeout = 60.seconds
              )
            )
            .futureValue

        try {
            // Two beats at 20s interval — wait ~45s to ensure both completed. Short sleeps +
            // rootToken check give early-exit if the connection dies mid-wait.
            val deadline = System.nanoTime() + 45.seconds.toNanos
            while System.nanoTime() < deadline && !conn.rootToken.isCancelled do
                Thread.sleep(500)

            assert(
              !conn.rootToken.isCancelled,
              s"connection died during stay-alive: cause=${conn.rootToken.cause}"
            )
            assert(conn.rtt.isDefined, "no RTT observed — keep-alive never completed a beat")
        } finally conn.close().futureValue
    }

    test("chain-sync: tip advances via Fs2BlockchainStreamProvider against preview relay") {
        requireEnabled()
        given ExecutionContext = ExecutionContext.global

        val config = StreamProviderConfig(
          appId = "scalus.it.preview-relay-smoke",
          cardanoInfo = CardanoInfo.preview,
          chainSync = ChainSyncSource.N2N(relayHost, relayPort, NetworkMagic.Preview.value),
          backup = BackupSource.NoBackup,
          enginePersistence =
              scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore.noop
        )

        // Preview blocks every ~20s on average. Four minutes is enough for a handful of tips
        // with generous headroom for relay jitter. Per the design doc Tier 5 spec we assert
        // monotonic advancement (each tip.slot >= previous) across ≥ 3 tips to catch a provider
        // that delivers a single stale tip and then stalls.
        val tipCount = new AtomicLong(0L)
        val firstSlot = new AtomicLong(-1L)
        val lastSlot = new AtomicLong(-1L)
        val monotonic = new AtomicReference[Boolean](true)

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d
            for {
                provider <- Fs2BlockchainStreamProvider.create(config)
                streamOutcome <- provider
                    .subscribeTip()
                    .evalMap { tip =>
                        IO {
                            val slot = tip.point.slot
                            val n = tipCount.incrementAndGet()
                            if n == 1L then firstSlot.set(slot)
                            val prev = lastSlot.getAndSet(slot)
                            if prev >= 0L && slot < prev then monotonic.set(false)
                        }
                    }
                    .interruptAfter(4.minutes)
                    .compile
                    .drain
                    .attempt
                _ <- provider.close()
            } yield {
                streamOutcome match {
                    case Left(t) =>
                        fail(
                          s"tip stream terminated with error: ${t.getClass.getSimpleName}: ${t.getMessage} " +
                              s"(tipCount=${tipCount.get}, firstSlot=${firstSlot.get}, lastSlot=${lastSlot.get})",
                          t
                        )
                    case Right(_) => ()
                }
                assert(
                  tipCount.get >= 3L,
                  s"expected ≥ 3 tips in 4 minutes, got ${tipCount.get} (firstSlot=${firstSlot.get}, lastSlot=${lastSlot.get})"
                )
                assert(firstSlot.get > 0L, s"first slot should be concrete, got ${firstSlot.get}")
                assert(
                  lastSlot.get > firstSlot.get,
                  s"slot should advance: firstSlot=${firstSlot.get}, lastSlot=${lastSlot.get}"
                )
                assert(
                  monotonic.get,
                  s"slot must be non-decreasing across tips; firstSlot=${firstSlot.get}, lastSlot=${lastSlot.get}"
                )
            }
        }.unsafeRunSync()
    }
}
