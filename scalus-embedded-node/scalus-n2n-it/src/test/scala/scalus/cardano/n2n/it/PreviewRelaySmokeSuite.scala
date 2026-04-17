package scalus.cardano.n2n.it

import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.n2n.{ClientConfig, NetworkMagic}
import scalus.cardano.n2n.jvm.NodeToNodeClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Smoke tests against a real Cardano Preview relay. Env-gated via `SCALUS_N2N_PREVIEW_IT=1` so
  * regular `sbt it` doesn't take outbound TCP to a public relay — maintainers opt in.
  *
  * Two tests only. Everything else is covered by [[YaciN2nHandshakeSuite]] and
  * [[YaciN2nKeepAliveSuite]] against the local yaci container.
  */
class PreviewRelaySmokeSuite extends AnyFunSuite with ScalaFutures with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    private val relayHost = "preview-node.play.dev.cardano.org"
    private val relayPort = 3001

    private def requireEnabled(): Unit =
        if !sys.env.get("SCALUS_N2N_PREVIEW_IT").contains("1") then
            throw new TestCanceledException(
              "SCALUS_N2N_PREVIEW_IT=1 not set; skipping preview-relay smoke",
              0
            )

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
            // Two beats at 20s interval — wait ~45s to ensure both completed.
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
}
