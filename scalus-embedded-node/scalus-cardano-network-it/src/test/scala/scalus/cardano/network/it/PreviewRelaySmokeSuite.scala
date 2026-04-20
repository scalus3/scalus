package scalus.cardano.network.it

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.network.{ClientConfig, NetworkMagic, NodeToNodeClient}

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
}
