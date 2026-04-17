package scalus.cardano.n2n.it

import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.n2n.{ClientConfig, NetworkMagic}
import scalus.cardano.n2n.jvm.NodeToNodeClient
import scalus.testing.yaci.YaciDevKit

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Yaci-devkit-backed keep-alive IT. Connects to the container, runs the keep-alive loop with a
  * tight interval for a few seconds, and verifies that multiple beats complete and RTT is
  * populated with a reasonable value.
  */
class YaciN2nKeepAliveSuite extends AnyFunSuite with YaciDevKit with ScalaFutures with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(60, Seconds), interval = Span(200, Millis))

    test("keep-alive completes multiple beats against yaci; RTT populated and bounded") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        // Tight interval so we observe several beats within a few seconds, rather than the
        // default 30s cadence which would push the test into a multi-minute run.
        val fastConfig = ClientConfig.default.copy(
          keepAliveInterval = 500.millis,
          keepAliveTimeout = 5.seconds
        )

        val conn = NodeToNodeClient
            .connect(host, port, NetworkMagic.YaciDevnet, fastConfig)
            .futureValue

        try {
            // First beat: RTT becomes defined.
            eventually(assert(conn.rtt.isDefined))
            val firstRtt = conn.rtt.get
            // Loopback-Docker RTT is typically sub-10ms but allow generous headroom for CI.
            assert(firstRtt < 2.seconds, s"implausibly large first RTT: $firstRtt")

            // Wait for at least three beat intervals to confirm the loop is sustained.
            val deadline = System.nanoTime() + 5.seconds.toNanos
            while System.nanoTime() < deadline && !conn.rootToken.isCancelled do
                Thread.sleep(100)

            assert(
              !conn.rootToken.isCancelled,
              s"connection died during keep-alive: cause=${conn.rootToken.cause}"
            )
        } finally conn.close().futureValue
    }

    test("client close() completes cleanly") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        val conn = NodeToNodeClient
            .connect(host, port, NetworkMagic.YaciDevnet)
            .futureValue

        conn.close().futureValue
        assert(conn.rootToken.isCancelled)
    }
}
