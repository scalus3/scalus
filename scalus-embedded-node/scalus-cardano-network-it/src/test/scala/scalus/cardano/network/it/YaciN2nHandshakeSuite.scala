package scalus.cardano.network.it

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.network.{NetworkMagic, NodeToNodeClient}
import scalus.testing.yaci.YaciDevKit

import scala.concurrent.ExecutionContext.Implicits.global

/** Yaci-devkit-backed handshake IT. Starts the yaci container, connects to its Cardano-node N2N
  * port, performs the handshake, and verifies the negotiated version.
  *
  * Runs under `sbt it`. Requires Docker.
  */
class YaciN2nHandshakeSuite extends AnyFunSuite with YaciDevKit with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(60, Seconds), interval = Span(200, Millis))

    test("handshake against yaci-devkit negotiates v14 or v16") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        val conn = NodeToNodeClient
            .connect(host, port, networkMagic = NetworkMagic.YaciDevnet)
            .futureValue

        try {
            val version = conn.negotiatedVersion.version
            assert(
              version == 14 || version == 15 || version == 16,
              s"expected a supported version, got $version"
            )
            assert(!conn.rootToken.isCancelled)
        } finally conn.close().futureValue
    }
}
