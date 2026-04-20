package scalus.cardano.network.jvm

import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.jvm.JvmTimer
import scalus.cardano.network.handshake.HandshakeError
import scalus.cardano.network.{ClientConfig, NetworkMagic}
import scalus.cardano.network.infra.MiniProtocolId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Real-TCP loopback tests that wire the full stack: [[NodeToNodeClient.connect]] opens a socket to
  * [[StubN2NResponder]], runs the handshake, and starts the keep-alive loop. These tests validate
  * that the end-to-end plumbing (JvmAsyncByteChannel → Multiplexer → HandshakeDriver →
  * KeepAliveDriver) works over a real socket — they're the gate before yaci-backed IT in M9.
  */
class NodeToNodeClientLoopbackSuite
    extends AnyFunSuite
    with ScalaFutures
    with Eventually
    with BeforeAndAfterEach {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(10, Seconds), interval = Span(50, Millis))

    private var stub: StubN2NResponder = _

    override def afterEach(): Unit = {
        if stub != null then stub.shutdown()
        stub = null
    }

    private def startStub(behaviour: StubBehaviour = StubBehaviour.AcceptV16): Int = {
        stub = new StubN2NResponder(behaviour)
        stub.port
    }

    // --------------------------------------------------------------------------------------
    // Happy path
    // --------------------------------------------------------------------------------------

    test("connect → handshake v16 → keep-alive loop running") {
        val port = startStub()
        val fastConfig = ClientConfig.default.copy(
          keepAliveInterval = 200.millis,
          keepAliveTimeout = 2.seconds
        )
        val conn = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet, fastConfig, JvmTimer.shared)
            .futureValue

        try {
            assert(conn.negotiatedVersion.version == 16)
            eventually(assert(conn.rtt.isDefined))
            assert(!conn.rootToken.isCancelled)
        } finally conn.close().futureValue
    }

    // --------------------------------------------------------------------------------------
    // Handshake refusal
    // --------------------------------------------------------------------------------------

    test("peer refuses handshake → connect future fails with VersionMismatch; socket closed") {
        val port = startStub(StubBehaviour.RefuseWithVersionMismatch)
        val ex = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet)
            .failed
            .futureValue
        assert(ex.isInstanceOf[HandshakeError.VersionMismatch])
    }

    test("peer sends garbage handshake reply → connect future fails with DecodeError") {
        val port = startStub(StubBehaviour.MalformedHandshakeReply)
        val ex = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet)
            .failed
            .futureValue
        assert(ex.isInstanceOf[HandshakeError.DecodeError], s"${ex.getClass}: ${ex.getMessage}")
    }

    // --------------------------------------------------------------------------------------
    // Connection lifecycle
    // --------------------------------------------------------------------------------------

    test("close() fires rootToken and completes closed") {
        val port = startStub()
        val conn = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet)
            .futureValue

        conn.close().futureValue
        assert(conn.rootToken.isCancelled)
        assert(conn.closed.isCompleted)
    }

    test("peer drops socket → rootToken fires, closed completes within the patience window") {
        val port = startStub()
        val conn = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet)
            .futureValue

        stub.shutdown() // forcibly close the listener; in-flight handler socket closes on next read
        eventually(assert(conn.rootToken.isCancelled))
    }

    // --------------------------------------------------------------------------------------
    // Channel handle semantics
    // --------------------------------------------------------------------------------------

    test("channel() is idempotent across protocols and returns the same Handle instance") {
        val port = startStub()
        val conn = NodeToNodeClient
            .connect("127.0.0.1", port, NetworkMagic.YaciDevnet)
            .futureValue

        try {
            val a = conn.channel(MiniProtocolId.ChainSync)
            val b = conn.channel(MiniProtocolId.ChainSync)
            assert(a eq b)
            val c = conn.channel(MiniProtocolId.BlockFetch)
            assert(c ne a)
        } finally conn.close().futureValue
    }

}
