package scalus.cardano.network

import org.scalatest.RecoverMethods.recoverToExceptionIf
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken, CancelledException}
import scalus.cardano.node.stream.DeltaBufferPolicy
import scalus.uplc.builtin.ByteString

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Loopback tests for [[Multiplexer]] driven through [[PipeAsyncByteChannel]]. The peer side is
  * modelled by writing raw SDU bytes onto the pipe — directly, not through a second mux — so we can
  * precisely control direction, protocol, and frame shape for every test case.
  */
class MultiplexerSuite extends AnyFunSuite with ScalaFutures with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(3, Seconds), interval = Span(10, Millis))

    private def bytes(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))

    private def muxOnPipe(
        config: MuxConfig = MuxConfig()
    ): (Multiplexer, AsyncByteChannel, CancelSource) = {
        val (ourEnd, peerEnd) = PipeAsyncByteChannel.pair()
        val root = CancelSource()
        val mux = new Multiplexer(ourEnd, root, config)
        (mux, peerEnd, root)
    }

    /** Write a raw Responder-direction frame onto the peer end of the pipe — arrives at our mux as
      * an inbound SDU.
      */
    private def peerSend(
        peerEnd: AsyncByteChannel,
        proto: MiniProtocolId,
        payload: ByteString,
        dir: Direction = Direction.Responder,
        timestamp: Int = 0
    ): Unit = {
        val header = Sdu.encodeHeader(timestamp, proto, dir, payload.bytes.length)
        val frame = new Array[Byte](Sdu.HeaderSize + payload.bytes.length)
        System.arraycopy(header, 0, frame, 0, Sdu.HeaderSize)
        System.arraycopy(payload.bytes, 0, frame, Sdu.HeaderSize, payload.bytes.length)
        Await.result(peerEnd.write(ByteString.unsafeFromArray(frame), CancelToken.never), 1.second)
    }

    test("channel() is idempotent — same handle instance for repeated calls on a protocol") {
        val (mux, _, _) = muxOnPipe()
        val a = mux.channel(MiniProtocolId.Handshake)
        val b = mux.channel(MiniProtocolId.Handshake)
        assert(a eq b)
    }

    test("inbound frame is delivered to the matching protocol's receive()") {
        val (mux, peer, _) = muxOnPipe()
        val hs = mux.channel(MiniProtocolId.Handshake)
        peerSend(peer, MiniProtocolId.Handshake, bytes("hello"))
        val got = hs.receive().futureValue
        assert(got.exists(_.bytes sameElements bytes("hello").bytes))
    }

    test("send() splits a large payload across SDUs; peer reads them reassembled") {
        val (mux, peer, _) = muxOnPipe(MuxConfig(sduMaxPayload = 100))
        val hs = mux.channel(MiniProtocolId.Handshake)
        val payload = Array.tabulate[Byte](350)(i => (i & 0xff).toByte)
        hs.send(ByteString.unsafeFromArray(payload)).futureValue

        val collected = new scala.collection.mutable.ArrayBuffer[Byte]
        for _ <- 0 until 4 do {
            val header = peer.readExactly(Sdu.HeaderSize, CancelToken.never).futureValue.get
            val parsed = Sdu.parseHeader(header)
            assert(parsed.protocol.contains(MiniProtocolId.Handshake))
            assert(parsed.direction == Direction.Initiator)
            assert(parsed.length <= 100)
            val sduPayload = peer.readExactly(parsed.length, CancelToken.never).futureValue.get
            collected ++= sduPayload.bytes
        }
        assert(collected.toArray sameElements payload)
    }

    test("unknown protocol wire on inbound fires root with UnexpectedFrameException") {
        val (mux, peer, root) = muxOnPipe()
        val _ = mux.channel(MiniProtocolId.Handshake)

        // Inject a Responder-dir frame with protocol wire=42 (unassigned).
        val header = Sdu.encodeHeader(0, protocolWire = 42, Direction.Responder, 0)
        Await.result(peer.write(ByteString.unsafeFromArray(header), CancelToken.never), 1.second)

        eventually { assert(root.token.isCancelled) }
        assert(root.token.cause.exists(_.isInstanceOf[UnexpectedFrameException]))
    }

    test("inbound SDU for a protocol with no active route fires root") {
        val (_, peer, root) = muxOnPipe()
        // No `channel(Handshake)` call — no active route.
        peerSend(peer, MiniProtocolId.Handshake, bytes("surprise"))
        eventually { assert(root.token.isCancelled) }
        assert(root.token.cause.exists(_.isInstanceOf[UnexpectedFrameException]))
    }

    test("root cancel fails pending receive on every active protocol handle") {
        val (mux, _, root) = muxOnPipe()
        val hs = mux.channel(MiniProtocolId.Handshake)
        val ka = mux.channel(MiniProtocolId.KeepAlive)

        val pendingHs = hs.receive()
        val pendingKa = ka.receive()
        root.cancel(new RuntimeException("user close"))

        val hsEx = recoverToExceptionIf[RuntimeException](pendingHs).futureValue
        assert(hsEx.getMessage == "user close")
        val kaEx = recoverToExceptionIf[RuntimeException](pendingKa).futureValue
        assert(kaEx.getMessage == "user close")
    }

    test("route isolation: finishClose on one protocol leaves root + siblings alive") {
        val (mux, peer, root) = muxOnPipe()
        val hs = mux.channel(MiniProtocolId.Handshake)
        val ka = mux.channel(MiniProtocolId.KeepAlive)

        mux.finishClose(MiniProtocolId.Handshake, Direction.Responder)

        // Handshake route is closed: receive fails at the entry guard.
        val hsEx = recoverToExceptionIf[CancelledException](hs.receive()).futureValue
        assert(hsEx ne null)
        assert(!root.token.isCancelled)

        // KeepAlive is untouched — a fresh frame reaches its consumer.
        peerSend(peer, MiniProtocolId.KeepAlive, bytes("alive"))
        val got = ka.receive().futureValue
        assert(got.exists(_.bytes sameElements bytes("alive").bytes))
    }

    test("bounded mailbox overflow surfaces on the state machine's next pull") {
        val (mux, peer, root) = muxOnPipe(MuxConfig(mailboxCapacity = DeltaBufferPolicy.Bounded(3)))
        val hs = mux.channel(MiniProtocolId.Handshake)

        // Flood far more than capacity. No puller active yet; offers fill the buffer and the
        // next offer after capacity triggers the overflow terminal.
        for i <- 0 until 50 do
            peerSend(peer, MiniProtocolId.Handshake, ByteString.fromArray(Array[Byte](i.toByte)))

        // Drain until the overflow arrives via pull. The mailbox is pure transport; the
        // state machine (pull consumer) sees the failure and decides how to escalate.
        var sawOverflow = false
        var pulls = 0
        while !sawOverflow && pulls < 80 do {
            try Await.result(hs.receive(), 2.seconds)
            catch {
                case _: scalus.cardano.infra.ScalusBufferOverflowException => sawOverflow = true
            }
            pulls += 1
        }
        assert(sawOverflow, s"expected overflow within first 80 pulls, none seen")
        // Root is NOT cancelled by the mux — state machines own escalation.
        assert(!root.token.isCancelled)
    }

    test(
      "Draining consumes frames off the wire and discards them; finishClose transitions to Closed"
    ) {
        val (mux, peer, root) = muxOnPipe()
        val hs = mux.channel(MiniProtocolId.Handshake)
        val ka = mux.channel(MiniProtocolId.KeepAlive) // marker route — proves reader caught up

        mux.beginDraining(MiniProtocolId.Handshake, Direction.Responder)

        for _ <- 0 until 5 do peerSend(peer, MiniProtocolId.Handshake, bytes("ignored"))
        // Marker on a different protocol: when we receive it, FIFO guarantees all 5 Handshake
        // frames ahead of it have already been routed (and silently discarded due to Draining).
        peerSend(peer, MiniProtocolId.KeepAlive, bytes("barrier"))

        val marker = ka.receive().futureValue
        assert(marker.exists(_.bytes sameElements bytes("barrier").bytes))
        assert(!root.token.isCancelled, "draining frames must not fire root")

        mux.finishClose(MiniProtocolId.Handshake, Direction.Responder)
        // Route is now Closed — `hs.receive()` fails at the entry guard.
        val _ = recoverToExceptionIf[CancelledException](hs.receive()).futureValue

        // Next frame on Closed route fires root.
        peerSend(peer, MiniProtocolId.Handshake, bytes("late"))
        eventually { assert(root.token.isCancelled) }
        assert(root.token.cause.exists(_.isInstanceOf[UnexpectedFrameException]))
    }

    test("send fails fast after root has been cancelled") {
        val (mux, _, root) = muxOnPipe()
        val hs = mux.channel(MiniProtocolId.Handshake)
        root.cancel(new RuntimeException("gone"))

        val _ = recoverToExceptionIf[AsyncByteChannel.ChannelClosedException](
          hs.send(bytes("too late"))
        ).futureValue
    }
}
