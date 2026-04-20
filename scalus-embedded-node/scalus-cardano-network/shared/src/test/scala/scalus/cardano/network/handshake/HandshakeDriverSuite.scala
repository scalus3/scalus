package scalus.cardano.network.handshake

import io.bullet.borer.Cbor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken, FakeTimer}
import scalus.cardano.network.handshake.HandshakeMessage.*
import scalus.cardano.network.NetworkMagic
import scalus.cardano.network.infra.MiniProtocolBytes
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

/** Drives the [[HandshakeDriver]] state machine against a scripted peer backed by a simple
  * in-memory byte-stream. The peer's reply is staged before or after the driver's `send`, and
  * timing is deterministic via [[FakeTimer]].
  */
class HandshakeDriverSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    // --------------------------------------------------------------------------------------
    // Test harness — a MiniProtocolBytes backed by a queue of CBOR-encoded replies.
    // --------------------------------------------------------------------------------------

    /** Test stub that honours the per-call `cancel` token — mirrors the production
      * `Multiplexer.Handle`'s promise/listener wiring for mid-wait cancellation. No constructor
      * scope: callers pass their cancel token each call.
      */
    private final class ScriptedBytes extends MiniProtocolBytes {
        private val lock = new AnyRef
        private val inbound = mutable.ArrayDeque.empty[Option[ByteString]]
        private var pending: Option[Promise[Option[ByteString]]] = None
        val sentOutbound = mutable.ArrayBuffer.empty[ByteString]

        def receive(cancel: CancelToken = CancelToken.never): Future[Option[ByteString]] =
            lock.synchronized {
                if cancel.isCancelled then
                    Future.failed(cancel.cause.getOrElse(new RuntimeException("cancelled")))
                else if inbound.nonEmpty then Future.successful(inbound.removeHead())
                else {
                    val p = Promise[Option[ByteString]]()
                    pending = Some(p)
                    cancel.onCancel { () =>
                        lock.synchronized {
                            if pending.exists(_ eq p) then {
                                pending = None
                                val _ = p.tryFailure(
                                  cancel.cause.getOrElse(new RuntimeException("cancelled"))
                                )
                            }
                        }
                    }
                    p.future
                }
            }

        def send(message: ByteString, cancel: CancelToken = CancelToken.never): Future[Unit] = {
            sentOutbound += message
            Future.unit
        }

        def stage(reply: HandshakeMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(reply).toByteArray)))

        def stageRawBytes(bs: ByteString): Unit = offer(Some(bs))

        def stageEof(): Unit = offer(None)

        private def offer(value: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    val _ = p.trySuccess(value)
                case None => inbound.append(value)
            }
        }
    }

    /** Standard fixture: fresh FakeTimer + CancelScope + ScriptedBytes peer whose cancelScope is
      * the scope's token. Tests that need finer control over the timer build the trio inline.
      */
    private def newFixture(): (ScriptedBytes, CancelSource, FakeTimer) = {
        val timer = new FakeTimer()
        val scope = CancelSource()
        (new ScriptedBytes, scope, timer)
    }

    private val magic = NetworkMagic.Preview

    // --------------------------------------------------------------------------------------
    // Happy path
    // --------------------------------------------------------------------------------------

    test("driver sends MsgProposeVersions with v14 + v16 and returns on MsgAcceptVersion") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)

        // Peer accepts at v16.
        val acceptedData = NodeToNodeVersionData.V16(
          networkMagic = magic,
          initiatorOnlyDiffusionMode = false,
          peerSharing = 0,
          query = false,
          perasSupport = false
        )
        peer.stage(MsgAcceptVersion(16, acceptedData))

        val negotiated = f.futureValue
        assert(negotiated == NegotiatedVersion(16, acceptedData))

        // And the driver must have sent a well-formed MsgProposeVersions.
        assert(peer.sentOutbound.size == 1)
        val proposal =
            Cbor.decode(peer.sentOutbound.head.bytes).to[HandshakeMessage].value
        proposal match {
            case MsgProposeVersions(table) =>
                assert(table.keySet == Set(14, 16))
                assert(
                  table(14).asInstanceOf[NodeToNodeVersionData.V14].networkMagic.value ==
                      magic.value
                )
                assert(
                  table(16).asInstanceOf[NodeToNodeVersionData.V16].perasSupport == false
                )
            case other => fail(s"expected MsgProposeVersions, got $other")
        }
    }

    // --------------------------------------------------------------------------------------
    // Peer-refused paths
    // --------------------------------------------------------------------------------------

    test("MsgRefuse/VersionMismatch surfaces as HandshakeError.VersionMismatch") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        peer.stage(MsgRefuse(RefuseReason.VersionMismatch(List(11, 12, 13))))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.VersionMismatch])
        val vm = ex.asInstanceOf[HandshakeError.VersionMismatch]
        assert(vm.proposed == Set(14, 16))
        assert(vm.peerSupported == List(11, 12, 13))
    }

    test("MsgRefuse/Refused surfaces as HandshakeError.Refused") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        peer.stage(MsgRefuse(RefuseReason.Refused(16, "magic mismatch")))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.Refused])
        val r = ex.asInstanceOf[HandshakeError.Refused]
        assert(r.versionTried == 16)
        assert(r.reason == "magic mismatch")
    }

    test("MsgRefuse/HandshakeDecodeError maps to Refused with 'peer decode error' prefix") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        peer.stage(MsgRefuse(RefuseReason.HandshakeDecodeError(14, "bad cbor")))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.Refused])
        val r = ex.asInstanceOf[HandshakeError.Refused]
        assert(r.versionTried == 14)
        assert(r.reason.contains("bad cbor"))
    }

    // --------------------------------------------------------------------------------------
    // Protocol violations
    // --------------------------------------------------------------------------------------

    test("MsgQueryReply in initiator flow surfaces UnexpectedMessage with the payload") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        val reply = MsgQueryReply(
          VersionTable(VersionNumber.V14 -> NodeToNodeVersionData.V14(magic, true, 0, false))
        )
        peer.stage(reply)

        val ex = f.failed.futureValue.asInstanceOf[HandshakeError.UnexpectedMessage]
        assert(ex.received == reply)
    }

    test("MsgProposeVersions back from peer surfaces UnexpectedMessage with the payload") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        val reply = MsgProposeVersions(
          VersionTable(VersionNumber.V16 -> NodeToNodeVersionData.V16(magic, true, 0, false, false))
        )
        peer.stage(reply)

        val ex = f.failed.futureValue.asInstanceOf[HandshakeError.UnexpectedMessage]
        assert(ex.received == reply)
    }

    test("EOF before reply surfaces as DecodeError") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        peer.stageEof()

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.DecodeError])
    }

    test("garbage bytes surface as DecodeError") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)
        // 0xFF alone isn't decodable as a top-level item; pad to two bytes to push past
        // insufficient-input.
        peer.stageRawBytes(ByteString.unsafeFromArray(Array[Byte](0xff.toByte, 0x00.toByte)))

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.DecodeError], s"got ${ex.getClass}: ${ex.getMessage}")
    }

    // --------------------------------------------------------------------------------------
    // Timeout path — driven deterministically by FakeTimer
    // --------------------------------------------------------------------------------------

    test(
      "timeout fires HandshakeError.Timeout and marks cancelScope with that cause"
    ) {
        val timer = new FakeTimer()
        val scope = CancelSource()
        val peer = new ScriptedBytes
        val f = HandshakeDriver.run(peer, magic, scope, timer, timeout = 30.seconds)

        // No reply staged — driver is awaiting. Advance past deadline.
        timer.advance(29.seconds)
        assert(!f.isCompleted, "driver must still be waiting before deadline")
        timer.advance(2.seconds)

        val ex = f.failed.futureValue
        assert(ex.isInstanceOf[HandshakeError.Timeout])
        // The handshake scope is cancelled with the timeout exception as cause.
        // A caller composing this driver into a bigger connection scope decides whether
        // to escalate further; isolation from the parent is the caller's concern, not
        // the driver's.
        assert(scope.token.isCancelled)
        assert(scope.token.cause.exists(_.isInstanceOf[HandshakeError.Timeout]))
    }

    test("early success cancels the scheduled timeout (no stray pending entries)") {
        val timer = new FakeTimer()
        val scope = CancelSource()
        val peer = new ScriptedBytes
        val f = HandshakeDriver.run(peer, magic, scope, timer)

        peer.stage(
          MsgAcceptVersion(
            16,
            NodeToNodeVersionData.V16(magic, true, 0, false, false)
          )
        )
        val _ = f.futureValue
        assert(timer.pendingCount == 0, "timeout scheduler should be drained on success")
    }

    test("outer scope cancel before reply propagates the outer cause") {
        val (peer, scope, timer) = newFixture()
        val f = HandshakeDriver.run(peer, magic, scope, timer)

        val outerCause = new RuntimeException("outer cancel")
        scope.cancel(outerCause)
        val ex = f.failed.futureValue
        // The outer cancel flows through the linked deadline token and back up as the
        // stored cause — the driver does not classify upstream cancels as timeouts.
        assert(ex eq outerCause, s"expected outer cause, got $ex")
    }
}
