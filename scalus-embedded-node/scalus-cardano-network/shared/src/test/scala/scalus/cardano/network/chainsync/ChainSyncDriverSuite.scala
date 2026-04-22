package scalus.cardano.network.chainsync

import io.bullet.borer.Cbor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken}
import scalus.cardano.ledger.BlockHash
import scalus.cardano.network.ChainSyncError
import scalus.cardano.network.infra.MiniProtocolBytes
import scalus.cardano.network.chainsync.ChainSyncMessage.*
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class ChainSyncDriverSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    // --------------------------------------------------------------------------------------
    // Harness — scripted MiniProtocolBytes modelled on the handshake suite's version.
    // --------------------------------------------------------------------------------------

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

        def stage(reply: ChainSyncMessage): Unit =
            offer(Some(ByteString.unsafeFromArray(Cbor.encode(reply).toByteArray)))

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

    private def sampleHash(seed: Int): BlockHash =
        BlockHash.fromByteString(ByteString.fromArray(Array.fill(32)(seed.toByte)))

    private def pt(slot: Long, hashSeed: Int): Point.BlockPoint =
        Point.BlockPoint(slot, sampleHash(hashSeed))

    private def tipAt(slot: Long, blockNo: Long, hashSeed: Int): Tip =
        Tip(pt(slot, hashSeed), blockNo)

    private def newDriver(): (ScriptedBytes, ChainSyncDriver) = {
        val peer = new ScriptedBytes
        val driver = new ChainSyncDriver(peer, CancelSource().token)
        (peer, driver)
    }

    private def decodeSent(peer: ScriptedBytes, idx: Int): ChainSyncMessage =
        Cbor.decode(peer.sentOutbound(idx).bytes).to[ChainSyncMessage].value

    // --------------------------------------------------------------------------------------
    // findIntersect
    // --------------------------------------------------------------------------------------

    test("findIntersect sends MsgFindIntersect and returns on MsgIntersectFound") {
        val (peer, driver) = newDriver()
        val candidate = pt(100L, 1)
        val peerTip = tipAt(200L, 20, 2)

        val f = driver.findIntersect(Seq(candidate))
        peer.stage(MsgIntersectFound(candidate, peerTip))

        val (p, t) = f.futureValue
        assert(p == candidate)
        assert(t == peerTip)
        assert(decodeSent(peer, 0) == MsgFindIntersect(List(candidate)))
    }

    test("findIntersect fails with NoIntersection on MsgIntersectNotFound") {
        val (peer, driver) = newDriver()
        val peerTip = tipAt(500L, 50, 5)

        val f = driver.findIntersect(Seq(pt(1L, 1)))
        peer.stage(MsgIntersectNotFound(peerTip))

        val err = f.failed.futureValue
        err match {
            case ChainSyncError.NoIntersection(tip) => assert(tip == peerTip)
            case other                              => fail(s"expected NoIntersection, got $other")
        }
    }

    test("findIntersect tolerates empty candidate list (for IntersectSeeker's Origin probe)") {
        val (peer, driver) = newDriver()
        val f = driver.findIntersect(Seq.empty)
        peer.stage(MsgIntersectFound(Point.Origin, Tip.origin))

        val _ = f.futureValue
        assert(decodeSent(peer, 0) == MsgFindIntersect(List.empty))
    }

    test("findIntersect rejects unexpected message as Decode error") {
        val (peer, driver) = newDriver()
        val f = driver.findIntersect(Seq(pt(1L, 1)))

        // Peer wrongly sends MsgRollForward in response to FindIntersect.
        peer.stage(
          MsgRollForward(era = 6, ByteString.fromArray(Array[Byte](1, 2, 3)), tipAt(1L, 1, 1))
        )
        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("awaiting intersect reply"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("findIntersect fails cleanly on EOF") {
        val (peer, driver) = newDriver()
        val f = driver.findIntersect(Seq(pt(1L, 1)))
        peer.stageEof()

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("peer eof"))
            case other => fail(s"expected Decode error for EOF, got $other")
        }
    }

    // --------------------------------------------------------------------------------------
    // next() — core loop
    // --------------------------------------------------------------------------------------

    test("next returns Forward on MsgRollForward") {
        val (peer, driver) = newDriver()
        val hdr = ByteString.fromArray(Array[Byte](0x84.toByte, 1, 2, 3))
        val tip = tipAt(100L, 10, 1)

        val f = driver.next()
        peer.stage(MsgRollForward(era = 6, hdr, tip))

        val ev = f.futureValue
        assert(ev == Some(ChainSyncEvent.Forward(6, hdr, tip)))
        assert(decodeSent(peer, 0) == MsgRequestNext)
    }

    test("next returns Backward on MsgRollBackward") {
        val (peer, driver) = newDriver()
        val to = pt(50L, 3)
        val tip = tipAt(100L, 10, 1)

        val f = driver.next()
        peer.stage(MsgRollBackward(to, tip))

        assert(f.futureValue == Some(ChainSyncEvent.Backward(to, tip)))
    }

    test("next returns Backward on rollback to origin") {
        val (peer, driver) = newDriver()
        val f = driver.next()
        peer.stage(MsgRollBackward(Point.Origin, Tip.origin))

        assert(f.futureValue == Some(ChainSyncEvent.Backward(Point.Origin, Tip.origin)))
    }

    test("next transparently waits through MsgAwaitReply until a real event") {
        val (peer, driver) = newDriver()
        val hdr = ByteString.fromArray(Array[Byte](1, 2, 3))

        val f = driver.next()
        // Peer parks us, then finally delivers.
        peer.stage(MsgAwaitReply)
        peer.stage(MsgAwaitReply)
        peer.stage(MsgRollForward(era = 6, hdr, tipAt(1L, 1, 1)))

        val ev = f.futureValue
        ev match {
            case Some(ChainSyncEvent.Forward(6, bytes, _)) => assert(bytes == hdr)
            case other                                     => fail(s"expected Forward, got $other")
        }

        // Driver must only have sent one MsgRequestNext — AwaitReply is a server-side state,
        // not a client-driven re-request.
        assert(peer.sentOutbound.size == 1)
    }

    test("next returns None on peer MsgDone") {
        val (peer, driver) = newDriver()
        val f = driver.next()
        peer.stage(MsgDone)
        assert(f.futureValue == None)
    }

    test("subsequent next() after MsgDone fails with IllegalStateException") {
        val (peer, driver) = newDriver()
        val f = driver.next()
        peer.stage(MsgDone)
        val _ = f.futureValue

        val failed = driver.next().failed.futureValue
        assert(failed.isInstanceOf[IllegalStateException])
    }

    test("next surfaces Decode on unexpected intersect reply shape") {
        val (peer, driver) = newDriver()
        val f = driver.next()
        peer.stage(MsgIntersectFound(pt(1L, 1), tipAt(1L, 1, 1)))

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("awaiting next event"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    // --------------------------------------------------------------------------------------
    // close()
    // --------------------------------------------------------------------------------------

    test("close sends MsgDone and is idempotent") {
        val (peer, driver) = newDriver()
        driver.close().futureValue
        driver.close().futureValue // no-op, no second send

        assert(peer.sentOutbound.size == 1)
        assert(decodeSent(peer, 0) == MsgDone)
    }

    test("findIntersect after close fails with IllegalStateException") {
        val (_, driver) = newDriver()
        driver.close().futureValue

        val f = driver.findIntersect(Seq(pt(1L, 1)))
        val err = f.failed.futureValue
        assert(err.isInstanceOf[IllegalStateException])
    }

    // --------------------------------------------------------------------------------------
    // Multi-event dialogue — covers the post-intersect "send requestNext in a loop" pattern.
    // --------------------------------------------------------------------------------------

    test("multi-block chain dialogue: findIntersect + 3 forwards + rollback + forward") {
        val (peer, driver) = newDriver()

        val anchor = pt(0L, 0)
        peer.stage(MsgIntersectFound(anchor, tipAt(10L, 10, 10)))
        val intersect = driver.findIntersect(Seq(anchor)).futureValue._1
        assert(intersect == anchor)

        val hdrs = List(
          ByteString.fromArray(Array[Byte](1)),
          ByteString.fromArray(Array[Byte](2)),
          ByteString.fromArray(Array[Byte](3))
        )
        hdrs.foreach(h => peer.stage(MsgRollForward(era = 6, h, tipAt(10L, 10, 10))))
        peer.stage(MsgRollBackward(pt(2L, 2), tipAt(10L, 10, 10)))
        peer.stage(
          MsgRollForward(era = 6, ByteString.fromArray(Array[Byte](99.toByte)), tipAt(11L, 11, 11))
        )

        val events = (1 to 5).map(_ => driver.next().futureValue.get).toList
        events match {
            case ChainSyncEvent.Forward(_, b1, _) ::
                ChainSyncEvent.Forward(_, b2, _) ::
                ChainSyncEvent.Forward(_, b3, _) ::
                ChainSyncEvent.Backward(p, _) ::
                ChainSyncEvent.Forward(_, _, _) :: Nil =>
                assert(b1 == hdrs(0))
                assert(b2 == hdrs(1))
                assert(b3 == hdrs(2))
                assert(p == pt(2L, 2))
            case other => fail(s"unexpected event sequence: $other")
        }

        // Driver sent one FindIntersect + one MsgRequestNext per next() call.
        assert(peer.sentOutbound.size == 6)
    }
}
