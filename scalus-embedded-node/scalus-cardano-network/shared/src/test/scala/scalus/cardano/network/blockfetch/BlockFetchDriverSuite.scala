package scalus.cardano.network.blockfetch

import io.bullet.borer.Cbor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken}
import scalus.cardano.ledger.BlockHash
import scalus.cardano.network.blockfetch.BlockFetchMessage.*
import scalus.cardano.network.chainsync.Point
import scalus.cardano.network.ChainSyncError
import scalus.cardano.network.infra.MiniProtocolBytes
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class BlockFetchDriverSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

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

        def stage(reply: BlockFetchMessage): Unit =
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

    private def pt(slot: Long, seed: Int): Point.BlockPoint = Point.BlockPoint(slot, sampleHash(seed))

    private def newDriver(): (ScriptedBytes, BlockFetchDriver) = {
        val peer = new ScriptedBytes
        val driver = new BlockFetchDriver(peer, CancelSource().token)
        (peer, driver)
    }

    private def decodeSent(peer: ScriptedBytes, idx: Int): BlockFetchMessage =
        Cbor.decode(peer.sentOutbound(idx).bytes).to[BlockFetchMessage].value

    test("fetchOne sends MsgRequestRange and returns block on happy path") {
        val (peer, driver) = newDriver()
        val point = pt(100L, 1)
        // Valid CBOR (empty array) — MsgBlock codec now slices the block sub-value from the
        // inner [era, block] tag24 wrapper, so arbitrary byte sequences are no longer legal.
        val blockBytes = ByteString.fromArray(Array[Byte](0x80.toByte))

        val f = driver.fetchOne(point)
        peer.stage(MsgStartBatch)
        peer.stage(MsgBlock(era = 6, blockBytes))
        peer.stage(MsgBatchDone)

        val result = f.futureValue
        result match {
            case Right(FetchedBlock(era, bs)) =>
                assert(era == 6)
                assert(bs == blockBytes)
            case other => fail(s"expected Right(FetchedBlock), got $other")
        }
        assert(decodeSent(peer, 0) == MsgRequestRange(point, point))
    }

    test("fetchOne returns Left(MissingBlock) on MsgNoBlocks") {
        val (peer, driver) = newDriver()
        val point = pt(100L, 1)

        val f = driver.fetchOne(point)
        peer.stage(MsgNoBlocks)

        val result = f.futureValue
        result match {
            case Left(ChainSyncError.MissingBlock(p)) =>
                assert(p == Point.toChainPoint(point))
            case other => fail(s"expected Left(MissingBlock), got $other")
        }
    }

    test("fetchOne fails on unexpected MsgBatchDone without a block") {
        val (peer, driver) = newDriver()
        val f = driver.fetchOne(pt(1L, 1))
        peer.stage(MsgStartBatch)
        peer.stage(MsgBatchDone) // no MsgBlock between StartBatch and BatchDone

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("awaiting block"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("fetchOne fails on multi-block batch (M5 requests single-block range only)") {
        val (peer, driver) = newDriver()
        val f = driver.fetchOne(pt(1L, 1))
        peer.stage(MsgStartBatch)
        peer.stage(MsgBlock(era = 6, ByteString.fromArray(Array[Byte](0x01)) /* CBOR int 1 */))
        // Peer wrongly sends a second block instead of BatchDone.
        peer.stage(MsgBlock(era = 6, ByteString.fromArray(Array[Byte](0x02)) /* CBOR int 2 */))

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("awaiting batch done"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("fetchOne fails on MsgStartBatch without MsgRequestRange response type") {
        val (peer, driver) = newDriver()
        val f = driver.fetchOne(pt(1L, 1))
        // Peer sends ClientDone (not its role) instead of StartBatch/NoBlocks.
        peer.stage(MsgClientDone)

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("awaiting batch start"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("fetchOne fails cleanly on peer EOF before StartBatch") {
        val (peer, driver) = newDriver()
        val f = driver.fetchOne(pt(1L, 1))
        peer.stageEof()

        val err = f.failed.futureValue
        err match {
            case d: ChainSyncError.Decode =>
                assert(d.getMessage.toLowerCase.contains("peer eof"))
            case other => fail(s"expected Decode error, got $other")
        }
    }

    test("sequential fetches return to Idle between requests") {
        val (peer, driver) = newDriver()
        val p1 = pt(100L, 1)
        val p2 = pt(200L, 2)
        val b1 = ByteString.fromArray(Array[Byte](0x01)) /* CBOR int 1 */
        val b2 = ByteString.fromArray(Array[Byte](0x02)) /* CBOR int 2 */

        // First fetch
        val f1 = driver.fetchOne(p1)
        peer.stage(MsgStartBatch)
        peer.stage(MsgBlock(era = 6, b1))
        peer.stage(MsgBatchDone)
        val r1 = f1.futureValue
        assert(r1.map(_.blockBytes) == Right(b1))

        // Second fetch on the same driver
        val f2 = driver.fetchOne(p2)
        peer.stage(MsgStartBatch)
        peer.stage(MsgBlock(era = 6, b2))
        peer.stage(MsgBatchDone)
        val r2 = f2.futureValue
        assert(r2.map(_.blockBytes) == Right(b2))

        assert(peer.sentOutbound.size == 2)
        assert(decodeSent(peer, 0) == MsgRequestRange(p1, p1))
        assert(decodeSent(peer, 1) == MsgRequestRange(p2, p2))
    }

    test("close sends MsgClientDone and is idempotent") {
        val (peer, driver) = newDriver()
        driver.close().futureValue
        driver.close().futureValue

        assert(peer.sentOutbound.size == 1)
        assert(decodeSent(peer, 0) == MsgClientDone)
    }

    test("fetchOne after close fails with IllegalStateException") {
        val (_, driver) = newDriver()
        driver.close().futureValue

        val err = driver.fetchOne(pt(1L, 1)).failed.futureValue
        assert(err.isInstanceOf[IllegalStateException])
    }
}
