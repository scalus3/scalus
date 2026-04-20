package scalus.cardano.network.chainsync

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.BlockHash
import scalus.cardano.network.chainsync.ChainSyncMessage.*
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.*

class ChainSyncMessageSuite extends AnyFunSuite {

    private def roundTrip(m: ChainSyncMessage): Unit = {
        val bytes = Cbor.encode(m).toByteArray
        val decoded = Cbor.decode(bytes).to[ChainSyncMessage].value
        assert(decoded == m, s"round-trip mismatch for $m\nbytes=${bytes.toHex}")
    }

    private val sampleHash: BlockHash =
        BlockHash.fromByteString(
          ByteString.fromArray(Array.tabulate(32)(i => (i + 1).toByte))
        )
    private val samplePoint: Point = Point.BlockPoint(12345L, sampleHash)
    private val sampleTip: Tip = Tip(samplePoint, 42L)

    test("round-trip MsgRequestNext") {
        roundTrip(MsgRequestNext)
    }

    test("round-trip MsgAwaitReply") {
        roundTrip(MsgAwaitReply)
    }

    test("round-trip MsgDone") {
        roundTrip(MsgDone)
    }

    test("round-trip MsgRollForward — Conway era") {
        val headerBytes = ByteString.fromArray(Array[Byte](0x84.toByte, 0x01, 0x02, 0x03, 0x04))
        roundTrip(MsgRollForward(era = 6, headerBytes, sampleTip))
    }

    test("round-trip MsgRollForward — empty inner header") {
        roundTrip(MsgRollForward(era = 1, ByteString.fromArray(Array.emptyByteArray), sampleTip))
    }

    test("round-trip MsgRollBackward — to block point") {
        roundTrip(MsgRollBackward(samplePoint, sampleTip))
    }

    test("round-trip MsgRollBackward — to origin") {
        roundTrip(MsgRollBackward(Point.Origin, Tip.origin))
    }

    test("round-trip MsgFindIntersect — empty candidate list") {
        roundTrip(MsgFindIntersect(List.empty))
    }

    test("round-trip MsgFindIntersect — mixed points") {
        roundTrip(MsgFindIntersect(List(Point.Origin, samplePoint)))
    }

    test("round-trip MsgIntersectFound") {
        roundTrip(MsgIntersectFound(samplePoint, sampleTip))
    }

    test("round-trip MsgIntersectNotFound — origin tip") {
        roundTrip(MsgIntersectNotFound(Tip.origin))
    }

    test("round-trip MsgIntersectNotFound — block tip") {
        roundTrip(MsgIntersectNotFound(sampleTip))
    }

    test("Point.Origin encodes as empty array") {
        // 80 — array(0)
        assert(Cbor.encode(Point.Origin: Point).toByteArray.toHex == "80")
    }

    test("Tip.origin encodes as [[], 0]") {
        // 82      array(2)
        //   80    array(0)     ; origin point
        //   00    uint 0        ; blockNo
        assert(Cbor.encode(Tip.origin).toByteArray.toHex == "828000")
    }

    test("golden: MsgDone — [7]") {
        // 81 07 — array(1), uint 7
        assert(Cbor.encode(MsgDone: ChainSyncMessage).toByteArray.toHex == "8107")
    }

    test("golden: MsgRequestNext — [0]") {
        assert(Cbor.encode(MsgRequestNext: ChainSyncMessage).toByteArray.toHex == "8100")
    }

    test("golden: MsgAwaitReply — [1]") {
        assert(Cbor.encode(MsgAwaitReply: ChainSyncMessage).toByteArray.toHex == "8101")
    }

    test("decode rejects unknown tag") {
        val ex = intercept[Exception] {
            // [99] — tag 99 not a valid chain-sync message
            Cbor.decode(Array[Byte](0x81.toByte, 0x18, 0x63)).to[ChainSyncMessage].value
        }
        assert(ex.getMessage.toLowerCase.contains("tag"))
    }

    test("decode rejects wrong arity for tag") {
        // [2] — MsgRollForward tag without header/tip
        val ex = intercept[Exception] {
            Cbor.decode(Array[Byte](0x81.toByte, 0x02)).to[ChainSyncMessage].value
        }
        // With arrLen=1 but tag=2 needing arrLen=3, the "other" case triggers.
        assert(ex.getMessage.toLowerCase.contains("tag"))
    }

    test("Point.fromChainPoint round-trip with origin") {
        val cp = scalus.cardano.node.stream.ChainPoint.origin
        val p = Point.fromChainPoint(cp)
        assert(p == Point.Origin)
        assert(Point.toChainPoint(p) == cp)
    }

    test("Point.fromChainPoint round-trip with block point") {
        val cp = scalus.cardano.node.stream.ChainPoint(12345L, sampleHash)
        val p = Point.fromChainPoint(cp)
        assert(p == samplePoint)
        assert(Point.toChainPoint(p) == cp)
    }
}
