package scalus.cardano.network.blockfetch

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.BlockHash
import scalus.cardano.network.blockfetch.BlockFetchMessage.*
import scalus.cardano.network.chainsync.Point
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.*

class BlockFetchMessageSuite extends AnyFunSuite {

    private def roundTrip(m: BlockFetchMessage): Unit = {
        val bytes = Cbor.encode(m).toByteArray
        val decoded = Cbor.decode(bytes).to[BlockFetchMessage].value
        assert(decoded == m, s"round-trip mismatch for $m\nbytes=${bytes.toHex}")
    }

    private val sampleHash: BlockHash =
        BlockHash.fromByteString(
          ByteString.fromArray(Array.tabulate(32)(i => (i + 1).toByte))
        )
    private val start: Point = Point.BlockPoint(100L, sampleHash)
    private val end: Point = Point.BlockPoint(200L, sampleHash)

    test("round-trip MsgRequestRange") {
        roundTrip(MsgRequestRange(start, end))
    }

    test("round-trip MsgRequestRange with Origin start") {
        roundTrip(MsgRequestRange(Point.Origin, end))
    }

    test("round-trip MsgClientDone") {
        roundTrip(MsgClientDone)
    }

    test("round-trip MsgStartBatch") {
        roundTrip(MsgStartBatch)
    }

    test("round-trip MsgNoBlocks") {
        roundTrip(MsgNoBlocks)
    }

    test("round-trip MsgBlock — tag24([era, block]) wire format") {
        // BlockFetch's MsgBlock is `[4, tag24([era_u16, blockCbor])]` — era is *inside* the
        // tag24 wrapper, unlike ChainSync headers which put era outside. Cross-ref: pallas's
        // `BlockWrapper<T> = (u16, T)` used by `MultiEraBlock::decode_conway` et al.
        //
        // The inner `blockCbor` is parsed as a full CBOR item (the decoder slices its raw
        // bytes), so the fixture must be valid CBOR — pick an empty-array `[0x80]` so the
        // test exercises the envelope codec without dragging in a real block fixture.
        val blockBytes = ByteString.fromArray(Array[Byte](0x80.toByte))
        roundTrip(MsgBlock(era = 6, blockBytes))
    }

    test("round-trip MsgBatchDone") {
        roundTrip(MsgBatchDone)
    }

    test("golden: MsgClientDone — [1]") {
        assert(Cbor.encode(MsgClientDone: BlockFetchMessage).toByteArray.toHex == "8101")
    }

    test("golden: MsgStartBatch — [2]") {
        assert(Cbor.encode(MsgStartBatch: BlockFetchMessage).toByteArray.toHex == "8102")
    }

    test("golden: MsgNoBlocks — [3]") {
        assert(Cbor.encode(MsgNoBlocks: BlockFetchMessage).toByteArray.toHex == "8103")
    }

    test("golden: MsgBatchDone — [5]") {
        assert(Cbor.encode(MsgBatchDone: BlockFetchMessage).toByteArray.toHex == "8105")
    }

    test("decode rejects unknown tag") {
        val ex = intercept[Exception] {
            // [99] — tag 99 not a valid block-fetch message
            Cbor.decode(Array[Byte](0x81.toByte, 0x18, 0x63)).to[BlockFetchMessage].value
        }
        assert(ex.getMessage.toLowerCase.contains("tag"))
    }

    test("decode rejects wrong arity for MsgRequestRange tag") {
        // [0] — MsgRequestRange tag without start/end
        val ex = intercept[Exception] {
            Cbor.decode(Array[Byte](0x81.toByte, 0x00)).to[BlockFetchMessage].value
        }
        assert(ex.getMessage.toLowerCase.contains("tag"))
    }
}
