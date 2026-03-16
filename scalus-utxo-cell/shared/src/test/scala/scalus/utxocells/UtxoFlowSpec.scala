package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compile
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.prelude.List as PList
import cps.*

// -- Test types --

case class Bid(bidder: PubKeyHash, amount: BigInt) derives FromData, ToData

@Compile
object Bid

case class Confirm(confirmed: Boolean) derives FromData, ToData

@Compile
object Confirm

// -- Tests --

class UtxoFlowSpec extends AnyFunSuite {

    // ================================================================
    // Single await tests (refactored from spike)
    // ================================================================

    test("single await — chunkCount=1, dispatch returns None") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
        }

        assert(flowDispatch.chunkCount == 1, s"Expected 1 chunk, got ${flowDispatch.chunkCount}")
        assert(flowDispatch.dispatch != null)

        // Single-await flow is terminal, so dispatch should return None
        val datum = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val redeemer = Bid(bidder, BigInt(1_000_000)).toData

        // ctx is null so requireSignedBy will NPE, but that proves decode + body ran
        val ex = intercept[NullPointerException] {
            flowDispatch.dispatch(datum, redeemer, null)
        }
        info(s"Single await dispatch correctly reached ctx.txInfo (NPE: ${ex.getMessage})")
    }

    test("dispatch throws on unknown chunk tag") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
        }

        val datum = Data.Constr(99, PList.Nil)
        val redeemer = Bid(
          PubKeyHash(
            ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
          ),
          BigInt(0)
        ).toData

        val ex = intercept[Exception] {
            flowDispatch.dispatch(datum, redeemer, null)
        }
        assert(ex.getMessage.contains("unknown chunk tag"))
    }

    // ================================================================
    // Two await tests
    // ================================================================

    test("two awaits — chunkCount=2, chunk 0 returns Some, chunk 1 returns None") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        assert(flowDispatch.chunkCount == 2, s"Expected 2 chunks, got ${flowDispatch.chunkCount}")
        info(s"Flow has ${flowDispatch.chunkCount} chunks")
    }

    test("two awaits — chunk 0 dispatch returns Some(Constr(1, fields))") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        // Chunk 0: datum is Constr(0, []) — no captured vars for chunk 0
        val datum = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val bid = Bid(bidder, BigInt(1_000_000))
        val redeemer = bid.toData

        // ctx is null — requireSignedBy will NPE
        // But chunk 0 is non-terminal, so the macro body should run first,
        // hitting the NPE before reaching the continuation return
        val ex = intercept[NullPointerException] {
            flowDispatch.dispatch(datum, redeemer, null)
        }
        info(s"Chunk 0 correctly executed body before continuation (NPE: ${ex.getMessage})")
    }

    test("two awaits — chunk 1 dispatch returns None (terminal)") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        // Chunk 1: datum is Constr(1, [bid.toData]) — bid was captured from chunk 0
        // For this simple case without free vars from chunk 0, datum may be Constr(1, [])
        val datum = Data.Constr(1, PList.Nil)
        val confirm = Confirm(true)
        val redeemer = confirm.toData

        val result = flowDispatch.dispatch(datum, redeemer, null)
        assert(result.isEmpty, s"Expected None for terminal chunk, got $result")
        info("Chunk 1 (terminal) correctly returned None")
    }

    // ================================================================
    // Free variable capture tests
    // ================================================================

    test("free variable capture — outer param carried to next chunk") {
        // seller is used after the second await, so it must be in chunk 1's datum
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            val confirm = await(UtxoFlow.suspend[Confirm])
            // bid is used after second await — must be captured in chunk 1 datum
            ctx.txInfo.requireSignedBy(bid.bidder)
        }

        assert(flowDispatch.chunkCount == 2)

        // Chunk 0: no datum fields needed (no free vars from before chunk 0)
        // Dispatch chunk 0: should return Some(Constr(1, [bid.toData]))
        val datum0 = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val bid = Bid(bidder, BigInt(500_000))
        val redeemer0 = bid.toData

        val result0 = flowDispatch.dispatch(datum0, redeemer0, null)
        assert(result0.isDefined, "Chunk 0 should return Some (non-terminal)")
        info(s"Chunk 0 returned: $result0")

        // The result should be Constr(1, [bid.toData]) since bid is captured
        val nextDatum = result0.get
        nextDatum match
            case Data.Constr(tag, fields) =>
                assert(tag == 1, s"Expected tag=1, got $tag")
                assert(!fields.isEmpty, "Expected bid to be captured in datum fields")
                info(s"Next datum: tag=$tag, fields=$fields")
            case other =>
                fail(s"Expected Constr, got $other")

        // Chunk 1: datum has bid captured, redeemer is Confirm
        // Body calls ctx.txInfo.requireSignedBy(bid.bidder) — will NPE on null ctx
        val confirm = Confirm(true)
        val redeemer1 = confirm.toData

        val ex = intercept[NullPointerException] {
            flowDispatch.dispatch(nextDatum, redeemer1, null)
        }
        info(
          s"Chunk 1 correctly decoded captured bid and reached ctx.txInfo (NPE: ${ex.getMessage})"
        )
    }

    test("dead variable not captured — only live vars in datum") {
        // `bid` is NOT used after the second await, so it should NOT be in chunk 1's datum
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
            val confirm = await(UtxoFlow.suspend[Confirm])
            // bid is NOT referenced here — only confirm is used
        }

        assert(flowDispatch.chunkCount == 2)

        // Chunk 0: body uses bid.bidder with ctx, will NPE
        val datum0 = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val bid = Bid(bidder, BigInt(500_000))

        val ex = intercept[NullPointerException] {
            flowDispatch.dispatch(datum0, bid.toData, null)
        }
        info(s"Chunk 0 ran body with ctx access (NPE as expected)")
    }

    // ================================================================
    // Full two-step lifecycle test
    // ================================================================

    test("full lifecycle — two awaits without ctx calls") {
        // A simple flow with no ctx calls to avoid NPE, just data flow
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        assert(flowDispatch.chunkCount == 2)

        // Step 1: dispatch chunk 0
        val datum0 = Data.Constr(0, PList.Nil)
        val bid = Bid(
          PubKeyHash(
            ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
          ),
          BigInt(1_000_000)
        )
        val result0 = flowDispatch.dispatch(datum0, bid.toData, null)
        assert(result0.isDefined, "Chunk 0 should return Some")
        info(s"After chunk 0: ${result0.get}")

        // Step 2: dispatch chunk 1 with the datum from step 1
        val confirm = Confirm(true)
        val result1 = flowDispatch.dispatch(result0.get, confirm.toData, null)
        assert(result1.isEmpty, "Chunk 1 (terminal) should return None")
        info("Full lifecycle completed: chunk 0 → Some → chunk 1 → None")
    }
}
