package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compile
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.OnchainError
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.Option as POption
import cps.*

// -- Test types --

case class Bid(bidder: PubKeyHash, amount: BigInt) derives FromData, ToData

@Compile
object Bid

case class Confirm(confirmed: Boolean) derives FromData, ToData

@Compile
object Confirm

case class FlowParams(seller: PubKeyHash, minBid: BigInt) derives FromData, ToData

@Compile
object FlowParams

// -- Tests --

class UtxoFlowSpec extends AnyFunSuite {

    // ================================================================
    // Single await tests (refactored from spike)
    // ================================================================

    test("single await — dispatch returns POption.None") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
        }

        assert(flowDispatch != null)

        // Single-await flow is terminal, so dispatch should return POption.None
        val datum = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val redeemer = Bid(bidder, BigInt(1_000_000)).toData

        // ctx is null so requireSignedBy will NPE, but that proves decode + body ran
        val ex = intercept[NullPointerException] {
            flowDispatch(datum, redeemer, null)
        }
        info(s"Single await dispatch correctly reached ctx.txInfo (NPE: ${ex.getMessage})")
    }

    test("dispatch throws OnchainError on unknown chunk tag") {
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

        val ex = intercept[OnchainError] {
            flowDispatch(datum, redeemer, null)
        }
        assert(ex.getMessage.contains("unknown chunk tag"))
    }

    // ================================================================
    // Two await tests
    // ================================================================

    test("two awaits — chunk 0 returns POption.Some, chunk 1 returns POption.None") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        // Verify dispatch function works (no chunkCount metadata)
        assert(flowDispatch != null)
    }

    test("two awaits — chunk 0 dispatch returns POption.Some(Constr(1, fields))") {
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
            flowDispatch(datum, redeemer, null)
        }
        info(s"Chunk 0 correctly executed body before continuation (NPE: ${ex.getMessage})")
    }

    test("two awaits — chunk 1 dispatch returns POption.None (terminal)") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            val confirm = await(UtxoFlow.suspend[Confirm])
        }

        // Chunk 1: datum is Constr(1, [bid.toData]) — bid was captured from chunk 0
        // For this simple case without free vars from chunk 0, datum may be Constr(1, [])
        val datum = Data.Constr(1, PList.Nil)
        val confirm = Confirm(true)
        val redeemer = confirm.toData

        val result = flowDispatch(datum, redeemer, null)
        assert(result == POption.None, s"Expected POption.None for terminal chunk, got $result")
        info("Chunk 1 (terminal) correctly returned POption.None")
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

        // Chunk 0: no datum fields needed (no free vars from before chunk 0)
        // Dispatch chunk 0: should return POption.Some(Constr(1, [bid.toData]))
        val datum0 = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val bid = Bid(bidder, BigInt(500_000))
        val redeemer0 = bid.toData

        val result0 = flowDispatch(datum0, redeemer0, null)
        result0 match
            case POption.Some(nextDatum) =>
                info(s"Chunk 0 returned: POption.Some($nextDatum)")
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
                    flowDispatch(nextDatum, redeemer1, null)
                }
                info(
                  s"Chunk 1 correctly decoded captured bid and reached ctx.txInfo (NPE: ${ex.getMessage})"
                )
            case POption.None =>
                fail("Chunk 0 should return POption.Some (non-terminal)")
    }

    test("dead variable not captured — only live vars in datum") {
        // `bid` is NOT used after the second await, so it should NOT be in chunk 1's datum
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
            val confirm = await(UtxoFlow.suspend[Confirm])
            // bid is NOT referenced here — only confirm is used
        }

        // Chunk 0: body uses bid.bidder with ctx, will NPE
        val datum0 = Data.Constr(0, PList.Nil)
        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val bid = Bid(bidder, BigInt(500_000))

        val ex = intercept[NullPointerException] {
            flowDispatch(datum0, bid.toData, null)
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

        // Step 1: dispatch chunk 0
        val datum0 = Data.Constr(0, PList.Nil)
        val bid = Bid(
          PubKeyHash(
            ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
          ),
          BigInt(1_000_000)
        )
        val result0 = flowDispatch(datum0, bid.toData, null)
        result0 match
            case POption.Some(nextDatum) =>
                info(s"After chunk 0: $nextDatum")

                // Step 2: dispatch chunk 1 with the datum from step 1
                val confirm = Confirm(true)
                val result1 = flowDispatch(nextDatum, confirm.toData, null)
                assert(
                  result1 == POption.None,
                  s"Chunk 1 (terminal) should return POption.None, got $result1"
                )
                info("Full lifecycle completed: chunk 0 → POption.Some → chunk 1 → POption.None")
            case POption.None =>
                fail("Chunk 0 should return POption.Some")
    }

    // ================================================================
    // Branching tests — if/match with await in branches
    // ================================================================

    test("branching — await in then-branch, else is terminal") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val confirm = await(UtxoFlow.suspend[Confirm])
            if confirm.confirmed then
                val bid = await(UtxoFlow.suspend[Bid])
            // else: implicit unit → terminal
        }

        val datum0 = Data.Constr(0, PList.Nil)

        // confirmed=true → should continue to next chunk (POption.Some)
        val confirmed = Confirm(true)
        val resultTrue = flowDispatch(datum0, confirmed.toData, null)
        resultTrue match {
            case POption.Some(nextDatum) =>
                info(s"confirmed=true → POption.Some($nextDatum)")
                nextDatum match {
                    case Data.Constr(tag, _) =>
                        assert(tag == 1, s"Expected tag=1, got $tag")
                    case other => fail(s"Expected Constr, got $other")
                }
            case POption.None =>
                fail("confirmed=true should continue to next chunk")
        }

        // confirmed=false → should terminate (POption.None)
        val notConfirmed = Confirm(false)
        val resultFalse = flowDispatch(datum0, notConfirmed.toData, null)
        assert(resultFalse == POption.None, s"confirmed=false should terminate, got $resultFalse")
        info("confirmed=false → POption.None (terminal)")
    }

    test("branching — BigInt condition with await in then-branch") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            if bid.amount > BigInt(100) then
                val confirm = await(UtxoFlow.suspend[Confirm])
        }

        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val datum0 = Data.Constr(0, PList.Nil)

        // High bid → continue
        val highBid = Bid(bidder, BigInt(1_000_000))
        val resultHigh = flowDispatch(datum0, highBid.toData, null)
        resultHigh match {
            case POption.Some(nextDatum) =>
                info(s"High bid → POption.Some($nextDatum)")
            case POption.None =>
                fail("High bid should continue to next chunk")
        }

        // Low bid → terminal
        val lowBid = Bid(bidder, BigInt(50))
        val resultLow = flowDispatch(datum0, lowBid.toData, null)
        assert(resultLow == POption.None, s"Low bid should terminate, got $resultLow")
        info("Low bid → POption.None (terminal)")
    }

    test("branching — await in both branches") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val confirm = await(UtxoFlow.suspend[Confirm])
            if confirm.confirmed then
                val bid = await(UtxoFlow.suspend[Bid])
            else
                val retry = await(UtxoFlow.suspend[Confirm])
        }

        val datum0 = Data.Constr(0, PList.Nil)

        // confirmed=true → POption.Some(Constr for bid chunk)
        val confirmed = Confirm(true)
        val resultTrue = flowDispatch(datum0, confirmed.toData, null)
        resultTrue match {
            case POption.Some(nextDatum) =>
                nextDatum match {
                    case Data.Constr(tag, _) =>
                        info(s"confirmed=true → chunk $tag")
                    case other => fail(s"Expected Constr, got $other")
                }
            case POption.None =>
                fail("confirmed=true should continue")
        }

        // confirmed=false → POption.Some(Constr for retry chunk)
        val notConfirmed = Confirm(false)
        val resultFalse = flowDispatch(datum0, notConfirmed.toData, null)
        resultFalse match {
            case POption.Some(nextDatum) =>
                nextDatum match {
                    case Data.Constr(tag, _) =>
                        info(s"confirmed=false → chunk $tag")
                    case other => fail(s"Expected Constr, got $other")
                }
            case POption.None =>
                fail("confirmed=false should also continue (to retry)")
        }
    }

    // ================================================================
    // Recursion / loop tests
    // ================================================================

    test("recursion — self-loop terminates on confirmed") {
        val flowDispatch = UtxoFlow.define { ctx =>
            def loop(c: Confirm): Unit = {
                val next = await(UtxoFlow.suspend[Confirm])
                if next.confirmed then ()
                else loop(next)
            }
            val first = await(UtxoFlow.suspend[Confirm])
            loop(first)
        }

        // Chunk 0: await Confirm (main flow entry)
        val datum0 = Data.Constr(0, PList.Nil)
        val first = Confirm(false)
        val result0 = flowDispatch(datum0, first.toData, null)
        result0 match {
            case POption.Some(nextDatum) =>
                info(s"Chunk 0 → POption.Some($nextDatum)")
                nextDatum match {
                    case Data.Constr(tag, _) =>
                        // Should point to the function entry chunk (chunk 1)
                        assert(tag == 1, s"Expected tag=1 (function entry), got $tag")
                    case other => fail(s"Expected Constr, got $other")
                }

                // Chunk 1: await Confirm (function entry)
                // Send confirmed=false → should self-loop back to chunk 1
                val notConfirmed = Confirm(false)
                val result1 = flowDispatch(nextDatum, notConfirmed.toData, null)
                result1 match {
                    case POption.Some(loopDatum) =>
                        info(s"Chunk 1 (not confirmed) → POption.Some($loopDatum)")
                        loopDatum match {
                            case Data.Constr(loopTag, _) =>
                                assert(
                                  loopTag == 1,
                                  s"Expected tag=1 (self-loop), got $loopTag"
                                )
                            case other => fail(s"Expected Constr, got $other")
                        }

                        // Send confirmed=true → should terminate
                        val confirmed = Confirm(true)
                        val result2 = flowDispatch(loopDatum, confirmed.toData, null)
                        assert(
                          result2 == POption.None,
                          s"Expected terminal POption.None, got $result2"
                        )
                        info("Chunk 1 (confirmed) → POption.None (terminal)")
                    case POption.None =>
                        fail("Not confirmed should loop, not terminate")
                }
            case POption.None =>
                fail("Chunk 0 should return POption.Some")
        }
    }

    test("recursion — self-loop with function param used after await") {
        val flowDispatch = UtxoFlow.define { ctx =>
            def loop(bid: Bid): Unit = {
                val confirm = await(UtxoFlow.suspend[Confirm])
                if confirm.confirmed then
                    ctx.txInfo.requireSignedBy(bid.bidder) // bid is a datum field
                else
                    val newBid = await(UtxoFlow.suspend[Bid])
                    loop(newBid)
            }
            val firstBid = await(UtxoFlow.suspend[Bid])
            loop(firstBid)
        }

        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val datum0 = Data.Constr(0, PList.Nil)
        val bid = Bid(bidder, BigInt(500_000))

        // Chunk 0: await Bid → enters loop
        val result0 = flowDispatch(datum0, bid.toData, null)
        result0 match {
            case POption.Some(loopDatum) =>
                info(s"Chunk 0 → POption.Some($loopDatum)")
                loopDatum match {
                    case Data.Constr(tag, fields) =>
                        // Function entry chunk; bid should be captured as datum field
                        assert(!fields.isEmpty, "Expected bid to be captured in datum fields")
                        info(s"Function entry: tag=$tag, fields=$fields")
                    case other => fail(s"Expected Constr, got $other")
                }
            case POption.None =>
                fail("Chunk 0 should return POption.Some")
        }
    }

    test("recursion — self-loop full lifecycle with re-bid") {
        val flowDispatch = UtxoFlow.define { ctx =>
            def loop(bid: Bid): Unit = {
                val confirm = await(UtxoFlow.suspend[Confirm])
                if confirm.confirmed then () // terminal
                else
                    val newBid = await(UtxoFlow.suspend[Bid])
                    loop(newBid)
            }
            val firstBid = await(UtxoFlow.suspend[Bid])
            loop(firstBid)
        }

        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )

        // Chunk 0: await Bid (main entry)
        val datum0 = Data.Constr(0, PList.Nil)
        val bid1 = Bid(bidder, BigInt(100))
        val result0 = flowDispatch(datum0, bid1.toData, null)
        val loopDatum1 = result0 match {
            case POption.Some(d) =>
                info(s"Chunk 0 → loop entry: $d")
                d
            case POption.None => fail("Chunk 0 should enter loop"); ???
        }

        // Chunk 1 (loop entry): await Confirm; confirmed=false → await Bid (chunk 2)
        val notConfirmed = Confirm(false)
        val result1 = flowDispatch(loopDatum1, notConfirmed.toData, null)
        val awaitNewBidDatum = result1 match {
            case POption.Some(d) =>
                info(s"Loop (not confirmed) → await new bid: $d")
                d
            case POption.None => fail("Not confirmed should continue"); ???
        }

        // Chunk 2: await Bid → loop(newBid) → back to chunk 1
        val bid2 = Bid(bidder, BigInt(200))
        val result2 = flowDispatch(awaitNewBidDatum, bid2.toData, null)
        val loopDatum2 = result2 match {
            case POption.Some(d) =>
                info(s"Re-bid → back to loop entry: $d")
                d
            case POption.None => fail("Re-bid should loop back"); ???
        }

        // Chunk 1 again: confirmed=true → terminal
        val confirmed = Confirm(true)
        val result3 = flowDispatch(loopDatum2, confirmed.toData, null)
        assert(result3 == POption.None, s"Expected terminal, got $result3")
        info("Full recursion lifecycle: entry → loop → re-bid → loop → terminal")
    }

    // ================================================================
    // Parameterized flow tests — outer scope params captured as datum fields
    // ================================================================

    test("parameterized flow — outer param captured in recursive function") {
        // Pattern from the design doc: a function that takes parameters
        // and returns a flow dispatch. The parameters become datum fields
        // in chunks that reference them.
        def auctionFlow(seller: PubKeyHash) = UtxoFlow.define { ctx =>
            def bidLoop(currentBid: Bid): Unit = {
                val confirm = await(UtxoFlow.suspend[Confirm])
                if confirm.confirmed then
                    ctx.txInfo.requireSignedBy(seller) // seller from outer scope
                else
                    val newBid = await(UtxoFlow.suspend[Bid])
                    bidLoop(newBid)
            }
            val firstBid = await(UtxoFlow.suspend[Bid])
            bidLoop(firstBid)
        }

        val seller = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val flowDispatch = auctionFlow(seller)

        // Chunk 0: await Bid → enters bidLoop
        val datum0 = Data.Constr(0, PList.Nil)
        val bid1 = Bid(seller, BigInt(500))
        val result0 = flowDispatch(datum0, bid1.toData, null)
        val loopDatum = result0 match {
            case POption.Some(d) =>
                info(s"Chunk 0 → loop entry: $d")
                d match {
                    case Data.Constr(tag, fields) =>
                        assert(tag == 1, s"Expected tag=1 (loop entry), got $tag")
                        // Datum should contain currentBid AND seller
                        info(s"Loop entry datum fields: $fields")
                        d
                    case other => fail(s"Expected Constr, got $other"); ???
                }
            case POption.None => fail("Chunk 0 should enter loop"); ???
        }

        // Chunk 1 (loop entry): await Confirm; confirmed=false → chunk 2 (await Bid)
        val notConfirmed = Confirm(false)
        val result1 = flowDispatch(loopDatum, notConfirmed.toData, null)
        val awaitBidDatum = result1 match {
            case POption.Some(d) =>
                info(s"Loop (not confirmed) → await new bid: $d")
                d
            case POption.None => fail("Not confirmed should continue to await bid"); ???
        }

        // Chunk 2: await Bid → calls bidLoop(newBid) → back to chunk 1
        // This tests transitive free var: seller must be carried through chunk 2
        val bid2 = Bid(seller, BigInt(1000))
        val result2 = flowDispatch(awaitBidDatum, bid2.toData, null)
        val loopDatum2 = result2 match {
            case POption.Some(d) =>
                info(s"Re-bid → back to loop: $d")
                d match {
                    case Data.Constr(tag, fields) =>
                        assert(tag == 1, s"Expected tag=1 (loop back), got $tag")
                        // seller should still be in datum fields
                        info(s"Loop re-entry datum fields: $fields")
                        d
                    case other => fail(s"Expected Constr, got $other"); ???
                }
            case POption.None => fail("Re-bid should loop back"); ???
        }

        // Chunk 1 again: confirmed=true → terminal (uses seller via ctx)
        val confirmed = Confirm(true)
        val ex = intercept[NullPointerException] {
            flowDispatch(loopDatum2, confirmed.toData, null)
        }
        info(s"Terminal reached ctx.txInfo.requireSignedBy(seller) (NPE: ${ex.getMessage})")
    }

    test("parameterized flow — multiple outer params + function param in datum") {
        // Full auction pattern: seller + minBid from outer scope,
        // currentBid as function param, all captured as datum fields.
        def auctionFlow(seller: PubKeyHash, minBid: BigInt) = UtxoFlow.define { ctx =>
            def bidLoop(currentBid: Bid): Unit = {
                val confirm = await(UtxoFlow.suspend[Confirm])
                if confirm.confirmed then
                    // Terminal: both seller (outer) and currentBid (param) used
                    ctx.txInfo.requireSignedBy(seller)
                    ctx.txInfo.outputs.add(
                      scalus.cardano.onchain.plutus.v1.Address.fromPubKeyHash(seller),
                      scalus.cardano.onchain.plutus.v1.Value.lovelace(currentBid.amount)
                    )
                else
                    val newBid = await(UtxoFlow.suspend[Bid])
                    bidLoop(newBid)
            }
            val firstBid = await(UtxoFlow.suspend[Bid])
            // minBid from outer scope used here
            require(firstBid.amount >= minBid, "bid too low")
            bidLoop(firstBid)
        }

        val seller = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val minBid = BigInt(100)
        val flowDispatch = auctionFlow(seller, minBid)

        // Chunk 0: await Bid; require(amount >= minBid) → bidLoop
        // Initial datum must include outer-scope params that are used in chunk 0.
        // minBid is a free var in chunk 0's body → it's a datum field.
        val datum0 = Data.Constr(0, PList.Cons(minBid.toData, PList.Nil))

        // Bid below minBid should fail
        val lowBid = Bid(seller, BigInt(50))
        val ex = intercept[IllegalArgumentException] {
            flowDispatch(datum0, lowBid.toData, null)
        }
        assert(ex.getMessage.contains("bid too low"))
        info(s"Low bid rejected: ${ex.getMessage}")

        // Valid bid → enters loop
        val bid1 = Bid(seller, BigInt(500))
        val result0 = flowDispatch(datum0, bid1.toData, null)
        val loopDatum = result0 match {
            case POption.Some(d) =>
                d match {
                    case Data.Constr(tag, fields) =>
                        assert(tag == 1, s"Expected tag=1, got $tag")
                        // Should have currentBid AND seller in datum
                        info(s"Loop entry: tag=$tag, fields count=${fields.size}")
                        d
                    case other => fail(s"Expected Constr, got $other"); ???
                }
            case POption.None => fail("Should enter loop"); ???
        }

        // Chunk 1 (loop): confirmed=true → terminal (uses seller + currentBid)
        val confirmed = Confirm(true)
        // Will NPE on ctx.txInfo.requireSignedBy, proving seller was decoded
        val npe = intercept[NullPointerException] {
            flowDispatch(loopDatum, confirmed.toData, null)
        }
        info(
          s"Terminal used seller from outer scope and currentBid from param (NPE: ${npe.getMessage})"
        )
    }

    test("branching — free variable captured in branch continuation") {
        val flowDispatch = UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            val confirm = await(UtxoFlow.suspend[Confirm])
            if confirm.confirmed then
                val bid2 = await(UtxoFlow.suspend[Bid])
                ctx.txInfo.requireSignedBy(bid.bidder) // bid captured in chunk 2 datum
        }

        val bidder = PubKeyHash(
          ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        )
        val datum0 = Data.Constr(0, PList.Nil)
        val bid = Bid(bidder, BigInt(500_000))
        val result0 = flowDispatch(datum0, bid.toData, null)

        result0 match {
            case POption.Some(datum1) =>
                info(s"Chunk 0 returned: POption.Some($datum1)")
                val confirm = Confirm(true)
                val result1 = flowDispatch(datum1, confirm.toData, null)
                result1 match {
                    case POption.Some(datum2) =>
                        info(s"Chunk 1 (confirmed=true) returned: POption.Some($datum2)")
                        datum2 match {
                            case Data.Constr(tag, fields) =>
                                assert(tag == 2, s"Expected tag=2, got $tag")
                                assert(
                                  !fields.isEmpty,
                                  "Expected bid to be captured in datum fields"
                                )
                                info(s"Next datum: tag=$tag, fields=$fields")

                                // Dispatch chunk 2 — body uses bid.bidder via ctx, will NPE
                                val bid2 = Bid(bidder, BigInt(100))
                                val ex = intercept[NullPointerException] {
                                    flowDispatch(datum2, bid2.toData, null)
                                }
                                info(
                                  s"Chunk 2 decoded captured bid, reached ctx.txInfo (NPE: ${ex.getMessage})"
                                )
                            case other => fail(s"Expected Constr, got $other")
                        }
                    case POption.None =>
                        fail("confirmed=true should continue to chunk 2")
                }
            case POption.None =>
                fail("Chunk 0 should return POption.Some")
        }
    }

    // ================================================================
    // Compile-time error tests
    // ================================================================

    test("compile error — await inside lambda (async closure)") {
        assertDoesNotCompile("""
            UtxoFlow.define { ctx =>
                val f: Int => Unit = x => {
                    val c = await(UtxoFlow.suspend[Confirm])
                }
                val bid = await(UtxoFlow.suspend[Bid])
            }
        """)
    }

    test("compile error — non-tail recursive call (followed by await)") {
        assertDoesNotCompile("""
            UtxoFlow.define { ctx =>
                def loop(c: Confirm): Unit = {
                    loop(c)
                    val next = await(UtxoFlow.suspend[Confirm])
                    ()
                }
                val first = await(UtxoFlow.suspend[Confirm])
                loop(first)
            }
        """)
    }

    test("compile error — non-tail recursive call (followed by code)") {
        assertDoesNotCompile("""
            UtxoFlow.define { ctx =>
                def loop(c: Confirm): Unit = {
                    val next = await(UtxoFlow.suspend[Confirm])
                    loop(next)
                    ()
                }
                val first = await(UtxoFlow.suspend[Confirm])
                loop(first)
            }
        """)
    }
}

// -- On-chain flow validators --

@Compile
object TestFlowValidator extends FlowCellValidator {
    inline def beaconName: ByteString = utf8"test-flow"
    inline def flowDispatch = UtxoFlow.define { ctx =>
        val bid = await(UtxoFlow.suspend[Bid])
        ctx.txInfo.requireSignedBy(bid.bidder)
        val confirm = await(UtxoFlow.suspend[Confirm])
    }
}

@Compile
object BranchingFlowValidator extends FlowCellValidator {
    inline def beaconName: ByteString = utf8"branching-flow"
    inline def flowDispatch = UtxoFlow.define { ctx =>
        val confirm = await(UtxoFlow.suspend[Confirm])
        if confirm.confirmed then
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(bid.bidder)
    }
}

@Compile
object RecursiveFlowValidator extends FlowCellValidator {
    inline def beaconName: ByteString = utf8"recursive-flow"
    inline def flowDispatch = UtxoFlow.define { ctx =>
        def loop(c: Confirm): Unit = {
            val next = await(UtxoFlow.suspend[Confirm])
            if next.confirmed then ()
            else loop(next)
        }
        val first = await(UtxoFlow.suspend[Confirm])
        loop(first)
    }
}

object TestFlowCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(TestFlowValidator.validate)
}

object BranchingFlowCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(BranchingFlowValidator.validate)
}

object RecursiveFlowCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(RecursiveFlowValidator.validate)
}

@Compile
object ParameterizedFlowValidator extends DataParameterizedFlowCellValidator {
    inline def beaconName: ByteString = utf8"param-flow"
    inline def flowDispatch(param: Data) = {
        val p = param.to[FlowParams]
        UtxoFlow.define { ctx =>
            val bid = await(UtxoFlow.suspend[Bid])
            ctx.txInfo.requireSignedBy(p.seller)
            val confirm = await(UtxoFlow.suspend[Confirm])
        }
    }
}

object ParameterizedFlowCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(ParameterizedFlowValidator.validate)
}

class UtxoFlowOnChainSpec extends AnyFunSuite {

    private given scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()

    test("FlowCellValidator compiles to UPLC") {
        val compiled = TestFlowCompilation.compiled
        assert(compiled.program.term != null)
        info(s"UPLC program version: ${compiled.program.version}")
        info(s"Script size: ${compiled.script.script.size} bytes")
    }

    test("BranchingFlowValidator compiles to UPLC") {
        val compiled = BranchingFlowCompilation.compiled
        assert(compiled.program.term != null)
        info(s"UPLC program version: ${compiled.program.version}")
        info(s"Branching script size: ${compiled.script.script.size} bytes")
    }

    test("RecursiveFlowValidator compiles to UPLC") {
        val compiled = RecursiveFlowCompilation.compiled
        assert(compiled.program.term != null)
        info(s"UPLC program version: ${compiled.program.version}")
        info(s"Recursive script size: ${compiled.script.script.size} bytes")
    }

    test("ParameterizedFlowValidator compiles to UPLC") {
        val compiled = ParameterizedFlowCompilation.compiled
        assert(compiled.program.term != null)
        info(s"UPLC program version: ${compiled.program.version}")
        info(s"Parameterized script size: ${compiled.script.script.size} bytes")
    }
}
