package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compile
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.{CardanoInfo, Value as LedgerValue, *}
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.v1.{PubKeyHash, Value}
import scalus.cardano.onchain.plutus.prelude.{require, List as PList, Option as POption}
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import cps.*

import UtxoCellBuilder.*

// ================================================================
// Data types
// ================================================================

/** Current highest bid — carried as a datum field through the bid loop. */
case class HighBid(bidder: PubKeyHash, amount: BigInt) derives FromData, ToData

@Compile
object HighBid

/** Redeemer for the recursive auction flow. */
enum BidAction derives FromData, ToData:
    case PlaceBid(bidder: PubKeyHash, amount: BigInt)
    case CloseSale

@Compile
object BidAction

// ================================================================
// Flow validator
// ================================================================

/** Recursive auction contract using UtxoFlow.
  *
  * Demonstrates:
  *   - Recursive bid loop (tail-recursive local function)
  *   - Match-based branching on enum redeemer
  *   - Datum field capture across transactions (highBid)
  *   - setContinuingValue for tracking locked ADA
  *
  * State machine:
  * {{{
  *   Chunk 0 (entry): await BidAction
  *     PlaceBid → requireSignedBy, setContinuingValue, enter bidLoop
  *     CloseSale → terminal (no bids)
  *
  *   Chunk 1 (bidLoop): datum=[highBid], await BidAction
  *     PlaceBid → require amount > current, requireSignedBy, setContinuingValue, self-loop
  *     CloseSale → requireSignedBy(winner), terminal
  * }}}
  */
@Compile
object RecursiveAuctionFlow extends FlowCellValidator {
    inline def beaconName: ByteString = utf8"recursive-auction"
    inline def flowDispatch = UtxoFlow.define { ctx =>
        def bidLoop(highBid: HighBid): Unit = {
            val action = await(UtxoFlow.suspend[BidAction])
            action match
                case BidAction.PlaceBid(bidder, amount) =>
                    require(amount > highBid.amount, "bid too low")
                    ctx.txInfo.requireSignedBy(bidder)
                    ctx.setContinuingValue(Value.lovelace(amount))
                    bidLoop(HighBid(bidder, amount))
                case BidAction.CloseSale =>
                    // Winner signs to close
                    ctx.txInfo.requireSignedBy(highBid.bidder)
        }
        val first = await(UtxoFlow.suspend[BidAction])
        first match
            case BidAction.PlaceBid(bidder, amount) =>
                ctx.txInfo.requireSignedBy(bidder)
                ctx.setContinuingValue(Value.lovelace(amount))
                bidLoop(HighBid(bidder, amount))
            case BidAction.CloseSale =>
                ()
    }
}

object RecursiveAuctionCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(RecursiveAuctionFlow.validate)
}

// ================================================================
// Tests
// ================================================================

class RecursiveAuctionSpec extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val compiled = RecursiveAuctionCompilation.compiled

    val flowDef = new UtxoFlowDef(
      compiled = compiled,
      tokenName = RecursiveAuctionFlow.beaconName,
      flowDispatch = RecursiveAuctionFlow.flowDispatch
    )

    private val bobPkh = PubKeyHash(Bob.addrKeyHash)
    private val charlesPkh = PubKeyHash(Charles.addrKeyHash)

    private def createEmulator(): Emulator = Emulator(
      initialUtxos = Map(
        Input(genesisHash, 0) -> Output(Alice.address, LedgerValue.ada(1000)),
        Input(genesisHash, 1) -> Output(Bob.address, LedgerValue.ada(1000)),
        Input(genesisHash, 2) -> Output(Charles.address, LedgerValue.ada(1000))
      )
    )

    test("RecursiveAuctionFlow compiles to UPLC") {
        assert(compiled.script.script.size > 0)
        info(s"Recursive auction script size: ${compiled.script.script.size} bytes")
    }

    test("Auction lifecycle: init → bid → outbid → close") {
        val emulator = createEmulator()

        // 1. Init auction (seller = Alice)
        val initTx = TxBuilder(testEnv)
            .initUtxoFlow(flowDef, Data.Constr(0, PList.Nil), LedgerValue.ada(5))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        assert(emulator.submitSync(initTx).isRight, "Init should succeed")

        val cell0 = flowDef.findUtxo(emulator.utxos)
        assert(cell0.isDefined, "Should find flow UTxO by beacon")

        // 2. First bid: Bob bids 3 ADA
        val bid1 = BidAction.PlaceBid(bobPkh, BigInt(3_000_000))
        val bidTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, bid1.toData, emulator.utxos)
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        assert(emulator.submitSync(bidTx).isRight, "First bid should succeed")

        val cell1 = flowDef.findUtxo(emulator.utxos)
        assert(cell1.isDefined, "Should find flow UTxO after bid")
        val datum1 = flowDef.currentDatum(cell1.get)
        datum1 match
            case Data.Constr(tag, _) =>
                assert(tag == 1, s"Expected tag=1 (bidLoop entry), got $tag")
                info(s"After first bid: datum=$datum1")
            case other => fail(s"Expected Constr, got $other")

        // 3. Outbid: Charles bids 5 ADA
        val bid2 = BidAction.PlaceBid(charlesPkh, BigInt(5_000_000))
        val outbidTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, bid2.toData, emulator.utxos)
            .complete(emulator.utxos, Charles.address)
            .sign(Charles.signer)
            .transaction

        assert(emulator.submitSync(outbidTx).isRight, "Outbid should succeed")

        val cell2 = flowDef.findUtxo(emulator.utxos)
        assert(cell2.isDefined, "Should find flow UTxO after outbid")
        val datum2 = flowDef.currentDatum(cell2.get)
        datum2 match
            case Data.Constr(tag, _) =>
                assert(tag == 1, s"Expected tag=1 (still in bidLoop), got $tag")
                info(s"After outbid: datum=$datum2")
            case other => fail(s"Expected Constr, got $other")

        // 4. Close: Charles (highest bidder) closes the auction
        val closeTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, BidAction.CloseSale.toData, emulator.utxos)
            .complete(emulator.utxos, Charles.address)
            .sign(Charles.signer)
            .transaction

        assert(emulator.submitSync(closeTx).isRight, "Close should succeed")

        val cell3 = flowDef.findUtxo(emulator.utxos)
        assert(cell3.isEmpty, "Beacon should be burned after close")
    }

    test("Bid too low is rejected") {
        val emulator = createEmulator()

        // Init + first bid (3 ADA)
        val initTx = TxBuilder(testEnv)
            .initUtxoFlow(flowDef, Data.Constr(0, PList.Nil), LedgerValue.ada(5))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(emulator.submitSync(initTx).isRight)

        val bid1 = BidAction.PlaceBid(bobPkh, BigInt(3_000_000))
        val bidTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, bid1.toData, emulator.utxos)
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction
        assert(emulator.submitSync(bidTx).isRight)

        // Try outbid with 2 ADA (should fail — require(amount > highBid.amount))
        val lowBid = BidAction.PlaceBid(charlesPkh, BigInt(2_000_000))
        val ex = intercept[RequirementError] {
            TxBuilder(testEnv)
                .advanceUtxoFlow(flowDef, lowBid.toData, emulator.utxos)
        }
        assert(
          ex.getMessage.contains("bid too low"),
          s"Expected 'bid too low', got: ${ex.getMessage}"
        )
    }

    test("Close without bids") {
        val emulator = createEmulator()

        // Init
        val initTx = TxBuilder(testEnv)
            .initUtxoFlow(flowDef, Data.Constr(0, PList.Nil), LedgerValue.ada(5))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(emulator.submitSync(initTx).isRight)

        // Close immediately (CloseSale on entry → terminal without bids)
        val closeTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, BidAction.CloseSale.toData, emulator.utxos)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        assert(emulator.submitSync(closeTx).isRight, "Close without bids should succeed")

        val cell = flowDef.findUtxo(emulator.utxos)
        assert(cell.isEmpty, "Beacon should be burned")
    }
}
