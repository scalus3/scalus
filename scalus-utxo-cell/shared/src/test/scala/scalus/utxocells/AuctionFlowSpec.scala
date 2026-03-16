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
import scalus.cardano.onchain.plutus.v1.{Address, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.Option as POption
import scalus.testing.kit.Party.{Alice, Bob}
import cps.*

import UtxoCellBuilder.*

// -- Redeemer types for the auction flow --

case class AuctionBid(bidder: PubKeyHash, amount: BigInt) derives FromData, ToData

@Compile
object AuctionBid

case class AuctionClose(dummy: BigInt) derives FromData, ToData

@Compile
object AuctionClose

// -- Flow validator --

@Compile
object AuctionFlowValidator extends FlowCellValidator {
    inline def beaconName: ByteString = utf8"auction-flow"
    inline def flowDispatch = UtxoFlow.define { ctx =>
        // Chunk 0: accept a bid, set continuing value, require bidder signature
        val bid = await(UtxoFlow.suspend[AuctionBid])
        ctx.txInfo.requireSignedBy(bid.bidder)
        ctx.setContinuingValue(Value.lovelace(bid.amount))

        // Chunk 1: close the auction, pay the seller
        // bid.bidder and bid.amount are captured as free variables in chunk 1's datum
        val close = await(UtxoFlow.suspend[AuctionClose])
        // Terminal: seller gets the bid amount
        ctx.txInfo.outputs.add(
          Address.fromPubKeyHash(bid.bidder),
          Value.lovelace(bid.amount)
        )
    }
}

object AuctionFlowCompilation {
    given Options = Options.debug
    lazy val compiled = PlutusV3.compile(AuctionFlowValidator.validate)
}

// -- Tests --

class AuctionFlowSpec extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val compiled = AuctionFlowCompilation.compiled

    val flowDef = new UtxoFlowDef(
      compiled = compiled,
      tokenName = AuctionFlowValidator.beaconName,
      flowDispatch = AuctionFlowValidator.flowDispatch
    )

    test("AuctionFlow compiles to UPLC") {
        info(s"Auction flow script size: ${compiled.script.script.size} bytes")
        assert(compiled.script.script.size > 0)
    }

    test(
      "AuctionFlow lifecycle: init -> bid (with setContinuingValue) -> close (with outputs.add)"
    ) {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, LedgerValue.ada(1000)),
          Input(genesisHash, 1) -> Output(Bob.address, LedgerValue.ada(1000))
        )

        val emulator = Emulator(initialUtxos = initialUtxos)

        // 1. Init: mint beacon + send initial datum Constr(0, [])
        val initialDatum = Data.Constr(0, PList.Nil)
        val mintTx = TxBuilder(testEnv)
            .initUtxoFlow(flowDef, initialDatum, LedgerValue.ada(10))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"Init should succeed: $mintResult")

        val cellUtxo = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo.isDefined, "Should find flow UTxO by beacon")

        // 2. Advance chunk 0: Bob bids 5 ADA
        //    - requireSignedBy(bid.bidder) → Bob must sign
        //    - setContinuingValue(Value.lovelace(5_000_000)) → cell value updated
        val bidderPkh = PubKeyHash(Bob.addrKeyHash)
        val bid = AuctionBid(bidder = bidderPkh, amount = BigInt(5_000_000))
        val bidTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, bid.toData, emulator.utxos)
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        val bidResult = emulator.submitSync(bidTx)
        assert(bidResult.isRight, s"Bid should succeed: $bidResult")

        // Verify datum advanced to chunk 1 with captured variables
        val cellUtxo2 = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo2.isDefined, "Should find flow UTxO after bid")
        val bidDatum = flowDef.currentDatum(cellUtxo2.get)
        bidDatum match
            case Data.Constr(tag, fields) =>
                assert(tag == 1, s"Expected tag=1 after bid, got $tag")
                assert(!fields.isEmpty, "Chunk 1 datum should have captured bid fields")
                info(s"Chunk 1 datum: tag=$tag, fields=$fields")
            case other =>
                fail(s"Expected Constr datum, got $other")

        // Verify the cell value was updated by setContinuingValue
        val cellValue = cellUtxo2.get.output.value
        info(s"Cell value after bid: $cellValue")

        // 3. Advance chunk 1: Close auction (terminal)
        //    - outputs.add(bid.bidder address, bid.amount) → pays bidder back (simplified auction)
        //    - Terminal: beacon is burned
        val close = AuctionClose(dummy = BigInt(0))
        val closeTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, close.toData, emulator.utxos)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val closeResult = emulator.submitSync(closeTx)
        assert(closeResult.isRight, s"Close should succeed: $closeResult")

        // Verify flow UTxO is gone (beacon burned)
        val cellUtxo3 = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo3.isEmpty, "Flow UTxO should be consumed after terminal")
    }
}
