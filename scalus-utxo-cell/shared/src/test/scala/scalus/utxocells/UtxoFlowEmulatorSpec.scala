package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.testing.kit.Party.{Alice, Bob}

class UtxoFlowEmulatorSpec extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val compiled = TestFlowCompilation.compiled

    val flowDef = new UtxoFlowDef(
      compiled = compiled,
      tokenName = TestFlowValidator.beaconName,
      flowDispatch = TestFlowValidator.flowDispatch
    )

    import UtxoCellBuilder.*

    test("UtxoFlow lifecycle: init -> advance (bid) -> advance (confirm, terminal)") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000)),
          Input(genesisHash, 1) -> Output(Bob.address, Value.ada(1000))
        )

        // Use default validators — full ledger rule checking including script evaluation
        val emulator = Emulator(
          initialUtxos = initialUtxos
        )

        // 1. Init: mint beacon + send initial datum Constr(0, [])
        val initialDatum = Data.Constr(0, PList.Nil)
        val mintTx = TxBuilder(testEnv)
            .initUtxoFlow(flowDef, initialDatum, Value.ada(10))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"Init transaction should succeed: $mintResult")

        // Verify cell exists with initial datum
        val cellUtxo = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo.isDefined, "Should find flow UTxO by beacon")
        val initialStoredDatum = flowDef.currentDatum(cellUtxo.get)
        assert(initialStoredDatum == initialDatum, "Initial datum should match")

        // 2. Advance chunk 0: Bid redeemer with Bob's real PubKeyHash
        //    Bob signs so requireSignedBy(bid.bidder) passes on-chain
        val bidderPkh = PubKeyHash(Bob.addrKeyHash)
        val bid = Bid(bidder = bidderPkh, amount = BigInt(1_000_000))
        val advanceTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, bid.toData, emulator.utxos)
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction

        val advanceResult = emulator.submitSync(advanceTx)
        assert(advanceResult.isRight, s"Advance (bid) should succeed: $advanceResult")

        // Verify updated datum
        val cellUtxo2 = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo2.isDefined, "Should find flow UTxO after advance")
        val nextDatum = flowDef.currentDatum(cellUtxo2.get)
        nextDatum match
            case Data.Constr(tag, _) =>
                assert(tag == 1, s"Expected tag=1 after advance, got $tag")
            case other =>
                fail(s"Expected Constr datum, got $other")

        // 3. Advance chunk 1: Confirm redeemer (terminal) → beacon burn
        val confirm = Confirm(true)
        val terminalTx = TxBuilder(testEnv)
            .advanceUtxoFlow(flowDef, confirm.toData, emulator.utxos)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val terminalResult = emulator.submitSync(terminalTx)
        assert(terminalResult.isRight, s"Terminal advance should succeed: $terminalResult")

        // 4. Verify flow UTxO is gone (beacon burned)
        val cellUtxo3 = flowDef.findUtxo(emulator.utxos)
        assert(cellUtxo3.isEmpty, "Flow UTxO should be consumed after terminal transition")
    }
}
