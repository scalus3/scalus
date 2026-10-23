package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.node.{Emulator, SubmitError}
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

    // ParameterizedFlowValidator checks requireSignedBy(p.seller) in chunk 0, where p is decoded
    // from the script parameter. The parameter must come from the script, not from the datum.
    private val sellerParams = FlowParams(PubKeyHash(Alice.addrKeyHash), BigInt(100))
    private val paramFlowDef = new UtxoFlowDef(
      compiled = ParameterizedFlowCompilation.compiled(sellerParams.toData),
      tokenName = ParameterizedFlowValidator.beaconName,
      flowDispatch = ParameterizedFlowValidator.flowDispatch(sellerParams.toData)
    )

    private def paramFlowEmulator(): Emulator = Emulator(
      initialUtxos = Map(
        Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000)),
        Input(genesisHash, 1) -> Output(Bob.address, Value.ada(1000))
      )
    )

    /** Bob creates the cell with `initialDatum`. The builder does not run the script, so a bad
      * datum reaches the emulator, which runs the mint policy.
      */
    private def mintParamFlow(
        emulator: Emulator,
        initialDatum: Data
    ): Either[SubmitError, TransactionHash] = {
        val mintTx = TxBuilder(testEnv, PlutusScriptEvaluator.constMaxBudget(testEnv))
            .initUtxoFlow(paramFlowDef, initialDatum, Value.ada(10))
            .complete(emulator.utxos, Bob.address)
            .sign(Bob.signer)
            .transaction
        emulator.submitSync(mintTx)
    }

    private def assertMintRejected(initialDatum: Data): Unit = {
        mintParamFlow(paramFlowEmulator(), initialDatum) match
            case Left(SubmitError.ScriptFailure(message, logs, _, _)) =>
                assert(
                  (message +: logs).exists(
                    _.contains("UtxoFlow: initial datum must be the entry state")
                  ),
                  s"Unexpected script failure: $message"
                )
            case other => fail(s"Mint with a forged initial datum must fail, got $other")
    }

    private val bid = Bid(bidder = PubKeyHash(Bob.addrKeyHash), amount = BigInt(1_000_000))

    test("parameterized flow: the seller from the script parameter signs chunk 0") {
        val emulator = paramFlowEmulator()
        val mintResult = mintParamFlow(emulator, Data.Constr(0, PList.Nil))
        assert(mintResult.isRight, s"Init transaction should succeed: $mintResult")
        val advanceTx = TxBuilder(testEnv)
            .advanceUtxoFlow(paramFlowDef, bid.toData, emulator.utxos)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        val result = emulator.submitSync(advanceTx)
        assert(result.isRight, s"Advance signed by the parameter seller should succeed: $result")
    }

    test("Fail: parameterized flow: an initial datum cannot carry chunk 0 fields") {
        // Bob tries to put his own FlowParams into the first datum. The script parameter says
        // Alice is the seller, and chunk 0 reads it from the parameter, not from the datum.
        val bobParams = FlowParams(PubKeyHash(Bob.addrKeyHash), BigInt(100))
        assertMintRejected(Data.Constr(0, PList.Cons(bobParams.toData, PList.Nil)))
    }

    test("Fail: parameterized flow: an initial datum cannot start at a later chunk") {
        // Constr(1, []) would resume at chunk 1 and skip requireSignedBy(p.seller) in chunk 0
        assertMintRejected(Data.Constr(1, PList.Nil))
    }
}
