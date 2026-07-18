package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.PlutusV3
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.utils.await

/** UTXOs carrying a reference script (deployed scripts) must only be selected as a last resort by
  * `complete()`, both for inputs and for collateral. Spending one destroys the deployed script and
  * adds the Conway tiered ref-script fee.
  */
class RefScriptUtxoSelectionTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    def input(index: Int): TransactionInput = Input(genesisHash, index)

    val refScript: Script.PlutusV3 = PlutusV3.alwaysOk.script

    val refScriptUtxo: TransactionOutput = TransactionOutput(
      Alice.address,
      Value.ada(20),
      None,
      Some(ScriptRef(refScript))
    )

    val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = testEnv.network,
      payment = ShelleyPaymentPart.Script(refScript.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    test("complete does not spend a reference script UTxO when plain funds suffice") {
        val provider = Emulator(
          Map(
            input(0) -> TransactionOutput(Alice.address, Value.ada(100)),
            input(1) -> refScriptUtxo
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val spent = tx.body.value.inputs.toSeq
        assert(
          !spent.contains(input(1)),
          s"complete() selected the reference script UTxO as input; inputs = $spent"
        )
    }

    test("complete does not use a reference script UTxO as collateral when alternatives exist") {
        val lockedUtxo = Utxo(
          input(2),
          TransactionOutput(scriptAddress, Value.ada(5), DatumOption.Inline(Data.I(42)))
        )
        val provider = Emulator(
          Map(
            input(0) -> TransactionOutput(Alice.address, Value.ada(100)),
            input(1) -> refScriptUtxo,
            lockedUtxo.input -> lockedUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(lockedUtxo, Data.I(0), refScript)
            .complete(provider, Alice.address)
            .await()
            .transaction

        val spent = tx.body.value.inputs.toSeq
        val collateral = tx.body.value.collateralInputs.toSeq
        assert(
          !spent.contains(input(1)) && !collateral.contains(input(1)),
          s"reference script UTxO used (inputs = $spent, collateral = $collateral)"
        )
    }

    test("complete spends a reference script UTxO as a last resort") {
        val provider = Emulator(
          Map(input(1) -> refScriptUtxo)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(
          tx.body.value.inputs.toSeq.contains(input(1)),
          "reference script UTxO must still be spendable when it is the only source of funds"
        )
    }

    test("complete uses reference script UTxOs when plain funds only partially cover") {
        val provider = Emulator(
          Map(
            input(0) -> TransactionOutput(Alice.address, Value.ada(8)),
            input(1) -> refScriptUtxo
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(20))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val spent = tx.body.value.inputs.toSeq
        assert(
          spent.contains(input(0)) && spent.contains(input(1)),
          s"expected both plain and ref-script UTxOs to be spent; inputs = $spent"
        )
    }
}
