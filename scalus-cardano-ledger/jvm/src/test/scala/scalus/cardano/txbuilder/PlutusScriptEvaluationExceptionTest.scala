package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.compiler.Options
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.TestUtil.genAdaOnlyPubKeyUtxo

/** Tests that PlutusScriptEvaluationException includes source position when a compiled script fails
  * through the real PlutusScriptEvaluator pipeline.
  */
class PlutusScriptEvaluationExceptionTest extends AnyFunSuite {

    given env: CardanoInfo = CardanoInfo.mainnet

    // Compiled with default options (traces enabled) — source position points to THIS file
    val failingScript: PlutusV3[Data => Unit] = {
        given Options = Options.default
        PlutusV3.compile { (sc: Data) =>
            throw new Exception("always fails")
        }
    }

    def createScriptLockedUtxo(script: PlutusScript): Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        val newOutput = Output(
          address = Address(Mainnet, Credential.ScriptHash(script.scriptHash)),
          value = utxo.output.value,
          inlineDatum = 42.toData,
        )
        utxo.copy(output = newOutput)
    }

    test("PlutusScriptEvaluator produces source position in PlutusScriptEvaluationException") {
        val scriptUtxo = createScriptLockedUtxo(failingScript.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                .spend(scriptUtxo, Data.unit, failingScript.script)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        ex.getCause match
            case evalEx: PlutusScriptEvaluationException =>
                // Source position should be present and point to this file
                val pos = evalEx.failedSourcePosition.getOrElse(
                  fail("failedSourcePosition should be present")
                )
                assert(
                  !pos.isEmpty,
                  s"source position should not be empty, got: $pos"
                )
                assert(
                  pos.file.contains("PlutusScriptEvaluationExceptionTest.scala"),
                  s"source position should reference this file, got: ${pos.show}"
                )

                // The error message should include the source position
                assert(
                  evalEx.getMessage.contains("\nat "),
                  s"error message should contain source position, got:\n${evalEx.getMessage}"
                )
                assert(
                  evalEx.getMessage.contains("PlutusScriptEvaluationExceptionTest.scala"),
                  s"error message should contain source file name, got:\n${evalEx.getMessage}"
                )

                // Script hash should match
                assert(
                  evalEx.failedScriptHash == failingScript.script.scriptHash,
                  "failedScriptHash should match the script"
                )
            case other =>
                fail(s"Expected PlutusScriptEvaluationException, got: ${other.getClass.getName}")
    }
}
