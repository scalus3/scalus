package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.builtin.ByteString
import scalus.builtin.ToData.*
import scalus.uplc.eval.Result
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.testing.kit.ScalusTest
import scalus.uplc.Program

class TmpValidatorTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironmentWithoutEvaluator
    private val compiledContract = TmpContract.debugCompiledContract

    private val address1 = TestUtil.createTestAddress("a" * 56)
    private val address2 = TestUtil.createTestAddress("b" * 56)

    private val amount1: Long = 100_000_000L
    private val amount2: Long = 50_000_000L

    private val datum = ByteString.fromString("datum").toData
    private val redeemer = ByteString.fromString("redeemer").toData
    private val scriptAddress =
        Address(env.network, Credential.ScriptHash(compiledContract.script.scriptHash))

    private val lockTx: Transaction = {
        val wallet = TestUtil.createTestWallet(address1, amount1 + amount2)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(amount1)

        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs((utxo.input, utxo.output), witness)
        }
        builder.payToScript(scriptAddress, value, datum).build().toOption.get
    }

    private val lockUtxo = TestUtil
        .findUtxoByAddressAndDatum(lockTx, scriptAddress, Some(DatumOption.Inline(datum)))
        .get

    lazy private val unlockTx: (Transaction, Result) = {
        val wallet = TestUtil.createTestWallet(address2, amount2)
        val context = BuilderContext(env, wallet)
        val tx = {
            val (input, output) = lockUtxo
            val witness = ThreeArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptValue(compiledContract.script),
              redeemer = redeemer,
              datum = Datum.DatumInlined,
              additionalSigners = Set.empty
            )

            PaymentBuilder(context)
                .withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(lockUtxo), witness))
                .payTo(address2, output.value)
                .build()
                .toOption
                .get
        }

        val utxos: Utxos = Map(lockUtxo) ++ wallet.utxo
        val result = runValidator(tx, utxos)

        (tx, result)
    }

    private def runValidator(tx: Transaction, utxos: Utxos) = {
        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, lockUtxo._1, RedeemerTag.Spend, env)

        val allScripts = AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val script = scriptAddress.scriptHashOption.flatMap(allScripts.get).get
        val program = Program.fromCborByteString(script.script)

        val result = program.runWithDebug(scriptContext)

        // TODO: Fix alphaEq for Program
        assert(program alphaEq compiledContract.program)
        assert(result alphaEq compiledContract.program.runWithDebug(scriptContext))
        assert(script == compiledContract.script)
        assert(program.cborByteString == compiledContract.program.cborByteString)
        assert(program.cborByteString == compiledContract.script.script)

        result
    }

    ignore("unlock TmpContract UTXO") {
        val (tx, result) = unlockTx
        assert(result.isSuccess)
    }
}
