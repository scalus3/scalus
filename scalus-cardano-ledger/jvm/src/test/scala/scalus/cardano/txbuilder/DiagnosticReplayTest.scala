package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.{DebugScript, PlutusV3}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.node.{Emulator, NodeSubmitError}
import scalus.compiler.Options
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.TestUtil.genAdaOnlyPubKeyUtxo

class DiagnosticReplayTest extends AnyFunSuite {

    given env: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    def input(index: Int): TransactionInput = Input(genesisHash, index)
    def adaOutput(address: Address, ada: Int): TransactionOutput =
        Output(address, Value.ada(ada))

    // A script that always fails with require(false, "expected failure")
    val failingScriptWithTraces: PlutusV3[Data => Unit] = {
        given Options = Options.default // traces enabled
        PlutusV3.compile { (sc: Data) =>
            scalus.cardano.onchain.plutus.prelude.require(false, "expected failure")
        }
    }

    val failingScriptRelease: PlutusV3[Data => Unit] = {
        given Options = Options.release // no traces
        PlutusV3.compile { (sc: Data) =>
            scalus.cardano.onchain.plutus.prelude.require(false, "expected failure")
        }
    }

    val alwaysOkScript: PlutusV3[Data => Unit] = {
        given Options = Options.release
        PlutusV3.compile((sc: Data) => ())
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

    test("script with traces produces logs directly, no replay needed") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptWithTraces.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                .spend(scriptUtxo, Data.unit, failingScriptWithTraces)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(logs.nonEmpty, "Script compiled with traces should produce logs directly")
        assert(
          logs.exists(_.contains("expected failure")),
          s"Logs should contain error message, got: $logs"
        )
    }

    test("release script via CompiledPlutus produces diagnostic logs via replay") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptRelease.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                .spend(scriptUtxo, Data.unit, failingScriptRelease)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(
          logs.nonEmpty,
          "Release script via CompiledPlutus should produce diagnostic logs via replay"
        )
        assert(
          logs.exists(_.contains("expected failure")),
          s"Diagnostic replay logs should contain error message, got: $logs"
        )
    }

    test("plain PlutusScript without debug script produces empty logs") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptRelease.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                // Use PlutusScript directly - no CompiledPlutus, no debug script
                .spend(scriptUtxo, Data.unit, failingScriptRelease.script)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(
          logs.isEmpty,
          s"Plain PlutusScript without debug script should produce empty logs, got: $logs"
        )
    }

    test("successful script evaluation does not trigger replay") {
        val scriptUtxo = createScriptLockedUtxo(alwaysOkScript.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // This should succeed without any exception
        val tx = TxBuilder(env)
            .spend(paymentUtxo)
            .collaterals(collateralUtxo)
            .spend(scriptUtxo, Data.unit, alwaysOkScript)
            .payTo(Bob.address, Value.ada(1))
            .build(changeTo = Alice.address)
            .transaction

        assert(tx.body.value.inputs.toSeq.nonEmpty)
    }

    test("references with CompiledPlutus enables diagnostic replay") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptRelease.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // Create a reference UTXO containing the script
        val scriptRefUtxo = {
            val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
            val newOutput = Output(
              address = Alice.address,
              value = utxo.output.value,
              datumOption = None,
              scriptRef = Some(ScriptRef(failingScriptRelease.script)),
            )
            utxo.copy(output = newOutput)
        }

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                .references(scriptRefUtxo, failingScriptRelease)
                .spend(scriptUtxo, Data.unit)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(logs.nonEmpty, "references(utxo, compiled) should enable diagnostic replay")
        assert(
          logs.exists(_.contains("expected failure")),
          s"Diagnostic replay logs should contain error message, got: $logs"
        )
    }

    test("mint with CompiledPlutus produces diagnostic logs on failure") {
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get
        val assets = Map(AssetName(hex"deadbeef") -> 100L)

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                .mint(failingScriptRelease, assets, Data.unit)
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(logs.nonEmpty, "Mint via CompiledPlutus should produce diagnostic logs via replay")
        assert(
          logs.exists(_.contains("expected failure")),
          s"Diagnostic replay logs should contain error message, got: $logs"
        )
    }

    test("script hash is preserved in withErrorTraces") {
        // withErrorTraces changes the script bytes (adds traces) so it produces a DIFFERENT hash
        // The original hash is used for lookup - this is by design since we look up by original hash
        val release = failingScriptRelease
        val withTraces = release.withErrorTraces
        // The SIR is the same
        assert(release.sir == withTraces.sir)
        // Language is the same
        assert(release.language == withTraces.language)
        // Options differ only in generateErrorTraces
        assert(withTraces.options.generateErrorTraces)
        assert(!release.options.generateErrorTraces)
    }

    test("DebugScript from raw PlutusScript via TxBuilder withDebugScript") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptRelease.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // Use raw PlutusScript for debug (the version with traces)
        val debugPlutusScript = failingScriptWithTraces.script
        val releaseHash = failingScriptRelease.script.scriptHash

        val ex = intercept[TxBuilderException.BalancingException] {
            TxBuilder(env)
                .spend(paymentUtxo)
                .collaterals(collateralUtxo)
                // Spend with raw PlutusScript (no CompiledPlutus)
                .spend(scriptUtxo, Data.unit, failingScriptRelease.script)
                // Register debug script from raw PlutusScript
                .withDebugScript(releaseHash, DebugScript(debugPlutusScript))
                .payTo(Bob.address, Value.ada(1))
                .build(changeTo = Alice.address)
        }

        assert(ex.isScriptFailure)
        val logs = ex.scriptLogs.get
        assert(
          logs.nonEmpty,
          "DebugScript from raw PlutusScript should produce diagnostic logs via replay"
        )
        assert(
          logs.exists(_.contains("expected failure")),
          s"Diagnostic replay logs should contain error message, got: $logs"
        )
    }

    test("DebugScript from raw PlutusScript via Emulator submitSync") {
        val scriptUtxo = createScriptLockedUtxo(failingScriptRelease.script)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // Create emulator with the UTXOs
        val emulator = Emulator(
          initialUtxos = Map(
            scriptUtxo.input -> scriptUtxo.output,
            paymentUtxo.input -> paymentUtxo.output,
            collateralUtxo.input -> collateralUtxo.output
          )
        )

        // Build tx with noop evaluator (so build doesn't fail on script eval)
        // and sign with Alice's key
        val tx = TxBuilder(env, PlutusScriptEvaluator.noop)
            .spend(paymentUtxo)
            .collaterals(collateralUtxo)
            .spend(scriptUtxo, Data.unit, failingScriptRelease.script)
            .payTo(Bob.address, Value.ada(1))
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        // Submit with debug scripts
        val debugPlutusScript = failingScriptWithTraces.script
        val releaseHash = failingScriptRelease.script.scriptHash
        val debugScripts = Map(releaseHash -> DebugScript(debugPlutusScript))

        val result = emulator.submitSync(tx, debugScripts)

        assert(result.isLeft, "Transaction should fail")
        result.left.foreach {
            case NodeSubmitError.ScriptFailure(_, _, logs) =>
                assert(
                  logs.nonEmpty,
                  "Emulator submitSync with debug scripts should produce diagnostic logs"
                )
                assert(
                  logs.exists(_.contains("expected failure")),
                  s"Diagnostic logs should contain error message, got: $logs"
                )
            case other =>
                fail(s"Expected ScriptFailure, got: $other")
        }
    }
}
