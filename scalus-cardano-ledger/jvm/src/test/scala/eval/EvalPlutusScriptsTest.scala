package eval

import io.bullet.borer.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.platform
import scalus.cardano.ledger.*
import scalus.uplc.eval.*

class EvalPlutusScriptsTest extends AnyFunSuite {

    test("evalPlutusScripts with CBOR files") {
        // Read transaction CBOR bytes using platform-specific file I/O
        val tx = platform
            .readFile(
              "scalus-examples/js/src/main/ts/tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )

        // Read UTxO CBOR bytes using platform-specific file I/O
        val utxo = platform
            .readFile(
              "scalus-examples/js/src/main/ts/utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
            )

        // Evaluate Plutus scripts
        val redeemers = evalPlutusScripts(tx, utxo, SlotConfig.Mainnet)

        // Verify results
        assert(redeemers.length == 2, "Should have 2 redeemers evaluated")
    }

    def evalPlutusScripts(
        txCborBytes: Array[Byte],
        utxoCborBytes: Array[Byte],
        slotConfig: SlotConfig
    ): Seq[Redeemer] = {
        val tx = Transaction.fromCbor(txCborBytes)
        val utxo =
            Cbor.decode(utxoCborBytes).to[Map[TransactionInput, TransactionOutput]].value
        val params: ProtocolParams = CardanoInfo.mainnet.protocolParams
        val costModels = params.costModels
        val evaluator = PlutusScriptEvaluator(
          slotConfig = slotConfig,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
          costModels = costModels,
          mode = EvaluatorMode.EvaluateAndComputeCost,
          debugDumpFilesForTesting = false
        )
        evaluator.evalPlutusScripts(tx, utxo)
    }

}
