package eval

import io.bullet.borer.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform
import scalus.cardano.ledger.*

import java.nio.file.Files

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
        val redeemers = evalPlutusScripts(tx, utxo, SlotConfig.mainnet)

        // Verify results
        assert(redeemers.length == 2, "Should have 2 redeemers evaluated")
    }

    test("report dumps use stable names + manifest and overwrite on re-evaluation") {
        val dir = Files.createTempDirectory("scalus-dump-test")
        try {
            val report = EvaluatorReportConfig(
              enabled = true,
              outputDir = dir.toString,
              artifacts = Set(DumpArtifact.Flat)
            )

            // Two evaluations of the same tx must not accumulate duplicate files.
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)

            val flats = Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .map(_.getName)
                .filter(_.endsWith(".flat"))
                .sorted

            assert(flats.length == 2, s"expected 2 stable .flat files, got ${flats.mkString(", ")}")
            assert(
              flats.forall(_.matches(".*-PlutusV\\d-.*\\.flat")),
              s"flat names should encode scriptHash/language/tag/index, got ${flats.mkString(", ")}"
            )

            val manifest = new String(
              Files.readAllBytes(dir.resolve("manifest.json")),
              "UTF-8"
            )
            assert(manifest.contains("\"txId\""))
            assert(manifest.contains("\"scripts\""))
            assert(manifest.contains("\"spentBudget\""))
        } finally
            // Best-effort cleanup of the temp dir
            Option(dir.toFile.listFiles()).getOrElse(Array.empty[java.io.File]).foreach(_.delete())
            Files.deleteIfExists(dir)
    }

    private lazy val tx7430: Array[Byte] = platform.readFile(
      "scalus-examples/js/src/main/ts/tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
    )
    private lazy val utxo7430: Array[Byte] = platform.readFile(
      "scalus-examples/js/src/main/ts/utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"
    )

    def evalPlutusScripts(
        txCborBytes: Array[Byte],
        utxoCborBytes: Array[Byte],
        slotConfig: SlotConfig,
        report: EvaluatorReportConfig = EvaluatorReportConfig.disabled
    ): Seq[Redeemer] = {
        val tx = Transaction.fromCbor(txCborBytes)
        val utxo =
            Cbor.decode(utxoCborBytes).to[Map[TransactionInput, TransactionOutput]].value
        val params: ProtocolParams = CardanoInfo.mainnet.protocolParams
        val costModels = params.costModels
        val evaluator = PlutusScriptEvaluator(
          slotConfig = slotConfig,
          initialBudget = ExUnits.enormous,
          protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
          costModels = costModels,
          mode = EvaluatorMode.EvaluateAndComputeCost,
          report = report,
          logBudgetDifferences = false
        )
        evaluator.evalPlutusScripts(tx, utxo)
    }

}
