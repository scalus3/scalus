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

    test("profile = Full writes per-script HTML + CSV reports") {
        val dir = Files.createTempDirectory("scalus-profile-test")
        try {
            val report = EvaluatorReportConfig(
              enabled = true,
              outputDir = dir.toString,
              artifacts = Set.empty, // profile only, no .flat dump
              profile = ProfileLevel.Full
            )
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)

            val files = Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .map(_.getName)
            assert(
              files.exists(_.endsWith(".profile.html")),
              s"expected a profile.html, got ${files.mkString(", ")}"
            )
            assert(
              files.exists(_.endsWith(".profile.csv")),
              s"expected a profile.csv, got ${files.mkString(", ")}"
            )
            val htmlName = files.find(_.endsWith(".profile.html")).get
            val html = new String(Files.readAllBytes(dir.resolve(htmlName)), "UTF-8")
            assert(html.contains("Scalus CEK Machine Profile"))
            assert(html.contains("By Source Location"))
            // Execution-unit prices are always attached, so the derived fee columns render.
            assert(html.contains("Fee (lov)"), "expected a fee column in the HTML report")
            assert(html.contains("fee=") && html.contains("ADA"), "expected a total fee line")
            val csvName = files.find(_.endsWith(".profile.csv")).get
            val csv = new String(Files.readAllBytes(dir.resolve(csvName)), "UTF-8")
            assert(
              csv.startsWith("section,key,detail,count,mem,cpu,fee\n"),
              "expected a fee column in the CSV report"
            )
        } finally
            Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .foreach(_.delete())
            Files.deleteIfExists(dir)
    }

    test("profile = Full writes schemaVersion'd profile.json and a profile-manifest.json") {
        val dir = Files.createTempDirectory("scalus-profile-manifest-test")
        try {
            val report = EvaluatorReportConfig(
              enabled = true,
              outputDir = dir.toString,
              artifacts = Set.empty, // profile only, no .flat dump
              profile = ProfileLevel.Full
            )
            // Two evaluations of the same tx must overwrite manifest runs, not duplicate them.
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)

            val files = Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .map(_.getName)

            val jsonName = files.find(_.endsWith(".profile.json")).get
            val profileJson = new String(Files.readAllBytes(dir.resolve(jsonName)), "UTF-8")
            assert(profileJson.contains("\"schemaVersion\": 1"))

            val manifest = new String(
              Files.readAllBytes(dir.resolve("profile-manifest.json")),
              "UTF-8"
            )
            assert(manifest.contains("\"schemaVersion\": 1"))
            // The tx runs 2 scripts; re-evaluation must not duplicate the runs.
            assert(
              "\"scriptHash\"".r.findAllIn(manifest).size == 2,
              s"expected 2 runs in manifest:\n$manifest"
            )
            assert(manifest.contains("\"language\""))
            assert(manifest.contains("\"redeemer\""))
            assert(manifest.contains("\"budget\""))
            // Every file the manifest lists must exist on disk.
            val listed = "\"file\": \"([^\"]+)\"".r.findAllMatchIn(manifest).map(_.group(1)).toSeq
            assert(listed.nonEmpty)
            listed.foreach { f =>
                assert(Files.exists(dir.resolve(f)), s"manifest lists missing file $f")
            }
        } finally
            Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .foreach(_.delete())
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
