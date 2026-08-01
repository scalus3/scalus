package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{EvaluatorReportConfig, ExUnits, ProfileLevel}
import scalus.uplc.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.utils.ScalusSourcePos

import java.nio.file.Files

/** The `<key>.uplc.json` artifact [[ProfileReportWriter]] writes next to a full profile, and its
  * `"uplc"` entry in `profile-manifest.json`.
  */
class ProfileReportWriterUplcTest extends AnyFunSuite {

    private val annotated: Term =
        Term.Builtin(AddInteger, UplcAnnotation(ScalusSourcePos("/src/A.scala", 3, 0, 3, 5), "f"))

    private val emptyProfile: ProfilingData = ProfilingData(
      bySourceLocation = Nil,
      byFunction = Nil,
      byLocationFunction = Nil,
      transitions = Nil,
      totalBudget = ExUnits(memory = 0, steps = 0)
    )

    private def fullReport(dir: java.nio.file.Path) = EvaluatorReportConfig(
      enabled = true,
      outputDir = dir.toString,
      profile = ProfileLevel.Full
    )

    test("uplc.json is written and indexed in the manifest") {
        val dir = Files.createTempDirectory("scalus-uplc-test")
        ProfileReportWriter.write(
          emptyProfile,
          fullReport(dir),
          "cafe01",
          "PlutusV3",
          "Spend",
          0,
          _ => (),
          Some(annotated)
        )
        val uplcFile = dir.resolve("cafe01-Spend-0.uplc.json")
        assert(Files.exists(uplcFile))
        val json = new String(Files.readAllBytes(uplcFile), "UTF-8")
        assert(json.contains("\"schemaVersion\":1") || json.contains("\"schemaVersion\": 1"))
        assert(json.contains("addInteger"))
        val manifest =
            new String(Files.readAllBytes(dir.resolve("profile-manifest.json")), "UTF-8")
        assert(manifest.contains("\"uplc\""))
        assert(manifest.contains("cafe01-Spend-0.uplc.json"))
    }

    test("no artifact for a term without source info") {
        val dir = Files.createTempDirectory("scalus-uplc-test2")
        ProfileReportWriter.write(
          emptyProfile,
          fullReport(dir),
          "cafe02",
          "PlutusV3",
          "Spend",
          0,
          _ => (),
          Some(Term.Const(Constant.Integer(1)))
        )
        assert(!Files.exists(dir.resolve("cafe02-Spend-0.uplc.json")))
    }

    test("no artifact below profile level Full") {
        val dir = Files.createTempDirectory("scalus-uplc-test3")
        val summary = EvaluatorReportConfig(
          enabled = true,
          outputDir = dir.toString,
          profile = ProfileLevel.Summary
        )
        ProfileReportWriter.write(
          emptyProfile,
          summary,
          "cafe03",
          "PlutusV3",
          "Spend",
          0,
          _ => (),
          Some(annotated)
        )
        assert(!Files.exists(dir.resolve("cafe03-Spend-0.uplc.json")))
    }

    test("no artifact when no term is passed") {
        val dir = Files.createTempDirectory("scalus-uplc-test4")
        ProfileReportWriter.write(
          emptyProfile,
          fullReport(dir),
          "cafe04",
          "PlutusV3",
          "Spend",
          0,
          _ => ()
        )
        assert(!Files.exists(dir.resolve("cafe04-Spend-0.uplc.json")))
    }
}
