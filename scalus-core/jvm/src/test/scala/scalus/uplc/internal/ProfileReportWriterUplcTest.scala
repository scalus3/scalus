package scalus.uplc.internal

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{EvaluatorReportConfig, ExUnits, ProfileDestination, ProfileFormat, ProfileLevel, ProfileOutput}
import scalus.uplc.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.uplc.eval.ProfilingData
import scalus.utils.ScalusSourcePos

import java.nio.file.{Files, Path}

import UplcSourceMapRenderer.given

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

    private def fullReport(dir: Path) = EvaluatorReportConfig(
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
        val map = readFromArray[UplcSourceMap](Files.readAllBytes(uplcFile))
        assert(map.schemaVersion == 1)
        assert(map.uplc.contains("addInteger"))
        val manifest =
            new String(Files.readAllBytes(dir.resolve("profile-manifest.json")), "UTF-8")
        assert(manifest.contains("\"uplc\""))
        assert(manifest.contains("cafe01-Spend-0.uplc.json"))
    }

    test("each redeemer of one script gets the artifact, with identical content") {
        val dir = Files.createTempDirectory("scalus-uplc-test6")
        def writeFor(index: Int): Unit = ProfileReportWriter.write(
          emptyProfile,
          fullReport(dir),
          "cafe06",
          "PlutusV3",
          "Spend",
          index,
          _ => (),
          Some(annotated)
        )
        writeFor(0)
        writeFor(1) // second redeemer: served from the per-script render cache
        val first = Files.readAllBytes(dir.resolve("cafe06-Spend-0.uplc.json"))
        val second = Files.readAllBytes(dir.resolve("cafe06-Spend-1.uplc.json"))
        assert(first.sameElements(second))
    }

    test("a cache hit skips rewriting an existing artifact") {
        val dir = Files.createTempDirectory("scalus-uplc-test7")
        def writeOnce(): Unit = ProfileReportWriter.write(
          emptyProfile,
          fullReport(dir),
          "cafe07",
          "PlutusV3",
          "Spend",
          0,
          _ => (),
          Some(annotated)
        )
        writeOnce()
        val uplcFile = dir.resolve("cafe07-Spend-0.uplc.json")
        assert(Files.exists(uplcFile))
        // Documented trade-off: the same (path, term) is written once; an externally deleted
        // file is not restored until the script recompiles or the cache entry is evicted.
        Files.delete(uplcFile)
        writeOnce()
        assert(!Files.exists(uplcFile))
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

    test("no artifact, and no manifest, for a console-only report") {
        val dir = Files.createTempDirectory("scalus-uplc-test5")
        val consoleOnly = EvaluatorReportConfig(
          enabled = true,
          outputDir = dir.toString,
          profile = ProfileLevel.Full,
          profileOutputs = Seq(ProfileOutput(ProfileFormat.Text, ProfileDestination.Console))
        )
        var consoleOutput = ""
        ProfileReportWriter.write(
          emptyProfile,
          consoleOnly,
          "cafe05",
          "PlutusV3",
          "Spend",
          0,
          line => consoleOutput += line,
          Some(annotated)
        )
        assert(consoleOutput.nonEmpty, "the console rendering itself must still happen")
        assert(!Files.exists(dir.resolve("cafe05-Spend-0.uplc.json")))
        assert(!Files.exists(dir.resolve("profile-manifest.json")))
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
