package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.DeBruijnedProgram

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Regression tests for Plutus use-case benchmarks from
  * https://saib-inc.github.io/cardano-plutus-vm-benchmark/
  *
  * These tests ensure that the Scalus CEK machine can evaluate all benchmark scripts. Previously,
  * the benchmark used PlutusV1VM which doesn't support Case-on-builtins (UPLC 1.1.0 / CIP-0137),
  * causing coop and guardrail scripts to fail with NonConstrScrutinized.
  */
class PlutusUseCasesBenchmarkTest extends AnyFunSuite {

    private val dataDir =
        Paths.get("scalus-core/jvm/src/test/resources/data/plutus_use_cases")

    // Use PlutusV2 VM with vanRossemPV:
    // - V2 language semantics: no CIP-117 Unit return requirement (these are raw UPLC programs)
    // - vanRossemPV protocol version: enables Case-on-builtins (CIP-0137)
    private val vm = PlutusVM.makePlutusV2VM(MajorProtocolVersion.vanRossemPV)

    private def allFlatFiles: Seq[Path] = {
        Files
            .list(dataDir)
            .iterator()
            .asScala
            .filter(_.toString.endsWith(".flat"))
            .toSeq
            .sortBy(_.getFileName.toString)
    }

    private def scriptName(p: Path): String =
        p.getFileName.toString.stripSuffix(".flat")

    // Scripts that previously failed on CEK with PlutusV1VM (NonConstrScrutinized)
    // These use Case on Bool (UPLC 1.1.0 / CIP-0137)
    private val caseOnBoolScripts = Set(
      "coop-1",
      "coop-2",
      "coop-3",
      "coop-4",
      "coop-5",
      "coop-6",
      "coop-7",
      "guardrail-sorted-large",
      "guardrail-sorted-small",
      "guardrail-unsorted-large",
      "guardrail-unsorted-small"
    )

    for file <- allFlatFiles do {
        val name = scriptName(file)
        test(s"CEK evaluates $name") {
            val bytes = Files.readAllBytes(file)
            val program = DeBruijnedProgram.fromFlatEncoded(bytes)
            // Should not throw - all scripts should evaluate successfully
            // with vanRossemPV protocol version that supports Case on builtins
            val result =
                vm.evaluateScript(program, RestrictingBudgetSpender(ExUnits.enormous), NoLogger)
            assert(result != null, s"$name should produce a result")
        }
    }

    // Verify that Case-on-Bool scripts specifically fail with PlutusV1VM
    // (documenting the root cause of the benchmark failures)
    for name <- caseOnBoolScripts.toSeq.sorted do {
        test(s"CEK PlutusV1 rejects Case-on-Bool in $name") {
            val v1vm = PlutusVM.makePlutusV1VM()
            val bytes = Files.readAllBytes(dataDir.resolve(s"$name.flat"))
            val program = DeBruijnedProgram.fromFlatEncoded(bytes)
            assertThrows[NonConstrScrutinized] {
                v1vm.evaluateScript(
                  program,
                  RestrictingBudgetSpender(ExUnits.enormous),
                  NoLogger
                )
            }
        }
    }
}
