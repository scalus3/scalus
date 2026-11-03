package scalus.testing.conformance

import org.scalatest.funsuite.*
import scalus.cardano.ledger.rules.CardanoMutator
import scalus.testing.conformance.CardanoLedgerVectors.*

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  *
  * This version uses EvaluateNoScripts mode to skip budget checking, making it suitable for fast
  * build tests. For full conformance testing with budget validation, use the integration tests in
  * scalus-cardano-ledger-it module.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    private def summarizeResult(result: scala.util.Try[CardanoMutator.Result]): String =
        result match
            case scala.util.Failure(e) => s"Failure(${e.getClass.getSimpleName}: ${e.getMessage})"
            case scala.util.Success(Left(e)) =>
                s"Left(${e.getClass.getSimpleName}: ${e.getMessage})"
            case scala.util.Success(Right(_)) => "Right(...)"

    // The UTXO, UTXOW and UTXOS vectors, and the DELEG, CERTS and GOVCERT ones, spec [CV-1].
    // Old format: directories contain ".UTXO" (e.g., "Conway.Imp.AlonzoImpSpec.UTXOS...")
    private val ruleDirs = Seq(".UTXO", ".DELEG.", ".CERTS.", ".GOVCERT.")

    private val govCert = "Conway.Imp.ConwayImpSpec - Version 10.GOVCERT."

    /** The cases that the ledger rejects and Scalus accepts, because Scalus keeps no committee
      * state: `ConwayCommitteeHasPreviouslyResigned` and `ConwayCommitteeIsUnknown`
      * (C/Rules/GovCert.hs:194-208). Phase 2 of the ledger rules spec adds them (sections 5.7 and
      * 11.2, `2026-11-02-emulator-ledger-rules-design.md`). Each listed case must still fail, so
      * phase 2 must remove it from this list.
      */
    private val expectedFailures: Map[String, Set[String]] = Map(
      s"${govCert}fails for.registering a resigned CC member hotkey" -> Set("3", "6"),
      s"${govCert}fails for.resigning a nonexistent CC member hotkey" -> Set("1"),
      s"${govCert}succeeds for.resigning a non-CC key" -> Set("0")
    )

    for vector <- vectorNames()
            .filter(v => ruleDirs.exists(v.contains))
            .filterNot(_.contains("Bootstrap Witness"))
    do {
        test("Conformance test vector: " + vector):
            val runs = runVector(vector)
            // A vector with no cases would pass without checking anything, for example when the
            // extracted vectors lost their files.
            assert(runs.nonEmpty, s"vector $vector has no test cases")
            val expectedToFail = expectedFailures.getOrElse(vector, Set.empty)
            val mismatched =
                runs.filter(run => run.success != (run.result.isSuccess && run.result.get.isRight))
            val failures = for
                run <- mismatched
                if !expectedToFail.contains(run.file)
            yield s"  [${run.file}] expected=${if run.success then "pass" else "fail"}, got=${summarizeResult(run.result)}"
            val fixed = (expectedToFail -- mismatched.map(_.file)).toSeq.sorted.map(file =>
                s"  [$file] is an expected failure but now matches the ledger: remove it from expectedFailures"
            )
            // spec [SC-14]: a vector that expects success must also reach its newLedgerState
            val stateMismatches = runs.flatMap { run =>
                (run.success, run.result, run.newLedgerState) match
                    case (true, scala.util.Success(Right(actual)), Some(expected)) =>
                        LedgerStateComparison
                            .withoutExcluded(
                              vector,
                              LedgerStateComparison.compare(expected, actual),
                              LedgerStateComparison.exclusions
                            )
                            .map(m =>
                                s"  [${run.file}] state field ${m.field} differs: ${m.detail}"
                            )
                    case (true, _, None) => List(s"  [${run.file}] has no newLedgerState")
                    case _               => Nil
            }
            val all = failures ++ fixed ++ stateMismatches
            if all.nonEmpty then fail(s"${all.size} case(s) failed:\n${all.mkString("\n")}")
    }
}
