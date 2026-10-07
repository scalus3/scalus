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

    // Test all vectors with UTXO cases
    // Old format: directories contain ".UTXO" (e.g., "Conway.Imp.AlonzoImpSpec.UTXOS...")
    for vector <- vectorNames()
            .filter(v => v.contains(".UTXO"))
            .filterNot(_.contains("Bootstrap Witness"))
    do {
        test("Conformance test vector: " + vector):
            val runs = runVector(vector)
            val failures = for
                run <- runs
                if run.success != (run.result.isSuccess && run.result.get.isRight)
            yield s"  [${run.file}] expected=${if run.success then "pass" else "fail"}, got=${summarizeResult(run.result)}"
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
            val all = failures ++ stateMismatches
            if all.nonEmpty then fail(s"${all.size} case(s) failed:\n${all.mkString("\n")}")
    }
}
