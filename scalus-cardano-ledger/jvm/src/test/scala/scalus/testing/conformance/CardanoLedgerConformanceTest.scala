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
            val failures = for
                case (x, success, result) <- testVector(vector)
                if success != (result.isSuccess && result.get.isRight)
            yield s"  [$x] expected=${if success then "pass" else "fail"}, got=${summarizeResult(result)}"
            if failures.nonEmpty then
                fail(s"${failures.size} case(s) failed:\n${failures.mkString("\n")}")
    }
}
