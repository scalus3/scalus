package scalus.testing.conformance

import org.scalatest.funsuite.*
import scalus.cardano.ledger.EvaluatorMode
import scalus.testing.conformance.CardanoLedgerVectors.*

/** Cardano Ledger Conformance Test Suite (Integration Tests with Budget Validation)
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation. Uses EvaluatorMode.Validate which validates that script
  * execution stays within the budget specified in redeemers, spec `[kit.vectors.it-mode]`. The
  * check is `checkVector`, the same as in the JVM suite, spec `[kit.vectors.both-suites]`.
  *
  * ==Test Vector Format (December 2025)==
  *
  * Test vectors use the new format with named directories (e.g., "UTXOS.PlutusV1.can use reference
  * scripts"). The vectors were regenerated from latest cardano-ledger ImpSpec tests and include:
  *   - `pparamsHash` field for protocol parameters lookup
  *   - `testState` field with the test path name
  *
  * @see
  *   `scalus-cardano-ledger-it/src/test/resources/TEST_VECTORS.md` for vector regeneration
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {
    // Without the vectors the suite would register no vector test and pass.
    test("the conformance vectors are on the classpath") {
        assert(vectorNames().nonEmpty)
    }

    for vector <- vectorNames() do
        test("Conformance test vector: " + vector):
            val problems = checkVector(vector, EvaluatorMode.Validate)
            if problems.nonEmpty then
                fail(s"${problems.size} problem(s):\n${problems.mkString("\n")}")
}
