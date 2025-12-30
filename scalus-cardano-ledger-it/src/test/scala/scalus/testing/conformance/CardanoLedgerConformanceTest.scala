package scalus.testing.conformance

import org.scalatest.funsuite.*
import scalus.cardano.ledger.EvaluatorMode
import scalus.testing.conformance.CardanoLedgerVectors.*

/** Cardano Ledger Conformance Test Suite (Integration Tests with Budget Validation)
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation. Uses EvaluatorMode.Validate which validates that script
  * execution stays within the budget specified in redeemers.
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
    for vector <- vectorNames() do
        test("Conformance test vector: " + vector):
            for
                case (x, success, result) <- testVector(vector, EvaluatorMode.Validate)
                if success != (result.isSuccess && result.get.isRight)
            do fail(s"[$vector/$x]($success) $result")
}
