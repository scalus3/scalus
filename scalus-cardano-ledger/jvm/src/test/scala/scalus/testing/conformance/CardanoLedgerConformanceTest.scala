package scalus.testing.conformance

import org.scalatest.funsuite.*
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

    // Test all vectors with UTXO cases
    // Old format: directories contain ".UTXO" (e.g., "Conway.Imp.AlonzoImpSpec.UTXOS...")
    for vector <- vectorNames()
            .filter(v => v.contains(".UTXO"))
            .filterNot(_.contains("Bootstrap Witness"))
    do {
        test("Conformance test vector: " + vector):
            for
                case (x, success, result) <- testVector(vector)
                if success != (result.isSuccess && result.get.isRight)
            do fail(s"[$vector/$x]($success) $result")
    }
}
