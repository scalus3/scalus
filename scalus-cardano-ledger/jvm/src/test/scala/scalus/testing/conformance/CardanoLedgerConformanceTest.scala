package scalus.testing.conformance

import org.scalatest.funsuite.*
import scalus.cardano.ledger.EvaluatorMode
import scalus.testing.conformance.CardanoLedgerVectors.*

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  *
  * It runs the scripts with `EvaluatorMode.EvaluateAndComputeCost`.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    // The UTXO, UTXOW and UTXOS vectors, and the DELEG, CERTS and GOVCERT ones, spec [CV-1].
    // Old format: directories contain ".UTXO" (e.g., "Conway.Imp.AlonzoImpSpec.UTXOS...")
    private val txRuleDirs = Seq(".UTXO", ".DELEG.", ".CERTS.", ".GOVCERT.")

    // The GOV, EPOCH, RATIFY, ENACT and LEDGER vectors, spec [CV-2]. Each case is one LEDGER
    // transition, so these dirs test the tx rules and the stored proposals of their scenarios.
    private val govRuleDirs = Seq(".GOV.", ".EPOCH.", ".RATIFY.", ".ENACT.", ".LEDGER.")

    private val ruleDirs = txRuleDirs ++ govRuleDirs

    test("spec [CV-2]: the GOV, EPOCH, RATIFY, ENACT and LEDGER dirs are run") {
        val dirs = vectorNames().filter(v => govRuleDirs.exists(v.contains))
        assert(dirs.size == 56 + 13 + 46 + 16 + 1)
        assert(expectedFailures.keySet.subsetOf(vectorNames().toSet))
    }

    test("spec [CV-8]: each case runs at the epoch its new state records") {
        def epochs(name: String) = runVector(conway + name).map(_.epoch)
        // The new state of case 6 records 919; a later case runs at least at the epoch of an
        // earlier one, D43.
        assert(
          epochs("RATIFY.When CC expired.SPOs alone can't enact hard-fork") ==
              List.fill(6)(899L) ++ List.fill(4)(919L)
        )
        // The new DRep expiries of its vote txs give the epochs 901, 903 and 905.
        assert(
          epochs(
            "UTXOS.PlutusV3 Initialization.Updating CostModels and setting the govPolicy afterwards succeeds"
          ) == List.fill(12)(899L) ++ List.fill(5)(901L) ++ List.fill(8)(903L) ++
              List.fill(4)(905L)
        )
    }

    for vector <- vectorNames()
            .filter(v => ruleDirs.exists(v.contains))
    do {
        test("Conformance test vector: " + vector):
            val problems = checkVector(vector, EvaluatorMode.EvaluateAndComputeCost)
            if problems.nonEmpty then
                fail(s"${problems.size} problem(s):\n${problems.mkString("\n")}")
    }
}
