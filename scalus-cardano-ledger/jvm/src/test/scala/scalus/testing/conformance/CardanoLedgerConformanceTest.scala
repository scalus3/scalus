package scalus.testing.conformance

import org.scalatest.funsuite.*
import scalus.cardano.ledger.rules.CardanoMutator
import scalus.testing.conformance.CardanoLedgerVectors.*

import scala.util.{Failure, Success, Try}

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  *
  * It runs the scripts with `EvaluatorMode.EvaluateAndComputeCost`, the default of `runVector`.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    private def summarizeResult(result: Try[CardanoMutator.Result]): String =
        result match
            case Failure(e) => s"Failure(${e.getClass.getSimpleName}: ${e.getMessage})"
            case Success(Left(e)) =>
                s"Left(${e.getClass.getSimpleName}: ${e.getMessage})"
            case Success(Right(_)) => "Right(...)"

    // The UTXO, UTXOW and UTXOS vectors, and the DELEG, CERTS and GOVCERT ones, spec [CV-1].
    // Old format: directories contain ".UTXO" (e.g., "Conway.Imp.AlonzoImpSpec.UTXOS...")
    private val txRuleDirs = Seq(".UTXO", ".DELEG.", ".CERTS.", ".GOVCERT.")

    // The GOV, EPOCH, RATIFY, ENACT and LEDGER vectors, spec [CV-2]. Each case is one LEDGER
    // transition, so these dirs test the tx rules and the stored proposals of their scenarios.
    private val govRuleDirs = Seq(".GOV.", ".EPOCH.", ".RATIFY.", ".ENACT.", ".LEDGER.")

    private val ruleDirs = txRuleDirs ++ govRuleDirs

    private val conway = "Conway.Imp.ConwayImpSpec - Version 10."

    /** The cases that the ledger and Scalus decide differently, keyed by vector name, each with its
      * cause (D37). Each listed case must still fail, so a fix must remove it from this list.
      */
    private val expectedFailures: Map[String, Set[String]] = Map(
      // A Word64 above Long.MaxValue that `Coin(value: Long)` cannot hold: withdrawal amounts of
      // a TreasuryWithdrawals in the state (EnactSpec.hs:129-137). The harness cannot decode the
      // old state, spec section 20 Q9.
      s"${conway}ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal" ->
          Set("10", "11", "12", "13"),
      // A threshold denominator of 10^19 that `UnitInterval(Long, Long)` cannot hold, in an
      // UpdateCommittee in the state (GovSpec.hs:807-820), spec section 20 Q9.
      s"${conway}GOV.Voting.committee member can not vote on UpdateCommittee action" ->
          Set("2", "3"),
      // The vote comes three epochs after the proposal (`passNEpochs 3`, GovSpec.hs:788-798), and
      // no field of the dump records the passed epochs, so the harness runs the vote at the
      // proposal's epoch, where the action has not expired. The ledger disables this test in its
      // own conformance run (`disableInConformanceIt`). GovMutatorTest covers
      // VotingOnExpiredGovAction, spec [CV-8], D43.
      s"${conway}GOV.Voting.expired gov-actions" -> Set("5")
    )

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
            val runs = runVector(vector)
            // A vector with no cases would pass without checking anything, for example when the
            // extracted vectors lost their files.
            assert(runs.nonEmpty, s"vector $vector has no test cases")
            val expectedToFail = expectedFailures.getOrElse(vector, Set.empty)
            // A thrown exception is no verdict: it matches neither a pass nor a fail.
            val mismatched = runs.filter(run =>
                run.result match
                    case Success(result) => run.success != result.isRight
                    case Failure(_)      => true
            )
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
                    case (true, Success(Right(actual)), Some(expected)) =>
                        LedgerStateComparison
                            .withoutExcluded(
                              vector,
                              LedgerStateComparison.compare(expected, actual),
                              LedgerStateComparison.exclusions
                            )
                            .map(m =>
                                s"  [${run.file}] state field ${m.field} differs: ${m.detail}"
                            )
                    case (true, Success(_), None) =>
                        List(s"  [${run.file}] has no newLedgerState")
                    case _ => Nil
            }
            val all = failures ++ fixed ++ stateMismatches
            if all.nonEmpty then fail(s"${all.size} case(s) failed:\n${all.mkString("\n")}")
    }
}
