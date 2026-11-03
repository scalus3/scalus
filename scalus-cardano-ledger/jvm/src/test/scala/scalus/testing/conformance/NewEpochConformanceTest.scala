package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{GovAction, GovActionId, GovActionState}
import scalus.cardano.ledger.epoch.internal.{EpochState, EpochTransition}
import scalus.testing.conformance.LedgerState.*
import scalus.testing.conformance.LedgerStateComparison.{Exclusion, FieldMismatch}
import scalus.testing.conformance.NewEpochVectors.Case

import scala.util.{Failure, Try}

/** Runs the epoch transition on the Dugite NEWEPOCH dumps and compares the result with the dump's
  * own output state, spec [CV-9] to [CV-12].
  */
class NewEpochConformanceTest extends AnyFunSuite {

    private lazy val loaded: List[(String, Try[Case])] =
        NewEpochVectors.caseNames().map(name => name -> Try(NewEpochVectors.load(name)))

    private lazy val cases: List[Case] = loaded.flatMap(_._2.toOption)

    /** Spec [CV-12], [CV-13]: the fields of a case whose pulser enacts nothing that phase 3 does
      * not own. Each entry names the phase that removes it.
      */
    private val phase3Exclusions: List[Exclusion] = List(
      Exclusion("fees", "phase 4 [CV-13]", ""),
      Exclusion("instantStake", "phase 4 [CV-13]", "")
    )

    /** Spec [CV-12], [CV-13]: in a case whose pulser enacts an action, every field but `pools.*`
      * and `donation` depends on the enactment of phase 5.
      */
    private val enactmentExclusions: List[Exclusion] = phase3Exclusions ++ List(
      "utxo",
      "deposited",
      "govState.proposals",
      "govState.roots",
      "govState.committee",
      "govState.constitution",
      "accounts",
      "accounts.balance",
      "accounts.deposit",
      "accounts.stakePoolDelegation",
      "accounts.dRepDelegation",
      "dreps",
      "dreps.expiry",
      "dreps.anchor",
      "dreps.deposit",
      "dreps.delegates",
      "committeeState",
      "dreps.numDormantEpochs"
    ).map(Exclusion(_, "phase 5 [CV-13]", ""))

    /** The cases the runner cannot decode, by case name, each with its cause (D37). */
    private val undecodable: Map[String, String] = Map(
      // A TreasuryWithdrawals in the gov state withdraws more than Long.MaxValue, which
      // `Coin(value: Long)` cannot hold (D44). The dump has no test name; the Imp scenario with
      // such a proposal is "Withdrawals exceeding maxBound Word64 submitted in a single proposal"
      // (conway Imp/EnactSpec.hs:129-137), which CardanoLedgerConformanceTest lists too.
      "test_5226" -> "a withdrawal above Long.MaxValue",
      "test_5227" -> "a withdrawal above Long.MaxValue"
    )

    /** The cases that still differ, by case name, each with its cause (D37). Each listed case must
      * still fail, so a fix must remove it from this list.
      */
    private val expectedFailures: Map[String, String] = Map.empty

    /** The mismatches of one case, spec [CV-12]. */
    private def run(c: Case): List[FieldMismatch] = {
        val (ledger, epoch) = EpochTransition.transition(
          c.before.ledger.ruleState,
          EpochState(c.before.accountState, c.before.lastEpoch),
          c.boundary
        )
        val enactsNothing = c.enacted.isEmpty
        val exclusions = if enactsNothing then phase3Exclusions else enactmentExclusions
        val fields = LedgerStateComparison.withoutExcluded(
          c.name,
          LedgerStateComparison.compare(c.after.ledger, ledger),
          exclusions
        ) ++ (
          // spec [EB-21]: nesEL, D66
          if epoch.lastEpoch == c.after.lastEpoch then Nil
          else
              List(
                FieldMismatch("lastEpoch", s"expected ${c.after.lastEpoch}, got ${epoch.lastEpoch}")
              )
        )
        if !enactsNothing then fields
        else
            // the reward update moves the fees, deltaT, into the treasury: phase 4
            val expectedTreasury = c.after.accountState.treasury - c.deltaT
            val treasury =
                if epoch.accountState.treasury == expectedTreasury then Nil
                else
                    List(
                      FieldMismatch(
                        "treasury",
                        s"expected $expectedTreasury, got ${epoch.accountState.treasury}"
                      )
                    )
            // spec [EB-12a], [EB-12b]: the transition removed the pulser's expired set and the
            // descendants of it, which the ledger removes too but leaves out of rsExpired
            // (conway Governance/Proposals.hs:465-484)
            val before = c.before.ledger.utxos.govState.proposals
            val removed: Set[GovActionId] =
                before.map(_.id).toSet -- ledger.govState.proposals.map(_.id)
            val expected = withDescendants(c.expired, before)
            val expired =
                if removed == expected then Nil
                else List(FieldMismatch("expired", s"expected $expected, got $removed"))
            fields ++ treasury ++ expired
    }

    /** `ids` and the proposals that descend from them; a child comes after its parent. */
    private def withDescendants(
        ids: Set[GovActionId],
        proposals: Vector[GovActionState]
    ): Set[GovActionId] =
        proposals.foldLeft(ids) { (acc, p) =>
            if parent(p.procedure.govAction).exists(acc) then acc + p.id else acc
        }

    private def parent(action: GovAction): Option[GovActionId] = action match
        case a: GovAction.ParameterChange    => a.prevActionId
        case a: GovAction.HardForkInitiation => a.prevActionId
        case a: GovAction.NoConfidence       => a.prevActionId
        case a: GovAction.UpdateCommittee    => a.prevActionId
        case a: GovAction.NewConstitution    => a.prevActionId
        case _                               => None

    test("spec [CV-9]: the corpus holds the 533 NEWEPOCH cases") {
        assert(loaded.size == 533)
        val failed = loaded.collect { case (name, Failure(e)) => name -> e.toString }.toMap
        assert(failed.keySet == undecodable.keySet, failed.mkString("\n"))
    }

    test("spec [CV-10]: each boundary follows the last epoch of its input state") {
        cases.foreach { c =>
            assert(c.boundary == c.before.lastEpoch + 1, c.name)
            assert(c.after.lastEpoch == c.boundary, c.name)
        }
    }

    test("spec [CV-11]: the pulser of 106 cases enacts an action, 427 enact nothing") {
        assert(cases.count(_.enacted.nonEmpty) == 106)
        // the two undecodable cases enact nothing either
        assert(cases.count(_.enacted.isEmpty) == 427 - undecodable.size)
    }

    test("spec [CV-12]: the epoch transition gives the output state of every case") {
        assert(expectedFailures.keySet.subsetOf(cases.map(_.name).toSet))
        val results = cases.map(c => c.name -> run(c))
        val failing = results.filter(_._2.nonEmpty)
        val unexpected = failing.filterNot((name, _) => expectedFailures.contains(name))
        val fixed = expectedFailures.keySet -- failing.map(_._1)
        val report = unexpected
            .map((name, fields) =>
                s"$name:" + fields.map(m => s"\n  ${m.field}: ${m.detail}").mkString
            )
            .mkString("\n")
        assert(unexpected.isEmpty, s"${unexpected.size} cases differ:\n$report")
        assert(fixed.isEmpty, s"listed in expectedFailures but pass: $fixed")
    }
}
