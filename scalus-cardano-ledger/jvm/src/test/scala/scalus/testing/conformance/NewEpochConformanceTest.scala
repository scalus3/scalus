package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{GovAction, GovActionId, GovActionState, ProtocolParams, UnitInterval}
import scalus.cardano.ledger.epoch.{PoolPerformance, StakeSnapshot}
import scalus.cardano.ledger.epoch.internal.{EpochEnv, EpochState, EpochTransition}
import scalus.testing.conformance.LedgerState.*
import scalus.testing.conformance.LedgerStateComparison.{compareMap, compareValue, Exclusion, FieldMismatch}
import scalus.testing.conformance.NewEpochVectors.Case

import scala.util.{Failure, Try}

/** Runs the epoch transition on the Dugite NEWEPOCH dumps and compares the result with the dump's
  * own output state, spec [CV-9] to [CV-16].
  */
class NewEpochConformanceTest extends AnyFunSuite {

    private lazy val loaded: List[(String, Try[Case])] =
        NewEpochVectors.caseNames().map(name => name -> Try(NewEpochVectors.load(name)))

    private lazy val cases: List[Case] = loaded.flatMap(_._2.toOption)

    /** Spec [CV-12], [CV-13]: the fields of a case whose pulser enacts nothing that the runner does
      * not compare. `instantStake` stays out for good (D5, D81): Scalus keeps no instant stake, and
      * [CV-15] checks the stake through `snapshots.mark`.
      */
    private val phase3Exclusions: List[Exclusion] = List(
      Exclusion("instantStake", "never, D5 and D81", "")
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

    /** The settings of the transition: none of them changes a compared field, since the reward
      * update the transition computes ([EB-22]) has no expected value in `st_out` (D81).
      *
      * The dumps hold no genesis, so the epoch length and `f` are the mainnet values, not those of
      * the Imp-test genesis. [CV-16] can use them only while [CV-14] pins `blocks = 0`: with no
      * blocks `eta = 0`, whatever `expectedBlocks` is. A corpus with blocks needs the genesis.
      */
    private def env(params: ProtocolParams): EpochEnv =
        EpochEnv(params, 432_000L, PoolPerformance.Measured, UnitInterval(1, 20))

    /** The mismatches of one case, spec [CV-12], [CV-15]. */
    private def run(c: Case): List[FieldMismatch] = {
        val (ledger, epoch) = EpochTransition.transition(
          c.before.ledger.ruleState,
          EpochState(
            c.before.accountState,
            c.before.lastEpoch,
            c.before.snapshots,
            c.before.blocksPrev,
            c.before.blocksCur,
            Some(c.rewardUpdate)
          ),
          c.boundary,
          env(c.params)
        )
        val enactsNothing = c.enacted.isEmpty
        val exclusions = if enactsNothing then phase3Exclusions else enactmentExclusions
        val after = c.after
        val fields = LedgerStateComparison.withoutExcluded(
          c.name,
          LedgerStateComparison.compare(after.ledger, ledger),
          exclusions
        ) ++ List.concat(
          // spec [EB-21]: nesEL, D66
          compareValue("lastEpoch", after.lastEpoch, epoch.lastEpoch),
          // spec [CV-15]: in both branches, since SNAP runs before RATIFY and ENACT
          compareValue("reserves", after.accountState.reserves, epoch.accountState.reserves),
          compareSnapshot("snapshots.mark", after.snapshots.mark, epoch.snapshots.mark),
          compareSnapshot("snapshots.set", after.snapshots.set, epoch.snapshots.set),
          compareSnapshot("snapshots.go", after.snapshots.go, epoch.snapshots.go),
          compareValue("snapshots.fee", after.snapshots.fee, epoch.snapshots.fee),
          compareMap("blocksMadePrev", after.blocksPrev, epoch.blocksMadePrev, Nil),
          compareMap("blocksMadeCur", after.blocksCur, epoch.blocksMadeCur, Nil)
        )
        if !enactsNothing then fields
        else
            // spec [CV-15]: [EB-5] moves deltaT into the treasury, so no adjustment
            val treasury =
                compareValue("treasury", after.accountState.treasury, epoch.accountState.treasury)
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

    /** Spec [CV-15]: one snapshot, each of its three maps under its own field. The stake ratio of
      * each pool is checked in the decoder, so equal pools have equal ratios.
      */
    private def compareSnapshot(
        field: String,
        expected: StakeSnapshot,
        actual: StakeSnapshot
    ): List[FieldMismatch] = List.concat(
      compareMap(s"$field.activeStake", expected.activeStake, actual.activeStake, Nil),
      compareMap(s"$field.delegations", expected.delegations, actual.delegations, Nil),
      compareMap(s"$field.pools", expected.pools, actual.pools, Nil)
    )

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

    test("the corpus holds the 533 NEWEPOCH cases") {
        // spec [CV-9]
        assert(loaded.size == 533)
        val failed = loaded.collect { case (name, Failure(e)) => name -> e.toString }.toMap
        assert(failed.keySet == undecodable.keySet, failed.mkString("\n"))
    }

    test("each boundary follows the last epoch of its input state") {
        // spec [CV-10]
        cases.foreach { c =>
            assert(c.boundary == c.before.lastEpoch + 1, c.name)
            assert(c.after.lastEpoch == c.boundary, c.name)
        }
    }

    test("the pulser of 106 cases enacts an action, 427 enact nothing") {
        // spec [CV-11]
        assert(cases.count(_.enacted.nonEmpty) == 106)
        // the two undecodable cases enact nothing either
        assert(cases.count(_.enacted.isEmpty) == 427 - undecodable.size)
    }

    test("the cases cover the snapshots and the fees, not blocks or rewards") {
        // spec [CV-14]. Pins what the corpus covers, so that a decoder that reads empty snapshots
        // fails: the Imp tests make no blocks, so eta = 0, no reward is paid and only the fee
        // path is tested
        def counts(f: Case => Boolean) = cases.count(f)
        val nonEmpty = List(
          "mark stake" -> counts(_.before.snapshots.mark.activeStake.nonEmpty),
          "mark pools" -> counts(_.before.snapshots.mark.pools.nonEmpty),
          "go stake" -> counts(_.before.snapshots.go.activeStake.nonEmpty),
          "go pools" -> counts(_.before.snapshots.go.pools.nonEmpty),
          "fee" -> counts(_.before.snapshots.fee.value > 0),
          "deltaR" -> counts(_.rewardUpdate.deltaR.value != 0),
          "rewards" -> counts(_.rewardUpdate.rewards.nonEmpty),
          "blocks" -> counts(c => c.before.blocksPrev.nonEmpty || c.before.blocksCur.nonEmpty)
        )
        assert(
          nonEmpty == List(
            "mark stake" -> 231,
            "mark pools" -> 277,
            "go stake" -> 125,
            "go pools" -> 165,
            "fee" -> 198,
            "deltaR" -> 198,
            "rewards" -> 0,
            "blocks" -> 0
          )
        )
    }

    test("the reward calculation on st gives the reward update of st") {
        // spec [CV-16]
        val differing = cases.filter { c =>
            val before = c.before
            val computed = EpochTransition.rewardUpdate(
              before.snapshots.go,
              before.snapshots.fee,
              before.blocksPrev,
              before.accountState.reserves,
              env(c.prevParams)
            )
            computed != c.rewardUpdate
        }
        assert(differing.isEmpty, differing.map(_.name).mkString(", "))
    }

    test("the epoch transition gives the output state of every case") {
        // spec [CV-12]
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
