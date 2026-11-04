package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ProtocolParams, UnitInterval}
import scalus.cardano.ledger.epoch.{PoolPerformance, RatifyState, StakeSnapshot}
import scalus.cardano.ledger.epoch.internal.{EpochBookkeeping, EpochEnv, EpochTransition, Ratify}
import scalus.testing.conformance.VectorLedgerState.*
import scalus.testing.conformance.LedgerStateComparison.{compareMap, compareValue, FieldMismatch}
import scalus.testing.conformance.NewEpochVectors.Case

import scala.util.{Failure, Try}

/** Runs the epoch transition on the Dugite NEWEPOCH dumps and compares the result with the dump's
  * own output state, spec [CV-9] to [CV-20].
  */
class NewEpochConformanceTest extends AnyFunSuite {

    private lazy val loaded: List[(String, Try[Case])] =
        NewEpochVectors.caseNames().map(name => name -> Try(NewEpochVectors.load(name)))

    private lazy val cases: List[Case] = loaded.flatMap(_._2.toOption)

    /** The cases the runner cannot decode, by case name, each with its cause (D37). */
    private val undecodable: Map[String, String] = Map(
      // A TreasuryWithdrawals in the gov state withdraws more than Long.MaxValue, which
      // `Coin(value: Long)` cannot hold (D44). The dump has no test name; the Imp scenario with
      // such a proposal is "Withdrawals exceeding maxBound Word64 submitted in a single proposal"
      // (conway Imp/EnactSpec.hs:129-137), which CardanoLedgerConformanceTest lists too.
      "test_5226" -> "a withdrawal above Long.MaxValue",
      "test_5227" -> "a withdrawal above Long.MaxValue"
    )

    /** Spec [CV-20]: the output pulser of these cases enacts a HardForkInitiation to 12.0, which
      * Scalus does not accept (owner, Q8; [EN-4a], D91). They may differ in `pendingRatify.*` only.
      * Each listed case must still fail, so a fix must remove it from this list.
      */
    private val expectedFailures: Map[String, String] = List(
      "test_5339",
      "test_5440",
      "test_5461",
      "test_6937",
      "test_7002",
      "test_7480",
      "test_7507"
    ).map(_ -> "[EN-4]: a HardForkInitiation to 12.0 is not accepted").toMap

    /** The settings of the transition: none of them changes a compared field, since the reward
      * update the transition computes ([EB-22]) has no expected value in `st_out` (D81).
      *
      * The epoch length and `f` are those of the Imp genesis, `sgEpochLength = 4320` and
      * `sgActiveSlotsCoeff = 20 %! 100` (shelley testlib Test/Cardano/Ledger/Shelley/ImpTest.hs:
      * 806-814). Only `blocks = 0`, which [CV-14] pins, makes `eta = 0` today.
      */
    private def env(params: ProtocolParams): EpochEnv =
        EpochEnv(
          params,
          DugiteLedgerVectors.impGenesisSlotConfig.epochLength,
          PoolPerformance.Measured,
          UnitInterval(1, 5)
        )

    /** The mismatches of one case, spec [CV-12], [CV-15], [CV-17] to [CV-19]. */
    private def run(c: Case): List[FieldMismatch] = {
        val before = c.before
        val (ledger, epoch, params) = EpochTransition.transition(
          before.ledger.ruleState,
          EpochBookkeeping(
            before.accountState,
            before.lastEpoch,
            before.snapshots,
            before.blocksPrev,
            before.blocksCur,
            Some(c.rewardUpdate),
            c.govBefore.prevParams,
            c.govBefore.ratify
          ),
          c.boundary,
          env(c.govBefore.params)
        )
        val after = c.after
        val gov = c.govAfter
        LedgerStateComparison.withoutExcluded(
          c.name,
          LedgerStateComparison.compare(after.ledger, ledger),
          LedgerStateComparison.exclusions
        ) ++ List.concat(
          // spec [EB-21]: nesEL, D66
          compareValue("lastEpoch", after.lastEpoch, epoch.lastEpoch),
          compareValue("treasury", after.accountState.treasury, epoch.accountState.treasury),
          compareValue("reserves", after.accountState.reserves, epoch.accountState.reserves),
          compareSnapshot("snapshots.mark", after.snapshots.mark, epoch.snapshots.mark),
          compareSnapshot("snapshots.set", after.snapshots.set, epoch.snapshots.set),
          compareSnapshot("snapshots.go", after.snapshots.go, epoch.snapshots.go),
          compareValue("snapshots.fee", after.snapshots.fee, epoch.snapshots.fee),
          compareMap("blocksMadePrev", after.blocksPrev, epoch.blocksMadePrev, Nil),
          compareMap("blocksMadeCur", after.blocksCur, epoch.blocksMadeCur, Nil),
          // spec [CV-18]
          compareValue("params", gov.params, params),
          compareValue("prevParams", gov.prevParams, epoch.prevParams),
          compareRatify(gov.ratify, epoch.pendingRatify),
          // spec [CV-19]: on the state after the transition, as the pulser of st_out reads it
          compareMap(
            "dRepDistribution",
            gov.dRepDistribution,
            Ratify.dRepDistribution(ledger),
            Nil
          ),
          compareMap(
            "poolDistribution",
            gov.poolDistribution,
            Ratify.poolDistribution(ledger, epoch.snapshots.mark).stake,
            Nil
          )
        )
    }

    /** Spec [CV-18]: the pending ratify result field by field, without the enact state's treasury
      * and previous pparams, which Scalus does not keep ([DM-12a]).
      */
    private def compareRatify(expected: RatifyState, actual: RatifyState): List[FieldMismatch] = {
        val e = expected.enactState
        val a = actual.enactState
        List.concat(
          compareValue("pendingRatify.enacted", expected.enacted, actual.enacted),
          compareValue("pendingRatify.expired", expected.expired, actual.expired),
          compareValue("pendingRatify.delayed", expected.delayed, actual.delayed),
          compareValue("pendingRatify.committee", e.committee, a.committee),
          compareValue("pendingRatify.constitution", e.constitution, a.constitution),
          compareValue("pendingRatify.params", e.params, a.params),
          compareMap("pendingRatify.withdrawals", e.withdrawals, a.withdrawals, Nil),
          compareValue("pendingRatify.roots", e.roots, a.roots)
        )
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
        assert(cases.count(_.govBefore.ratify.enacted.nonEmpty) == 106)
        // the two undecodable cases enact nothing either
        assert(cases.count(_.govBefore.ratify.enacted.isEmpty) == 427 - undecodable.size)
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

    test("the output pulsers cover the ratify result, the distributions and the pparams") {
        // spec [CV-17] to [CV-19]. Pins what the output side of the corpus covers, so that a
        // decoder that reads an empty pulser or the input pparams fails
        def counts(f: Case => Boolean) = cases.count(f)
        val covered = List(
          "enacted" -> counts(_.govAfter.ratify.enacted.nonEmpty),
          "expired" -> counts(_.govAfter.ratify.expired.nonEmpty),
          "delayed" -> counts(_.govAfter.ratify.delayed),
          "withdrawals" -> counts(_.govAfter.ratify.enactState.withdrawals.nonEmpty),
          "pparams changed" -> counts(c => c.govAfter.params != c.govBefore.params),
          "DRep stake" -> counts(_.govAfter.dRepDistribution.nonEmpty),
          "DRep stake of 0" -> counts(_.govAfter.dRepDistribution.values.exists(_.value == 0)),
          "pool stake" -> counts(_.govAfter.poolDistribution.nonEmpty),
          // [RT-4a]: a proposal deposit of a delegator added to its pool's mark stake
          "pool deposits" -> counts(c =>
              c.govAfter.poolDistribution.exists((pool, stake) =>
                  c.after.snapshots.mark.pools.get(pool).exists(_.stake != stake)
              )
          )
        )
        assert(
          covered == List(
            "enacted" -> 106,
            "expired" -> 22,
            "delayed" -> 85,
            "withdrawals" -> 6,
            "pparams changed" -> 21,
            "DRep stake" -> 326,
            "DRep stake of 0" -> 9,
            "pool stake" -> 336,
            "pool deposits" -> 49
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
              env(c.govBefore.prevParams)
            )
            computed != c.rewardUpdate
        }
        assert(differing.isEmpty, differing.map(_.name).mkString(", "))
    }

    /** The fields of the pending ratify result that a HardForkInitiation to 12.0 changes when
      * Scalus does not accept it, spec [CV-20], [EN-4a].
      */
    private val hardForkRatifyFields = Set(
      "pendingRatify.enacted",
      "pendingRatify.delayed",
      "pendingRatify.params",
      "pendingRatify.roots"
    )

    test("the epoch transition gives the output state of every case") {
        // spec [CV-12], [CV-15], [CV-17] to [CV-20]
        assert(expectedFailures.keySet.subsetOf(cases.map(_.name).toSet))
        val results = cases.map(c => c.name -> run(c))
        val failing = results.filter(_._2.nonEmpty)
        // spec [CV-20]: a listed case may differ in the pending ratify result only
        val unexpected = failing.flatMap { (name, fields) =>
            val left =
                if expectedFailures.contains(name) then
                    fields.filterNot(m => hardForkRatifyFields(m.field))
                else fields
            if left.isEmpty then None else Some(name -> left)
        }
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
