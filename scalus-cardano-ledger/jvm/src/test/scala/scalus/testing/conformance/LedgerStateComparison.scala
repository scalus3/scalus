package scalus.testing.conformance

import scalus.cardano.ledger.*
import scalus.testing.conformance.LedgerState.*

/** Compares the state Scalus computes for a conformance vector with the vector's `newLedgerState`,
  * field by field (spec 13.0a).
  *
  * Field names follow the Haskell ledger state: `utxo`, `deposited`, `fees`, `govState.*`,
  * `instantStake` and `donation` of `UTxOState`; `accounts.*` of `DState`; `pools.*` of `PState`;
  * `dreps.*` and `committeeState` of `VState`. A map field without a suffix (`accounts`, `dreps`)
  * means the key sets differ.
  */
object LedgerStateComparison {

    /** One field of the resulting state that differs from `newLedgerState`. spec [SC-14a] */
    final case class FieldMismatch(field: String, detail: String)

    /** A field left out of the comparison, spec [SC-14b].
      *
      * @param field
      *   the field name, as in [[FieldMismatch.field]]
      * @param removedBy
      *   the step that removes the entry, spec [SC-14c]: a section of the spec, such as `13.1
      *   [SC-3e]`, or `never, D<n>` when a decision keeps it out for good
      * @param vectorNameContains
      *   the entry applies only to vectors whose name contains this text; empty means every vector
      */
    final case class Exclusion(field: String, removedBy: String, vectorNameContains: String)

    /** The one exclusion list, spec [SC-14b], [CV-12], [CV-18]. `instantStake` stays out for good
      * (`docs/superpowers/specs/2026-11-02-emulator-ledger-rules-design.md` D5, D81): Scalus keeps
      * no instant stake, and [CV-15] checks the stake through `snapshots.mark`.
      */
    val exclusions: List[Exclusion] = List(
      Exclusion("instantStake", "never, D5 and D81", "")
    )

    def withoutExcluded(
        vectorName: String,
        mismatches: List[FieldMismatch],
        exclusions: List[Exclusion]
    ): List[FieldMismatch] =
        mismatches.filterNot(m =>
            exclusions.exists(e => e.field == m.field && vectorName.contains(e.vectorNameContains))
        )

    /** Compares `actual` with `expected`. Returns one entry per mismatching field, in a fixed
      * order. spec [SC-14], [SC-14a]
      */
    def compare(expected: LedgerState, actual: rules.State): List[FieldMismatch] = {
        val exp = expected.utxos
        val expCerts = expected.certs
        val actCerts = actual.certState
        List.concat(
          compareMap("utxo", exp.utxo, actual.utxos, Nil),
          compareValue("deposited", exp.deposited, actual.deposited),
          compareValue("fees", exp.fees, actual.fees),
          // spec [CV-7]; the DRep pulser and the future pparams are not decoded ([CV-6])
          compareProposals(exp.govState.proposals, actual.govState.proposals),
          compareValue("govState.roots", exp.govState.roots, actual.govState.roots),
          compareValue("govState.committee", exp.govState.committee, actual.govState.committee),
          compareValue(
            "govState.constitution",
            exp.govState.constitution,
            actual.govState.constitution
          ),
          compareMap("instantStake", exp.stakeDistribution, actual.stakeDistribution, Nil),
          compareValue("donation", exp.donation, actual.donation),
          compareMap(
            "accounts",
            expCerts.dstate.accounts,
            actCerts.dstate.accounts,
            List(
              "balance" -> (_.balance),
              "deposit" -> (_.deposit),
              "stakePoolDelegation" -> (_.stakePoolDelegation),
              "dRepDelegation" -> (_.dRepDelegation)
            )
          ),
          compareMap(
            "pools.stakePoolParams",
            expCerts.pstate.stakePools,
            actCerts.pstate.stakePools,
            Nil
          ),
          compareMap(
            "pools.futureStakePoolParams",
            expCerts.pstate.futureStakePoolParams,
            actCerts.pstate.futureStakePoolParams,
            Nil
          ),
          compareMap("pools.retiring", expCerts.pstate.retiring, actCerts.pstate.retiring, Nil),
          compareMap("pools.deposits", expCerts.pstate.deposits, actCerts.pstate.deposits, Nil),
          // spec [CV-5]
          compareMap(
            "pools.vrfKeyHashes",
            expCerts.pstate.vrfKeyHashes,
            actCerts.pstate.vrfKeyHashes,
            Nil
          ),
          compareMap(
            "dreps",
            expCerts.vstate.toVotingState.dreps,
            actCerts.vstate.dreps,
            List(
              "expiry" -> (_.expiry),
              "anchor" -> (_.anchor),
              "deposit" -> (_.deposit),
              "delegates" -> (_.delegates)
            )
          ),
          // spec [GC-3]
          compareMap(
            "committeeState",
            expCerts.vstate.committeeState,
            actCerts.vstate.committeeState,
            Nil
          ),
          // spec [CV-4]: the reset of [DR-3] shows only here
          compareValue(
            "dreps.numDormantEpochs",
            expCerts.vstate.numDormantEpochs,
            actCerts.vstate.numDormantEpochs
          )
        )
    }

    /** The proposals in order, each with its id, votes, procedure and epochs, spec [CV-7]. */
    private def compareProposals(
        expected: Vector[GovActionState],
        actual: Vector[GovActionState]
    ): List[FieldMismatch] =
        if expected == actual then Nil
        else
            val (expectedIds, actualIds) = (expected.map(_.id), actual.map(_.id))
            val detail =
                if expectedIds != actualIds then
                    s"expected ids ${show(expectedIds)}, got ${show(actualIds)}"
                else
                    expected
                        .zip(actual)
                        .collect {
                            case (e, a) if e != a =>
                                s"\n    ${show(e.id)}: expected ${show(e)}, got ${show(a)}"
                        }
                        .mkString
            List(FieldMismatch("govState.proposals", detail))

    private[conformance] def compareValue[A](
        field: String,
        expected: A,
        actual: A
    ): List[FieldMismatch] =
        if expected == actual then Nil
        else List(FieldMismatch(field, s"expected ${show(expected)}, got ${show(actual)}"))

    /** Key-set differences are reported under `field`. For a key in both maps, each differing
      * projection in `parts` is reported under `field.part`. With no `parts`, any value difference
      * is reported under `field`.
      */
    private[conformance] def compareMap[K, V](
        field: String,
        expected: Map[K, V],
        actual: Map[K, V],
        parts: List[(String, V => Any)]
    ): List[FieldMismatch] = {
        val missing = expected.keySet -- actual.keySet
        val extra = actual.keySet -- expected.keySet
        val common = (expected.keySet & actual.keySet).toList
        val keyMismatch =
            if missing.isEmpty && extra.isEmpty then Nil
            else
                List(
                  FieldMismatch(
                    field,
                    s"missing ${missing.map(show).mkString("[", ", ", "]")}, " +
                        s"extra ${extra.map(show).mkString("[", ", ", "]")}"
                  )
                )
        val valueMismatches =
            if parts.isEmpty then
                val differing = common.filter(k => expected(k) != actual(k))
                if differing.isEmpty then Nil
                else List(FieldMismatch(field, differing.map(diffLine(expected, actual)).mkString))
            else
                for
                    (part, project) <- parts
                    differing = common.filter(k => project(expected(k)) != project(actual(k)))
                    if differing.nonEmpty
                yield FieldMismatch(
                  s"$field.$part",
                  differing
                      .map(k =>
                          s"\n    ${show(k)}: expected ${show(project(expected(k)))}, " +
                              s"got ${show(project(actual(k)))}"
                      )
                      .mkString
                )
        keyMismatch ++ valueMismatches
    }

    private def diffLine[K, V](expected: Map[K, V], actual: Map[K, V])(k: K): String =
        s"\n    ${show(k)}: expected ${show(expected(k))}, got ${show(actual(k))}"

    private def show(value: Any): String = {
        val text = pprint.apply(value, height = 20).plainText
        if text.length > 400 then text.take(400) + "..." else text
    }
}
