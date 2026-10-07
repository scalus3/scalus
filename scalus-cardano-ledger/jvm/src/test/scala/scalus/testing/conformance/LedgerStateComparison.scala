package scalus.testing.conformance

import io.bullet.borer.Cbor
import scalus.cardano.ledger.*
import scalus.testing.conformance.LedgerState.*

/** Compares the state Scalus computes for a conformance vector with the vector's `newLedgerState`,
  * field by field (spec 13.0a).
  *
  * Field names follow the Haskell ledger state: `utxo`, `deposited`, `fees`, `govState`,
  * `instantStake` and `donation` of `UTxOState`; `accounts.*` of `DState`; `pools.*` of `PState`;
  * `dreps.*` of `VState`. A map field without a suffix (`accounts`, `dreps`) means the key sets
  * differ.
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
      *   [SC-3e]`, or [[NoStep]] when no step of the spec models the field yet
      * @param vectorNameContains
      *   the entry applies only to vectors whose name contains this text; empty means every vector
      */
    final case class Exclusion(field: String, removedBy: String, vectorNameContains: String)

    /** Marks an entry that no step of the spec removes yet. */
    val NoStep: String = "no step in the spec yet"

    /** The one exclusion list, spec [SC-14b]. Steps refer to
      * `docs/superpowers/specs/2026-10-07-scalus-emulator-provider-design.md`. Shrink it as the
      * steps land.
      */
    val exclusions: List[Exclusion] = List(
      // Not modeled: no rule reads or writes the governance state (spec 13.0a).
      Exclusion("govState", NoStep, ""),
      // Not modeled: no mutator updates the instant stake.
      Exclusion("instantStake", NoStep, ""),
      // Not modeled: no mutator updates utxosDeposited.
      Exclusion("deposited", NoStep, ""),
      // Not modeled: no mutator updates the delegators of a DRep.
      Exclusion("dreps.delegates", NoStep, ""),
      // Not modeled: no mutator bumps DRep expiry on governance activity.
      Exclusion("dreps.expiry", NoStep, "")
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
          compareValue("govState", encodeGovState(exp.govState), encodeGovState(actual.govState)),
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
          )
        )
    }

    private def encodeGovState(govState: GovState): String =
        govState.map(e => scalus.utils.Hex.bytesToHex(Cbor.encode(e).toByteArray)).mkString(",")

    private def compareValue[A](field: String, expected: A, actual: A): List[FieldMismatch] =
        if expected == actual then Nil
        else List(FieldMismatch(field, s"expected ${show(expected)}, got ${show(actual)}"))

    /** Key-set differences are reported under `field`. For a key in both maps, each differing
      * projection in `parts` is reported under `field.part`. With no `parts`, any value difference
      * is reported under `field`.
      */
    private def compareMap[K, V](
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
