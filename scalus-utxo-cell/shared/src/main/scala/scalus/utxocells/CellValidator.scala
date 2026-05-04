package scalus.utxocells

import scalus.compiler.Compile
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v1.PolicyId
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** Validator base trait for UtxoCell state machines (non-parameterized).
  *
  * Unlike [[Validator]], this trait passes `ScriptContext` through to `spendCell` and `mintCell`,
  * enabling `@OnChainSubstitute` on [[CellContext]]. The transition function receives
  * `ScriptContext` as `CellContext` via `asInstanceOf`.
  *
  * All methods are `inline` — Scala 3 expands them before the Scalus plugin runs. Same pattern as
  * [[Validator]].
  *
  * Usage:
  * {{{
  * @Compile
  * object MyCell extends CellValidator {
  *     def transition(state: S, action: A, ctx: CellContext): Option[S] = ...
  *
  *     inline override def spendCell(
  *         datum: Option[Data], redeemer: Data, sc: ScriptContext, ownRef: TxOutRef
  *     ): Unit = {
  *         val state = datum.getOrFail("missing").to[S]
  *         val action = redeemer.to[A]
  *         val nextState = transition(state, action, sc.asInstanceOf[CellContext])
  *         UtxoCellLib.verifySpendResult(nextState, sc.txInfo, ownRef)
  *     }
  * }
  * }}}
  */
@Compile
trait CellValidator {

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(ownRef, datum) =>
                spendCell(datum, sc.redeemer, sc, ownRef)
            case ScriptInfo.MintingScript(policyId) =>
                mintCell(sc.redeemer, policyId, sc)
            case _ => fail("CellValidator: unsupported script purpose")
    }

    inline def spendCell(
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit

    inline def mintCell(
        redeemer: Data,
        policyId: PolicyId,
        sc: ScriptContext
    ): Unit
}

/** Validator base trait for UtxoCell state machines with a Data parameter.
  *
  * Same pattern as [[CellValidator]] but supports parameterized scripts. The parameter is applied
  * at UPLC level.
  */
@Compile
trait DataParameterizedCellValidator {

    inline def validate(param: Data)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(ownRef, datum) =>
                spendCell(param, datum, sc.redeemer, sc, ownRef)
            case ScriptInfo.MintingScript(policyId) =>
                mintCell(param, sc.redeemer, policyId, sc)
            case _ => fail("CellValidator: unsupported script purpose")
    }

    inline def spendCell(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit

    inline def mintCell(
        param: Data,
        redeemer: Data,
        policyId: PolicyId,
        sc: ScriptContext
    ): Unit
}
