package scalus.verify.lean

import scalus.compiler.sir.SIRType
import scalus.verify.SirTypes

/** A Lean type that exists for the values of a Scala type, in a library the checks import: what a
  * variable of the Scala type can be in Lean, in place of the `Data` of its encoding restricted by
  * a premise (docs/design/verification-details/uplc-blaster.md, "Mapping a variable").
  *
  * A value of the Lean type reaches a program as `encoder value`, its `Data`. The Lean type need
  * not have exactly the values of the Scala type: CardanoLedgerApi's script context keeps some
  * parts as any `Data`, as the validity range, and so has more values. A variable of it is one that
  * has more values than its type, as one mapped as `Data` is.
  */
final case class LeanTypeBinding(
    /** The full name of the Scala type's declaration. */
    scalaType: String,
    /** The Lean type, by its full name. */
    leanType: String,
    /** The Lean module a check imports for it. */
    module: String,
    /** The Lean function, `leanType → Data`, of a value's `Data`. */
    encoder: String
) {

    /** The Lean term of the `Data` of `value`, a term of the Lean type. */
    def toData(value: String): String = s"($encoder $value)"
}

object LeanTypeBinding {

    /** A script context of Plutus V3: CardanoLedgerApi's, whose `IsData` encoding is Plutus's, and
      * Scalus's too (`LeanPredicateTest` checks that each reads the other's).
      */
    val scriptContextV3: LeanTypeBinding = LeanTypeBinding(
      "scalus.cardano.onchain.plutus.v3.ScriptContext",
      "CardanoLedgerApi.V3.Contexts.ScriptContext",
      "ScalusProofs.Ledger",
      "CardanoLedgerApi.IsData.Class.IsData.toData"
    )

    /** The Lean types that exist, by the Scala types they are of. */
    val known: Map[String, LeanTypeBinding] =
        Map(scriptContextV3.scalaType -> scriptContextV3)

    /** The Lean type of `tp`, where one exists. */
    def of(tp: SIRType): Option[LeanTypeBinding] =
        known.get(SirTypes.declarationName(SirTypes.unwrap(tp)))
}
