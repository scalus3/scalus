package scalus.verify

/** A Boolean function of Lean on `Data`, which a statement says holds of a value's `Data`
  * ([[Prop.Satisfies]]). It is no program a tactic runs: only Lean evaluates it, from a module of
  * the library of the workspace the checks run in.
  *
  * The predicates are those a Lean library states, as the rules of the ledger in IOG's
  * CardanoLedgerApi, read of the `Data` a program takes: [[LeanPredicate.validScriptContextV3]],
  * which [[Props.isValid]] states.
  */
final case class LeanPredicate(
    /** The full name of the Lean function, `Data → Bool`. */
    name: String,
    /** The Lean module that defines it, which a check that uses it imports. */
    module: String,
    /** What it says, for a reader. */
    description: String,
    /** The same predicate of a value of a Lean type, by that type's full name: a function of the
      * value, which a variable of that type is given in place of its `Data`.
      */
    typed: Map[String, String] = Map.empty
)

object LeanPredicate {

    /** That a `Data` is the encoding of a script context of Plutus V3 that the ledger can give a
      * script: the rules of `CardanoLedgerApi.V3.Contexts.validScriptContext`, of whatever purpose,
      * as CardanoLedgerApi reads the `Data` (`ScalusProofs/Ledger.lean`). Among them: the
      * transaction has inputs, and its own input where it spends one, with its datum; its fee is
      * positive and it is balanced; its validity range is not empty; its lists and maps are sorted
      * as the ledger sorts them; the redeemer is the transaction's for the purpose.
      */
    val validScriptContextV3: LeanPredicate = LeanPredicate(
      "ScalusProofs.Ledger.validScriptContextV3",
      "ScalusProofs.Ledger",
      "a script context of Plutus V3 the ledger can give a script " +
          "(CardanoLedgerApi.V3.Contexts.validScriptContext)",
      Map(
        "CardanoLedgerApi.V3.Contexts.ScriptContext" ->
            "CardanoLedgerApi.V3.Contexts.validScriptContext"
      )
    )
}
