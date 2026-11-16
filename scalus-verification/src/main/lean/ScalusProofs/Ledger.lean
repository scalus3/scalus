import CardanoLedgerApi.V3

/-! The ledger's rules of a script context, of the `Data` a script is given.

    IOG's CardanoLedgerApi states the rules in Lean, over its own types of the ledger's values,
    which have the `Data` encodings Plutus gives them, and Scalus too. A statement of Scalus is of
    the `Data` its programs take, so each rule here reads a `Data` as the library's type, by its
    `IsData` instance, and holds of a `Data` that the library reads and whose value meets the rule.
    A `Data` that is no encoding of the type meets none.

    The `isValid` premise of a statement (`scalus.verify.Props.isValid`) is one of these. -/

namespace ScalusProofs.Ledger

open PlutusCore.Data (Data)
open CardanoLedgerApi.IsData.Class (IsData)

/-- `d` is the `Data` of a script context of Plutus V3 that the ledger can give a script: the
    encoding of a context, as the ledger encodes one, which is valid by
    `CardanoLedgerApi.V3.Contexts.validScriptContext`, of whatever purpose.

    `d` is the encoding of the context it is read as, `IsData.toData`: a `Data` that the library
    reads leniently, as one with a field more, is none the ledger gives. So a statement of the valid
    contexts as the library's typed values, `∀ c, validScriptContext c → P (IsData.toData c)`, is of
    every `Data` this holds of. -/
def validScriptContextV3 (d : Data) : Bool :=
  match (IsData.fromData d : Option CardanoLedgerApi.V3.Contexts.ScriptContext) with
  | some ctx => IsData.toData ctx == d && CardanoLedgerApi.V3.Contexts.validScriptContext ctx
  | none => false

end ScalusProofs.Ledger
