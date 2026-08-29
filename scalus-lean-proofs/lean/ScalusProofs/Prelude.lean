import PlutusCore.UPLC
import Blaster

/-! Shared helpers for the Scalus proof suite. -/

namespace ScalusProofs.Prelude

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open PlutusCore.UPLC.PlutusScript (PlutusScript)
open PlutusCore.UPLC.CekMachine (cekExecuteProgram)

/-- Integer arguments as UPLC constant terms. -/
def ints (xs : List Integer) : List Term :=
  xs.map (fun x => Term.Const $ Const.Integer x)

/-- Run `p` on integer arguments `xs` for at most `n` CEK steps. `none` means the machine did
    not halt with an integer within the budget. -/
def runInts (p : PlutusScript) (xs : List Integer) (n : Nat) : Option Integer :=
  fromFrameToInt (cekExecuteProgram p.script (ints xs) n)

/-- The smallest step count at which `p` halts with an integer on `xs`, searching up to `hi`.

    Use this to choose a `#prep_uplc` budget. Do NOT derive budgets from Scalus's own step
    count: Plutus charges per `Eval` transition while this machine counts `Eval` and `Return`,
    so the Lean figure is roughly 1.85x larger.

    A budget below the true step count makes the machine return `State.Error`, which silently
    makes conditional theorems vacuous and falsifies equations. A falsification is not a bug
    until it is reproduced at twice the budget. -/
def steps (p : PlutusScript) (xs : List Integer) (hi : Nat) : Option Nat :=
  (List.range hi).find? (fun n => (runInts p xs n).isSome)

end ScalusProofs.Prelude
