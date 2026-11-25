import PlutusCore.UPLC
import PlutusCore.UPLC.CekMachine.Lemmas
import ScalusProofs.Run

/-! What a program does within some number of steps of Lean's machine, with no budget.

    A check of `blaster-uplc` is of a run of a budget (`ScalusProofs.Run`): a program on a list
    longer than the budget's runs reach is not of it. A lemma written by hand of the machine's run
    itself, as of a mapping (`ScalusProofs.Mappings`), says what a program does within some number
    of steps: a run that halts or fails stays in that state, so `∃ n` is how the run ends, for
    inputs of any size. -/

namespace ScalusProofs.Runs

open PlutusCore.Default (BuiltinSemanticsVariant)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.CekMachine
open ScalusProofs.Run

/-- `runFor`, which stops where the machine halts or fails, is `stepN`, which goes on: a terminal
    state stays. -/
theorem runFor_eq_stepN (sv : BuiltinSemanticsVariant) (s : State) (n : Nat) :
    runFor sv s n = stepN sv s n := by
  induction n generalizing s with
  | zero => cases s <;> rfl
  | succ n ih =>
    cases s with
    | Halt V => exact (stepN_of_terminal sv _ (Or.inl ⟨V, rfl⟩) _).symm
    | Error => exact (stepN_of_terminal sv _ (Or.inr (Or.inl rfl)) _).symm
    | BudgetExhausted =>
      simp only [runFor, stepN]
      exact ih _
    | Eval st ρ t =>
      simp only [runFor, stepN]
      exact ih _
    | Return st v =>
      simp only [runFor, stepN]
      exact ih _

/-- `p` on `args` returns the Boolean `b`, within some number of steps. -/
def returns (p : Program) (args : List Term) (b : Bool) : Prop :=
  ∃ n, fromFrameToBool (runProgramFor p args n) = some b

/-- `p` on `args` halts, within some number of steps. -/
def succeeds (p : Program) (args : List Term) : Prop :=
  ∃ n, halted (runProgramFor p args n) = true

/-- `p` on `args` fails, within some number of steps. -/
def fails (p : Program) (args : List Term) : Prop :=
  ∃ n, failed (runProgramFor p args n) = true

end ScalusProofs.Runs
