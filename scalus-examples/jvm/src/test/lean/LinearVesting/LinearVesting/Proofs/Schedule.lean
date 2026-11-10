import LinearVesting.Generated.vesting_schedule_zero_before_start
import LinearVesting.Generated.vesting_schedule_everything_from_the_end

/-!
Proofs written by hand of statements about the vesting schedule.

A statement is a module that Scalus writes from the Scala sources, under `LinearVesting/Generated`:
`statement : Prop`, over the programs of the statement's tests as they are compiled now. A proof
here is a theorem of that proposition, so `lake build` checks it against the code as it is: where
the schedule changes, the generated proposition changes with it, and a proof that no longer
holds no longer builds.

These two are closed by `blaster`, as the checks of `VestingVerificationTest` are. A statement
Lean does not finish so, as one of a loop over a list of any length, is to be proved here by
other means.
-/

namespace LinearVesting.Proofs

open LinearVesting.Generated

-- `blaster` closes a goal that the solver finds valid with `admit`: it does not reconstruct a
-- proof for Lean's kernel.
set_option warn.sorry false

/-- Nothing is vested before the start. -/
theorem zero_before_start : vesting_schedule_zero_before_start.statement := by
  unfold vesting_schedule_zero_before_start.statement
  blaster

/-- Everything is vested from the end of the period on. -/
theorem everything_from_the_end : vesting_schedule_everything_from_the_end.statement := by
  unfold vesting_schedule_everything_from_the_end.statement
  blaster

end LinearVesting.Proofs
