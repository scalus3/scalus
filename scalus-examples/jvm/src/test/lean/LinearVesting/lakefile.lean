import Lake
open Lake DSL

-- The Lean workspace of the linear vesting example. `VestingVerificationTest` runs its checks
-- in it, and proofs about the validator written by hand belong here.
package «LinearVesting» where
  -- As in Scalus's own workspace.
  moreGlobalServerArgs := #["--threads=4"]
  -- Blaster and PlutusCore are those of Scalus's workspace: cloned and built there once, for
  -- every workspace that requires its library. The manifest here must name the revisions the
  -- manifest there names, so `lake update` there is followed by `lake update` here.
  packagesDir := "../../../../../../scalus-verification/src/main/lean/.lake/packages"

require ScalusProofs from "../../../../../../scalus-verification/src/main/lean"

@[default_target]
lean_lib «LinearVesting» where
  -- Every module under `LinearVesting/` is built: those Scalus writes under `Generated/`, and a
  -- proof written by hand, without being imported from `LinearVesting.lean`.
  globs := #[.andSubmodules `LinearVesting]
