import Lake
open Lake DSL

-- The Lean workspace of the constant-product AMM example. `AmmVerificationTest` runs its checks
-- in it, and proofs about the AMM written by hand belong here.
package «Amm» where
  -- As in Scalus's own workspace.
  moreGlobalServerArgs := #["--threads=4"]
  -- Blaster and PlutusCore are those of Scalus's workspace: cloned and built there once, for
  -- every workspace that requires its library. The manifest here must name the revisions the
  -- manifest there names, so `lake update` there is followed by `lake update` here.
  packagesDir := "../../../../../../scalus-verification/src/main/lean/.lake/packages"

require ScalusProofs from "../../../../../../scalus-verification/src/main/lean"

-- What a check imports is imported by the root module, so building this library builds what the
-- checks need, and no more.
@[default_target]
lean_lib «Amm» where

-- The statements Scalus writes under `Amm/Generated` and the proofs written by hand under
-- `Amm/Proofs`: `lake build AmmProofs`, or `sbt leanProve`. Building a statement runs its programs
-- symbolically, which takes minutes, and a proof may not hold yet, so they are not of the default
-- target.
lean_lib «AmmProofs» where
  roots := #[]
  globs := #[.submodules `Amm.Generated, .submodules `Amm.Proofs]
