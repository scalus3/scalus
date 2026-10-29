import Lake
open Lake DSL

-- The Lean workspace of the USDCx minting policy example: the statements IOG's
-- CardanoLedgerApiBlaster makes of the Plinth policy, of the script `UsdcxMintingPolicy`
-- compiles to.
--
-- Unlike the vesting workspace, it does not share Scalus's packages: `CardanoLedgerApi` is
-- built with its own Blaster and PlutusCore, pinned here to the revisions the statements were
-- checked with. That Blaster sets the Z3 simplifier `recfun-finder`, so it needs Z3 5.3.1 or
-- later: the flake's `lean-z3-5` shell (#437) has it, the others not yet. CI does not build
-- this workspace. To check the statements, run `lake build` here in `nix develop .#lean-z3-5`,
-- or `UsdcxMintingPolicyVerificationTest`, which runs it, with that Z3 on the PATH: about 7
-- minutes, after a first build of the packages of about 30.
package «UsdcxMintingPolicy» where
  moreGlobalServerArgs := #["--threads=4"]
  moreLeanArgs := #["--threads=4"]

-- The root's requirements take precedence over those of `CardanoLedgerApi`, which name
-- branches.
require Blaster from git
  "https://github.com/input-output-hk/Lean-blaster" @ "81ab3ed058ae236564f4a0934f7adfa6a4df875c"
require PlutusCore from git
  "https://github.com/input-output-hk/PlutusCoreBlaster" @ "48ca4660eab2a7506b981040bedcb8926dd291e7"
require CardanoLedgerApi from git
  "https://github.com/input-output-hk/CardanoLedgerApiBlaster" @ "9938562fd452351655fe2f6b63e583c62422687c"

-- The script, which `#import_uplc` reads. As a target, it is an input of the library, so a
-- new script is checked again; Lake does not see what a command reads.
target scalusScript : System.FilePath :=
  inputTextFile "../../resources/scalus/examples/usdcx/minting_policy_scalus.hex"

@[default_target]
lean_lib «UsdcxMintingPolicy» where
  extraDepTargets := #[`scalusScript]
