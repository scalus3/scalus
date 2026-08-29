import Lake
open Lake DSL

package «ScalusProofs» where
  moreGlobalServerArgs := #["--threads=4"]
  moreLeanArgs := #["--threads=4"]
  -- PlutusCore is pinned to a fork until input-output-hk/PlutusCoreBlaster#40 merges.
  -- That PR fixes Frame.CaseScrutinee, without which `blaster` does not terminate on any
  -- program using UPLC `case`, which is every PV11 program Scalus emits.
  require PlutusCore from git
    "https://github.com/nau/PlutusCoreBlaster" @ "fix/case-scrutinee-smt-blowup"
  -- Blaster `main`, NOT beta-lambda-cache-optimization: the branches have diverged and
  -- neither is a superset. `main` carries the Int.ediv/Int.emod fix our integer targets
  -- need and measured about 4x faster on gcd equivalence.
  require Blaster from git
    "https://github.com/input-output-hk/Lean-blaster" @ "main"

@[default_target]
lean_lib «ScalusProofs» where
