import Lake
open Lake DSL

package «ScalusProofs» where
  moreGlobalServerArgs := #["--threads=4"]
  moreLeanArgs := #["--threads=4"]
  -- PlutusCore is pinned to a fork until input-output-hk/PlutusCoreBlaster#40 merges.
  -- That PR fixes Frame.CaseScrutinee: `main` still selects the branch of a `case` on an
  -- integer tag with `Ms[n.toNat]?`, on which `blaster` does not terminate, and every PV11
  -- program Scalus emits uses `case`. The fork's branch is #40 brought up to `main`
  -- (nau/PlutusCoreBlaster#1, into #40's branch).
  require PlutusCore from git
    "https://github.com/rssh/PlutusCoreBlaster" @ "case-scrutinee-on-main"
  -- Blaster `main`, NOT beta-lambda-cache-optimization: the branches have diverged and
  -- neither is a superset. `main` carries the Int.ediv/Int.emod fix our integer targets
  -- need and measured about 4x faster on gcd equivalence. It needs Z3 5.4.0 or later, which
  -- the flake's shells have (`blasterZ3`).
  require Blaster from git
    "https://github.com/input-output-hk/Lean-blaster" @ "main"

@[default_target]
lean_lib «ScalusProofs» where
