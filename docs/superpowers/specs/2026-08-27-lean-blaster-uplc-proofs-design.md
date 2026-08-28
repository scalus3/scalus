# Proving Scalus stdlib properties in Lean with Blaster

Date: 2026-08-27
Status: design approved, implementation not started

## Goal

Take small functions from the Scalus prelude, compile them to UPLC exactly as a contract
would, load the compiled bytes into Lean 4, and prove properties about them with the
Blaster SMT backend.

This tests two things at once that our Scala-level tests cannot separate:

1. the **standard library** does what it claims, and
2. the **code generator** preserves that meaning down to the UPLC bytes we ship.

The second is the part we cannot get any other way. A Scala unit test exercises the JVM
interpretation of `Math.gcd`; this exercises the flat-encoded program a validator actually
executes on chain.

## Background: the upstream stack

Three IOG repositories, all Lean 4 v4.24.0:

| Repo | Role |
| --- | --- |
| `input-output-hk/Lean-blaster` | SMT backend for Lean 4. The `blaster` tactic and `#blaster` command, Z3 underneath. |
| `input-output-hk/PlutusCoreBlaster` | Lean model of UPLC: terms, flat/CBOR decoder, CEK machine, builtins, crypto. |
| `input-output-hk/CardanoLedgerApiBlaster` | Ledger API V1/V2/V3 script-context types and validity rules. |

The workflow they provide:

```lean
#import_uplc myProg PlutusV3 single_cbor_hex "path/to/prog.flat"
def i2 (x y : Integer) : List Term := [.Const (.Integer x), .Const (.Integer y)]
#prep_uplc p myProg i2 500          -- symbolically unrolls the CEK machine 500 steps
theorem gcd_nonneg : ∀ (x y r : Integer),
    (fromFrameToInt $ p.prop x y) = some r → r ≥ 0 := by blaster
```

`#import_uplc` accepts `textual`, `flat`, `flat_hex`, `single_cbor_hex`, `double_cbor_hex`.
Scalus `Program.cborEncoded` rendered as hex is exactly `single_cbor_hex`.

## What the spike established

All of the following was run and verified, not assumed.

- **Scalus UPLC decodes and executes correctly in the Lean model.** Independently checked
  values: `gcd 12 18 = 6`, `sqrt 10000 = 100`, `exp2 10 = 1024`, `clamp 9 1 5 = 5`,
  `min 3 5 = 3`, `List.Cons(3, Cons(4, Nil)).foldLeft(0)(_+_) = 7`.
- **27 properties proved, plus 4 negative controls that were correctly falsified**, across
  `Math`, `List`, `Option`, and an optimizer-equivalence pair, under both PV10 and PV11.
  Proof times are seconds, except `gcd` equivalence at 53 s. These were throwaway spike
  files; the catalogue below is what gets built properly.
- **Toolchain comes from nix.** nixpkgs 25.11 provides `elan` 4.1.2 (which fetches Lean
  4.24.0) and `z3` 4.15.4. Blaster documents 4.15.2; 4.15.4 worked for every proof.
- **CardanoLedgerApiBlaster is not needed** for stdlib work. `PlutusCore.UPLC` plus
  `Blaster` suffices. It is only required later, for validators.

### The PV11 blocker, found and fixed

Scalus at PV11 lowers boolean branching and ADT matching to UPLC `case`. Every such program
made Blaster diverge: `Math.min` at PV11 produced no result in 10 minutes, while the same
function at PV10 (`ifThenElse` plus `delay`/`force`) proved in 4.2 s.

Root cause was in `PlutusCoreBlaster`'s `Frame.CaseScrutinee` handler, in Blaster's optimizer
rather than in Z3. Two independent problems: splitting `Const.Bool` into `false`/`true`
patterns left the compiled decision tree stuck on `Bool.casesOn` for a symbolic payload, which
froze the whole 15-alternative match; and `Ms[n.toNat]?` is an opaque recursive application
for a symbolic tag, which is what every ADT match lowers to.

Fixed in https://github.com/input-output-hk/PlutusCoreBlaster/pull/40. The restructuring is
proved semantics-preserving in `PlutusCore/UPLC/CekMachine/Lemmas.lean`, with each arm stated
equal to the formulation it replaces; an axiom audit shows the lemmas depend only on
`propext`, `Classical.choice`, and `Quot.sound`. The conformance suite is unchanged (one
pre-existing failure, `Term.Var`, fails identically on `main`).

After the fix, the same PV11 property set proves in 3.1 s.

**Consequence for this design: we target PV11, the lowering Scalus actually ships.** No PV10
compromise is needed. PV10 remains available and becomes useful as a second data point rather
than a fallback.

## Architecture

```
Scalus source (prelude/Math.scala, prelude/List.scala, ...)
  │  PlutusV3.compile(...)  under a pinned Options
  ▼
Program ──► .cborEncoded ──► hex ──► lean/ScalusProofs/Generated/math_gcd.flat   [committed]
  │                                            │
  │  scalus.uplc.eval CEK on sample inputs     │  #import_uplc ... single_cbor_hex
  ▼                                            ▼
measured step counts ──► budget ────────► #prep_uplc target args <budget>
                                               │
                                          by blaster ──► Z3
```

The exporter also evaluates each target on the JVM and emits `#guard` lines asserting the Lean
CEK produces the same value. That is free differential testing of the two CEK implementations,
and it makes an under-sized budget fail loudly instead of silently making theorems vacuous.

Two halves, one generated interface between them. The Scala half owns *what* gets compiled
and *with which options*; the Lean half owns *what is claimed about it*. The generated
directory is the whole contract, and it is committed, so the Lean half builds with no JVM
present.

## Module layout

```
scalus-lean-proofs/
├── README.md                          # how to run, pinned revs, the budget rule
├── src/main/scala/scalus/lean/
│   ├── ProofTarget.scala              # name, program, arity, sample inputs
│   ├── ProofTargets.scala             # the catalogue: what we export
│   └── ExportUplc.scala               # main: writes .flat + Generated/Targets.lean
└── lean/
    ├── lean-toolchain                 # leanprover/lean4:v4.24.0
    ├── lakefile.lean
    ├── lake-manifest.json             # committed, exact revs
    └── ScalusProofs/
        ├── Prelude.lean               # arg helpers, `steps` calibration tool
        ├── Generated/                 # committed: *.flat + Targets.lean
        ├── Sanity.lean                # alwaysOk / alwaysFail + negative control
        ├── Math.lean
        └── Data.lean                  # List / Option / codec round-trips
```

### Dependency pinning

```lean
require PlutusCore from git "https://github.com/nau/PlutusCoreBlaster" @ "fix/case-scrutinee-smt-blowup"
require Blaster    from git "https://github.com/input-output-hk/Lean-blaster" @ "main"
```

The fork pin is temporary and must be documented in the README with a link to PR #40. Switch
to upstream `main` once merged.

Two upstream hazards to record:

- `CardanoLedgerApiBlaster`'s committed `lake-manifest.json` pins a Blaster rev that has been
  force-pushed out of existence, so a fresh `lake build` fails there. Ours must not repeat
  that: commit the manifest, and let CI fail loudly rather than silently `lake update`.
- Lean-blaster `main` and its `beta-lambda-cache-optimization` branch have diverged and
  neither is a superset. We pin `main`: it carries the `Int.ediv`/`Int.emod` fix that our
  integer targets depend on, and measured ~4x faster on `gcd` equivalence (53 s vs 3 m 40 s).

### sbt

Project `scalusLeanProofs`, JVM only, modelled on the existing `llmApiGen` project:
`PluginDependency`, `publish / skip := true`, `disablePlugins(MimaPlugin)`.

- `exportLeanUplc` regenerates `lean/ScalusProofs/Generated/`.
- `checkLeanUplcUpToDate` fails if a fresh export differs from what is committed. Same shape
  as the existing `llms-api.txt` freshness gate.

### flake.nix

A new `lean` devShell with `elan`, `z3`, `git`, `bashInteractive`. Separate from `default` so
the normal Scala shell does not grow a Lean toolchain.

## Compile options

```scala
Options.releaseUntagged.copy(valueBuiltins = false)
```

- `addScalusTag = false` keeps the tag wrapper out of the term.
- `valueBuiltins = false` is **required**: the Lean model has no CIP-153 `Value` builtins
  (`lookupCoin`, `insertCoin`, `unionValue`, `scaleValue`, `valueContains`, `valueData`,
  `unValueData`) and no CIP-138 array builtins (`indexArray`, `lengthOfArray`, `listToArray`).
  Unmerged upstream branches `value-builtins` and `array-builtins` add them; revisit then.
- Everything else is the normal release lowering, so PV11 `case` is exercised.

## Budget discipline

`#prep_uplc` unrolls the CEK machine a fixed number of steps. Proofs are therefore bounded:
"for all inputs, up to N steps". Past the budget the machine returns `State.Error`, so
`fromFrameToInt` gives `none` and conditional properties hold vacuously. This is sound but
easy to misread.

**Budgets are calibrated in Lean, not in Scala.** Scalus's CEK and the Lean model do not count
the same thing: Plutus charges per `Eval` transition, while the Lean `runSteps` counts `Eval`
and `Return`. Measured, the Lean count is about 1.85x the Scalus one (`min` 23 vs 13, `abs`
26 vs 14, `gcd 12 18` 161 vs 87). A budget derived as `2 x scalusSteps` would therefore carry
almost no real headroom: for `gcd` it yields 174, while `gcd -19 14` needs 203 Lean steps. That
is exactly the false-positive described below.

So: budgets are named constants in the hand-written proof files, set from measurements taken
with the Lean calibration helper, with generous headroom. The exporter does not guess them.

One rule, in the README and in every CI failure message:

> **A falsification is not a bug until it is reproduced at twice the budget.**

This is not hypothetical. During the spike, `gcd` optimizer-equivalence reported a
counterexample at `x = -19, y = 14`. It was false: the optimized program halts at step 282,
the unoptimized at 307, and the budget was 300. Both return 1.

A calibration helper goes in `ScalusProofs/Prelude.lean`, using the executable CEK to find the
exact minimum halting step count for the Lean machine without invoking the prover:

```lean
def steps (p : PlutusScript) (xs : List Integer) (hi : Nat) : Option Nat :=
  (List.range hi).find? (fun n => (fromFrameToInt (cekExecuteProgram p.script (ints xs) n)).isSome)
```

## Property catalogue

Every group carries at least one negative control: a deliberately wrong statement that must be
falsified. Without it, a vacuous proof is indistinguishable from a real one.

**Sanity.** `alwaysOk` succeeds for all `Data`; `alwaysFail` never succeeds.

**Math** (`prelude/Math.scala`). `abs`: total, non-negative, `r = x ∨ r = -x`. `min`/`max`:
bounds, is-one-of, `min + max = x + y`. `clamp`: in range given `lo ≤ hi`, identity inside the
range. `gcd`: non-negative, divides both. `exp2`/`pow`: recurrence. `log2`: bounds.
`sqrt`/`isSqrt`: `r² ≤ n < (r+1)²`.

**Data structures.** `List` fold and length identities on bounded lists; `Option` match
branches; `fromData(toData(x)) = x` round-trips for a few case classes and enums.

**Codegen equivalence.** The highest-value group, and the one no Scala test can express. The
same source compiled two ways must compute the same function:

- `optimizeUplc` on versus off
- PV10 versus PV11 lowering
- `SirToUplcV3Lowering` versus `SimpleSirToUplcLowering`

Proven in the spike for `gcd` with the optimizer on and off.

## CI

`.github/workflows/lean-proofs.yml`, scheduled nightly plus `workflow_dispatch`. Not part of
`sbtn ci`: proof times are seconds today but will grow, and Z3 has enough nondeterminism that
a PR-blocking gate would be a nuisance.

Steps: `nix develop .#lean`, `lake build`, then `lake env lean` over each proof file. Any
result other than the expected `Valid` / `Expected Falsified` fails the job.

`checkLeanUplcUpToDate` runs in the normal JVM CI, so a codegen change that would invalidate
the committed artifacts is caught at PR time even though the proofs are not re-run.

## Limitations, to be stated in the README

1. **Blaster does not reconstruct proofs.** On `Valid` it uses `admit`. This is a strong
   testing tool, not a kernel-checked guarantee. The trusted base includes Blaster's
   translation and Z3.
2. **Proofs are bounded** by the CEK step budget.
3. **We prove against the Lean model of UPLC**, not the Plutus reference implementation. The
   evidence for that model being faithful is that it passes the plutus-conformance corpus:
   1109 of 1110 generated modules, the single failure being a pre-existing textual-parser
   mismatch on `var.uplc` that is unrelated to evaluation.
4. **`Value` and array builtins are out of scope** until the model gains them.
5. **We depend on a fork** until PR #40 merges.

## Out of scope for this design

Validators and script contexts via `CardanoLedgerApiBlaster`. That needs a different argument
encoding (`SpendingInput` and the `validSpendingContext` rules) and a much larger step budget.
It is the natural next project once the stdlib pipeline is running, and the module layout
leaves room for it.
