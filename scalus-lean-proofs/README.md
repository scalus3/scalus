# scalus-lean-proofs

Proves properties of Scalus prelude functions against the **compiled UPLC**, not against the
Scala source, using IOG's [Blaster](https://github.com/input-output-hk/Lean-blaster) SMT
backend for Lean 4 and their [Lean model of UPLC](https://github.com/input-output-hk/PlutusCoreBlaster).

This tests the standard library and the code generator at the same time. A Scala unit test
exercises the JVM interpretation of `Math.gcd`; these proofs exercise the flat-encoded program
a validator actually runs on chain.

## Layout

- `src/main/scala/scalus/lean/` - the target catalogue and the exporter.
- `lean/ScalusProofs/Generated/` - **generated and committed**. One `.flat` hex file per
  target plus `Targets.lean`. Never edit by hand; run `sbt exportLeanUplc`.
- `lean/ScalusProofs/*.lean` - the hand-written properties.

## Running

```bash
sbt exportLeanUplc                      # regenerate lean/ScalusProofs/Generated
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake build'
nix develop ../..#lean --accept-flake-config --command bash -c 'lake env lean ScalusProofs/Math.lean'
```

Expect `✅ Valid` per theorem and `✅ Expected Falsified` per negative control. Anything
else, including `Undetermined`, is a failure.

## Adding a target

1. Add a `ProofTarget` to `ProofTargets.scala` with sample inputs and expected values. The
   Scala test runs those samples through Scalus's own CEK, so a wrong expectation is caught
   before it reaches Lean.
2. `sbt exportLeanUplc`, and commit the generated files.
3. Calibrate a budget (see below), then write the properties in a `lean/ScalusProofs/*.lean`
   file. Every group needs a negative control.

## The budget rule

`#prep_uplc target args N` symbolically unrolls the CEK machine `N` steps. Proofs are
therefore bounded: "for all inputs, up to N steps". Past the budget the machine returns
`State.Error`, so `fromFrameToInt` gives `none`, conditional theorems hold vacuously and
equations get falsified.

> **A falsification is not a bug until it is reproduced at twice the budget.**

This is not hypothetical. A `gcd` optimizer-equivalence proof once reported a counterexample
at `x = -19, y = 14`. It was false: the optimized program halts at step 282, the unoptimized
at 307, and the budget was 300. Both return 1.

The opposite mistake is just as bad. Proof cost grows superlinearly in the budget: for
`gcd_nonneg`, whose worst sample needs 203 steps, budget 250 proves in 2 seconds, 350 takes
52 seconds, and 500 does not finish. **Aim for about 1.25x the measured maximum as a floor,
not a target.** Small absolute budgets carry more headroom than that: the cost curve is flat
at these sizes, so `Math.lean`'s targets run at 1.5x to 2.1x their measured worst path
(40/26, 60/39, 150/70) with no cost penalty, while `Data.lean` sits closer to the floor
(260/209, 120/86). What matters is staying above the measured worst path while staying well
below the point where cost explodes; if a proof is slow, lower the budget toward the measured
maximum, do not raise it.

Calibrate with `ScalusProofs.Prelude.steps`, which finds the exact minimum for the Lean
machine:

```lean
#eval steps mathGcd [(-19), 14] 8000
```

Do **not** derive budgets from Scalus's own step count. Plutus charges per `Eval` transition
while this machine counts `Eval` and `Return`, so the Lean figure is about 1.85x larger.

## Pinned dependencies

- `PlutusCore` from `nau/PlutusCoreBlaster` @ `fix/case-scrutinee-smt-blowup`. **Temporary
  fork.** Switch to upstream `main` once
  [input-output-hk/PlutusCoreBlaster#40](https://github.com/input-output-hk/PlutusCoreBlaster/pull/40)
  merges. Without that fix, `blaster` does not terminate on any program using UPLC `case`,
  which is every PV11 program Scalus emits.
- `Blaster` from `input-output-hk/Lean-blaster` @ `main`. Not the
  `beta-lambda-cache-optimization` branch: the two have diverged and neither is a superset.
  `main` carries the `Int.ediv`/`Int.emod` fix these integer targets need.

Upstream force-pushes. If `lake build` fails with `git exited with code 128`, a pinned rev is
gone; re-run `lake update` and commit the new `lake-manifest.json`.

## Coverage

Implemented: `abs`, `min`, `max`, `clamp` fully; `exp2` only on its `exp < 0` branch; prelude
`Option` and `List`. Not implemented: `gcd` properties, `sqrt`/`isSqrt`, `log2`, `pow`, `Data`
round-trips, and codegen equivalence. Three exported targets carry no properties at all
(`math_gcd`, `math_sqrt`, `math_gcd_unopt`); they are still exported so their generated
differential checks still guard codegen. See
[`docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md`](../docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md)'s
"What is not proved, and why" for the reasons.

## Limitations

1. **Blaster does not reconstruct proofs.** On `Valid` it uses `admit`. This is strong
   differential testing, not a kernel-checked guarantee. The trusted base includes Blaster's
   translation and Z3.
2. **Proofs are bounded** by the CEK step budget.
3. **We prove against the Lean model of UPLC**, not the Plutus reference implementation. The
   evidence for that model is that it passes the plutus-conformance corpus.
4. **`Value` and array builtins are out of scope.** Targets must compile with
   `valueBuiltins = false`; the model has no CIP-153 or CIP-138 builtins.
5. **Functions reaching the CIP-121/122 bitwise builtins cannot be proved generically.**
   Blaster cannot translate `BitVec`, whose width is a value index rather than a type
   parameter, and `ByteString` is built on it. Any property quantifying over a symbolic input
   whose CEK trace reaches `shiftByteString`, `integerToByteString` or `byteStringToInteger`
   fails to translate, at every budget. This is why `Math.exp2` is proved only on its
   `exp < 0` early return.
