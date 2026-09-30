# scalus-verification

Formal verification of Scalus code. Statements are written in Scala and proved by pluggable
tactics; the design is in
[`docs/design/verification-overview.md`](../docs/design/verification-overview.md). What exists
today is the first backend, the `blaster-uplc` tactic `UplcBlaster`, described below. This module
was called `scalus-lean-proofs`, with Scala package `scalus.lean`, until 2026-09-25.

`UplcBlaster` proves properties of Scalus functions against their **compiled UPLC**, not against
the Scala source, using IOG's [Blaster](https://github.com/input-output-hk/Lean-blaster) SMT
backend for Lean 4 and their [Lean model of UPLC](https://github.com/input-output-hk/PlutusCoreBlaster).
This tests the standard library and the code generator at the same time: a Scala unit test
exercises the JVM interpretation of `Math.clamp`, while these proofs are about the program a
validator actually runs on chain.

## Layout

- `src/main/scala/scalus/verify/` - `Prop` and the `Props` syntax (`forAll`, `exists`,
  `existsLet`, `call`, `denotes`, `equal`), `Quantifiable`, the function table (`FunctionRef`,
  `FunctionDef`) and `Verifier`. `Prop` captures statements without evaluating them; `Verifier`
  registers functions and statements, then passes goals to tactics. A successful proof retains a
  backend-specific `ProofArtifact`.
- `src/main/scala/scalus/verify/lean/` - common `Prop` to Lean proposition generation. The first
  version supports `Boolean` and `BigInt`; other data types fail explicitly.
- `src/main/scala/scalus/verify/uplcblaster/` - the `UplcBlaster` tactic and its compile options.
- `src/main/lean/` - the Lean workspace the tactic runs in. `ScalusProofs/Run.lean` holds the
  budgeted CEK runner and the `#prep_uplc_run` command its generated checks use.
- `src/test/scala/scalus/verify/uplcblaster/PreludeProofsTest.scala` - properties of `Math` and
  of prelude data structures, each with a negative control and samples.

## Statements

```scala
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.UplcBlaster

val clamp = FunctionDef(Math.clamp)
val verifier = Verifier.empty
verifier.addFunction(clamp)
val inRange = verifier.statement(
  "clamp_in_range",
  forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
      Prop(lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
  )
)
verifier.verify(inRange, UplcBlaster(budget = 120)) // Proven
```

`UplcBlaster` proves statements with a prefix of universal quantifiers over `BigInt` and
`Boolean`, and a body without quantifiers: Boolean tests, total calls, `denotes` and `equal`,
combined with `&&`, `||`, `!`, `==>` and `<=>`. Each test becomes its own closed UPLC predicate
over the quantified values. The tactic writes a Lean file in a temporary directory that imports
them, runs `lake env lean` in this workspace, and reads Blaster's verdict. The workspace must be
built (`lake build`).

- **Valid** means the statement holds without the budget. A test in a positive position must halt
  within the budget with `true`; one in a negative position (a premise, or under `!`) must only
  neither halt with `false` nor fail within the budget. `denotes` is read the same way. The
  predicates run with `ScalusProofs.Run.runFor`, which keeps a failing program (`State.Error`)
  apart from an exhausted budget, so a statement that a program fails, such as
  `!denotes(BigInt(7) / BigInt(0))`, can be proved (design doc §6.2).
- **Falsified** is replayed on the Scalus CEK with a large budget. If the statement is false
  there too, the result is `Refuted` with the counterexample; otherwise it is `Inconclusive`, and
  the message says the counterexample is spurious. A spurious counterexample means the budget is
  too small.
- Anything else, including other binder types or a quantifier inside the body, is
  `Inconclusive`.

A closed statement, without quantified variables, such as a sample
`callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))`, has nothing for
Blaster to search. Lean decides it by running its predicates, with `native_decide` over the
computable `runProgramFor`; its proof kind is `LeanNative`, which trusts the Lean compiler instead
of Blaster and Z3. This also works for programs Blaster cannot reduce, such as those reaching
`integerToByteString` ([Lean-blaster#273](https://github.com/input-output-hk/Lean-blaster/issues/273)).

A call of a function registered in the verifier is linked to that function's compiled program, so
the proof is about those exact bytes. A call is total: it claims that the function returns. Other
`@Compile` definitions a test uses are compiled with the test. A function of several parameters
is called with a tuple written out, as in `callRef(clamp.ref, (x, lo, hi))(r => lo <= r)`, and its
program is applied to each value in turn. Arguments and results must be `BigInt` or `Boolean`.

Binder bodies are statements, so quantifiers, calls and connectives nest:
`forAll[BigInt](x => denotes(BigInt(10) / x) ==> Prop(x != BigInt(0)))`. Each test or expression
is compiled as a closed lambda over the variables it uses. A variable may only be used inside them:
for a Boolean `if`, write `Prop(if c then a else b)` around the whole condition.

## Running

```bash
cd scalus-verification/src/main/lean && lake build && cd -   # once, and after a Lean change
sbt scalusVerification/test
```

The tests that run Lean need `lake` on the `PATH` (the default and `ci` nix shells have it) and
the built workspace. Without them those tests are canceled, as in ci-jvm. With the
`SCALUS_REQUIRE_LEAN` environment variable set, as in the Lean-Proofs workflow, they fail instead.

## Adding a property

1. Register the function: `FunctionDef(Obj.method)` for a `@Compile` method, or
   `FunctionDef.named("name", (x: BigInt) => ...)` for an `inline def` or any other function.
2. State its samples as closed calls, and check them on the Scalus CEK and through Lean, as
   `PreludeProofsTest` does. A wrong expectation is caught on the Scalus CEK before it reaches
   Lean.
3. State the properties, and a negative control that must be refuted. Choose a budget (below).

## The budget rule

The Lean machine runs each predicate for at most `N` steps, and Blaster reasons about that
bounded run. The polarity rule makes a proof at any budget hold without the budget, but a budget
that is too small turns a true statement into a spurious counterexample:

> **A falsification is not a bug until it is replayed.** `UplcBlaster` replays every
> counterexample on the Scalus CEK and reports one that does not replay as spurious.

Proof cost grows superlinearly in the budget: for a `gcd` property whose worst sample needs 203
steps, budget 250 proved in 2 seconds, 350 took 52 seconds, and 500 did not finish. Small
budgets are cheap, so a budget well above a small program's worst path costs nothing; for a
large program, stay close above its worst path, and lower the budget if a proof is slow.

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

`PreludeProofsTest` proves `abs`, `min`, `max` and `clamp` fully, `exp2` only on its `exp < 0`
branch, and an `Option` match and a `List` fold. `gcd`, `gcd` compiled without the optimizer, and
`sqrt` are checked only on samples. Not implemented: `gcd` properties, `sqrt`/`isSqrt`, `log2`,
`pow`, `Data` round-trips, and codegen equivalence. See
[`docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md`](../docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md)'s
"What is not proved, and why" for the reasons.

## Limitations

1. **Blaster does not reconstruct proofs.** On `Valid` it uses `admit`. This is strong
   differential testing, not a kernel-checked guarantee. The trusted base includes Blaster's
   translation and Z3.
2. **Proofs are bounded** by the CEK step budget, and the polarity rule makes them hold without
   it.
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
