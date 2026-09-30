# `UplcBlaster`: the `blaster-uplc` tactic

How `blaster-uplc` from [the verification overview](../verification-overview.md) (§6.2) is built.
Code: `scalus-verification/src/main/scala/scalus/verify/uplcblaster/UplcBlaster.scala` and
`scalus-verification/src/main/lean/ScalusProofs/Run.lean`. Tests: `UplcBlasterTest` and
`PreludeProofsTest`. For how to use it, see `scalus-verification/README.md`.

## Pipeline

```
Prop ──lower──► Lowered(binders, body: LeafFormula, leaves: Vector[Program])
                  │
                  ▼
     Check.lean in a temporary directory ──► lake env lean, run in src/main/lean
                  │
   with binders:  ├─ ✅ Valid ─────────────► Proven (ProofKind.Blaster)
                  ├─ ❌ Falsified ─────────► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  ├─ ⚠️ Undetermined ──────► Inconclusive
   closed:        ├─ native_decide holds ──► Proven (ProofKind.LeanNative)
                  ├─ native_decide false ──► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  └─ anything else ────────► Inconclusive("Lean exited with code …: …")
```

## The fragment

`UplcBlaster.lower` accepts:

- a prefix of universal quantifiers over `BigInt` and `Boolean`;
- a body without quantifiers, built with `&&`, `||`, `!`, `==>` and `<=>` from these leaves:
  - a test, `Prop.Bool`;
  - a total `call` whose continuation is a test or another total call;
  - a partial call, `whenReturns(f, a)(r => p)`, whose continuation is the same;
  - `denotes(e)`, where `e` is a `BigInt` or a `Boolean`;
  - `equal(a, b)` over `BigInt` or `Boolean`.

`<=>` becomes two implications, because each of its operands occurs in both polarities. Anything
else lowers to `Left(reason)`, which the tactic returns as `Inconclusive`. That covers other
binder types, a quantifier after the prefix, a call whose continuation uses connectives or
`whenReturns`, and call arguments or results of other types.

## Leaves and `LeafFormula`

The leaves of a statement are the nodes of its `Prop` tree that are not connectives. `lower`
compiles each leaf to a program in `Lowered.leaves`. `LeafFormula` keeps the connectives, and
replaces each leaf with its index in `Lowered.leaves`.

| `Prop` leaf | Program, over the binders `x1 … xn` | `LeafFormula` |
|---|---|---|
| `Bool(b)` | `λ x1 … xn. b` | `Test(i)` |
| `Call(f, a, r, total = true, k)` | `λ x1 … xn. (λ r. k)(f a1 … am)` | `Test(i)` |
| `Equal(a, b)` | `λ x1 … xn. a = b`, with `equalsInteger` or Boolean equality | `Test(i)` |
| `Denotes(e)` | `λ x1 … xn. e` | `Denotes(i)` |
| `Call(f, a, r, total = false, k)` | `λ x1 … xn. f a1 … am`, and the total call's program | `Implies(Denotes(i), Test(i + 1))` |

Every program takes all the binders' values, in the order of the prefix, whether it uses them or
not.

A partial call, `whenReturns(f, a)(r => k)`, is rewritten to `denotes(f(a)) ==> call(f, a)(r => k)`
before it is lowered: if the function returns, the continuation holds of its result. The
`denotes` leaf returns the function's result rather than a Boolean. Where the call is a
conclusion, the polarity rule reads that `denotes` weakly, so a run of `f` that exhausts the
budget counts as one that may return, and the continuation must then hold within the budget. A
partial function that loops on some inputs therefore cannot be proved. Both leaves run the
function's program.

**Why one program per leaf.** A statement's truth depends on more than two outcomes per leaf. A
leaf can return a value, fail, or exhaust the budget. `!` on a `Prop` is true when a test fails,
while a failure inside one UPLC program would fail the whole program. And the polarity rule reads
each leaf by its position. So the connectives stay in Lean, over one run per leaf.

### Calls

A call is one SIR expression, `(result => continuation)(fn(arguments…))`, where `fn` is an
`ExternalVar` that carries the function's name. A call in the continuation nests the same way.

A function of several parameters is called with a tuple written out. `lower` takes that tuple's
components, the fields of `SIR.Constr("scala.TupleN")`, and keeps the compiler's `Let` and `Decl`
wrappers around each component. The program then applies the function to the components one at a
time, because compiled programs are curried. The data declarations of all the pieces move outside
the whole expression.

### Linking

`compileSirFunction` turns a leaf's SIR into a program:

1. `unlinkModuleDefinitions` removes the module definitions that `compile` put around the
   expression for functions in the function table. It keeps the definitions of other functions
   that the expression still uses, and those are compiled together with the leaf.
2. Each `ExternalVar` that names a table function becomes an outer parameter, before the binders.
3. The SIR is lowered to UPLC with `UplcBlaster.options`
   (`Options.releaseUntagged.copy(valueBuiltins = false)`). The result is applied, with a plain
   `Term.Apply`, to each function's own UPLC program (`Representation.Uplc`).

So a registered function's compiled bytes appear unchanged inside the predicate, and no
optimization runs across that boundary. A `@Compile` function that is not registered is compiled
together with the test, so the optimizer may inline it.

## Reading leaves by polarity

`renderFormula` reads each leaf by its position. `s` is the leaf's final state after at most
`budget` steps of `runFor` (next section).

| Leaf | Positive: a conclusion | Negative: a premise, or under `¬` |
|---|---|---|
| `Test` | `fromFrameToBool s = some true` | `fromFrameToBool s ≠ some false ∧ failed s = false` |
| `Denotes` | `halted s = true` | `failed s = false` |

`→` flips the polarity of its premise and `¬` that of its operand; `∧` and `∨` keep it. A
positive reading is strong: the program halts within the budget with the right result. A
negative reading is weak: the program neither halts with the wrong result nor fails within the
budget, so a run that exhausts the budget counts as possibly right. The overview (§6.2) shows why
a proof at any budget then holds without the budget.

Every reading is an equation between `Bool` values, for two reasons:

- Blaster translates `fromFrameToBool` on every goal. A `Prop`-valued match on
  `.Halt (.VCon (Const.Bool true))` fails to translate, on `Fin`, whenever the goal is
  falsifiable.
- `native_decide` can evaluate them (closed statements, below).

Replay reads the same `LeafFormula`, over the outcomes of the Scalus CEK (`holds`, below).

### `runFor`

PlutusCore's `runSteps` returns `State.Error` both when a program fails and when the step budget
runs out. `ScalusProofs/Run.lean` defines:

- `runFor v s n`, which makes the same steps as `runSteps`, but ends a run that is still going
  after `n` steps in the state it reached, not in `State.Error`. After `runFor`, `State.Error`
  means that the program failed.
- `runProgramFor p params n`, which is `cekExecuteProgram` over `runFor`, and is computable.
- `fromFrameToBool`, `failed` and `halted`, the readings above.
- `#prep_uplc_run name script inputs budget`, which is `#prep_uplc` over `runProgramFor`. It
  builds `fun xs => runProgramFor script (inputs xs) budget`, optimizes it with Blaster's
  `Optimize.main`, and defines `name` as noncomputable.

The weak readings need `runFor`. With `runSteps`, the negative reading of `denotes` could not
reject `State.Error`, which might be an exhausted budget, so it would be `True`. A premise
`denotes(f(x))` would then say nothing, and `!denotes(e)` could never be proved.

## The Lean check

For a statement over `x0 : BigInt` and `x1 : Boolean` with one leaf, the tactic writes:

```lean
import ScalusProofs.Run

namespace ScalusProofs.Runtime
open …

#import_uplc leaf0 PlutusV3 single_cbor_hex "/tmp/scalus-uplc-blaster-…/Leaf0.flat"

def arguments (x0 : Integer) (x1 : Bool) : List Term :=
  [Term.Const $ Const.Integer x0, Term.Const $ Const.Bool x1]

#prep_uplc_run prepared0 leaf0 arguments 100

#blaster (gen-cex: 1) [∀ (x0 : Integer) (x1 : Bool), (fromFrameToBool (prepared0 x0 x1) = some true)]

end ScalusProofs.Runtime
```

It runs `lake env lean Check.lean` with the workspace as the working directory, so the workspace
must be built (`lake build`). The temporary directory is removed afterwards.

The verdict is read from Blaster's output:

- `✅ Valid`;
- `❌ Falsified`, followed by lines `- xN: value` for the counterexample;
- `⚠️ Undetermined`.

Any other output, such as a translation error or a missing workspace, is returned in the
`Inconclusive` message. The message drops the `Successfully decoded` lines and is cut to 500
characters.

The artifact records the SHA-256 of each leaf's CBOR, the budget, Lean's output, the
counterexample and the proof kind.

## Closed statements

A statement without binders, such as a sample
`callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))`, has nothing for
Blaster to search. Blaster can also fail to reduce a concrete program. PlutusCore's
`integerToByteString` uses `Nat.log2`, and Blaster cannot reduce `Nat.log2` on a numeral
([input-output-hk/Lean-blaster#273](https://github.com/input-output-hk/Lean-blaster/issues/273)).
Even the samples of `exp2` and `sqrt` failed to translate.

So the tactic decides a closed statement by evaluation:

```lean
example : (fromFrameToBool (runProgramFor leaf0.script [] 80) = some true) := by native_decide
```

- **Exit code 0:** the result is `Proven`, with `ProofKind.LeanNative`. That proof trusts the Lean
  compiler, through `Lean.ofReduceBool`, instead of Blaster and Z3.
- **`native_decide` reports that the proposition is false:** the statement is replayed, as a
  falsification without values.

All leaves are evaluated in the one Lean process.

## Replay

A falsification under the budget can be spurious: a positive leaf that needs more steps reads as
false. `replay` works in four steps:

1. It completes Blaster's counterexample: a binder the model leaves unconstrained takes `0` or
   `false`.
2. It applies each leaf's program to the values.
3. It evaluates each program on the Scalus CEK, with a budget of a hundred times the mainnet
   per-transaction limit. That budget only guards against a program that does not terminate.
4. Each leaf's outcome is `Returned`, `Failed` or `Exhausted`. `holds` evaluates the
   `LeafFormula` over them, under the semantics of overview §3.3.

The result depends on what `holds` finds:

- **False:** `Refuted`, with the counterexample in the artifact.
- **True:** `Inconclusive`, with the message "Lean's counterexample (x = …) is spurious: … a test
  needs more than N steps".
- **Undecided, because a leaf exhausted the replay budget:** `Inconclusive`.

## Budgets

The budget counts Lean CEK transitions, both `Eval` and `Return`. It is not Plutus `ExUnits`.
Measured, the Lean count is about 1.85 times the number of steps Scalus counts: `min` needs 23
against 13, `abs` 26 against 14, and `gcd 12 18` 161 against 87. A budget of twice Scalus's step
count therefore leaves almost no headroom. In the original spike it produced a spurious
counterexample: optimizer equivalence of `gcd` failed at `x = -19, y = 14` because the programs
halt at steps 282 and 307, while the budget was 300.

Proof cost grows faster than linearly in the budget. On a `gcd` property whose worst input needs
203 steps:

| Budget | Proof time |
|---|---|
| 250 | 2 s |
| 350 | 52 s |
| 500 | no result in 500 s |

Programs whose step count does not depend on their input, such as `abs`, `min`, `max` and
`clamp`, prove over the whole domain at a small budget. A program whose step count grows with its
input, such as `gcd`, needs the input range written into the statement.

## Lean workspace and pins

The workspace is `scalus-verification/src/main/lean`: Lean 4.24.0 (`lean-toolchain`),
`lakefile.lean`, and a committed `lake-manifest.json`. `ScalusProofs.lean` only imports
`ScalusProofs.Run`. The checks are generated for each statement and are never stored.

- **PlutusCore** comes from the fork `nau/PlutusCoreBlaster` @ `fix/case-scrutinee-smt-blowup`,
  until input-output-hk/PlutusCoreBlaster#40 merges. Without the fix, Blaster does not terminate
  on UPLC `case`, which every PV11 program uses. The fix addresses two problems in the
  `Frame.CaseScrutinee` handler:
  - splitting `Const.Bool` left the decision tree stuck on `Bool.casesOn` for a symbolic value;
  - `Ms[n.toNat]?` is an opaque recursive application for a symbolic tag.

  Before the fix `Math.min` at PV11 gave no result in 10 minutes; after it, it proves in seconds.
- **Blaster** comes from `input-output-hk/Lean-blaster` @ `main`, not from the
  `beta-lambda-cache-optimization` branch. The two have diverged, and `main` has the
  `Int.ediv`/`Int.emod` fix that integer programs need.
- **Upstream force-pushes.** A lost revision shows up as `git exited with code 128`. Run
  `lake update` again, and commit the new manifest.
- **The toolchain** is `elan` and `z3`, from the default and `ci` nix shells.

CI: `.github/workflows/lean-proofs.yml` runs daily and on demand. It builds the workspace, then
runs `sbt scalusVerification/test` with `SCALUS_REQUIRE_LEAN=1`, so a test that cannot run Lean
fails instead of being canceled. In `ci-jvm` those tests are canceled.

## Limits

- **Types.** Binders, call arguments and results, and the operands of `denotes` and `equal` must
  be `BigInt` or `Boolean`. `Quantifiable` also covers `ByteString` and `Data`, which the tactic
  rejects.
- **Bitwise builtins.** Blaster cannot translate `BitVec`, whose width is a value index rather
  than a type parameter, and PlutusCore's `ByteString` is built on it. A statement with binders
  fails to translate, at every budget, when its programs reach `shiftByteString`,
  `integerToByteString` or `byteStringToInteger`. `Math.exp2`'s branch for `exp ≥ 0` is
  `byteStringToInteger(shiftByteString(hex"01", exp % 8) ++ integerToByteString(true, exp / 8, 0))`,
  so it is proved only for `exp < 0`.
- **`Value` and array builtins.** PlutusCoreBlaster's flat decoder has the CIP-153 `Value` and
  CIP-138 array builtins commented out. As of 2026-09-23, the upstream PRs were
  input-output-hk/PlutusCoreBlaster#15 (`Value`: conflicting, no review since 2026-05) and #12
  (arrays: mergeable). Until they land, programs are compiled with `valueBuiltins = false`.
- **One Lean process per statement,** which takes several seconds each.
- **No proof reconstruction.** Blaster closes a goal with an axiom; see overview §7.

## What `PreludeProofsTest` covers

Every function has samples: closed calls with their expected results. These are checked on the
Scalus CEK and decided in Lean by `native_decide`.

| Function | Properties proved | Negative control |
|---|---|---|
| `abs`, `min`, `max`, `clamp` | fully | yes |
| `exp2` | only for `exp < 0` | yes |
| an `Option` match, a fold over a two-element `List` | as stated | yes |
| `gcd`, `gcd` without the optimizer, `sqrt` | samples only | no |

The properties call their function with a total `call`, whose conclusion is read strongly, so each
one also claims that the function returns. The hand-written suite needed separate totality
theorems for that.

What is not stated, and why:

- **`gcd` properties.** Euclid's algorithm needs more steps for larger inputs, so a property needs
  an input range in the statement. The budgets it then needs are where proof cost explodes.
- **`sqrt` and `isSqrt`.** `sqrt` needs about 405 steps, which is also where proof cost explodes.
- **`log2` and `pow`.** They reach the bitwise builtins.
- **`Data` round-trips.** They need `Data` binders.
- **Codegen equivalence** (optimizer on and off, PV10 and PV11, the lowering backends). It can now
  be stated:

  ```scala
  forAll[BigInt, BigInt]((x, y) =>
      callRef(gcd.ref, (x, y))(r => callRef(gcdUnoptimized.ref, (x, y))(s => r == s))
  )
  ```

  For `gcd`, though, it hits the budget problem. The spike proved it at PV10 in 53 s; at PV11,
  attempts at budget 350 did not finish. Start with a function whose step count is constant.
