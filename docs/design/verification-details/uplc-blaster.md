# `UplcBlaster`: the `blaster-uplc` tactic

How `blaster-uplc` from [the verification overview](../verification-overview.md) (§6.2) is built.
Code: `scalus-verification/src/main/scala/scalus/verify/uplcblaster/UplcBlaster.scala` and
`scalus-verification/src/main/lean/ScalusProofs/Run.lean`. Tests: `UplcBlasterTest`,
`PreludeProofsTest` and `UplcBlasterLimitsTest`, and `VestingVerificationTest` in
`scalus-examples`. For how to use it, see `scalus-verification/README.md`.

## Pipeline

```
Prop ──lower──► Lowered(binders, body: LeafFormula, leaves: Vector[Program])
                  │
                  ▼
     Check.lean in a temporary directory ──► lake env lean, run in src/main/lean
                  │
   with binders:  ├─ ✅ Valid ─────────────► Proven (ProofKind.Blaster)
                  ├─ ❌ Falsified ─────────► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  ├─ ❌ Falsified, a witness is asked for ► Inconclusive(no replayable certificate)
                  ├─ ⚠️ Undetermined ──────► Inconclusive
   closed:        ├─ native_decide holds ──► Proven (ProofKind.LeanNative)
                  ├─ native_decide false ──► replay on the Scalus CEK ──► Refuted, or Inconclusive
                  ├─ backend/tool error ─────► Failed(reason)
                  └─ no result in time ────► Inconclusive(reason)
```

## The fragment

Preparation accepts statements built with:

- universal and existential quantifiers over `BigInt`, `Boolean`, `ByteString`, `Data`, and case
  classes whose fields are of these types or such case classes, at any proposition position;
- `existsLet(w)(x => p)`, which is lowered as the strict binding `let x = w in p` and introduces
  no SMT quantifier;
- `&&`, `||`, `!`, `==>` and `<=>` over these leaves:
  - a test, `Prop.Bool`;
  - a total `call` whose continuation is a test or another total call;
  - a partial call, `whenReturns(f, a)(r => p)`, whose continuation is the same;
  - `denotes(e)`, where `e` has any type;
  - `equal(a, b)` over `BigInt`, `Boolean` or `Data`.

Only quantified variables are limited, because Lean builds their values, and it builds only
`BigInt`, `Boolean`, `ByteString` and `Data` constants. A call's arguments and result, and the
operand of `denotes`, can have any type: they live inside a test's program, and the compiler passes
them.

**A variable of a case class** is expanded by `lower`, since Lean cannot build its value. It
ranges over the values of its constructor
([statement semantics](prop-semantics.md#1-values-and-quantifiers)), so it stands for one
variable per field, named `variable.field` in `Lowered.binders` and in a counterexample, and a
field that is a case class is expanded in turn. Each test that uses the variable starts with
`let variable = Constructor(fields…)`, which the compiler lowers to the class's own
representation, as it does for a constructor written in the test. An enum is not expanded: it has
several constructors. `Quantifiable` has an instance for every case class whose fields have one. The exception is a function registered
without SIR, which has no declared type: a call to it can pass only `BigInt`, `Boolean` and `Data`
values (see "The calling convention").

`<=>` becomes two implications, because each of its operands occurs in both polarities. Each leaf
program takes only the binders in scope at that leaf. Anything else produces a
`CompatibilityReport`, which the one-step API returns as `Unsupported`. That covers other binder
types, a call whose continuation
uses connectives or `whenReturns`, a polymorphic function, and a function whose program takes its
arguments differently from how the tests pass them.

A Valid result for an existential statement is a proof in the same sense as any other Blaster
result. The generated Lean proposition writes `∃ x, p x` in the equivalent form
`¬ ∀ x, ¬ p x`, so the generated quantifier binders all use the universal path. A Falsified
model is not by itself a replayable refutation of a statement that asks for a witness: showing that
none exists requires checking the whole quantified domain, not one concrete CEK run. Such a result
is therefore `Inconclusive`.

A quantifier asks for a witness by its position, not by its name: an `exists` in a positive
position, and a `forAll` in a negative one, under `!` or in the premise of `==>`. Either side of
`<=>` is in both. `!forAll(n => n * n != 4)` is true, with the witness 2; at a budget no run
finishes in, Lean falsifies it, and the body evaluated at whatever value the model leaves for `n`
would say that it is false. Where no quantifier asks for a witness, every variable ranges over all
its values, and the replayed assignment refutes the statement. That includes `!exists(x => p)`,
which is `∀ x, ¬ p x`: a value with `p` refutes it. `Refuted` is also available for `existsLet`,
because its witness has already been fixed and eliminated.

## Leaves and `LeafFormula`

The leaves of a statement are the nodes of its `Prop` tree that are not connectives. `lower`
compiles each leaf to a program in `Lowered.leaves`. `LeafFormula` keeps the connectives, and
replaces each leaf with its index in `Lowered.leaves`.

| `Prop` leaf | Program, over the binders `x1 … xn` | `LeafFormula` |
|---|---|---|
| `Bool(b)` | `λ x1 … xn. b` | `Test(i)` |
| `Call(f, a, r, total = true, k)` | `λ x1 … xn. (λ r. k)(f a1 … am)` | `Test(i)` |
| `Equal(a, b)` | `λ x1 … xn. a = b`, with `equalsInteger`, `equalsData` or Boolean equality | `Test(i)` |
| `Denotes(e)` | `λ x1 … xn. e` | `Denotes(i)` |
| `Call(f, a, r, total = false, k)` | `λ x1 … xn. f a1 … am`, and the total call's program | `Implies(Denotes(i), Test(i + 1))` |

Every program takes the binders in scope at its leaf, in declaration order, whether that leaf uses
all of them or not. This matters when a connective has a quantified proposition on only one side.

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
together with the test, so the optimizer may inline it. Other external references, such as the
compiler's own support functions behind `d.to[A]`, are left to the lowering.

**The calling convention.** The V3 lowering passes every value across a function's boundary in
its type's default representation (`SirTypeUplcGenerator.defaultRepresentation`): an integer as a
constant, a plain case class as a builtin `list data` of its fields (`ProdDataList`), a sum type
as `Data` (`DataConstr`), a type marked `@UplcRepr(UplcConstr)` as `constr` terms. A function
value's representation is `LambdaRepresentation(default(input), default(output))`, and a compiled
program's top-level value is converted to it, so a test calling a linked function and the
function's own program agree when both are lowered with the same options:

- the call's `ExternalVar` has the function's declared type, from its SIR, so an enum constructor
  such as `Circle(r)` is passed as the enum;
- `FunctionDef.fromCompiled` records the program's `UplcSignature`, the representation of each
  parameter and of the result, computed with the options it was compiled with;
- `lower` computes, from the same declared type, the signature the tests call it with under
  `UplcBlaster.options`, and returns `Left` on a mismatch, such as a program lowered by another
  backend. Both sides come from one type, so they differ only where the options do;
- a function registered with a UPLC program but no SIR has no declared type and no signature.
  A call to it takes its type from the values, which fixes the form only of `BigInt`, `Boolean`
  and `Data`, so other values are refused: an enum constructor would otherwise be passed as a
  list of its fields.

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

For a statement over `x0 : BigInt` and `x1 : Boolean` with one leaf, the tactic writes the file
below. A `Data` binder is declared as `(x2 : Data)` and passed as `Const.Data x2`.

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

Every check is a process of its own, which starts Lean and loads the workspace again. A Lean
server that stays across checks is built, and the tactic's move onto it is proposed:
[the Lean server](lean-server.md).

- **Time limit.** `UplcBlaster(budget, leanDirectory, timeout)` stops Lean, and the solver it
  started, after `timeout`, and is then inconclusive. Without it the tactic waits for Lean,
  which on some statements does not return
  ([Statements that do not finish](#statements-that-do-not-finish)). Blaster's own `timeout`
  option limits only the solver.
- **Keeping a check.** `UplcBlaster.writeCheck(lowered, budget, directory)` writes the leaves and
  `Check.lean` into a directory of the caller's, to read or to run by hand, for instance with
  `set_option profiler true`, which reports the time of each `#prep_uplc_run`.

The verdict is read from Blaster's output:

- `✅ Valid`;
- `❌ Falsified`, followed by lines `- xN: value` for the counterexample;
- `⚠️ Undetermined`.

Blaster has no machine-readable output: it reports only these log messages. Each counterexample
value is Z3's answer to `(eval xN)`, an SMT-LIB term that can span several indented lines, as a
`Data` value does. `SmtValues` reads it: `(- 1)` is a negative integer, `List.cons` and
`(as List.nil …)` build a list, `Prod.mk` a pair, and `(….ByteString.mk "ABC")` a byte string,
one character per byte, with SMT-LIB string escapes. Z3 abbreviates a large value, such as a long
list, with `(let ((a!1 …)) …)`, and starts each nested `let` on a new line without indentation; a
value is read up to the line that closes its parentheses, and the names are replaced by their
terms. Internally, `Blaster.Smt.Translate.main`
returns a structured `Result` (`Valid`, `Falsified` with the `name: value` strings, or
`Undetermined`). If the log format changes, a command in `Run.lean` can call it and print JSON
instead; the values would still be SMT-LIB terms.

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

1. It reads Blaster's counterexample and completes it: a binder the model leaves unconstrained
   takes `0`, `false` or `I 0`. Blaster omits such a binder, or Z3 answers with its SMT name,
   such as `$0`. A value it cannot read, such as a byte string with a character
   above 255, makes the result `Inconclusive`.
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
runs `sbt scalusVerification/test` and `VestingVerificationTest` with `SCALUS_REQUIRE_LEAN=1`, so
a test that cannot run Lean fails instead of being canceled. In `ci-jvm` those tests are canceled.

## Limits

- **Types.** A quantified variable is a `BigInt`, a `Boolean`, a `ByteString`, a `Data`, or a
  case class of them. An enum, a `List` or an `Option` cannot be quantified over yet; quantify
  over `Data` with `denotes(d.to[A]) ==> …`. The operands of `equal` must be `BigInt`, `Boolean`
  or `Data`. Call arguments and results can have any type.
- **Byte strings.** Lean's model stores a byte string as a `String`, so a `ByteString` variable
  ranges over more than byte strings. A proof holds of them all the same; a counterexample with
  a character above 255 cannot be read, and is inconclusive.
- **Byte-level builtins.** Blaster cannot translate `BitVec`, whose width is a value index rather
  than a type parameter. The model stores a byte string as a `String`, so comparing, appending
  and measuring byte strings translate, but reading one byte (`indexByteString`) goes through a
  `Char`, whose `UInt32` value is a `BitVec 32`, and the bitwise builtins use `BitVec` directly. A
  statement with binders fails to translate, at every budget, when its programs reach
  `indexByteString`, `shiftByteString`, `integerToByteString` or `byteStringToInteger` on a
  symbolic value. `Math.exp2`'s branch for `exp ≥ 0` is
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
- **`Data` round-trips** of case classes. `Data` binders make them expressible now, as
  `forAll[Data](d => denotes(d.to[A]) ==> …)`; none is stated yet.
- **Codegen equivalence** (optimizer on and off, PV10 and PV11, the lowering backends). It can now
  be stated:

  ```scala
  forAll[BigInt, BigInt]((x, y) =>
      callRef(gcd.ref, (x, y))(r => callRef(gcdUnoptimized.ref, (x, y))(s => r == s))
  )
  ```

  For `gcd`, though, it hits the budget problem. The spike proved it at PV10 in 53 s; at PV11,
  attempts at budget 350 did not finish. Start with a function whose step count is constant.

## What `VestingVerificationTest` covers

`scalus-examples/jvm/src/test/scala/scalus/examples/vesting/VestingVerificationTest.scala` states
properties of the vesting example. The JVM tests of `scalus-examples` depend on this module and
on its test support (`LeanProofs`), so a contract's proofs sit next to its other tests.

| About | Statement | Quantified over | Budget | Time |
|---|---|---|---|---|
| `linearVesting` | nothing is vested before the start | the datum, a `Config`, and the time | 400 | 6 s |
| | everything is vested from the end on | | | 5 s |
| | it returns: no division by a zero duration | | | 5 s |
| | its contract, read from its body: it returns, and for `initialAmount >= 0`, `0 <= vested <= initialAmount` | | | 6 s |
| | it does not decrease with time, for `initialAmount >= 0` | and a second time | | 6 s |
| the validator | an output without a datum cannot be spent | `txInfo`, redeemer, reference: any `Data` | 600 | 6 s |
| | a non-positive amount is rejected | `txInfo`, reference, datum: any `Data` | | 7 s |
| | nothing but spending is validated | `txInfo`, redeemer, script info: any `Data` | | 9 s |
| a withdrawal | a sample is accepted, and rejected unsigned | closed, `native_decide` | 12000 | 7 s |
| | unsigned, it is rejected | datum, locked and requested amounts, time, fee; outputs: any `Data` | | 13 s |
| | it leaves at least what has not vested | the same, and the amount paid | | 43 s |
| | after the end, everything locked can be withdrawn | datum, locked amount, time, fee | | 26 s |
| | the three guarantees `spend` states with `Spec.ensures`: the beneficiary signed, something is withdrawn, at least the unvested amount stays locked | a `Withdrawal`: the shape's numbers, and one signer of any key | | 67 s together; 3 min one by one |
| | had `linearVesting` expected `initialAmount >= 0`, its call would owe it: refuted; proved under the withdrawal's own contract | a `Withdrawal` | | 40 s |

The validator's specification is written in `VestingValidator` itself. `spend` states its three
guarantees at its head; `Verifier.guarantees` finds them in the code of the function that builds
a withdrawal and validates it, where the inlined handler ends up, and they are proved as one
statement, which runs the validator once and not once per clause. The
schedule's contract is an `ensuring` clause of `linearVesting`, read with
`Contract.inSource` and proved over a `Config` variable. Its bound is conditional, not a
precondition, because the validator would not establish one. It has two negative controls, stated
with `contract(linearVesting)(expects, ensures)`: a wrong postcondition and a bound without its
condition; the monotonicity statement and the three statements about withdrawals have
one each. For two of the latter, a refutation is an accepted withdrawal: the solver finds one in
about 45 s, and it is replayed on the Scalus CEK.

The obligation is `Verifier.obligations` on a function of a `Withdrawal` that builds the context
and calls the validator; see
[call-site obligations](prop-semantics.md#call-site-obligations). The registered validator itself
takes any `Data`, and its obligation is one of the
[statements that do not finish](#statements-that-do-not-finish).

**Withdrawals have one shape.** The context is written with the ledger types in the statement
(`ScriptContext(TxInfo(inputs = …), …).toData`), over integer variables: one input, the vesting
output; at most one output, to the beneficiary; a fixed beneficiary. A statement says nothing about
another shape.

What the suite shows about the tactic:

- **The budget is not the limit for a program without a data-dependent loop.** The validator makes
  thousands of steps on a withdrawal, and those statements prove at a budget of 12000. The cost
  explosion measured on `gcd` ([Budgets](#budgets)) comes from a loop whose length depends on a
  symbolic value, where every further step can branch.
- **A loop over a list of unknown length is not finished by Lean.** The outputs can be left as
  any `Data` in the statement about an unsigned withdrawal, because every run fails before the
  validator reads them. The same in the statement about the unvested amount gives no result; the
  test states it with a time limit. See
  [Statements that do not finish](#statements-that-do-not-finish).
- **The proofs are not about the published script.** `VestingContract` compiles with
  `Options.release`, which uses the `Value` builtins that Lean's model lacks
  ([Limits](#limits)); the statements are about the validator compiled with `UplcBlaster.options`.
- **Nested `inline def`s of a class cannot be used in a statement.** One that uses another leaves a
  reference to the class's `this` in the leaf, which the plugin rejects. They are in an object.

## Statements that do not finish

Some statements are natural to write, and Lean does not return on them. `UplcBlasterLimitsTest`
and one test of `VestingVerificationTest` state them, with a time limit. In each, how long a loop
runs depends on a quantified value.

These tests carry the ScalaTest tag `Unfinished`. The build leaves it out of `test` and
`testQuick`, in `Test / test / testOptions`, so `testOnly` still runs them: by suite, by name with
`-z`, or all of a suite's with `-n scalus.verify.uplcblaster.Unfinished`. A tag excluded in
`Test / testOptions` itself could not be included again from the command line, and a test marked
`ignore` cannot be run at all.

| Statement | Budget | Result | Time |
|---|---|---|---|
| a list's `length` is not negative | 100 to 800 | spurious counterexample | 5 to 7 s |
| `filter` does not lengthen a list | 100 | spurious counterexample | 4 s |
| | 200 | spurious counterexample | 8 s |
| | 400 | spurious counterexample | 101 s |
| `gcd(x, y) >= 0` | 200 | spurious counterexample | 5 s |
| | 400 | none | over 200 s |
| vesting: at least the unvested amount stays locked, for any outputs | 1000 | spurious counterexample | 10 s |
| | 1500 | spurious counterexample | 18 s |
| | 2000 to 12000 | none | over 150 s; at 4000, over 7 min |

**None of them can be proved at any budget.** Some input needs more steps than the budget, and
there a conclusion, read strongly, is false. Lean's counterexample is that input, a long list for
instance, and the replay finds the statement true on it: inconclusive. Proving them takes
induction, which this tactic does not do. The vesting statement is different: it is true within
the budget, as shown below.

**Where the time goes.** There are two mechanisms, told apart with `set_option profiler true` in
a check written by `writeCheck`, and by which process is busy.

- **Paths in Lean.** `#prep_uplc_run` has Blaster run the CEK machine symbolically for the whole
  budget, and every test of a symbolic value splits the run. `length` asks one question per
  element, whether the list ends, and one answer ends the run: one path per length, and a cost
  that hardly grows with the budget. `filter` also keeps or drops each element, and both runs go
  on: the paths double with every element the budget reaches. At budget 400 each of its two
  leaves takes 47 s in `#prep_uplc_run`, and the solver a few seconds.
- **Arithmetic in the solver.** `gcd` splits once per round, like `length`, and Lean prepares it
  in 5 s. But each path's condition nests one more remainder, which is nonlinear. Z3 does not
  return, and holds 7 GB after 34 s.

**The premise does not prune.** In the vesting statement the premise says that the withdrawal
would leave too little, and under it the validator fails before it reads the outputs. But each
leaf's program is prepared on its own, for all values of the variables, so Lean also follows the
runs that pass the check. Those search the outputs for the beneficiary's: a list of unknown
length, with a choice at every element. The cliff between budgets 1500 and 2000 is where the runs
reach it.

What that statement costs, by how much of the outputs is left open, at budget 12000:

| Outputs | Result | Time |
|---|---|---|
| one output to the beneficiary, of any amount | proved | 43 s |
| one output to an address that is any `Data`, in lovelace | proved | 89 s |
| one output that is any `Data` | Lean's heartbeat limit, in `#prep_uplc_run` | 137 s |
| any `Data` | none | over 7 min |

An output that is any `Data` has a value that is a map of unknown size, which the validator sums.
So a variable can be left open where the program only passes it on or compares it, and needs its
shape written out where the program loops over it.

What could change this:

- **Prepare a conclusion under its premise.** For `p ==> fails(f(a))`, one program that tests
  `p` first and runs `f(a)` only then. The runs that `p` excludes disappear only if the symbolic
  run asks the solver whether a path is feasible, which Blaster's does not.
- **Bound a list in the statement,** as the withdrawal statements do by hand: a quantifier over
  lists of at most `n` elements, expanded into `n + 1` shapes.
- **Induction,** in `lean-direct` (overview §6.3), for statements about every length.
