# scalus-verification

Formal verification of Scalus code. Statements are written in Scala and proved by pluggable
tactics; the design is in
[`docs/design/verification-overview.md`](../docs/design/verification-overview.md), and how the
built parts work in
[`docs/design/verification-details/`](../docs/design/verification-details/). What exists
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
- `src/main/scala/scalus/verify/lean/LeanServer.scala` - a running Lean language server for a
  workspace: `LeanServer.start(directory)`, `check(source, timeout)` and `close()`. The tactic
  runs its checks in one; see `docs/design/verification-details/lean-server.md`.
- `src/main/scala/scalus/verify/lean/LeanServerProvider.scala` - what gives a tactic its server,
  and `LeanServers`, the provider for a workspace: one server at a time, started when a check
  needs it.
- `src/test/scala/scalus/verify/uplcblaster/PreludeProofsTest.scala` - properties of `Math` and
  of prelude data structures, each with a negative control and samples.
- `src/test/scala/scalus/verify/uplcblaster/UplcBlasterLimitsTest.scala` - natural statements
  that Lean does not finish, each with a time limit. They carry the tag `Unfinished`, which
  `test` leaves out and `testOnly` runs.
- `scalus-examples/jvm/src/test/scala/scalus/examples/vesting/VestingVerificationTest.scala`, in
  the examples module - properties of the vesting contract: its schedule, and its validator on
  script contexts and withdrawals. The JVM tests of `scalus-examples` depend on this module.

## Statements

```scala
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.lean.LeanServers
import scalus.verify.uplcblaster.{Budget, UplcBlaster}

val clamp = FunctionDef(Math.clamp)
val verifier = Verifier.empty
verifier.addFunction(clamp)
val inRange = verifier.statement(
  "clamp_in_range",
  forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
      (lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
  )
)

// The Lean servers of the workspace, which the caller ends. A tactic is given them, and takes
// a server when it runs a check: the first check starts one.
val lean = LeanServers.in(Path.of("scalus-verification/src/main/lean"))
try verifier.verify(inRange, UplcBlaster(Budget.LeanSteps(120), lean)) // Proven
finally lean.close()
```

The later examples use this `lean`. The first argument is the budget: `Budget.LeanSteps(n)`,
the steps of Lean's machine a check lets each test's program run, or `Budget.Auto`, with which
the tactic finds them ("The budget rule").

`UplcBlaster` proves statements with universal and existential quantifiers over `BigInt`,
`Boolean`, `ByteString`, `Data` and case classes of them: Boolean tests, calls, `denotes` and
`equal`, combined with `&&`, `||`, `!`, `==>` and `<=>`. `existsLet` supplies and eliminates an
explicit witness, avoiding an SMT quantifier; ordinary `exists` is emitted to Blaster as the
classically equivalent `¬∀¬`. A variable of a case class, as in
`forAll[Config, BigInt]((config, time) => …)`, is one variable per field for Lean. Each test becomes
its own closed UPLC predicate over the quantified values in its scope. The tactic writes them
to files, gives its Lean server a check that imports them, and reads Blaster's verdict from
what Lean reports. The server loads the workspace once, so a small check takes a fraction of a
second after the first. The workspace must be built (`lake build`): the server does not build
it. A check that Lean's process does not survive, for want of stack or of memory, is a failed
result for its statement, and the server takes the next one. Where the server itself has ended,
`LeanServers` starts another for the next check. A tactic is given a `LeanServerProvider`, which
`LeanServers` is; one server of the caller's own is `() => Right(server)`.

- **Valid** means the statement holds without the budget. A test in a positive position must halt
  within the budget with `true`; one in a negative position (a premise, or under `!`) must only
  neither halt with `false` nor fail within the budget. `denotes` is read the same way. The
  predicates run with `ScalusProofs.Run.runFor`, which keeps a failing program (`State.Error`)
  apart from an exhausted budget, so a statement that a program fails, such as
  `!denotes(BigInt(7) / BigInt(0))`, can be proved (design doc §6.2).
- **Falsified** is replayed on the Scalus CEK with a large budget. If the statement is false
  there too, the result is `Refuted` with the counterexample; otherwise it is `Inconclusive`, and
  the message says the counterexample is spurious. A spurious counterexample means the budget is
  too small. A falsified statement that asks for a witness, with an ordinary `exists` or with a
  `forAll` under `!` or in a premise, is `Inconclusive`, because it has no finite counterexample
  that the Scalus CEK can replay to establish that no witness exists; use `existsLet` when a
  witness is known.
- Anything else, including other binder types or an unsupported call continuation, fails
  preparation with `Unsupported(CompatibilityReport(...))`.
- Lean does not return on some statements, typically one whose program loops over a list of
  unknown length. So a check has a time limit, by the clock: ten minutes
  (`UplcBlaster.defaultTimeout`), or the `timeout` of `UplcBlaster(budget, lean, timeout)`. A
  check that reaches it is given up, and is `Inconclusive`; see "Statements that do not finish"
  in the tactic's details. `withoutTimeout` lifts the limit. The reason says where the check
  was: still running a test's program symbolically, which a smaller budget may cure, or with
  Blaster and the solver. With `Budget.Auto` the limit is that of the whole search, and
  `withAttemptTimeout(t)` adds one for each of its checks.
- So is a check that Lean gives up itself, at its limit of work for one command
  (`maxHeartbeats`). A check sets that limit to twice Lean's default;
  `UplcBlaster(budget, lean).withMaxHeartbeats(n)` sets another, and `0` none. It is the quick
  end of a command that grows without bound, the same on every machine, and it does not end
  every check that does not finish: the solver's time does not count towards it.
- With the environment variable `SCALUS_LEAN_KEEP_CHECKS` set to a directory, every check that
  runs is also written there, to read or to run by hand with `lake lean Check.lean`.

`verifier.prepare(statement, tactic)` returns an opaque `PreparedRun` only when the complete goal
is in the tactic's language. Passing that evidence to `verifier.prove(prepared)` cannot return
`Unsupported`. The one-step `verify` method remains as a convenience. `Inconclusive(reason)` then
means a compatible execution could not decide the goal, while `Failed(reason)` reports a tool or
backend execution failure.

A closed statement, without quantified variables, such as a sample
`callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))`, has nothing for
Blaster to search. Lean decides it by running its predicates, with `native_decide` over the
computable `runProgramFor`; its proof kind is `LeanNative`, which trusts the Lean compiler instead
of Blaster and Z3. This also works for programs Blaster cannot reduce, such as those reaching
`integerToByteString` ([Lean-blaster#273](https://github.com/input-output-hk/Lean-blaster/issues/273)).

A call of a function registered in the verifier is linked to that function's compiled program, so
the proof is about those exact bytes. A `call` is total: it claims that the function returns.
`whenReturns(f, a)(r => p)` is partial: it claims `p` only where the function returns, and is
proved as `denotes(f(a)) ==> call(f, a)(r => p)`. The proof still needs `f` to return or fail
within the budget on every input: a run that exhausts the budget counts as one that may return,
so `p` must then hold, and a partial function that loops on some inputs cannot be proved. Other
`@Compile` definitions a test uses are compiled with the test. A function of several parameters is
called with a tuple written out, as in `callRef(clamp.ref, (x, lo, hi))(r => lo <= r)`, and its
program is applied to each value in turn. Arguments and results can have any type, case classes
included: the compiler passes them. A `FunctionDef` records how its program takes its arguments,
and the tactic rejects a function compiled with options that pass them differently.

A binder's body is a statement or a Boolean. Statements nest quantifiers, calls and connectives:
`forAll[BigInt](x => denotes(BigInt(10) / x) ==> (x != BigInt(0)))`. A Boolean body is one test,
`if`, `match` and local `val`s included: `forAll[Boolean, BigInt]((c, x) => if c then x > 0 else x < 0)`.
A Boolean operand of a connective is one test too. Each test or expression is compiled as a closed
lambda over the variables it uses. In a statement body, a variable may only be used inside them:
an `if` that chooses between statements is a compile error; state the cases with `==>`.
`Prop(...)` is needed only to make a Boolean a separate test on purpose: `!Prop(t)` holds when
`t` fails, while `!t` is one test that fails with it.

### Contracts

A contract states a function's precondition and postcondition (design doc §3.7):

```scala
val clamp = FunctionDef(Math.clamp)
verifier.addFunction(clamp)
val inRange = verifier.contract(
  "clamp_in_range",
  contract(clamp)(
    expects = (x, lo, hi) => lo <= hi,
    ensures = (x, lo, hi) => r => lo <= r && r <= hi
  )
)
verifier.verify(inRange, UplcBlaster(Budget.LeanSteps(120), lean)) // Proven
```

`contract` is partial: `∀ args. expects(args) ==> whenReturns(f, args)(r => ensures(args)(r))`,
so a call that fails satisfies it. `totalContract` also claims that the function returns where
`expects` holds. `verifier.contract` registers the statement as the function's contract, and
`verifier.contracts(clamp.ref)` lists them. The function's parameters must be quantifiable:
`BigInt`, `Boolean` or `Data`.

A contract can also state when its function returns and when it fails:

```scala
val div10 = FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)
contract(div10)(expects = x => true, ensures = x => r => r <= BigInt(10))
    .returnsWhen(x => x != BigInt(0))
    .failsWhen(x => x == BigInt(0))
```

Each clause is a sufficient condition, under `expects`, and arguments that satisfy neither are
left open. `totalContract` is the contract with `.returnsWhen(_ => true)`.

The same two exist as statements of their own:

```scala
returnsWhen(div10)(x => x != BigInt(0))   // ∀ x. x ≠ 0 ==> succeeds(div10, x)
failsWhen(div10)(x => x == BigInt(0))     // ∀ x. x = 0 ==> fails(div10, x)
```

`succeeds(f, a)` is the total call `call(f, a)(_ => true)`, and `fails(f, a)` is its negation.
`failsWhen` states what a function must reject, which its runtime `require`s implement. For any
expression, `succeeds(e)` is `denotes(e)` and `fails(e)` is `!denotes(e)`: its evaluation ends in
an error, as in `fails(BigInt(10) / x)` or `fails { require(x >= 0); x * 2 }`.

### Contracts in the function's body

A function can state its contract itself, with `Spec` from scalus-core's prelude:

```scala
import scalus.cardano.onchain.plutus.prelude.Spec
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring

@Compile
object Vault {
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}

val clamp = FunctionDef(Vault.clamp)
val stated = Contract.inSource(clamp).get              // None for a function that states none
verifier.verify(verifier.contract("clamp_in_range", stated), UplcBlaster(Budget.LeanSteps(120), lean)) // Proven
```

The clauses are no part of the script: they are removed before lowering, so its bytes and hash
are the same with and without them. They stay in the function's SIR, where `Contract.inSource`
reads them, and on the JVM they are checked and throw `SpecificationError`. The contract is
declared, proved and owed by callers like one built with `contract(f)(expects, ensures)`.

A validator's handler returns nothing, and states what holds of its parameters where it returns,
with `Spec.ensures(condition)` at its head. A handler is `inline`, so its clauses are in the code
of whatever calls `validate`; `verifier.guarantees(function)` finds every clause on a path of a
function's code and declares one statement for each:

```scala
val spends = FunctionDef.named("spends", (w: Withdrawal) => VestingValidator.validate(context(w)))
verifier.addFunction(spends)
val stated = verifier.guarantees(spends.ref)
stated.statements.foreach(verifier.verify(_, UplcBlaster(Budget.LeanSteps(12000), lean)))   // clause by clause
stated.together.foreach(verifier.verify(_, UplcBlaster(Budget.LeanSteps(12000), lean)))     // or as one statement
```

Each statement says that the function returns, so a tactic runs the function's body once per
clause for the statements one by one, and once for `together`, their conjunction.

A clause that relies on the function's own `Spec.expects` holds only where callers establish it:
`verifier.guarantees(contract)`, with the function's declared contract, states the clauses under
that contract's precondition.

`VestingValidator` is specified this way: three guarantees on `spend`, and the postcondition of
`linearVesting`.

### Call-site obligations

A precondition is an obligation of every caller. `verifier.obligations` declares it for each call
of a function with a contract, as a statement to prove:

```scala
@Compile
object Vault {
    def bounded(x: BigInt, lo: BigInt, hi: BigInt): BigInt =
        if lo <= hi then Math.clamp(x, lo, hi) else lo
}

val bounded = FunctionDef(Vault.bounded)
verifier.addFunction(bounded)
val CallObligations(statements, unsupported) = verifier.obligations(bounded.ref)
statements.foreach(owed => verifier.verify(owed, UplcBlaster(Budget.LeanSteps(120), lean))) // Proven
```

The statement says: where the call is reached, its arguments satisfy the callee's `expects`. It is
proved here because the branch establishes `lo <= hi`; `Math.clamp(x, 10, 0)` would be refuted.
A runtime `require(lo <= hi)` before the call establishes it as well, and so does the caller's own
contract, with `verifier.obligations(contractOfBounded)`. Calls inside function values are listed
in `unsupported`, not checked. A validator's call is checked through a function of a transaction
shape that builds the context and calls it, as `VestingVerificationTest` does for the schedule's
precondition.

## Running

```bash
cd scalus-verification/src/main/lean && lake build && cd -   # once, and after a Lean change
sbt scalusVerification/test                                                        # about 1 min of tests
sbt "scalusExamplesJVM/testOnly scalus.examples.vesting.VestingVerificationTest"   # about 2 min
sbt "scalusVerification/testOnly *UplcBlasterLimitsTest"   # the statements Lean does not finish
```

The tests that run Lean need `lake` on the `PATH` (the default and `ci` nix shells have it) and
the built library. A suite starts one Lean server for its checks, in its workspace, and closes
it after its last test. Without them those tests are canceled, as in ci-jvm. With the
`SCALUS_REQUIRE_LEAN` environment variable set, as in the Lean-Proofs workflow, they fail instead.

### Kept results

`PreludeProofsTest` and `VestingVerificationTest` keep the results of their statements, each in
a file beside its source, `<Suite>.proofs.json`, which is committed. An entry has the hashes of
what the result rests on: the statement as Lean is given it, the program of each test, and
Lean's side. See the [overview](../docs/design/verification-overview.md#56-kept-results).

- **With Lean**, a statement whose entry still agrees is not asked of Lean again: a kept proof
  is taken as it is, and a kept refutation is replayed on the Scalus CEK. A statement that
  changed is proved again, and its entry replaced. Commit the file with the change.
- **Without Lean**, as in ci-jvm, the same statements pass on what is kept. One whose entry no
  longer agrees is stale: its test is canceled, and the reason names the part that moved.
- **With `SCALUS_REQUIRE_LEAN`**, as in the Lean-Proofs workflow, every statement is asked of
  Lean whatever is kept, and a result that differs from the kept one fails.

`SCALUS_KEPT_RESULTS` says which of these a run is: `use`, `frozen`, `recalculate`, or `off` for
a run that keeps and takes nothing. After a change of the tactic that no fingerprint shows, run
with `recalculate`.

```scala
// Another suite keeps its results by naming the file.
override protected def keptResultsFile: Option[Path] = Some(
  LeanProofs.inSources("my-module", "src", "test", "scala", "my", "pkg").resolve("MyTest.proofs.json")
)

// Outside the tests: a verifier that keeps its results.
val verifier = Verifier.keeping(KeptResults.in(file, KeptResults.Mode.Use))
```

### The workspace of a suite

A suite's workspace is `leanWorkspace` of `LeanProofs`. Unless the suite overrides it, it is the
workspace of the library, `src/main/lean` in these sources, or the directory
`SCALUS_LEAN_WORKSPACE` names. `VestingVerificationTest` overrides it with a workspace of the
example, `scalus-examples/jvm/src/test/lean/LinearVesting`, where proofs about the validator
written by hand belong as well. Such a workspace is a Lake package that requires `ScalusProofs`
by its path, and sets its `packagesDir` to that of the library's workspace, so Blaster and
PlutusCore are cloned and built once for both. The suite's checks import the library only, so
they need the library built, and not the example's own module; `lake build` in the example's
workspace builds that module, in seconds. Its `lean-toolchain` and its manifest have to name
the Lean and the revisions the library's name: after `lake update` in the library's workspace,
run `lake update` in it too. The suite checks that they agree.

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

The path that counts is that of the runs the statement is about. A statement that a validator
rejects a transaction wants the budget of the rejection, not of an accepted transaction: the
vesting validator rejects an early withdrawal after about 1600 steps, before it reads the
outputs. With the outputs left open the statement is proved in 12 seconds at budget 1700, and
not at all at 12000, the budget of a whole withdrawal, where Lean follows the accepting runs
through a list of unknown length.

Do **not** derive budgets from Scalus's own step count. Plutus charges per `Eval` transition
while this machine counts `Eval` and `Return`, so the Lean figure is about 1.85x larger.

**`Budget.Auto` finds the budget.** The tactic starts at 100 steps. Where Lean's counterexample
is spurious, it has Lean's machine count what each test's program does on that counterexample,
and goes on at the least budget at which the statement holds there. That is the budget of the
runs the statement is about: a premise that is false need only be seen to be false, however
long the program of the conclusion runs. For the vesting statement above the search goes
100, 262, 649, 694, 1315, 1573, 1584, 1608, and proves it there, in 45 seconds for the eight
checks. The proof's `Artifact.budget` is the budget found, to give as `Budget.LeanSteps` where
the search is to be saved. A statement no budget proves, such as one about a loop over a whole
list, gets a longer counterexample from every check, and ends at the time limit.

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
[what `PreludeProofsTest` covers](../docs/design/verification-details/uplc-blaster.md#what-preludeproofstest-covers)
for the reasons.

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
5. **Functions reading single bytes or reaching the CIP-121/122 bitwise builtins cannot be
   proved generically.** Blaster cannot translate `BitVec`, whose width is a value index rather
   than a type parameter. The model reads a byte through a `Char`, whose value is a `BitVec 32`,
   and the bitwise builtins use `BitVec` directly. Any property quantifying over a symbolic input
   whose CEK trace reaches `indexByteString`, `shiftByteString`, `integerToByteString` or
   `byteStringToInteger` fails to translate, at every budget. Comparing, appending and measuring
   byte strings, as `Data` values need, translate. This is why `Math.exp2` is proved only on its
   `exp < 0` early return.
