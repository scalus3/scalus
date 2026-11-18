# Statements as Lean modules, and as documents of the Universal Annotation Language

How a statement that `blaster-uplc` proves is written out of the test that proves it: as a Lean
module of the project's workspace, of which a proof written by hand is a theorem, and as a
property of the [Universal Annotation Language](https://github.com/input-output-hk/UniversalAnnotationLanguage)
(UAL 1.0), for a consumer of that language to check. Both are made from the statement as the
tactic lowered it, so they are about the same compiled programs as the check.

Status: built, in part. The modules and the documents are written, and the Scalus sbt plugin has
`leanExport` and `ualExport`. The linear vesting example has its statements in its workspace, and
two proofs by hand of them; the AMM example has a proof by hand that the `lean-manual` tactic
checks and keeps ([overview §6.4](../verification-overview.md#64-lean-manual), phase 2a of §9).
Not built: a script of a blueprint as a property's scope; a consumer of the language that checks
the documents here.

## A statement as a Lean module

`UplcBlaster.statementModule(goal, budget, maxHeartbeats, module, directory)` writes a lowered
statement as the module `module`:

```lean
#import_uplc LinearVesting_Generated_vesting_schedule_zero_before_start_leaf0 PlutusV3 single_cbor_hex
  "LinearVesting/Generated/vesting_schedule_zero_before_start/Leaf0.flat"
…
#prep_uplc_run …_prepared0 …_leaf0 arguments0 400

def statement : Prop :=
  (∀ (x0 : ByteString) (x1 : Integer) (x2 : Integer) (x3 : Integer), (∀ (x4 : Integer),
    ((fromFrameToBool (…_prepared0 x0 x1 x2 x3 x4) ≠ some false ∧ failed (…_prepared0 …) = false) →
      (fromFrameToBool (…_prepared1 x0 x1 x2 x3 x4) = some true))))
```

- **The proposition is the one the check gives Blaster**, at the budget of the proof: a test that
  is claimed returns `true` within the budget, and one that is assumed neither returns `false` nor
  fails within it. By the polarity rule it implies the statement without the budget
  ([the tactic](uplc-blaster.md)). Where the statement was proved as written, and not in the
  rejecting form, the module is of the statement as written.
- **The programs are files beside the module**, `<Package>/Generated/<name>/Leaf<n>.flat`, the
  CBOR that `#import_uplc` reads, with each as text, `Leaf<n>.uplc`, to be read. The `.flat` files
  are committed with the module, so a checkout builds without sbt. The texts are not: Lean does not
  read them, they are most of the size, and they change with every change of the code, so
  `<Package>/Generated/.gitignore` leaves them out, and `leanExport` writes them again. The
  module's header has the SHA-256 of each program: Lake does not see what `#import_uplc` reads,
  and the header makes a changed program a changed module, which Lake builds again.
- **What the proof found is the module's axiom `proved : statement`.** A statement is written
  where it is proved, and the axiom says so, with the tactic, the budget and the kind of the
  proof in its comment. A proof by hand may use it as a lemma, and then rests on that result:
  `#print axioms` names `<Package>.Generated.<name>.proved` among the axioms of the proof. So a
  proof by hand of one statement can stand on what Blaster proved of another, and says which.
  The axiom is about the programs beside the module, of which the proof was made: where the code
  changes, the module is no longer the code's, and its test fails (below).
- **The names of the programs have the module's name.** `#import_uplc` and `#prep_uplc_run`
  declare their names outside the namespace they are written in, so two modules whose programs
  were both named `leaf0` could not be imported by one proof.

`LeanProofs.writeStatement(statement, result)` writes a statement that `result` proved into the
workspace of the proofs, as `<Package>.Generated.<name>`: the statement's name, with `_` for every
character a Lean name cannot have. It runs no Lean: a statement whose result is kept is written as
well as one that was proved now. It is lowered by the tactic that proved it, with that tactic's
plan and limit of Lean's work. A file is written where its text differs, so a module of code that
has not changed is not built again. Two statements whose names are one Lean name, as `a.b` and
`a_b`, are an error, as is a statement whose module would be that of the types: neither takes the
other's module. So are two statements of one property id in the export to the language. The modules are committed, as the types are
([Lean types for Scala types](lean-types.md)). A suite does not call it: `leanExport` has every
named statement that is proved written so ("The sbt tasks").

**A module is held to the code.** Where the export is off, every check of a named statement whose
module the workspace has compares the module with the one the export would write now: its whole
text, which has the formula, the budget, the SHA-256 of each program and the axiom of the proof,
and the programs beside it. A module that differs, as one of other programs or one written before
the axiom was, fails the statement's test where Lean can run, or is required, and says to run
`leanExport`: a proof by hand of it would be checked by `lake build` against programs that are not
the code's, or cite an axiom it does not have. Where Lean cannot run, the test is not run, as a
stale result is not. It needs no Lean: the module is written from the statement and its proof, of
which a kept result has what is needed.

## A proof by hand

A proof by hand is in the workspace's own modules, under `<Package>/Proofs`, as
`LinearVesting/Proofs/Schedule.lean`:

```lean
import LinearVesting.Generated.vesting_schedule_zero_before_start

theorem zero_before_start : vesting_schedule_zero_before_start.statement := by
  unfold vesting_schedule_zero_before_start.statement
  blaster
```

`lake build LinearVestingProofs` in the workspace, or `sbt leanProve`, checks it against the
programs of the code as it is. The statements and the proofs are a library of their own,
`<Package>Proofs`, and not of the default target, which is the root module that the checks need: a
statement runs its programs symbolically where it is built, and a proof may not hold yet, so neither
is to stand in the way of a check. Where the code changes, the generated proposition changes with
it, and a proof that no longer holds no longer builds. The two proofs there are closed by `blaster`, as the checks are, which admits the goal it
finds valid: they show the mechanism, and are no kernel proof. A proof by other means, an
induction over a list of any length, is what the statements tagged `Unfinished` want.

A proof by hand may cite what Blaster proved, as a lemma, by the axiom of its module:

```lean
theorem by_the_proof : vesting_schedule_zero_before_start.statement :=
  vesting_schedule_zero_before_start.proved
-- 'by_the_proof' depends on axioms: [propext, Classical.choice, Quot.sound,
--   LinearVesting.Generated.vesting_schedule_zero_before_start.proved]
```

## Checking a proof by hand: `lean-manual`

A statement no proof of Scalus found is written for a proof by hand: the same module, without the
axiom `proved`, and with a header that says it is to be proved by hand. A suite names the theorem
that proves it, with the tactic `lean-manual` (`LeanManual`):

```scala
checkByHand(
  depositDilutesNoToken,                         // the statement, declared as any other
  "Amm.Proofs.Deposit",                          // the module of the proof
  "Amm.Proofs.Deposit.deposit_dilutes_no_token", // the theorem
  500                                            // the steps of the statement's module
)
```

- **The statement's module is written as the others are.** Where the proofs export, `leanExport`
  writes it; where they do not, the workspace's module is held to the code, as the text the
  tactic would write now. A module that is missing, or of other programs, fails the test, and says
  to run `leanExport`.
- **Lake builds the proof's module first**, `lake build <module>`, and with it the statement's
  module. Lean's server does not build an import that is out of date, as one `leanExport` has just
  written again: it says "Imports are out of date and must be rebuilt", as it tells an editor to
  restart the file. A proof that Lake does not build is none, with Lake's errors.
- **Lean elaborates a check of the theorem**, in the workspace's server: `import <module>`,
  `import <the statement's module>`, `example : <the statement's module>.statement := <theorem>`,
  `#print axioms <theorem>`. An error, as a theorem of another statement (`type mismatch`), is no
  proof.
- **The axioms say what the proof rests on.** Lean's own only: `LeanKernelOfOptimizedRuns`, the
  kernel, of the runs as Blaster's optimizer gives them (overview §6.4). `Lean.ofReduceBool`:
  `LeanNative`. `<Package>.Generated.<name>.proved`: a lemma, the statement of that module, which is
  to be among the verifier's theorems, and which the proof records as used. The module is to state
  the axiom, as the module of a statement Scalus proved does, and a second check has Lean take each
  such axiom as that module's, so a proof cannot declare its own of the name. `sorry`, the result
  of the proof's own statement, or another axiom: no proof, a failure. Nor is Blaster's
  `blasterProven`: it proves any proposition where it is used without the tactic, and its uses do
  not tell one from the other. What Blaster proves, a proof by hand cites by the `proved` of a
  statement Scalus checked.
- **The proof's sources are read too.** They are its module, which is to be under
  `<Package>/Proofs`, and the modules there it imports. One that sets `debug.skipKernelTC` makes
  the proof none: Lean's kernel does not check what is declared under it, and `#print axioms` does
  not say so. That the kernel checked every declaration of them is not replayed, as `lean4checker`
  would.
- **The result is kept** as a check's is. Its fingerprint has, beside the workspace's environment,
  a hash of the proof's sources, which the environment leaves out: a change of a proof by hand
  makes the results of the proofs that import it stale, and no others. It keeps the hash of the
  module of each statement it cites, and stands no longer where one changed.
- **The statement's module, and its programs, are held to the code**, by the tactic, and by the
  suite before a result kept of it is taken, as the module of a statement proved is: the whole
  text, which has the formula, the budget, the programs' hashes and, of a proved statement, its
  axiom, and the programs themselves. Where they are not the code's and Lean can run, or is
  required, the test fails, and says to run `leanExport`; where Lean cannot run, as on a build of
  another Scala version, whose programs differ, the test is not run, as a stale result is not.
- **The first check of a proof may take minutes**: Lean builds the statement's module, whose
  programs it runs symbolically, before it elaborates the check. Most of the AMM deposit's is the
  branch of the first deposit, which its proof passes over, run for as many steps as the budget
  has: at 800 steps it took about ten minutes here and more than thirty on CI's runner, so its
  budget is the 473 steps its claim needs on a pool with liquidity (`measureRun`), and a little
  more. The Lean-Proofs workflow builds `AmmProofs` before the tests, and keeps the build.

Two costs of the modules, measured on the vesting workspace (2026-11-10):

- **A module runs its programs symbolically when it is built.** `#prep_uplc_run` prepares each
  program's run at the statement's budget, so building the module of a statement about the
  validator at 3300 steps takes about three minutes, and `lake build LinearVestingProofs` with all
  of them about as long. Lake builds a module again only where its text changed.
- **A proof by hand is to be under `<Package>/Proofs`.** `LeanWorkspace.environment` hashes the
  sources of the workspace's package, which a kept result rests on, and leaves out
  `<Package>/Generated` and `<Package>/Proofs`: a check imports Scalus's library only, and no
  module of the workspace, so neither changes a check. A proof elsewhere in the package makes
  every kept result of the workspace stale.

## A statement as a property of the Universal Annotation Language

`UplcBlaster.ualStatement(goal, budget, id)` reads a lowered statement in the language's terms.

- **Each test's program is a compiled function**, `<id>_leaf<n>`, of the language's function
  registry (`ONCHAIN [kind: function]`). It takes the variables in its test's scope, in order:
  natively an integer, a Boolean, a byte string or a string (`#integer`, `#boolean`, `#bytes`,
  `#string`), a builtin list as `#list`, and a variable of `Data` or over a type's encodings as raw
  `Data`. Its result is a native Boolean.
- **The proposition is read by polarity, in the language's words**: a test claimed is
  `f_returns … true`, one assumed is `¬ f_returns … false ∧ ¬ isUnsuccessful (f …)`, and `denotes`
  is `isSuccessful (f …)`, or `¬ isUnsuccessful (f …)` where it is assumed. `f` is the state of
  the machine after at most the context's steps, and the language keeps an unfinished run apart
  from a failed one, as `runFor` does here.
- **A variable over a type's encodings has the type's predicate as a premise**, and the
  definitions the predicate uses are the property's `PREDICATE` fragment: the language requires
  the domain premises in the proposition.
- **A statement checked within a budget is not exported**: it is no theorem.

`UalExport.bundle(preamble, blueprint, environment, properties)` writes the documents:

- `assurance.json`: the properties, each with its text, its formal source and its scope of
  functions; the fragments; the function registry, with each function's compiled code (CBOR in
  hexadecimal, `cbor-flat`) and its SHA-256; and, where there are contexts, their digests. No
  evidence: a proof here is no evidence of the language, which a consumer makes by checking.
- `checking-<property>.json`, where the manifest of a consumer's environment is given: the profile
  `uplc-step-check/v1`, the semantics variant, the steps of the proof as a `cek-steps` budget,
  acceptance by evaluation, and each function as a target with its hash and interface.
- `annotations.ual`: the same, as the annotations of the language in `/*@ … @*/`, to be read.

What is not settled:

- **Every property is of functions, none of a script of the blueprint.** The programs proved are
  compiled with the tactic's options (no Value builtins), and are not the published script. The
  blueprint is referred to as `plutus.json`, without a hash.
- **The steps.** Lean's machine here counts `Eval` and `Return` transitions. That the language's
  `cek-steps` count the same is not checked; a consumer that counts otherwise needs another budget.
- **The semantics variant** is `E`, the default of the Lean model the checks run with. Which
  variant a deployment's language and protocol version have is the checking configuration's to
  say, and is not derived here.
- **No consumer has checked these documents.** The checker of the language is in PlutusCoreBlaster
  `main`, which the workspace here cannot pin yet: it needs a revision of Lean-blaster on which our
  type predicates do not return (see "Pinned dependencies" in `scalus-verification/README.md`).
  The documents are not validated against the language's JSON schemas either.

## The sbt tasks

The Scalus sbt plugin exports by running the project's tests with an export on. The proofs of the
tests do the export: every statement that has a name and is proved, by Lean or by a kept result, is
exported as it is checked. A suite changes nothing.

```
sbt leanExport    # the statements as modules, under <Package>/Generated of the workspace
sbt ualExport     # the documents, a directory for each suite under ualExportDirectory

sbt "scalusExamplesJVM/leanExport scalus.examples.vesting.VestingVerificationTest"
```

Without arguments a task runs all the project's tests; with them, the tests they name, as
`testOnly` takes them, `-- -l scalus.verify.uplcblaster.Unfinished` included. Lean is not needed:
a statement whose result is kept is exported as one that was proved now.

- `ualExportDirectory` is `target/ual` unless the build says another. A suite's directory has the
  name of its kept results' file, `<Suite>` of `<Suite>.proofs.json`. A property says the name of
  the test that proved it.
- The tasks tell the tests by resources on their class path (`scalus/verify/lean/export`,
  `scalus/verify/ual/export`), as the workspace is told, and delete them after: a plain `test`
  exports nothing. `verify` says so that Lean is required (`scalus/verify/lean/require`), and
  exports nothing: a module of a statement whose code changed fails the statement's test, which
  says to run `leanExport`.
- Without sbt, the environment variables `SCALUS_LEAN_EXPORT` (set) and `SCALUS_UAL_EXPORT` (a
  directory) do the same. `SCALUS_UAL_ENVIRONMENT` names the manifest of a consumer's environment,
  for the checking contexts.

The plugin's tasks compile for sbt 1 and sbt 2. This repository's examples enable the plugin, and
its build compiles the plugin from source, so the tasks are those of `scalusExamplesJVM` here.

## Writing a statement about a validator

A statement of what a validator's handler guarantees is a statement about the script: `spend` is
inline, and the program is the entry point, `validate`, which reads the datum, the redeemer and the
transaction from the context. It is written with the validator's code in it:

```scala
// spend: Spec.ensures(txInfo.isSignedBy(configOf(datum).beneficiary))
forAllDataLimited[ScriptContext](ctx => fewInputs(ctx))(ctx =>
    succeeds(VestingValidator.validate(ctx.toData)) ==>?: signedByTheBeneficiary(ctx)
)
```

and not with a call of the validator registered as a function, `succeeds(validator, ctx.toData)`.
Written in, the validator's code is compiled with the condition, and what both read of the context
is read once; the program is the same, byte for byte, as the one `verifier.guarantees` makes of
the clause in `spend`. Called, the validator is a program of its own, and the context is read twice:
that statement did not come to the solver in ten minutes (2026-11-10), where the other is proved in
under three. Written in, the kept result of the statement has the statement hash and the program
hashes of the one `verifier.guarantees` made: it is that statement. The vesting example states its
three guarantees so.
