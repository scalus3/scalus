# Which clauses of a specification are checked

A function's code states its specification with `Spec.expects`, `Spec.ensures` and `ensuring`
([overview §3.7](../verification-overview.md#37-function-contracts-pre--and-postconditions)). A
clause is a claim, and a reader of the code takes it for one. This is how the verifier knows, for
every clause of every function it has, whether a proof checks it, and how a suite of proofs is held
to checking them.

Status: built. `Verifier.specifications`, `Verifier.open`, `LeanProofs.checkSpecifications`, and
the check at the end of a `LeanProofsSuite`.

## Why

A clause is read into a statement in two ways: `verifier.guarantees` makes a statement of each
`ensures` and `ensuring` in a function's code, and `FunctionContract.inSource` makes the function's
contract of the clauses at its head. A suite may also state the same thing in its own words, which
reads better (the vesting suite does, [statements as Lean modules](lean-export.md#writing-a-statement-about-a-validator)).

Without a record of which clauses are proved, two things go unseen:

- a clause no statement states, as one added to a validator after its proofs were written;
- a statement in the suite's words that no longer says what the clause says, after either changed.

Either way the code claims more than the proofs show.

## How a clause stands

`verifier.specifications` lists every clause in the code of the functions registered in the verifier,
by its file and line, with the functions whose code has it. The line is that of the clause's
condition: `Spec.ensures` is inline, so its call is that of `Spec.holdsOnReturn(_ => condition)`, at
the definition of `Spec.ensures`, and only the condition is where the function states it. By the
call, every `Spec.ensures` would be one clause, of `Spec.scala`. A clause is told apart by its file,
line and column; where one function's code has it more than once, as a handler inlined at two
places, each is a clause of its own, numbered by its occurrence, and needs a statement of its own.
Code that is not a function of its declared parameters is not read for statements, and its clauses
are unchecked, as no statement can be made of them. A clause of an inline function, such as a
validator's handler, is in the code of every function that calls it, and is one clause. Each has a
status:

- **Checked**, by a theorem, with the tactic that proved it, the kinds of its proof, and the limits
  of its variables where it has any (`forAllDataLimited`): a clause proved over contexts of at
  most two inputs is checked of those, and the status says so. An `ensures`
  or `ensuring` clause is checked by a theorem that states it: a statement `guarantees` declared of
  it, or a contract read from the code that has it (`verifier.contract`, or `verifier.statement` of
  the contract, as for `linearVesting`'s `ensuring`, with `returnsWhen` added). A theorem also checks
  the clause where the tactic that proved it finds it the same statement as one that states the
  clause: for `blaster-uplc`, the same check text and the same programs, which the fingerprint of a
  kept result has, without Lean's side (`Tactic.identity`). The tactic that proved the theorem
  tells, and not another of its name: a tactic with a fixed plan tells nothing. So a statement in the suite's words checks the clause, and
  it is held to the clause's code either way.
- **Open**: declared not proved with `verifier.open(statement, reason)`, of a statement that states
  the clause or is the same as one that does; or with `verifier.open(function, line, reason)`, of
  the clause itself, for a clause no statement can be made of, as one inside a function value.
  The status of such a clause says why no statement is made of it. The vesting validator's third clause, of what stays
  locked, is open: Lean does not finish it.
- **Owed by no caller**: an `expects` that no function of the verifier calls. A precondition is an
  obligation of the callers, not a claim of the function.
- **Unchecked**: neither, with the statements that would check it and what is missing.

An `expects` clause is checked where every function of the verifier that calls its function has its
obligation stated (`verifier.obligations`, under the contract read from the code) and proved, and no
call is one `obligations` could make no statement of, as one inside a function value.

A kept result is a proof of its statement, as in any run: the record needs no Lean.

## The check of a suite

`LeanProofs.checkSpecifications()` fails where a clause is unchecked, and names each with what is
missing; it returns the open ones. `LeanProofsSuite` calls it after the suite's last test, where every
test ran and passed but those tagged `Unfinished`, ignored or pending, and prints the open clauses.
An ignored test proves nothing, so the clauses it would check are unchecked, and fail the suite. A
run of some tests, or one in which a test failed or was canceled, does not hold the specifications
to what it checked, and says so where the suite's functions state clauses.
A suite whose functions state clauses for its own tests, as the verifier's `SpecificationsTest`,
overrides `checksSpecifications` to say no.

What the examples have:

| Function | Clause | Status |
|---|---|---|
| `VestingValidator.spend` | the beneficiary signed | checked, by `vesting.spend.beneficiary_signed_limited`, the same statement as the clause's, only where the context has at most two inputs |
| `VestingValidator.spend` | something is withdrawn | checked, by `vesting.spend.something_is_withdrawn` |
| `VestingValidator.spend` | what stays locked is at least what has not vested | open |
| `VestingValidator.linearVesting` | `ensuring`: between nothing and the amount | checked, by `vesting.schedule.stated_contract_holds` |
| `MintingPolicy.mint` | both clauses | checked, by the statement `guarantees` declared of them together |

The minting policy's clauses are proved as `guarantees` states them. Written as a sentence of the
suite's own, they compile to other programs, as the sentence builds the minting context again where
`mint` binds it once, so that sentence would not check them.
