# Properties over several transactions

How to prove a property of a contract over a sequence of transactions, such as "the beneficiary
never withdraws more than has vested", with the statements Scalus proves today, each about one
run of the validator, and one lemma in Lean, proved once for every contract.

Status: proposed, 2026-11-24. Nothing of it is built but what [What exists](#what-exists) lists.

## Why

Every statement of `blaster-uplc` is about one run of a validator, on one script context. Some
faults show only over several transactions. Remove the datum check of the vesting validator,
`require(contractOutput.hasInlineDatum(vestingDatum))`: each withdrawal still meets the three
guarantees of `spend`, as each is stated of the transaction's own datum. But a first withdrawal
can leave the rest under a datum of its own, with a duration of zero, and a second then takes
everything.

A protocol's claims are of this kind: no sequence of swaps lowers the product of the reserves
([protocol steps](protocol-steps.md)); everyone a pool ever pays proved, when they joined, that
they knew its code; the token is minted once.

## A state and a thread

A contract's **state** is the output that one of its transactions leaves for the next: its
address, its value and its datum. It is not the datum alone. The vesting validator reads the
amount released from the value: `initialAmount` less the lovelace locked.

A **thread** is a first output, and transactions each of which spends the output the one before it
left. The first output is made by a transaction that the script does not run on: the deposit.

## What the chain adds

Four facts of the ledger tie the transactions of a thread. None of them is in one script context.

1. **The link.** The next transaction spends the output that the previous one left. It names the
   output's reference, `(id, index)`, and the ledger puts the output, with its address, value and
   datum, into the next transaction's context as the resolved input.
2. **Time.** The next transaction is in a later block. Some block times `t1 ≤ t2` lie in the two
   validity ranges. A transaction's lower bound is at or before its block's time, but the next
   transaction's lower bound may be before the previous one's.
3. **The origin.** Anyone can make an output at a script's address, with any value and datum, and
   the script does not run. A property holds of the threads that start from a first state that
   satisfies a premise, `init`. A thread token, minted by a policy that checks the first datum,
   makes that premise a statement about the policy's code.
4. **Spent once.** An output is spent at most once in a chain. A property of a thread does not
   need this; a property of "ever", as that a token is minted once, does.

Threads are independent where a transaction cannot spend two outputs of the script: the vesting
validator rejects it (`MultipleVestingInputs`). A contract that merges or splits its outputs, as a
batcher does, has the set of its outputs at the address as its state.

CardanoLedgerApiBlaster has none of the four. It has the types of one transaction, the validity
of one script context (`validScriptContext`), and helpers on one context (`findOwnInput`,
`resolveInput`, `utxoConsumed`, `valueSpent`, `valueProduced`); no set of outputs, chain, trace or
block time. Its auction example, a protocol of many bids, proves properties of one transaction for
each redeemer. Blaster has a state machine of its own, `Blaster.StateMachine` (`init`, `next`,
`assumptions`, `invariants`), with `#bmc` for bounded search and `#kind` for k-induction, and no
ledger in it.

## Which properties are invariants

| Kind | Example | Form |
|---|---|---|
| of one transaction | an accepted withdrawal is signed by the beneficiary | a statement of one context: what is proved today |
| of a thread | what has left is at most what has vested, at every step | an invariant of the thread's state |
| of history | everyone a pool pays proved the code when they joined | an invariant, where the state keeps the history; a datum can (the list of who proved it) |
| global | the token is minted once | an invariant of the outputs of the whole chain, with fact 4 |
| liveness | the beneficiary can withdraw everything after the end | an invariant, and from every state it allows, an accepted transaction exists |
| of two traces | the order of two deposits does not change the shares | an invariant of a pair of traces, run side by side |

Every property that says that something bad never happens is an invariant, of a state that may
have to keep some history. This note is about those. The step of a liveness property, that an
accepted transaction exists, is a statement with `exists` over script contexts, the costliest kind
there is.

## The method

A property of every thread is proved from four parts.

1. **Init.** The first output satisfies the invariant `I`. No script runs, so this is a premise of
   the theorem, or a statement about a thread token's policy.
2. **The step.** A statement of `blaster-uplc` over one script context: where the validator accepts
   a context whose own input satisfies `I`, the output it leaves satisfies `I`. The input is the
   previous state, and it is in the context: what the previous transaction left, the ledger put
   there. The statement does not know which transaction made it, the deposit or the seventeenth
   withdrawal.
3. **Time.** Where the property speaks of a time, it holds at a later time too.
4. **The trace lemma.** In Lean, proved by hand once, for every contract: where the first output
   satisfies `I` and every step keeps it, every output of a thread satisfies `I`, and so does the
   input of every transaction of it. Its hypothesis is fact 1. It is an induction over the
   transactions of the thread.

```lean
inductive Thread : Output → List Context → Prop
  | done (o : Output) : Thread o []
  | last (o : Output) (c : Context) :
      ownInput c = o → accepted c → continuing c = none → Thread o [c]
  | next (o : Output) (c : Context) (cs : List Context) (o' : Output) :
      ownInput c = o → accepted c → continuing c = some o' → Thread o' cs →
      Thread o (c :: cs)

theorem thread_invariant (I : Output → Prop)
    (step : ∀ c o', I (ownInput c) → accepted c → continuing c = some o' → I o') :
    I o → Thread o cs → ∀ c ∈ cs, I (ownInput c)
```

A property of each step, such as a guarantee of the validator that holds where `I` holds of the
input, then holds of every transaction of the thread.

`Output`, `Context`, `ownInput`, `continuing` and `accepted` are parameters: the lemma holds of
any. An instance takes them from the statements of its step: `accepted c` is what the statement
says of the validator's run on `c`, and `ownInput` and `continuing` are the runs of the accessors
the statement uses. The link of fact 1 is the hypothesis `ownInput c = o`.

### An invariant has the thread's first state as a parameter

The vesting invariant, of a thread whose first datum is `C0`, and the property of each of its
steps `c`:

```
I(C0, o) := datum(o) = C0
P(C0, c) := C0.initialAmount − (lovelace(ownInput c) − amount(c)) ≤ linearVesting(C0, validFrom(c))
```

`lovelace(ownInput c) − amount(c)` is what stays locked after `c`. From a deposit of
`initialAmount`, with the value carried at each step, `P` says that what has been withdrawn is at
most what has vested.

Without `C0`, a datum replaced by a withdrawal would go unseen: with a duration of zero, a bound
measured by the new datum holds. With `C0`, the step is refuted.

### Vesting

The validator re-reads everything from its input each time, so its step needs nothing of the
previous state but its datum. `P` is guarantee #3, which `spend` states already, where `I` holds of
the input. The step that keeps `I` is a fourth guarantee, that the state is carried:

```scala
// 4. The state is carried: a withdrawal of less than everything leaves the rest in one output,
//    under the same datum.
Spec.ensures(
  amountOf(redeemer) === lockedBy(txInfo, txOutRef) ||
    (continuingDatum(txInfo, txOutRef) === datum
      && continuingLovelace(txInfo, txOutRef) === lockedBy(txInfo, txOutRef) - amountOf(redeemer))
)
```

`continuingDatum` and `continuingLovelace` are accessors as `lockedBy` is: only the clauses use
them, and they leave the script with the clauses. The init premise is a deposit of `initialAmount`
under `C0`, with `initialAmount ≥ 0`.

With the lemma, `P` holds of every withdrawal of the thread. Its time is the withdrawal's lower
bound, at or before its block's time (fact 2), and the schedule does not decrease
(`vesting.schedule.does_not_decrease`, proved): what has been withdrawn is at most what has
vested, at the time of each withdrawal and at any time after.

The negative control is the validator without its datum check: #4 is refuted, and #1–#3 still
hold.

## Two transactions in one statement

A statement can range over two contexts, with the link as a premise written in Scala
(`spendsContinuingOutputOf(c2, c1)`). It is the plainest form to read, and it is bounded: it says
nothing of a third transaction. It costs two symbolic runs of the validator in one check. The
method above needs none: it proves what such a statement says, for every length.

A refuted step may have a counterexample whose input no thread reaches: the invariant is then too
weak, and the contract may be right. To tell, its context is run as a `Scenario` of the testkit,
from a first output through the transactions that make that input, on the emulator.

## What a theorem rests on

- the trace lemma: Lean's kernel;
- the link, time and the origin: the hypotheses of `Thread` and the init premise, printed with the
  result;
- the steps: what `blaster-uplc` rests on, Lean's model of UPLC, Blaster and Z3, about Blaster's
  optimized run ([lean-manual](../verification-overview.md#64-lean-manual)), and about the program
  compiled with the tactic's options.

A theorem of a thread is checked by `lean-manual`: it cites the axiom `proved` of each step
statement, and its axioms are those and the kernel's.

## The cost

The steps are statements over script contexts, of the costliest kind. Measured on vesting: Lean
does not finish #3 over every context, nor over contexts of two inputs; `VestingVerificationTest`
declares it open. #4 checks the continuing output, which the validator reaches last, after it sums
the beneficiary's inputs and outputs. Three ways to a step Lean finishes:

1. **Limits of exact length**, as the contexts of the minting policy's statements: one input, and
   the outputs of a partial withdrawal, the beneficiary's and the continuing one.
2. **The step's guard** ([protocol steps](protocol-steps.md)): the carry and the arithmetic in a
   function of the claim, with no list, which Lean runs on every input within one budget, tied to
   the validator by the decomposition statements.
3. **A proof by hand** of the step, checked by `lean-manual`. By induction over the lists the
   validator walks, it covers contexts of any length, where the first two have limits. It is then
   a proof about Lean's machine on the compiled program, with as many steps as a run takes, and not
   about Blaster's run of one budget ([lean-manual](../verification-overview.md#64-lean-manual)): a
   run on a longer list takes more steps than any one budget. The loops of such a proof are those of
   [function mappings](function-mappings.md), A3.

## Relation to other notes

- [Protocol steps](protocol-steps.md): a model's theorem over many steps, as
  `Swap.product_never_falls`, is this induction over the model's `step`. The method here is the
  same induction over the compiled validator.
- Blaster's `#kind` proves an invariant of a state machine by k-induction. Over a model, it could
  find the strengthened invariant a step needs. Over the validator, each of its steps is a
  symbolic run.
- `ScenarioExplorer` of the testkit searches threads on the emulator to a depth: a bounded,
  concrete search. A thread it finds that breaks an invariant is a counterexample of a step.

## What exists

- Statements of one context, in the rejecting form, and the guarantees of `spend`
  (`VestingVerificationTest`).
- `vesting.schedule.does_not_decrease`, the time part of the vesting invariant.
- `lean-manual`, which checks a theorem written by hand and reads the statements it cites.
- `isValid(ctx)`, CardanoLedgerApi's validity of a V3 context, as a premise.

## Steps

1. **Guarantee #4 on vesting**, with its statement of contexts of the limits of #3, and the
   control: a copy of the validator without its datum check, in the tests, whose #4 is refuted.
2. **A step Lean finishes**: one of the three ways of [The cost](#the-cost), on #3 and #4.
3. **The trace lemma** in Scalus's Lean library, `ScalusProofs.Thread`. Its change makes every kept
   result stale once, so it goes in with another change of the library.
4. **The lemma from a statement.** A statement is written into the workspace in the form of its
   lowering, the rejecting form among them. A lemma for each form gives `step` from the statement.
5. **The vesting theorem**: in every thread from a deposit of `initialAmount` under `C0`, what has
   been withdrawn is at most what has vested, at every step. Written by hand, checked by `lean-manual`.
6. **A property of history**, on a contract whose datum keeps it, and **the origin** made a
   statement, by a thread token and its policy.

## Open questions

1. **The step's claim in Scala or in Lean.** A `Spec.ensures` clause is in the contract's code,
   and its accessors are compiled: an instance of the lemma takes their runs. A Lean predicate over
   CardanoLedgerApi's types, as `isValid` is, would have the lemma and the statement speak of the
   same functions, and the link of fact 1 would be about the ledger's own types.
2. **A thread over the ledger's types.** With the Scala accessors, `ownInput c = o` says what the
   accessors read. A statement that the accessors agree with `findOwnInput` on valid contexts
   would tie it to the ledger.
3. **A `Thread` type in Scala**, from which Scalus writes the instance of the lemma and its
   theorem's statement, or a convention.
4. **Blaster's `#kind`**, over a model or over the guard of a step.
