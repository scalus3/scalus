# The call rule

Hoare logic's rule for a call, on statements about compiled programs: a call of a function whose
contract is proved is replaced by a fresh value, of which the contract's postcondition is assumed
where its precondition holds. It is the contract VCG for modular proofs of phase
2 ([overview §3.7, §9](../verification-overview.md#37-function-contracts-pre--and-postconditions)),
which the overview says `blaster-uplc` cannot do, as it cannot abstract a callee inside compiled
code. [Calls as unknowns](multi-transaction.md#calls-as-unknowns-in-the-tactic) now does: it is the
rule with the trivial contract, and moves into it.

Status: proposed, 2026-11-25. Step 1 is built, 2026-11-26: the contracts a function states in its
code are premises of the unknowns of its calls ([What exists](#what-exists)).

## The rule

At a call `f(a)` in a statement's program, where `f` has a contract (`expects`, `ensures`, and
`returnsWhen`, `failsWhen`; [statement semantics §6](prop-semantics.md#6-contracts)):

1. **`expects_f(a)` at the call is a statement of its own**: the caller's obligation, as
   `Verifier.obligations` declares today. The proof does not rest on it: the premises of 3 hold
   under `expects_f(a)`, and where it is false they say nothing of `u`. Its refutation is a caller
   that calls `f` outside its precondition, where the contract gave the proof nothing. Two other
   forms are unsound: `expects_f(a)` assumed as a premise makes the statement hold vacuously where
   it is false, and the premises of 3 assumed without it may be false of what the call returns
   there.
2. **The call is an unknown `u`**, of `Option` of `f`'s result type: `None` is the call failing.
3. **Premises of `u`**, from the contract, under `expects_f(a)`: `u = Some(r) ⇒ ensures_f(a)(r)`;
   `returnsWhen_f(a) ⇒ u ≠ None`; `failsWhen_f(a) ⇒ u = None`. By the contract's theorem they
   hold of what the call returns, for every `a`: the rule needs no check of `expects_f(a)` to be
   sound.
4. **Equal calls share `u`**: two calls of `f` with one key, as the calls as unknowns key them.
5. **The proof cites the contract's theorem** as a lemma (`Proof.usedLemmas`): it rests on it,
   and not on the call's obligation (1).

A call of a function with no contract is the rule with the trivial one, `expects = true` and
`ensures = true`: the unknown of `CallsAsUnknowns` today.

Where the rule is sound:

- **The contract is of the program the caller runs**: the compiled code of `f` as it is in the
  caller, not its source. Scalus inlines library functions; the lemma of a loop is of its term,
  for any stack and environment, as `loop_returns` of `isSignedBy` is.
- **Termination, where the statement needs it.** The rejecting form says that the program fails.
  A call that ran forever would not fail, so the rule needs that `f` ends, fails or returns: a
  total contract, or the lemma that its loop ends. `¬ succeeds` (the script never accepts) needs
  only the partial contract.
- **A statement of the cost** changes under the rule: the cost of `f`'s run is no part of `u`.

## What exists

- The rule in `CallsAsUnknowns`
  (`scalus-verification/src/main/scala/scalus/verify/uplcblaster/CallsAsUnknowns.scala`). A
  function's contract is read from its code as linked into the program (`Specifications.stated`):
  the `ensuring` and `Spec.ensures` its body ends or starts with, and `Spec.expects` as the guard.
  A function whose code is a call of another on all its arguments has the other's contract, as
  `valueEq` has `eq`'s; a postcondition that is no function literal is no contract. A call of a
  function with a contract is an unknown whether it loops or not. Its premise is
  `match u { None => true; Some(r) => let p = a in ensures(p, r) }`, with the values of the
  program that the arguments read bound inside the `Some` case: they are part of the call's key,
  and computed where it returned. Where the contract has a precondition, the premise is an
  implication of two such tests, `expects(p) ==> ensures(p, r)`, whose first is false where the
  call fails: so where the precondition fails, as `xs.head` on the empty list, the guarantee is
  not assumed, and the premise does not fail with it. The result has a name of its own, which no
  parameter has. The premise is rewritten as a program is, so a call in its arguments or its
  condition is an unknown of the key it has in the program or in a clause, with a premise of its
  own, up to four deep. A key takes a `let` whose value the body reads as an argument of a call
  as that value in its place, where the call's key keeps its arguments' (a walk's): the inline
  `v - w` is `let other = w in minus(v, other)`, and its key is that of `minus(v, w)`.
  `withoutContracts` assumes none, for a statement that needs none.
- A contract stated in the code is partial: it says nothing where the call fails, and an unknown
  of the call may be `None`. A statement that reads what the call returns is stated where it
  returns, as the rejecting form `==>?:` does, or with `succeeds(...) ==>:`.
- What a proof rests on: `Proof.premises`, the clauses of the contracts it assumed. `Verifier.prove`
  cites a premise as a lemma where a theorem of the verifier, of every value, states its clause
  (`verifier.statement(name, FunctionContract.inSource(f))`, or `guarantees(f)`), and leaves the
  others as premises. `Proof.assumed` adds what the lemmas rest on, and the result names it
  ("assuming the contracts of ensuring at Value.scala:481, ..."), as does a clause's status in
  `Verifier.specifications`. A clause's place is where its code states it: a condition that is an
  inline function's code, as `a === b`, is placed where it was inlined to.
- The library's contracts, as `ensuring` in `scalus-core`
  (`scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Value.scala`):
  `eq(a, b)` gives `!equal || a.getLovelace === b.getLovelace`, `minus(a, b)` gives
  `r.getLovelace === a.getLovelace - b.getLovelace`, of values whose keys ascend as the type's do,
  and `lovelace(v)` gives `r.getLovelace === v`. Off-chain they are checked, as every `ensuring`
  is; they are no part of a script.
- Contracts: `Props.contract`, `Props.totalContract`
  (`scalus-verification/src/main/scala/scalus/verify/Prop.scala:514`, `:569`), and those a
  function states in its body with `Spec`, read by `FunctionContract.inSource` (`Prop.scala:197`).
- The caller's side: `Verifier.obligations`
  (`scalus-verification/src/main/scala/scalus/verify/Verifier.scala:412-431`, `Obligations.scala`).
- The unknown, its key and the trivial contract: `CallsAsUnknowns`
  (`scalus-verification/src/main/scala/scalus/verify/uplcblaster/CallsAsUnknowns.scala`; what is an
  unknown, `go` `:597`, `call` `:626`, `walk` `:647`; the key, `keyOf` `:808`).
- A postcondition given by hand as a premise: the probe's lemma on `Value`,
  `lovelaceOfTheRestFollows`
  (`scalus-examples/jvm/src/test/scala/scalus/examples/vesting/VestingWalksProbeTest.scala:282-283`),
  with which #4 of a copy of the vesting validator is proved (`:104-122`), and without which it is
  refuted (`:127-148`).
- A mapping proved by hand, the strongest `ensures`: the compiled `isSignedBy` returns
  CardanoLedgerApi's `txSignedBy pk tx`
  (`scalus-verification/src/main/lean/ScalusProofs/Mappings/IsSignedBy.lean:189`), with runs
  without a budget (`scalus-verification/src/main/lean/ScalusProofs/Runs.lean:41-50`).

## What to prove

Of vesting first: each library function its validator calls on the way to its four clauses
(`scalus-examples/jvm/src/main/scala/scalus/examples/vesting/VestingValidator.scala:75-133`). The
library functions are in `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/`.

| # | Statement | Of | Needed by | Status |
|---|---|---|---|---|
| 1 | the tie: in a run, a part that ends in a value can be replaced by the value | Lean's CEK machine, once | every proof with unknowns, so that it rests on no trusted rewrite (`ProofKind.BlasterOfUnknowns`, `Verifier.scala:127`) | not started |
| 2 | `getLovelace(v - lovelace(a)) = getLovelace(v) - a`, and with `===`: `x === v - lovelace(a) ⇒ x.getLovelace = v.getLovelace - a` | `Value.getLovelace` (`v1/Value.scala:701`, by `quantityOf` `:777`), `Value.minus` (`:480`), `Value.lovelace` (`:98`), `Value.eq` (`:359`) | vesting #4: the validator checks the value (`VestingValidator.scala:133`), the clause reads its lovelace (`:183`) | stated as a chain of three contracts, the `ensuring` of `eq`, `minus` and `lovelace`: `eq(x, y) ⇒ gl(x) = gl(y)`, `gl(minus(v, w)) = gl(v) - gl(w)`, `gl(lovelace(a)) = a`; assumed, not proved |
| 3 | it returns `txSignedBy pk tx`, of any number of signatories | `TxInfo.isSignedBy` (`v3/Contexts.scala:1075`) | a statement that reads the signature as the ledger does | proved: `IsSignedBy.lean:189` |
| 4 | it returns the input whose `outRef` is the given one, or fails where there is none; it ends | `TxInfo.findInputOrFail` (`v3/Contexts.scala:986`, its loop `:994-998`) | #3, #4: `lockedBy` (`VestingValidator.scala:167-168`) | not started |
| 5 | it returns the one element of which the predicate holds, or fails; it ends | `List.findUniqueOrFail`, `List.findUniqueOrElse` (`prelude/List.scala:1119`, `:1139`) | the validator's check of one vesting input (`VestingValidator.scala:82`), #4's continuing output (`v3/Contexts.scala:1046`) | not started |
| 6 | each ends | `TxInfo.findInputsByCredential`, `findOutputsByCredential` (`v3/Contexts.scala:1019`, `:1029`), the folds over values (`VestingValidator.scala:110-111`) | the rejecting form of #4, which runs the validator past them | not started |
| 7 | vesting #4 of the validator itself, of every context, with the contract of 2 | `vesting.spend.state_is_carried` (`scalus-examples/jvm/src/test/scala/scalus/examples/vesting/VestingVerificationTest.scala:439`), clause `VestingValidator.scala:64-66`, `carriesTheRest` `:174-184` | the step of the vesting thread ([several transactions](multi-transaction.md#vesting)) | proved with `CallsAsUnknowns`, assuming the three contracts of 2, at 2400 steps in two minutes (`:444`, 2026-11-26); it cites the proof of `linearVesting`'s contract, which the validator also calls |
| 8 | #1 and #3 again under the rule, which they need no contract for: proved `withoutContracts`, assuming none | `VestingVerificationTest.scala:365`, `:406` | that the move of `CallsAsUnknowns` keeps them | proved with `CallsAsUnknowns` |

2 is a lemma about a walk over a map of any size: proved by hand, by induction, as 3 was, or by
`blaster-uplc` with the walks of `Value`'s arithmetic as unknowns where that is enough. 4 to 6 are
of loops over a list, each as `loop_returns` of `isSignedBy` (`IsSignedBy.lean:103`).

## The move of `CallsAsUnknowns`

- **Contracts in the tactic.** Where the function of a walk has a contract that is a theorem of
  the verifier, its unknown gets the premises of the rule, and the proof cites the theorem; where
  it has none, no premise, as today.
- **Obligations beside the proof.** A walk's `expects` is the caller's obligation at that call, a
  statement `Verifier.obligations` declares, proved or refuted on its own; the proof does not rest
  on it (the rule, 1).
- **The kind.** A proof rests on the tie and on the contracts it cites. With the tie proved (1),
  it is a proof of the statement as written, of the kind of its parts.
- **The library's contracts.** Stated with `Spec` in the library's source, as `spend` states its
  guarantees, they leave its compiled code as it is (`SpecTest`). Decided so (open question 3).

## Steps

1. Contracts as premises of unknowns, in `CallsAsUnknowns`; the contract of 2 stated, and #4 (7)
   proved with it, resting on 2 as trusted until it is proved. Done 2026-11-26. Its control, the
   validator without its datum check, has a counterexample under the premises, which the replay
   finds no context for: a refutation with unknowns is confirmed only where Lean's context
   realizes them. The context that refutes it is a sample the copy accepts.
2. The proof of 2.
3. The lemmas that loops end (4 to 6), where a statement in the rejecting form needs them.
4. The tie (1).

## Open questions

1. **A contract of an inline function.** `findInputOrFail` and `findUniqueOrFail` are `inline`:
   in the caller's code there is no call of them, only their loop (`v3/Contexts.scala:994-998`).
   Their contract is to be found by the loop's term, or by its source position.
2. **The rewrite on SIR or on UPLC.** The tie is a lemma about two programs. Made on UPLC, the
   program with unknowns is the program with a part replaced; made on SIR, as now, the two are
   compiled each on its own, and are related only through the source.
3. **Where the library's contracts live.** Decided 2026-11-26: in `scalus-core`, with `Spec`, in
   the code of the functions they are of. The tactic reads them where the program links the
   function, and a contract of an inline function is inlined with its loop, which may answer open
   question 1.
