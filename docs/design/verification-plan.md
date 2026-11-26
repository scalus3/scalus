# Verification in Scalus — plan

Status: **plan**, 2026-11-26. The goals (§1) and the order (§3) are proposed, for review.

This document orders the verification work that is not built. [§9 of the
overview](verification-overview.md#9-plan) records what each phase built, and the designs are in
[`verification-details/`](verification-details/). Where an item has a design, it links to the
design, and this document says only what the order needs.

## 1. Goals

The goals are a sequence: each rests on less than the one before. Each has a check, and the
milestones of §3 that reach it.

Every goal keeps what the suites do now. The author of a validator states its guarantees in its
code, with `Spec`, which leaves its bytes as they are. Each guarantee is proved, refuted by a
counterexample that Scalus replays, or recorded as open with a reason. ci-jvm and the Lean-Proofs
workflow keep the results in step with the code. Each result says what it rests on
([overview §7](verification-overview.md#7-trust-model)).

| Goal | What holds | What it rests on | Check | Milestones |
|---|---|---|---|---|
| **G1** | Vesting's four guarantees, of every script context | Blaster and Z3; Blaster's optimizer; the CEK model; the call rule, trusted as a VC generator is; programs compiled with the tactic's options | the four are proved, and the validator without its datum check refutes the fourth (M1) | M0, M1 |
| **G2** | The same for every example: the payment splitters, HTLC, the one-shot minting policy | as G1 | no statement is tagged `Unfinished` without a named blocker (M2) | M2 |
| **G3** | G2's results, about the scripts as published | as G1, but the programs are the published bytes | the exit criterion of phase 3 (M5) | M5 |
| **G4** | The call rule proved sound for UPLC | as G3, without the call rule | no result of the examples is of kind `BlasterOfUnknowns` (M3) | M3 |
| **G5** | Chosen properties proved by Lean's kernel alone | the kernel and the CEK model | `#print axioms` names only Lean's own axioms, and the statement is about the run as Lean defines it, not Blaster's optimized run (M3, M7) | M3, M7 |

G1 to G3 are within reach now. G4 and G5 are stronger, and their cost is not known yet: M3 starts
with two spikes that measure it.

What the goals rest on:

- **The call rule.** A call whose callee has a contract is replaced by a value of which the
  postcondition holds, and a call of a function with no contract by any value: `CallsAsUnknowns`
  is the rule with the trivial contract. The contracts are proved as statements of their own; one
  not proved yet is cited as a premise and named in the result. The rule's soundness, and its
  implementation, are trusted, as a Hoare-logic tool's VC generator is. The implementation
  rewrites SIR, the program it makes is compiled apart from the original, and calls with one key
  share one unknown: a review found one key unsound (fixed in `d892e6d53`). G4 proves the rule in
  Lean, with the rewrite moved to UPLC, so that the lemma is about the two programs themselves.
- **Blaster's optimizer.** `#prep_uplc_run` optimizes a run with Blaster's `Optimize.main`, and
  the statement is about that term, which is not `runProgramFor` by `rfl`. For a result Blaster
  closes, the optimizer is part of Blaster. For a proof by hand it is the one step left, which G5
  removes.
- **The tactic's options.** The programs proved are compiled without the `Value` builtins, which
  the pinned PlutusCore lacks. G3 moves the pin.

No goal removes two things: the lowering of a statement into Lean (overview §7), which the
statement's module shows as Lean text, and Scalus's compilation of the statement's leaves, which a
side track tests (§4).

The goals say what a result rests on. What kinds of property are covered is a second axis:
properties over several transactions, as an invariant of a thread of a contract's outputs;
arithmetic with loops and proofs by hand (the AMM); a Lean model of a protocol's steps; off-chain
code; and a protocol of several validators with a thread token as the origin of its state
(M6 to M9). Whether that axis is part of this plan is §7's first decision.

## 2. Where we are

- Statements of one script context about real validators, proved by `blaster-uplc` over real
  contexts. With calls as unknowns, vesting's first and third guarantees are proved of every
  context.
- Proofs by hand, checked by `lean-manual` (the AMM's deposit, with its condition as a premise),
  and a mapping proved by hand: the compiled `isSignedBy` computes CardanoLedgerApi's
  `txSignedBy`.
- The thread and its lemma in Scalus's Lean library (`ScalusProofs.Thread`), and the vesting
  theorem of every thread, from the third and fourth guarantees as hypotheses.
- The last three were merged with PR #488 on 2026-11-26.
- Results today rest on what G1 lists. Of the exit criteria of §9, phase 0's is met. Phase 1's is
  met but for one part: a suite without a negative control does not fail yet. The criteria of the
  other phases are met in part.

## 3. Milestones

The milestones are in order within each track. Each has an exit criterion that a test or a Lean
build checks. M1–M4 lead to the goals of §1, and M6–M9 are the second axis. M5 runs beside
them, and the side tracks of §4 fit in between.

```
M0 ─► M1 ─► M2 ─► M3 ─► M4           G1, G2, then G4 and G5, then the rule for recursion
M5                                   G3: any time
M6                                   several transactions: starts now, ends after M1
M7 ─► M8 ─► M9                       the AMM, Lean models, off-chain code; M7 also reaches G5
§4                                   side tracks: any time
```

The first track comes first: it leads to G1 and G2.

### M0. Merge PR #488

Done, 2026-11-26: merged as `f1a55ab6b`, with Lean-Proofs green.

### M1. Vesting's guarantees of every context (G1)

1. Contracts as premises of unknowns in `CallsAsUnknowns`. The contract of `getLovelace` after
   `minus` is stated, and the fourth guarantee of the validator itself is proved with it. The
   contract is cited as a premise, and named in the result, until item 2 proves it
   ([the call rule](verification-details/call-rule.md#steps), step 1).
2. The proof of that contract: by hand, by induction over the map, as `isSignedBy`'s, or by
   `blaster-uplc` with the walks of `Value`'s arithmetic as unknowns (step 2).

Decide first: where the library's contracts live (§7, decision 2).

Exit: vesting's four guarantees are proved of every script context. The validator without its
datum check refutes the fourth guarantee, while the first three still hold.

### M2. The call rule beyond vesting (G2)

1. The statements tagged `Unfinished` in the example suites, through calls as unknowns and the
   contracts they need. On 2026-11-26 the tests tagged so are: 19 of the payment splitters, 8 of
   vesting (the fourth guarantee among them, which M1 proves), 6 of HTLC and 2 of the one-shot
   minting policy.
2. The contracts those statements need of the library: `findInputOrFail`, `findUniqueOrFail` and
   the other walks of the call rule's table (items 4 to 6). A call whose value a claim needs must
   return, so its contract says that it ends (step 3): total correctness, where a premise needs
   only partial correctness. Until M4, these contracts are proved by hand, by induction, as
   `isSignedBy`'s.
3. `findUniqueOrFail`'s contract speaks of its predicate: it returns the one element of which the
   predicate holds. It is stated and proved at each call, with the lambda written there in place
   of the predicate.

Decide first: how an inline function's contract is found (§7, decision 3). `findInputOrFail` and
`findUniqueOrFail` are inline.

Exit: each statement tagged `Unfinished` in the example suites finishes, or its reason names the
lemma or the upstream issue it waits for.

### M3. The call rule proved, and the run as Lean defines it (G4, G5)

1. **A spike of the tie**, 2 to 3 days: the lemma for one replaced subterm, in a fragment of the
   CEK machine without builtins. It measures what items 3 and 4 cost. The only data point so far
   is `isSignedBy`'s proof, one loop in about a day, and the estimate for items 3 and 4 before the
   spike, one to two months of Lean work, is a guess.
2. **A spike of `prepared = run`**, 1 to 2 days, on the AMM's redemption, where `rfl` gives up
   after 46 s: Blaster's rewriting lemmas (`Blaster/Optimize/Lemmas`) as `simp` rules
   ([overview §6.4](verification-overview.md#64-lean-manual)).
3. **The rewrite on UPLC.** Today the program with unknowns is compiled from SIR apart from the
   original, so no lemma relates the two (call rule, open question 2). On UPLC, the program with
   unknowns is the original with the calls' subterms replaced by variables. Finding a call's term
   is the hard part: an inline function leaves its loop, not a call.
4. **The tie** ([the call rule](verification-details/call-rule.md#what-to-prove), item 1, step 4):
   a simulation between two runs of PlutusCore's CEK machine, whose states are equal up to the
   replaced subterms, in terms, closures, environments and frames. Each case of `step` is a case
   of the proof. A proof with unknowns then rests on no rewrite, and `ProofKind.BlasterOfUnknowns`
   becomes the kind of its parts (G4).
5. **The run as Lean defines it** (G5): `prepared = run`, where the spike shows it can be
   proved. Otherwise, statements over `runProgramFor` itself, which `#blaster` optimizes inside
   its tactic. Those checks are slower (the deployed one-shot minting policy, its lists limited
   to one element: 858 s of `#blaster`'s optimization instead of 81 s), and a proof by hand of
   them steps through the CEK, as `isSignedBy`'s does.

The spikes can run at any time. Items 3 and 4 come after M2, because the tie has to cover every
form of unknown the rewrite makes, and M2 fixes those forms. Decide first: the rewrite on SIR or
on UPLC (§7, decision 4).

Exit: no result of the examples is of kind `BlasterOfUnknowns` (G4). A statement of the AMM is
proved by hand about the run as Lean defines it, and its `#print axioms` names only Lean's own
axioms (G5).

### M4. The rule for recursion

A recursive function's contract, proved from one pass through its body: the function's call of
itself is replaced by a value of which its contract holds, as the call rule replaces a call of
another function. With a measure that falls at each recursive call
([`Spec.decreases`](verification-details/function-mappings.md#a-measure-from-the-source-specdecreases)),
the contract is total.

1. The rule in `CallsAsUnknowns`: a `let rec` function's call of itself as an unknown under its
   own contract.
2. `Spec.decreases` on functions that are compiled, and the check that the measure falls at each
   recursive call and is not negative.
3. The rule's soundness in Lean, beside the tie of M3: induction over the run, or over the
   measure where the contract is total.

Exit: a contract of a loop of the library, as `findInputOrFail`'s, proved by `blaster-uplc` with
the rule, of lists of any length.

### M5. The published script (G3; closes phase 3)

1. PlutusCore at a revision with the `Value` and array builtins. They were merged into
   PlutusCoreBlaster `main` on 2026-11-12 (#12, arrays) and 2026-11-13 (#15, `Value`). Scalus
   pins a fork, `rssh/PlutusCoreBlaster` `case-scrutinee-on-main`: PR #40 merged with the `main`
   of 2026-11-10. So the pin moves to #40 merged with the current `main`, or to `main` itself once
   #40 merges.
2. Programs compiled with the `Value` builtins, which the tactic's options turn off today
   (`valueBuiltins = false`).
3. Script targets ([overview §3.6](verification-overview.md#36-targets-what-a-statement-is-about)):
   a statement about a `PlutusV3[A]` as published, so that its proof is about the script's bytes
   and hash.
4. The ledger's validity for one purpose, and for V1 and V2 (`validSpendingContext`, …), as
   premises ([Lean types](verification-details/lean-types.md#what-comes-next), item 3).

Item 1 does not wait on any other milestone, and makes item 2 possible.

Exit: the exit criterion of phase 3. Vesting's guarantees are proved about the script whose hash
its blueprint publishes.

### M6. Vesting over every thread (closes phase 3a)

1. The lemma from a statement
   ([several transactions](verification-details/multi-transaction.md#steps), step 4). A statement
   is written into the workspace in the form of its lowering, and a lemma for each form gives the
   thread's step from the statement. The rejecting form also needs a statement of its own: the
   clause returns wherever the validator accepts.
2. `withdrawn_at_most_vested` with the `proved` axioms of the third and fourth guarantees as its
   hypotheses, in place of the guarantees written by hand.

Item 1 can start now, on the third guarantee, which is proved. Item 2 needs M1's fourth.

Exit: the exit criterion of phase 3a. `lean-manual` checks `withdrawn_at_most_vested`. Its axioms
are the kernel's and the `proved` axioms of its step statements, and its premises (the link and
the deposit) are printed with it. The control of the criterion is M1's.

### M7. The AMM by hand and by mappings (closes phase 2a)

1. A workspace that builds from a fresh checkout. The fix is designed in
   [overview §6.4](verification-overview.md#64-lean-manual).
2. "Use Mathlib" as a setting of a workspace, also designed in §6.4.
3. The lemmas of Blaster's terms (`dite'`, `decide'` as `if`, `decide`) moved from
   `Amm/Proofs/Deposit.lean` into Scalus's Lean library.
4. `leanMapping(f, "g", expects)`
   ([function mappings](verification-details/function-mappings.md#steps), step 1):
   `AmmMath.redeemDatum` and `swapResult` mapped to the functions of a Lean model of the AMM,
   beside a refuted control.
5. The redemption's contract proved by hand, with Lean's kernel. `Amm/Proofs/Redeem.lean` only
   cites the statement's `proved` axiom, so it rests on Blaster's result, as the exit criterion
   does not allow.
6. The mapping of `Math.sqrt`, a loop (step 3). With it, the deposit's clause is a statement
   without a budget, and the clause as written is no longer false at every budget. The other way
   is Lean-blaster#273.

Exit: the exit criterion of phase 2a, with `lake build AmmProofs` passing in a fresh clone.

### M8. Protocol steps and off-chain code

1. The AMM's swap as the first step
   ([protocol steps](verification-details/protocol-steps.md#steps), step 2): `SwapClaim` and
   `SwapRules` in Scala; `Swap.guard` and `Swap.claim` in Lean; the mapping of the guard; the
   first decomposition statement; and `Swap.product_never_falls`.
2. Off-chain functions mapped to Lean functions
   ([function mappings](verification-details/function-mappings.md#steps), steps 2 to 4).
3. A protocol of several validators (protocol steps, step 4), with a thread token as the origin
   of its state ([several transactions](verification-details/multi-transaction.md#steps), step 6).

Depends on M7, for `leanMapping`. Item 3 depends on M6 as well.

Exit: `Swap.product_never_falls`, a theorem of the AMM's Lean model, holds of the validator
through the mapping of the guard.

### M9. `lean-direct`, decided again

Phase 2's `lean-direct` ([overview §6.3](verification-overview.md#63-lean-direct-mapping-to-functions-and-statements-defined-in-lean))
translates SIR to Lean. Most of what it was for is now reached another way: induction by a proof
by hand over a mapping (M7), and a callee's contract by the call rule (M1). What remains is A3,
the translator of off-chain functions applied to on-chain SIR (function mappings, step 4).

Decide after M8 (§7, decision 6): `lean-direct` as a tactic of its own, or A3 as all of it. The
same holds for `using`, `split` and `cases`.

## 4. Side tracks

Each is small and can be done at any time. Each closes a gap that an exit criterion or the trust
model names.

- **A suite without a negative control fails.** The trust model requires it
  ([overview §7](verification-overview.md#7-trust-model)), and it is the last part of phase 1's
  exit criterion.
- **The statement's leaves checked against the JVM.** A leaf, as a clause's condition, is
  compiled with the release options, the optimizer among them, and Lean proves the formula with
  its leaves as compiled. A differential test runs each leaf on the JVM and as UPLC, on random
  inputs. The test that spend's conditions return true on the samples the validator accepts does
  part of this. A stronger form compiles the leaves without the optimizer: they are small.
- **The rewrite checked on samples.** Until the tie (M3), the program with unknowns, with each
  unknown given its value, and the program as written agree on the samples of the suite.
- **Blaster's optimized run checked on samples.** Until `prepared = run` (M3), `prepared` and
  `runProgramFor` agree on sample inputs.
- **A report** (phase 1): which clauses are checked, by what proof, and what each proof rests on.
  It starts from `Verifier.specifications`
  ([which clauses are checked](verification-details/specification-checks.md)).
- **A Lean type agrees with its predicate**: `T.isEncoding d` equals the predicate of
  `DataEncoding` ([Lean types](verification-details/lean-types.md#what-comes-next), item 1).
- **The PlutusCore pin's comment.** nau/PlutusCoreBlaster#1 was merged into #40's branch on
  2026-11-11, so the fork can point at #40's branch. M5's item 1 moves the pin again.
- **`SIRLinker.isDefinition`** takes a binding for a definition when its name has a dot
  (`SIRLinker.scala:433`). So it also takes the plugin's local names (`_File.scala_L_C_match_…`)
  for definitions. Found while building calls as unknowns. To be checked, and filed if it is a
  bug.

## 5. Upstream

Checked on GitHub, 2026-11-26.

| Issue | State | What waits on it |
|---|---|---|
| input-output-hk/PlutusCoreBlaster#15 (`Value` builtins), #12 (arrays) | merged, 2026-11-13 and 2026-11-12 | nothing upstream now: M5, item 1 |
| input-output-hk/PlutusCoreBlaster#40 (`Frame.CaseScrutinee`) | open | the fork pin; every PV11 program uses `case` |
| input-output-hk/Lean-blaster#273 (`Nat.log2`) | open | the deposit by `blaster-uplc` (M7 goes around it by a mapping) |
| input-output-hk/Lean-blaster#389 (string goals unsound) | open | statements with `String` variables |
| input-output-hk/Lean-blaster#154 (`dite'` ill-typed) | open | proofs by hand over the optimized run; `prepared = run` (M3) |
| input-output-hk/Lean-blaster#463 (as-pattern recursion) | open | nothing: the ascending checks carry the previous key |

## 6. Proposed to drop or defer

| Item | Where | Why | Proposal |
|---|---|---|---|
| Verifier descriptors, `Verifier.default`, the `sbt verify` runner that discovers verifiers | overview §4.5, §5.4; phase 0 | test suites, and the sbt plugin's `verify`, which runs them with Lean required, do this | drop |
| A serialized `Prop` | overview §4.4; phase 0 | nothing reads one: kept results are named by fingerprints, and the exports are written from the lowering | defer until something needs it |
| Function tables from `@Compile` objects, `Logic` pseudo-functions | [statement capture](verification-details/prop-capture.md) | the macros capture what the suites state | defer |
| The `scalacheck` tactic | overview §6.5; phase 1 | evidence only, and property tests already stand beside the proofs | decide (§7, decision 7) |
| Statements harvested from `require` | overview §8.3 | no phase has it; with the call rule, a harvested statement costs little | decide (§7, decision 7) |
| Blaster's `#kind` over a model | several transactions, open question 4 | the thread lemma covers the invariants so far | defer |
| A blueprint's script as the scope of a UAL property; a consumer of the documents | [statements as Lean modules](verification-details/lean-export.md) | the scope fits with M5's script targets | with M5, or defer |
| A contract of a function for every function it takes (a contract of its argument inside its own), and so obligations of calls inside a function value | [statement semantics](verification-details/prop-semantics.md#call-site-obligations) | a validator writes its lambdas at the calls, so a contract stated at each call (M2) is enough so far | defer |
| The Scala kernel | overview §6.7; phase 4 | no backlog of lemmas calls for it | not planned |
| Refinement inference in the compiler; a counterexample as a runnable Scala test | the August plan, `docs/local/verification/implementation-plan.md` (not in git) | outside the phases of §9 | decide |

## 7. Decisions needed

1. Whether the second axis of §1 (M6 to M9: properties over several transactions, the AMM by
   hand, Lean models, off-chain code, a protocol of several validators) is part of this plan, or
   a plan of its own.
2. Where the library's contracts live: in `scalus-core`, stated with `Spec`, or in
   `scalus-verification` (call rule, open question 3). It blocks M1.
3. How an inline function's contract is found: by its loop's term or by its source position (call
   rule, open question 1). It blocks M2.
4. The rewrite of calls on SIR or on UPLC (call rule, open question 2). It blocks M3's items 3
   and 4; G4 needs UPLC.
5. Where mappings are declared: in the verifier, or as an annotation on the function (overview,
   open question 9). It blocks M7.
6. `lean-direct` as a tactic of its own (M9).
7. `scalacheck` and statements harvested from `require`: planned or dropped (§6).
