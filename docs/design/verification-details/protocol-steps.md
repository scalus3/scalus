# A protocol step in Scala and in Lean

How to write one step of a protocol, such as a swap of an AMM, so that its validator, its
transaction builder, a simulator and a Lean model are the same step, tied by proofs where they can
be and by tests where they cannot. The ties between functions are
[function mappings](function-mappings.md). This document is about what the functions are.

Status: proposed, 2026-11-20. Nothing of it is built but what [What exists](#what-exists) lists.

## Why

A **step** is one kind of transaction of a protocol, as one transition of the protocol's state: a
swap, a deposit or a redemption of an AMM, a buy on a bonding curve, a deposit into a pool. A
protocol that wants theorems about its steps has the step in three texts at least:

- the **validator**, which checks a transaction of the step;
- the **builder**, off-chain, which computes the transaction of a step that a user asks for;
- a **model**, in Lean, on which the theorems are proved.

A simulator, which runs many steps for numbers and charts, is a fourth, or calls the builder.

A validator checks a step in one function over the script context, and it interleaves four things:
it decodes the context, checks the layout of the transaction, reads amounts out of `Value`s, and
checks the arithmetic of the step. The AMM's `spend` is an example. Its math is apart already, in
`AmmMath`, which the builder calls too. But the rules of a swap are inline in `spend`: the amount in
is positive, the amount out covers the slippage bound, the product of the reserves does not fall,
the output's datum is the expected one, and the output holds the datum's reserves (`quantityOf`).
So:

- **No function holds the step's rules.** A mapping reaches `AmmMath.swapResult`, not the rules
  that the validator checks of its result.
- **A statement about the whole validator is the costliest check there is.** It ranges over script
  contexts, and the tactic walks their lists.
- **Lean's model of UPLC cannot run the release script.** PlutusCoreBlaster has no Value
  builtins yet ([overview §9](../verification-overview.md#9-plan), phase 3), and the step's rules
  sit next to `Value` code.
- **A builder of another protocol may be a second text of the arithmetic**, where the validator
  only checks what the builder computes ([two kinds of guard](#two-kinds-of-guard)), and nothing but
  tests ties the two.

## The layers of a step

| Layer | What it is | Example: the AMM's Swap |
|---|---|---|
| intent | what the user asks for | "swap `amountIn` of token 0, and get at least `minAmountOut`", or a quote (`AmmOffchain.swapQuote`) |
| claim | the numbers a transaction states for the step: the redeemer, the new datum, the amounts of the values the step reads, the mint quantities | `t0In`, `amountIn`, `minAmountOut`; the datum of the continuing output; the reserves that output holds |
| guard | a Boolean of the parameters, the old state and the claim: the arithmetic rules | the amount in is positive; the datum is `swapResult`'s; the amount out covers `minAmountOut`; the product does not fall; the output holds the datum's reserves |
| shape | the layout of the transaction, and reading the claim out of it | the own input, exactly one continuing output, its inline datum, the pool NFT stays in it |
| ledger | what the ledger enforces, and no rule states | the swapper pays the amount in; values are conserved |

A **claim** holds integers, byte strings, booleans and records of them: no `Value`, no
`ScriptContext`. The shape layer reads it out of the transaction (`claimOf`), and the builder
writes it into one.

A rule can belong to two layers. The AMM binds the datum's reserves to the tokens the continuing
output holds: `poolOutput.value.quantityOf(t0) === newDatum.r0`. The shape layer finds the
continuing output and reads the quantity into the claim; the guard checks that it equals the
datum's `r0`. "The pool NFT stays in the output" is shape alone.

### Two kinds of guard

- **The guard recomputes.** The validator computes the next state from the old one and the
  redeemer, and compares: the AMM's datum is `swapResult`'s. The builder calls the same function,
  so the claim function is the guard's own code, compiled and run on the JVM
  ([B1](function-mappings.md#b1-the-same-source-compiled-for-the-proof)).
- **The guard checks.** The validator cannot compute the next state within its budget: it takes a
  root, or the result of a search. The builder computes it, the claim carries it, and the guard
  checks it with inequalities, as a root `r` of `x` by `r^k ≤ x < (r+1)^k`. The claim function is
  then off-chain code that the validator never runs, the second instance of a
  [function mapping](function-mappings.md#instance-b-off-chain-code), and a theorem that the
  builder's claims pass the guard is the protocol's liveness.

## The step in Scala

```scala
// @Compile: the validator and the builder share it
case class SwapClaim(
    t0In: Boolean, amountIn: BigInt, minAmountOut: BigInt, // the redeemer
    next: AmmDatum,                                        // the datum of the continuing output
    held0: BigInt, held1: BigInt                           // the reserves that output holds
)

@Compile object SwapRules {
    def amountInPositive(c: SwapClaim): Boolean = c.amountIn > 0
    def productKept(d: AmmDatum, c: SwapClaim): Boolean = c.next.r0 * c.next.r1 >= d.r0 * d.r1
    def reservesHeld(c: SwapClaim): Boolean = c.held0 === c.next.r0 && c.held1 === c.next.r1
    ...
    def guard(feeNum: BigInt, feeDen: BigInt, d: AmmDatum, c: SwapClaim): Boolean =
        amountInPositive(c) && datumExpected(feeNum, feeDen, d, c) && slippageCovered(feeNum, feeDen, d, c)
            && productKept(d, c) && reservesHeld(c)
}

// the validator: the shape, then one call of the guard
case AmmRedeemer.Swap(t0In, amountIn, minAmountOut) =>
    val out = shape(tx, ownInput)                    // one continuing output, the NFT stays
    require(SwapRules.guard(p.feeNumerator, p.feeDenominator, datum, claimOf(out, t0In, amountIn, minAmountOut)))

// the builder (off-chain): the claim of an intent, which the transaction then carries
def swapClaim(p: AmmParams, d: AmmDatum, t0In: Boolean, amountIn: BigInt, minAmountOut: BigInt): SwapClaim
```

- **[ST-1]** A step's claim is a case class without `Value` or `ScriptContext`.
- **[ST-2]** A step's guard is a `@Compile` function of the parameters, the old state and the
  claim, and the conjunction of one function for each rule, named after the rule.
- **[ST-3]** The validator checks the shape and calls the guard. No arithmetic rule is checked
  outside the guard.
- **[ST-4]** The builder computes the claim with one function, and builds the transaction from it.

**Why a function for each rule.** A refusal can then name its rule, in a test, in the simulator and
in the model, and the rule's name is the key between a specification, the code and the proofs. The
validator calls the conjunction and says nothing more on chain, so a rule's name costs no budget.

**The script hash.** Moving the rules into functions may change the compiled script, or may not:
the optimizer inlines a function that has one call. A test of the script's bytes, as `SpecTest`
has for `ensuring` clauses, tells at once. A protocol that is deployed decides whether a new hash
is worth it.

## The step in Lean

```lean
structure AmmDatum where (r0 r1 lpSupply : Int)
structure SwapClaim where
  (t0In : Bool) (amountIn minAmountOut : Int) (next : AmmDatum) (held0 held1 : Int)

namespace Swap
def productKept (d : AmmDatum) (c : SwapClaim) : Bool := c.next.r0 * c.next.r1 ≥ d.r0 * d.r1
def guard (feeNum feeDen : Int) (d : AmmDatum) (c : SwapClaim) : Bool := amountInPositive c && … && reservesHeld c
def claim (feeNum feeDen : Int) (d : AmmDatum) (t0In : Bool) (amountIn minAmountOut : Int) : SwapClaim := …
end Swap

-- the process: a step of the protocol's state, with the ledger apart from the guard
def step : State → Action → Except RuleId State

theorem Swap.claim_passes : 0 ≤ feeNum ≤ feeDen → 0 < amountIn → minAmountOut ≤ out … →
    Swap.guard feeNum feeDen d (Swap.claim feeNum feeDen d t0In amountIn minAmountOut)
theorem Swap.product_never_falls : Reachable s → s'.r0 * s'.r1 ≥ s.r0 * s.r1 -- over every swap from s to s'
```

- **[ST-5]** The model has the claim, the guard and the claim function of each step, with the names
  and the argument order of the Scala functions.
- **[ST-6]** The model's `step` applies a claim that passes the guard, and the ledger's conditions,
  which it states apart from the guard. A claim that fails gives the ID of the first rule that
  fails.
- **[ST-7]** The theorems are of two kinds: a step keeps an invariant (`guard → invariant`), and a
  step can be taken (`claim` passes `guard`), which is liveness. A theorem over many steps, as
  that no sequence of swaps lowers the product, is the induction a model is for.

**The types are those of the Scala code.** [`LeanTypes`](lean-types.md) writes a `structure` for
each case class, with its `Data` encoding, in one module. A model that uses them requires the
workspace's package, and so Scalus's Lean library and its packages: PlutusCore, Blaster and
CardanoLedgerApi. That is no conflict with Mathlib: none of the three requires Mathlib or a package
of it, so a model's proofs with Mathlib and the library build in one workspace. What it costs is
the first build of those packages, and the Lean version that Blaster and PlutusCore pin.

- **[ST-8]** The model uses the types that `LeanTypes` writes, with the names of their fields.
- **[ST-9]** The module of the types imports what its declarations use: PlutusCore's data types
  (`PlutusCore.Data`, which brings its `ByteString` and `Integer`), and a library's module where a
  type is bound to that library's. Today it imports `ScalusProofs.Run`, which brings the runner,
  Blaster and the ledger's rules, so a model that imports only the types builds all of them. That
  the declarations need nothing else is to be checked.

A mapping's `g` then takes the generated types, as [function mappings](function-mappings.md#the-claim)
asks, and the model has one text of its types, the Scala one.

## The ties

| Scala | Lean | Tie | Rests on |
|---|---|---|---|
| `SwapRules.guard`, compiled | `Swap.guard` | a mapping, by `blaster-uplc` ([A1](function-mappings.md#a1-blaster)) | Lean's model of UPLC, Blaster, Z3 |
| the claim function, on the JVM | `Swap.claim` | the same source compiled ([B1](function-mappings.md#b1-the-same-source-compiled-for-the-proof)) where the guard recomputes; a translation ([B2](function-mappings.md#b2-a-translation-of-a-subset-of-scala-to-lean)) or random tests where it checks | the JVM agreeing with UPLC, or the translator and its primitives, or nothing |
| the validator | the shape, and `Swap.guard` | the decomposition statements below | Lean's model of UPLC, Blaster, Z3 |
| the simulator's step | `step` | random traces through both | nothing: evidence only |
| — | the theorems | Lean's kernel | the kernel |

**The guard has no loop, as a rule.** A step's arithmetic rules are comparisons of products and
sums. A function with no loop runs a number of steps that does not depend on its input, so one
budget covers every input, and Blaster decides the mapping where Z3 decides the arithmetic. Where
Z3 does not, a proof by hand of the mapping does ([A2](function-mappings.md#a2-a-proof-by-hand)).
The exception is a guard that recomputes through a loop, as the AMM's deposit does through
`Math.sqrt` on a first deposit: [A3](function-mappings.md#a3-a-loop), or the loop's case stated
apart.

**The guard has no `Value`, so Lean's model runs it.** The Value builtins are in the shape layer,
which reads the claim. The step's arithmetic is then the part of the validator that the model of
UPLC can run today.

**The decomposition statements** tie the validator to its guard. They are ordinary statements of
`blaster-uplc`, over script contexts, written in Scala:

```scala
// the validator accepts a Swap only if its claim passes the guard: the guard is necessary
forAllData[ScriptContext](ctx =>
    (isSwap(ctx) && succeeds(validator, ctx.toData)) ==>: SwapRules.guard(feeNumOf(ctx), feeDenOf(ctx), datumOf(ctx), claimOf(ctx))
)
// and a Swap whose shape holds and whose claim passes is accepted: the guard is sufficient
forAllData[ScriptContext](ctx =>
    (isSwap(ctx) && shapeHolds(ctx) && SwapRules.guard(...)) ==>: succeeds(validator, ctx.toData)
)
```

The first follows from [ST-3] where the validator is written as above, and the statement checks it.
Both range over script contexts, the costly kind, and may need their data limited
(`forAllDataLimited`). The guard's mapping and the model's theorems need no script context at all.

**What a theorem then says.** `Swap.product_never_falls` is proved of the model. With the guard's
mapping it holds of the compiled guard, and with the first decomposition statement it holds of
every transaction the validator accepts: "no sequence of swaps the validator accepts lowers the
product of the reserves". Its proof rests on the weakest of its ties, Blaster and Z3, and it is
about the program compiled for the proof, with the tactic's options, not the bytes of the release
script.

## What stays outside the step

- **Intent.** A quote, or "the largest amount in that keeps the price within a bound", is a search
  over the claim function. It has its own theorem: the result fits, and one more does not.
- **Contention.** Two transactions that spend the same state: one wins, and the other is built
  again. The model and a simulator run steps one after the other.
- **Several steps in one transaction.** The AMM's deposit runs two purposes of one script, the
  spend and the mint of LP tokens. A protocol with several validators in one transaction has a step
  for each. The model can have the joint step, but no statement of Scalus is about two validators
  in one transaction yet.
- **The ledger.** Its conditions are premises of the model's `step`, and the CardanoLedgerApi
  validity predicates, once a statement can take them for the whole transaction.

## What exists

- [Function mappings](function-mappings.md), proposed: the ties of the guard and of the claim
  function.
- `LeanTypes`, which writes the types and their encodings in one module ([lean-types.md](lean-types.md)).
- `forAllData[ScriptContext]`, `forAllDataLimited`, `succeeds` and `fails`, for the decomposition
  statements.
- `AmmMath`, already apart from the AMM's validator and called by its builder, with `ensuring`
  clauses that `AmmVerificationTest` proves.
- `SpecTest`, which checks that clauses leave a script's bytes unchanged, a pattern for the hash
  check of [ST-3].

## Steps

1. **The module of the types imports PlutusCore's data types only** ([ST-9]).
2. **The AMM's Swap as the first step.** `SwapClaim` and `SwapRules` in Scala, and what the
   extraction does to the script's bytes; `Swap.guard` and `Swap.claim` in Lean; the mapping of the
   guard (the first use of `leanMapping`); the first decomposition statement;
   `Swap.product_never_falls` in Lean.
3. **A guard that checks**, with a claim function only off-chain, by random tests first, by
   translation once B2 exists.
4. **A protocol of several validators**, to see what in this holds across a transaction.

## Open questions

1. **A `Step` type in Scalus?** A trait with the claim, the guard and the claim function, from
   which Scalus writes the Lean skeleton of the step (its structures and the signatures of `guard`
   and `claim`), or a convention only.
2. **The rule ID on chain.** A guard that returns the first failing rule costs budget on every
   transaction. A Boolean on chain and the rule functions off chain seem enough.
3. **The size of the decomposition statements**, over contexts of a real transaction, and the
   limits they need.
4. **The model of several steps in one transaction.**
5. **A model without Scalus's library.** Where a model must build without it, as on a newer Lean
   than Blaster and PlutusCore pin, `LeanTypes` could write the structures in a module that imports
   nothing, over `Int`, `Bool` and `String` (what PlutusCore's `ByteString` holds), and their
   encodings in a second module. A type with a `Data` part, or one bound to a library's type,
   would stay whole in the second. Not needed so far.
