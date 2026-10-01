# Statement semantics

What a `Prop` means, independently of the tactic that proves it, and what a function's
specification means. Part of [the verification overview](../verification-overview.md), which
states the rules briefly (§3.3, §3.7); this file gives them in full, and records the design
questions about specifications, with options and recommendations. How statements are captured is
in [statement capture](prop-capture.md).

Status: §1–§6 describe what is implemented. §7 proposes the meaning of specifications written
in a function's body; §8 records the open design questions.

## 1. Values and quantifiers

A quantifier ranges over the values of its type **as a Scalus program sees them**:

| Type | Domain |
|---|---|
| `BigInt`, `Boolean`, `ByteString`, `Data` | every value |
| a case class or enum | every value built from its constructors, with fields in their domains |
| `List[A]`, `Option[A]` | every list or option of values of `A` |
| a function type | not quantifiable yet (§8.4) |

A case class's domain is the image of its constructors, not every `Data` value that `FromData`
might decode. Quantifying over raw input, as a validator must (it receives any `Data`), is
written explicitly: `forAll[Data](d => …)`, with `d.to[A]` where the program decodes.

## 2. Tests, `denotes` and `equal`

- **A test holds iff its program returns `true`.** The program is the test compiled to UPLC and
  evaluated with on-chain semantics. A test that fails or does not terminate does not hold.
- **`denotes(e)` holds iff `e` returns,** with any value. It is the only way to state that code
  succeeds, or, negated, that it fails.
- **`equal(a, b)` holds iff both return and the values are equal**: `equalsInteger`,
  `equalsData`, or Boolean equality.

A test is ordinary code: Boolean `&&` and `||` short-circuit, `!` negates a returned value, and an
error anywhere makes the whole test fail.

## 3. Calls

- **`call(f, a)(r => p)` holds iff `f(a)` returns a value `r` and `p` holds of it** (total
  correctness).
- **`whenReturns(f, a)(r => p)` holds iff, when `f(a)` returns a value `r`, `p` holds of it**
  (partial correctness). It equals `denotes(f(a)) ==> call(f, a)(r => p)`.

A call runs the function's own compiled program, not a copy compiled with the test; how values
cross that boundary is in [the tactic's calling convention](uplc-blaster.md#linking).

## 4. Connectives

Connectives between statements are classical: `&&`, `||`, `!`, `==>`, `<=>` as in Lean and SMT.
A test's truth is two-valued (it holds or not), so a failing test is simply false at the
statement level.

The difference from Boolean operators inside a test shows only when an operand fails:

| Written | `x = 0` | Why |
|---|---|---|
| `!(10 / x > 1)`, one test | false | the test fails, so it does not hold |
| `!Prop(10 / x > 1)`, a negated statement | true | the statement `10 / x > 1` does not hold |
| `(10 / x > 1) \|\| (x == 0)`, one test | false | `\|\|` evaluates its left operand first, which fails |
| `Prop(10 / x > 1) \|\| Prop(x == 0)` | true | the right statement holds |

`&&` agrees in both readings. Capture keeps exactly the reading the user wrote: a Boolean
expression is one test, and a statement-level connective is written over statements.

## 5. Bounded tactics

A tactic that runs programs for at most a budget of steps reads each test by its position: a
positive test must halt with `true` within the budget, a negative one must not halt with `false`
nor fail within it. A proof at any budget then holds without the budget; a falsification may be
spurious and is replayed without the budget. See overview §6.2 and
[the tactic's reading](uplc-blaster.md#reading-leaves-by-polarity).

## 6. Contracts

A contract of `f` (implemented as `Props.contract`, `Props.totalContract`) is

```
∀ args. requires(args) ⇒ whenReturns(f, args)(r => ensures(args)(r))     partial
∀ args. requires(args) ⇒ call(f, args)(r => ensures(args)(r))            total
```

- **`requires` is an obligation of every caller, and an assumption of `f`.** Proving the contract
  assumes it; using the contract at a call site requires showing it there.
- **`ensures` is a guarantee of `f`**, under `requires`. A caller may assume it about the result.
- **Partial correctness is the default**: a contract says nothing about arguments on which `f`
  fails. The total form also claims that `f` returns where `requires` holds.
- **An entry point has no trusted caller.** A validator receives whatever a transaction
  supplies, so no one establishes its `requires`: a precondition there is an unsound assumption.
  A validator's contract has `requires = true`, and a condition the validator relies on is
  checked at runtime with `require`. What the validator guarantees is then an `ensures` on its
  success: "if the script succeeds, the transaction is signed by the beneficiary".

## 7. Specifications in the function's body (proposed)

A contract can be written next to the code it describes:

```scala
@Compile
object Math {
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.requires(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}
```

- **`Spec.requires(c)`, at the top of the body,** is the contract's precondition. Several
  clauses are conjoined. Placing them first makes the precondition read as part of the signature,
  as in Stainless and Dafny, and lets the reifier find them without analysing control flow.
- **`body.ensuring(r => c)`** is the postcondition, over the result `r` and the parameters. It
  is the `ensuring` idiom of Scala's `Predef`, provided by `Spec` as an extension the plugin can
  compile. It keeps the result type inferred; the earlier `spec.ensuresResult[A](r => …)` needed
  it written.
- **`Spec.total`** (open, overview §10 question 2) would make the contract total.

It means exactly the external contract `contract(clamp)(requires, ensures)` of §6, and is
registered with `Origin.Contract(clamp, total)` when the function table is built from the
object (see [function tables](prop-capture.md#function-tables-from-compile-objects-proposed)).

## 8. Design questions

### 8.1 Where preconditions come from: `Spec.requires`, `require`, or inference

Three sources can describe when a function may be called.

1. **An explicit `Spec.requires`** states intent: what callers must establish.
2. **The prelude's `require(c)`** compiles to `if c then () else error`. It is behaviour, not an
   assumption: the function fails unless `c` holds. Harvested, it gives a derived fact,
   `denotes(f(args)) ⇒ c(args)`: after a successful call, `c` held. For a validator, that is
   exactly "the script succeeds only if `c`" (overview §8.3).
3. **Inference by abstract interpretation.** Scalus programs are pure and immutable: no heap, no
   mutation, recursion only through `let rec`. Symbolic execution over SIR collects, per function,
   the path conditions of its error exits. Their negation is the *definedness condition*: the
   weakest precondition under which the function returns. Without recursion this is exact path
   enumeration, exponential only in the number of branches, which is small in practice;
   recursion needs summaries or bounded unrolling.

**Recommendation.** `Spec.requires` is the contract; `require` is behaviour.

- Methodologically, a specification says what the function *should* require, independently of
  how it is written; an inferred condition says what it *does* require, and changes with every
  refactoring. Verification is the check that the two agree, so the inferred one cannot replace
  the stated one.
- Modularity needs the stated one: a caller is checked against the callee's `requires` without
  analysing the callee's body, and a large inferred condition would leak implementation into
  every caller.
- Harvesting `require` and inference are still valuable, as tools: to suggest a missing
  `Spec.requires`, to discharge trivial obligations, and to check that a stated precondition
  makes the function total (`requires ⇒ denotes(f(args))`, the total contract).

### 8.2 Whether `Spec.requires` has runtime semantics

- **On chain: none, by default.** The clause is erased before lowering, so the script's bytes and
  hash do not change (measured for `VestingValidator` on the earlier branch). A precondition
  exists to save the check, which callers have established; paying for it on chain would turn it
  into a `require`.
- **On the JVM: checked.** `@Compile` code also runs as Scala, in tests and the emulator. There
  `Spec.requires(c)` throws when `c` is false, and `ensuring` checks the result, which catches a
  violated contract in ordinary tests at no on-chain cost.
- **On chain, optionally checked.** A compiler option, off by default, would lower the clauses
  as `require`, like assertions in a debug build, to test a contract on the CEK.
- **Callers are checked by the verifier, not by scalac.** That a call site establishes the
  callee's `requires` is a proof obligation over the caller's path condition, which needs a
  solver. `sbt verify` reports an unproved obligation, and can fail the build; scalac could
  catch only syntactic cases, such as a constant argument that violates a constant precondition.

So there is no duplication: a condition that must hold against adversarial input is a `require`
(behaviour, paid on chain); a condition that trusted callers establish is a `Spec.requires`
(free on chain, checked off chain and by the verifier).

### 8.3 Validators

A `Spec.requires` on a validator's entry point is unsound (§6), so the verifier should reject it.
The validator's specification is `ensures` clauses about success, and its robustness is the
total claim that it never fails in an unexpected way on any `Data`.

### 8.4 Function types

Higher-order functions need quantification over functions: `map`'s contract is about every
`f`, and a precondition on a function parameter puts a quantifier inside the premise. `lean-direct`
handles this natively, with `∀ (f : A → Option B)` and Z3's uninterpreted functions.
`blaster-uplc` cannot quantify over UPLC lambdas; within a step budget a function is observed at
finitely many points, so it can quantify over table functions instead, a finite map with a
default (and a failure entry), passed as `λx. lookup(table, x)`.
