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
- **`denotes(e)` holds iff `e` returns,** with any value. `succeeds(e)` is the same statement,
  and `fails(e)` is its negation, `!denotes(e)`: the evaluation of `e` ends in an error. They are
  about how the evaluation ends, whatever the type of `e`: `fails(x > 0)` holds for no `x`,
  because a comparison always returns a Boolean.
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

`succeeds` and `fails` (§2) also take a function of the table and its argument, and two more
forms quantify over a function's arguments. All are syntax, with no node of their own:

| Written | Means |
|---|---|
| `succeeds(f, a)` | `call(f, a)(_ => true)`: the call returns |
| `fails(f, a)` | `!succeeds(f, a)`: the call does not return |
| `returnsWhen(f)(args => c)` | `∀ args. c ==> succeeds(f, args)` |
| `failsWhen(f)(args => c)` | `∀ args. c ==> fails(f, args)` |

The function forms exist because a function registered under a synthetic name has no Scala
symbol to write in an expression; for an `@Compile` method, `fails(Math.sqrt(x))` and a call form
say the same, and both run the registered program.

`fails` holds of an evaluation that does not return, which by its meaning includes one that never
terminates. A bounded tactic proves the stronger fact, that the call fails within the budget,
and tells that apart from a budget that runs out (§5). `!Prop(t)` is a different statement: it
also holds when the test `t` returns `false`.

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
∀ args. expects(args) ⇒ whenReturns(f, args)(r => ensures(args)(r))      partial
∀ args. expects(args) ⇒ call(f, args)(r => ensures(args)(r))             total
```

- **`expects` is an obligation of every caller, and an assumption of `f`.** Proving the contract
  assumes it; using the contract at a call site requires showing it there.
- **`ensures` is a guarantee of `f`**, under `expects`. A caller may assume it about the result.
- **Partial correctness is the default**: a contract says nothing about arguments on which `f`
  fails. The total form also claims that `f` returns where `expects` holds.
- **An entry point has no trusted caller.** A validator receives whatever a transaction
  supplies, so no one establishes its `expects`: a precondition there is an unsound assumption.
  A validator's contract has `expects = true`, and a condition the validator relies on is
  checked at runtime with `require`. What the validator guarantees is then an `ensures` on its
  success: "if the script succeeds, the transaction is signed by the beneficiary".

### Call-site obligations

That a caller establishes a callee's precondition is checked per call (`Verifier.obligations`,
`Obligations.scala`). For a call `f(a, b)` in the body of `g`, where `f` has a declared
contract, the obligation is a statement over `g`'s parameters:

```
∀ g's parameters. [expects_g ⇒] ( denotes(reach) ⇒ check )
```

`reach` and `check` are two tests cut out of `g`'s SIR along the path to the call:

- the bindings before the call stay, and are evaluated;
- at an `if`, a `match`, `&&` or `||`, the branch that leads to the call is kept, and the others
  return `true`;
- the call itself becomes `let x = a; y = b in true` in `reach`, and
  `let x = a; y = b in expects_f` in `check`, with the variables of `f`'s contract bound to the
  arguments.

So `reach` runs what `g` runs before the call and fails when `g` does, and the obligation holds
where the call is not reached. `check` adds the precondition at the call.

| Caller | Obligation | Verdict |
|---|---|---|
| `clamp(x, 10, 0)` | `10 <= 0` | refuted |
| `clamp(x, lo, hi)` | `lo <= hi`, for all `lo`, `hi` | refuted |
| the same, in a function that itself expects `lo <= hi` | `lo <= hi ⇒ lo <= hi` | proved |
| `if lo <= hi then clamp(x, lo, hi) else lo` | `if lo <= hi then lo <= hi else true` | proved |
| `require(lo <= hi); clamp(x, lo, hi)` | where the `require` passes, `lo <= hi` | proved |

- **The caller's own contract is the premise.** `obligations(contractOfG)` assumes `g`'s
  precondition: `g` may rely on what its own callers establish. `obligations(g)` assumes nothing.
- **A runtime check counts.** The last row holds because the `require` is evaluated on the path:
  where it fails, `reach` fails. The same goes for a callee's own `require`s in a binding before
  the call. Runtime checks need no clause in a contract to be taken into account.
- **Recursion needs no unrolling.** A recursive call is a call like any other, checked under the
  function's own precondition.
- **The cut is conservative.** Code evaluated beside the path, as `h(a)` in `h(a) + f(b)`, is left
  out. If it would fail first, the call is never reached, yet the obligation is still asked.
- **Not supported yet:** a call inside a function value, as in `xs.map(x => f(x))`, and a
  precondition that is a statement, not a Boolean test. Such calls are returned in
  `CallObligations.unsupported`, with the reason, so that they are not taken as checked.

## 7. Specifications in the function's body (proposed)

A contract can be written next to the code it describes:

```scala
@Compile
object Math {
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}
```

- **`Spec.expects(c)`, at the top of the body,** is the contract's precondition. It is not
  called `requires`: the prelude's `require` is a runtime check, and the two words would read
  as one. Several
  clauses are conjoined. Placing them first makes the precondition read as part of the signature,
  as in Stainless and Dafny, and lets the reifier find them without analysing control flow.
- **`body.ensuring(r => c)`** is the postcondition, over the result `r` and the parameters. It
  is the `ensuring` idiom of Scala's `Predef`, provided by `Spec` as an extension the plugin can
  compile. It keeps the result type inferred; the earlier `spec.ensuresResult[A](r => …)` needed
  it written.
- **`Spec.total`** (open, overview §10 question 2) would make the contract total.

It means exactly the external contract `contract(clamp)(expects, ensures)` of §6, and is
registered with `Origin.Contract(clamp, total)` when the function table is built from the
object (see [function tables](prop-capture.md#function-tables-from-compile-objects-proposed)).

## 8. Design questions

### 8.1 Where preconditions come from: `Spec.expects`, `require`, or inference

Three sources can describe when a function may be called.

1. **An explicit `Spec.expects`** states intent: what callers must establish.
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

**Recommendation.** `Spec.expects` is the contract; `require` is behaviour.

- Methodologically, a specification says what the function *should* require, independently of
  how it is written; an inferred condition says what it *does* require, and changes with every
  refactoring. Verification is the check that the two agree, so the inferred one cannot replace
  the stated one.
- Modularity needs the stated one: a caller is checked against the callee's `expects` without
  analysing the callee's body, and a large inferred condition would leak implementation into
  every caller.
- Harvesting `require` and inference are still valuable, as tools: to suggest a missing
  `Spec.expects`, to discharge trivial obligations, and to check that a stated precondition
  makes the function total (`expects ⇒ denotes(f(args))`, the total contract).

### 8.2 Whether `Spec.expects` has runtime semantics

- **On chain: none, by default.** The clause is erased before lowering, so the script's bytes and
  hash do not change (measured for `VestingValidator` on the earlier branch). A precondition
  exists to save the check, which callers have established; paying for it on chain would turn it
  into a `require`.
- **On the JVM: checked.** `@Compile` code also runs as Scala, in tests and the emulator. There
  `Spec.expects(c)` throws when `c` is false, and `ensuring` checks the result, which catches a
  violated contract in ordinary tests at no on-chain cost.
- **On chain, optionally checked.** A compiler option, off by default, would lower the clauses
  as `require`, like assertions in a debug build, to test a contract on the CEK.
- **Callers are checked by the verifier, not by scalac.** That a call site establishes the
  callee's `expects` is a proof obligation over the caller's path condition, which needs a
  solver. `sbt verify` reports an unproved obligation, and can fail the build; scalac could
  catch only syntactic cases, such as a constant argument that violates a constant precondition.

So there is no duplication: a condition that must hold against adversarial input is a `require`
(behaviour, paid on chain); a condition that trusted callers establish is a `Spec.expects`
(free on chain, checked off chain and by the verifier).

### 8.3 Runtime checks in contracts: found, not written; `returnsWhen` and `failsWhen`

Should a contract mention a function's runtime checks, its `require`s?

**Not by restating them.** They are behaviour, and the verifier reads behaviour from the code.
Finding them is part of verification:

- a tactic that runs the code, as `blaster-uplc` does, executes them: a caller's call-site
  obligation runs the callee's own program, so a callee's `require` already constrains what its
  callers are held to;
- a modular tactic, which treats a callee abstractly, harvests them from SIR (§8.1): each
  `require(c)` on a path gives the derived fact `succeeds(f(args)) ⇒ c`, and the verifier can
  report the harvested condition, `checks(f, args)`, to the user.

**Failure is already a statement.** `!denotes(e)` says that `e` does not return, and for a
function `fails(f, args)` says it of a call (§3). `returnsWhen(f)(c)` and `failsWhen(f)(c)` state,
as statements of their own, when a function is meant to return and when to fail; they are
implemented.

**Proposed: the same two as clauses of a contract**, next to `expects` and `ensures`, each a
sufficient condition:

```scala
contract(withdraw)(
  expects = (owner, amount, ctx) => true,          // an entry point has no trusted caller
  returnsWhen = (owner, amount, ctx) => ctx.signedBy(owner) && amount <= ctx.balance,
  failsWhen = (owner, amount, ctx) => !ctx.signedBy(owner),
  ensures = (owner, amount, ctx) => r => …
)
```

```
∀ args. expects(args) ⇒   (returnsWhen(args) ⇒ succeeds(f, args))
                         ∧ (failsWhen(args) ⇒ fails(f, args))
                         ∧ whenReturns(f, args)(r => ensures(args)(r))
```

- **`failsWhen`** is what the function must reject. For a validator this is safety: nothing is
  spent without authorisation. It is the stated counterpart of the runtime checks, and proving
  it checks them against the intention.
- **`returnsWhen`** is what the function must accept. For a validator this is liveness:
  authorised spends are not locked.
- **The two need not cover every argument.** Between them the contract says nothing, which
  matters in practice: a validator's exact success condition includes every way its input can
  fail to decode, and few specifications want to spell that out. Stating both with complementary
  conditions, `failsWhen = !returnsWhen`, gives the exact characterisation. Conditions that
  overlap make the contract unsatisfiable, and it is refuted.
- **The existing forms are special cases.** A partial contract has neither clause; a total
  contract is `returnsWhen = true`, so `totalContract` becomes shorthand.

The clauses add nothing to what the standalone statements say, except that they hold under
`expects` and belong to the function's contract. The pair corresponds to JML's
`normal_behavior` and `exceptional_behavior` specification cases. The harvested
`checks(f, args)` is what the verifier can suggest as a first draft of `failsWhen`: its negation.

### 8.4 Validators

A `Spec.expects` on a validator's entry point is unsound (§6), so the verifier should reject it.
The validator's specification is `ensures` clauses about success, and its robustness is the
total claim that it never fails in an unexpected way on any `Data`.

### 8.5 Function types

Higher-order functions need quantification over functions: `map`'s contract is about every
`f`, and a precondition on a function parameter puts a quantifier inside the premise. `lean-direct`
handles this natively, with `∀ (f : A → Option B)` and Z3's uninterpreted functions.
`blaster-uplc` cannot quantify over UPLC lambdas; within a step budget a function is observed at
finitely many points, so it can quantify over table functions instead, a finite map with a
default (and a failure entry), passed as `λx. lookup(table, x)`.
