# Statement semantics

What a `Prop` means, independently of the tactic that proves it, and what a function's
specification means. Part of [the verification overview](../verification-overview.md), which
states the rules briefly (§3.3, §3.7); this file gives them in full, and records the design
questions about specifications, with options and recommendations. How statements are captured is
in [statement capture](prop-capture.md).

Status: §1–§7 describe what is implemented, §7 the specifications written in a function's body;
§8 records the design questions, and says of each what was built.

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

A contract of `f` (implemented as `Props.contract`, with the optional clauses `returnsWhen` and
`failsWhen`) is

```
∀ args. expects(args) ⇒   (returnsWhen(args) ⇒ succeeds(f, args))
                         ∧ (failsWhen(args) ⇒ fails(f, args))
                         ∧ whenReturns(f, args)(r => ensures(args)(r))
```

```scala
contract(div10)(expects = x => true, ensures = x => r => r <= BigInt(10))
    .returnsWhen(x => x != BigInt(0))
    .failsWhen(x => x == BigInt(0))
```

- **`expects` is an obligation of every caller, and an assumption of `f`.** Proving the contract
  assumes it; using the contract at a call site requires showing it there.
- **`ensures` is a guarantee about the result**, under `expects`. A caller may assume it of a
  call that returned.
- **`returnsWhen` and `failsWhen` are guarantees about the outcome.** Each is a sufficient
  condition: where it holds, the function returns, or fails. They are not obligations, so a
  caller owes nothing for them. Like `ensures`, they hold under `expects`: a clause about
  arguments that do not satisfy the precondition says nothing.
- **Partial correctness is the default**: without `returnsWhen`, a contract does not say that `f`
  ever returns. `Props.totalContract` is the contract with `.returnsWhen(_ => true)`.
- **An entry point has no trusted caller.** A validator receives whatever a transaction
  supplies, so no one establishes its `expects`: a precondition there is an unsound assumption.
  A validator's contract has `expects = true`, and a condition the validator relies on is
  checked at runtime with `require`. What the validator guarantees is then stated with
  `failsWhen` and `ensures`: "the script fails unless the transaction is signed by the
  beneficiary".

The two outcome clauses divide the arguments that satisfy `expects`:

| Arguments | The contract says |
|---|---|
| `returnsWhen` holds | `f` returns, and `ensures` holds of the result |
| `failsWhen` holds | `f` fails |
| neither | `f` may do either; if it returns, `ensures` holds |
| both | impossible, so the contract is refuted |

The clauses need not be each other's negation. The region where neither holds is left open on
purpose; for a validator it is typically malformed input, which few specifications want to spell
out. That a specification leaves no gap, `∀ args. expects ⇒ returnsWhen ∨ failsWhen`, is a
statement about the clauses alone, which can be proved separately when it is wanted.

The two are symmetric, and each equals a postcondition on the other outcome, by contraposition:

```
.failsWhen(c)      ≡   returns ⇒ ¬c      a postcondition on a run that returns
.returnsWhen(c)    ≡   fails ⇒ ¬c        a postcondition on a run that fails
```

`ensures` is the clause for the first kind, so `.failsWhen(c)` can also be written as the
postcondition `ensures = args => r => !Prop(c(args))`. The negation is of the statement, not
inside the test (§4): where `c` itself fails to evaluate, `failsWhen` claims nothing, while a
test `!c(args)` would fail and the contract with it. There is no clause for the second kind,
which is why only `returnsWhen` adds something a partial contract cannot say. A runtime
`require(c)` in the body shows up as the first kind: the function returns only when `c` held.

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
- **Names.** An obligation is named `caller/contract#n`; under the caller's own contract it is
  `callerContract/contract#n`, so both can be declared in one verifier.
- **An entry point is checked through a shape.** A validator takes any `Data`, and the code
  before a call usually walks a list of the transaction, which a bounded tactic does not finish
  ([uplc-blaster.md](uplc-blaster.md#statements-that-do-not-finish)). The caller is then a
  function of a transaction shape, which builds the context and calls the validator:
  `(w: Withdrawal) => Validator.validate(context(w))`. Its obligations are over the shape's
  numbers.

In the vesting example, a contract of `linearVesting` that expects `initialAmount >= 0` is not
established by the validator: its call is **refuted**, with a negative amount. The validator
reads the datum and does not check its numbers, and an entry point has no caller to rely on.
Under a contract of the withdrawal that expects such an amount, the obligation is proved. Either
the validator gets a `require`, or the bound moves from the precondition into `ensures` as a
condition. The example takes the second: `linearVesting` ends in
`.ensuring(vested => initialAmount < 0 || (vested >= 0 && vested <= initialAmount))`, and the
published script stays as it was. What `spend` guarantees is stated at its head, with
`Spec.ensures`: the beneficiary signed, something is withdrawn, and what stays locked is at least
what has not vested.

## 7. Specifications in the function's body

A contract can be written next to the code it describes, with
`scalus.cardano.onchain.plutus.prelude.Spec`:

```scala
@Compile
object Math {
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}

@Compile
object VestingValidator extends Validator {
    inline override def spend(datum: Option[Data], redeemer: Data, txInfo: TxInfo, ref: TxOutRef): Unit = {
        Spec.ensures(txInfo.isSignedBy(configOf(datum).beneficiary))
        ...
    }
}
```

The clauses are with the contract, next to the function they describe. What is stated about a
contract from outside, in a proof suite, needs no clause: it is a statement of the verifier.

- **`Spec.expects(c)`, at the top of the body,** is the contract's precondition. It is not
  called `requires`: the prelude's `require` is a runtime check, and the two words would read
  as one. Several
  clauses are conjoined. Placing them first makes the precondition read as part of the signature,
  as in Stainless and Dafny, and lets the reifier find them without analysing control flow.
- **`Spec.ensures(c)`, also at the top,** is a postcondition over the parameters: where the
  function returns, `c` holds. It is the clause of a validator's handler, which returns nothing:
  where the script succeeds, `c` holds of the transaction. A clause sees the parameters, not
  what the body computes from them, so it derives that again, usually through small functions
  written for the specification (`configOf(datum)`). Those are removed from the script with the
  clauses. It is not evaluated off-chain: at the top of a body it is not known yet whether the
  function returns.
- **`body.ensuring(r => c)`** is the postcondition, over the result `r` and the parameters. It
  is the `ensuring` idiom of Scala's `Predef`, provided by `Spec` as an extension the plugin can
  compile (`import Spec.ensuring`). It is applied to the body's last expression, or to the whole
  body, `{ … }.ensuring(r => c)`. What it is applied to is then typed without the function's
  result type: a branch that is the literal `0` is written `BigInt(0)`.
- **`Spec.total`** (open, overview §10 question 2) would make the contract total. Until then,
  `.returnsWhen(_ => true)` on the contract says it.

It means exactly the external contract `contract(clamp)(expects, ensures)` of §6.
`Contract.inSource(function)` reads it from the function's SIR (`Specifications.scala`), and
returns `None` for a function that states none. The contract is then declared with
`verifier.contract`, proved, and owed by callers like any other. Registering it when a function
table is built from an object (see
[function tables](prop-capture.md#function-tables-from-compile-objects-proposed)) is not built.

**Clauses inside a function's code.** A validator's handlers are `inline`, so once `validate` is
compiled the clauses of `spend` sit in a branch of its code, not at the top of a function.
`verifier.guarantees(function)` finds every `ensures` and `ensuring` clause on a path of the
function's code, and declares for each the statement

```
∀ parameters. denotes(body) ⇒ check
```

where `check` is the body cut along the path to the clause, as for a call-site obligation, with
the clause's condition in its place. It says: where the function returns through the clause, the
condition holds. The statements are named `function/ensures#n` (`Origin.Guarantee`). A clause
inside a function value is reported as unsupported. For a validator the function is one that
builds the context of a transaction shape and validates it (see call-site obligations above).
A call inside a clause is no call of the function, and owes nothing.

Several clauses are also declared as one statement, `function/ensures`
(`StatedGuarantees.together`, `Origin.Guarantees`):

```
∀ parameters. denotes(body) ⇒ check₁ ∧ … ∧ checkₙ
```

Every statement has the leaf `denotes(body)`, so a tactic that proves the clauses one by one runs
the function's body once per clause, and once for them together. The three clauses of
`VestingValidator.spend` take 67 s together, and 3 min one by one.

The statement is about the function alone, on every argument. A function's own `Spec.expects` is
no check, so a clause that relies on it is refuted this way: `clamp` returns for `lo > hi` too,
with a result that is not between them. `verifier.guarantees(contract)`, with the function's
declared contract, states the same clauses under that contract's precondition, named
`contract/ensures#n`, as `verifier.obligations(callerContract)` does for the calls.

**How it is kept and removed.** `Spec` is an `@Compile` object, so the plugin compiles a clause as
a call of one of its functions, which stays in the function's SIR. `EraseSpecifications`, the
first step of the SIR to UPLC pipeline, removes it:

- an `expects` or `ensures` statement goes, with its condition;
- `Spec.ensuring(body, condition)` becomes `body`;
- the definitions of the `Spec` functions go, and so does a function that only clauses used;
- the definitions that are left are nested again in the order the code alone meets them. Linking
  orders definitions by first reference, and a clause at the head of a function names functions
  before its code does; without this step the script's bytes depended on the clauses.

What is left is the SIR of the same code without the clauses. A program without clauses is not
touched.

**How it is checked that this works** (`SpecTest`, `SpecificationsTest`,
`VestingVerificationTest`):

| Claim | Check |
|---|---|
| a clause does not change the script | the same function with and without clauses compiles to the same bytes, under four option sets, also when a clause names functions in another order than the code, or uses a function of its own; `VestingContract`'s published hash is the same before and after `spend` and `linearVesting` were annotated |
| a clause is kept for the verifier | the function's SIR names `Spec$.expects` and `Spec$.ensuring`; `Contract.inSource` returns the contract, and `None` for an unspecified function |
| the contract is the code's | it is proved about the compiled function; a function with a wrong postcondition is refuted |
| a handler's guarantees hold | the clauses of an inlined handler are found in the entry point's code and proved; a wrong clause in a branch or at the head is refuted; the three clauses of `VestingValidator.spend` are proved for every withdrawal of the shape |
| callers owe the precondition | a caller that violates it is refuted, one that guards the call is proved, and one that only states the same `Spec.expects` is refuted alone and proved under its own contract |
| off-chain the clauses are checked | a violated `expects` or `ensuring` throws `SpecificationError` on the JVM, an `AssertionError` and no `OnchainError`, so a test that expects the script to fail does not pass on it; `ensures` is not evaluated |

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

- **On chain: none, by default (built).** The clause is erased before lowering, so the script's
  bytes and hash do not change. A precondition exists to save the check, which callers have
  established; paying for it on chain would turn it into a `require`.
- **On the JVM: checked (built).** `@Compile` code also runs as Scala, in tests and the emulator.
  There `Spec.expects(c)` throws when `c` is false, and `ensuring` checks the result, which
  catches a violated contract in ordinary tests at no on-chain cost.
- **On chain, optionally checked (not built).** A compiler option, off by default, would lower
  the clauses as `require`, like assertions in a debug build, to test a contract on the CEK.
- **Callers are checked by the verifier, not by scalac.** That a call site establishes the
  callee's `expects` is a proof obligation over the caller's path condition, which needs a
  solver. `sbt verify` reports an unproved obligation, and can fail the build; scalac could
  catch only syntactic cases, such as a constant argument that violates a constant precondition.

So there is no duplication: a condition that must hold against adversarial input is a `require`
(behaviour, paid on chain); a condition that trusted callers establish is a `Spec.expects`
(free on chain, checked off chain and by the verifier).

### 8.3 Runtime checks in contracts: found, not written

Should a contract mention a function's runtime checks, its `require`s?

**Not by restating them.** They are behaviour, and the verifier reads behaviour from the code.
Finding them is part of verification:

- a tactic that runs the code, as `blaster-uplc` does, executes them: a caller's call-site
  obligation runs the callee's own program, so a callee's `require` already constrains what its
  callers are held to;
- a modular tactic, which treats a callee abstractly, harvests them from SIR (§8.1): each
  `require(c)` on a path gives the derived fact `succeeds(f, args) ⇒ c`, and the verifier can
  report the harvested condition, `checks(f, args)`, to the user.

**What a contract states is the intention**, with `failsWhen` and `returnsWhen` (§6): what the
function must reject, and what it must accept. For a validator the first is safety, nothing is
spent without authorisation, and the second is liveness, authorised spends are not locked.
Proving the contract checks the runtime checks, and every other way the function can fail,
against that intention. The harvested `checks(f, args)` is what the verifier can suggest as a
first draft of `failsWhen`: its negation.

The pair corresponds to JML's `normal_behavior` and `exceptional_behavior` specification cases,
and the gap check to ACSL's `complete behaviors`.

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
