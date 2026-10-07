# Verification in Scalus — overview

Status: **draft design**. Date: 2026-10-01.

This document fixes the design. The details are in
[`verification-details/`](verification-details/):
[statement semantics](verification-details/prop-semantics.md),
[statement capture](verification-details/prop-capture.md),
[the `blaster-uplc` tactic](verification-details/uplc-blaster.md) and
[the Lean server](verification-details/lean-server.md), which the tactic runs its checks in.

This document describes how logical statements about Scalus code are written in Scala and how they
are proved. It fixes four layers and the contracts between them:

1. **`Prop`**, a runtime logical statement in Scalus: typed binders, connectives and tests.
2. **Syntax capture**, a compile-time mapping from Scala statement syntax to the explicit `Prop`
   nodes. Scala lambdas are only notation for introducing binders; they are not stored in `Prop`.
3. **Verification requests**, Scala values that pair a statement with the tactic that should prove it.
4. **Tactics**, the pluggable backends that discharge a statement: `blaster-uplc`, `lean-direct`,
   `scalacheck`, and later others.

The statement side lives in Scala, type-checked by scalac against the real definitions. The proof
side stays in the backends: Lean 4, Blaster and Z3 today.

---

## 1. Where we are

Two pieces of work exist.

- **`scalus-verification`** (formerly `scalus-lean-proofs`). It holds:
  - the `scalus.verify` statement model;
  - the macros that capture statements (§4.3);
  - the first `blaster-uplc` tactic, `UplcBlaster`, which proves `Prop` statements about compiled
    UPLC through Lean and Blaster (§6.2).

  The theorems about prelude functions that were first written by hand in Lean are now stated in
  Scala (`PreludeProofsTest`, §8.1). See `scalus-verification/README.md`.
- **`spec.requires` / `spec.ensures` / `spec.ensuresResult`** (local branch
  `feature/verification-blaster`, not pushed). These are in-body specification clauses. They are
  `inline` no-ops, so they erase completely from the compiled script, and nothing gives them
  meaning. `Spec.expects` and `ensuring` replace them (§4.3): `Spec` is in scalus-core's
  prelude, the clauses are erased before lowering, and `Contract.inSource` reads them
  ([statement semantics §7](verification-details/prop-semantics.md#7-specifications-in-the-functions-body)).

So a statement written in Scala now reaches one backend. The other tactics, contracts, the runner
and its cache (§5.4) are not built yet.

---

## 2. Architecture at a glance

```
 Scala source     verifier.statement("abs_nonneg", Math.abs) { f => forAll[BigInt](x => f(x) >= 0) }
                  def clamp(...) = { Spec.expects(lo <= hi); (...).ensuring(r => ...) }
                  (Scala syntax for building a typed logical statement)
      │
      │  capture — at compile time: macros build the skeleton, the plugin compiles the leaves (§4.3)
      ▼
 Prop             ∀ (x : Integer). Bool(f x ≥ 0)   with f := target `Math.abs`
                  (first-order, typed binders, SIR tests, serializable)
      │
      │  tactic
      ├──► blaster-uplc  Lean workspace: #import_uplc / #prep_uplc_run + generated goal + #blaster
      ├──► lean-direct   Lean workspace: functions and statements mapped to Lean + blaster / tactics
      ├──► scalacheck    optional backend with its own generators and evaluator
      ▼
 Verdict ──► Verifier.verify ──► Proven(Proof) / Refuted(Proof) / Inconclusive
                                └──► report, proof cache, policy tests
```

| Term | Meaning |
|---|---|
| `Prop` | The runtime logical statement object. Its syntax sugar is compiled into explicit binders, terms and logical constructors. |
| `Prop.Bool` | A Boolean SIR expression interpreted as a logical leaf by a backend. |
| SIR binder | A quantified variable represented by SIR, with its `SIRType`. |
| Target | What the statement is about: a `@Compile` function, or a compiled script such as a `PlutusV3[A]`. |
| Contract | A function's precondition and postcondition, written in its body (`spec.*`) or next to it (`contract(f)`). |
| Statement | A named logical claim that can be submitted for verification. |
| Verification request | A statement, its expected outcome and the tactic selected to check it. |
| Theorem | A statement together with a proof, produced by successful verification. |
| Tactic | A backend that turns a statement into a verdict. |
| `Verifier` | The runtime context containing function representations, statement declarations, available proofs and tactics. |
| `Verifier.verify` | Prepares and runs a tactic, returning a proof, refutation, unsupported report, inconclusive result, or backend failure. |
| `Proof` | A backend-specific proof artifact and the proved lemmas it used. |

---

## 3. `Prop`: logical statements

### 3.1 Surface

```scala
package scalus.verify
import Props.*

enum Prop { ... }                   // explicit runtime logical object; see below

object Props {
    inline def forAll[A: Quantifiable](inline body: A => Prop | Boolean): Prop // also 2 and 3 binders
    def forAllSIR[A](ident: PropExpr.Ident[A], body: Prop): Prop              // runtime constructor
    inline def exists[A: Quantifiable](inline body: A => Prop | Boolean): Prop
    inline def existsLet[A: Quantifiable](inline witness: A)(inline body: A => Prop | Boolean): Prop
    def existsSIR[A](ident: PropExpr.Ident[A], witness: Option[PropExpr[A]], body: Prop): Prop
    inline def denotes[A](inline e: A): Prop
    inline def equal[A](inline a: A, inline b: A): Prop
    // call, whenReturns, callRef, whenReturnsRef: §3.8
    implicit inline def booleanToProp(inline b: Boolean): Prop // a Boolean is one test
}
object Prop {
    inline def apply(inline b: Boolean): Prop // compiled Boolean leaf
}

// members of Prop
def &&(q: Prop): Prop
def ||(q: Prop): Prop
def ==>(q: Prop): Prop
def <=>(q: Prop): Prop
def unary_! : Prop
def implies(q: Prop): Prop          // ==> with the lowest precedence
def iff(q: Prop): Prop              // <=> with the lowest precedence
```

Implemented in `scalus-verification/.../scalus/verify/Prop.scala`. There `Prop` is an `enum`
holding the statement's explicit logical form at runtime. The Scala lambdas in the declarations
above are syntax sugar; compilation turns them into binder and body nodes. The verifier does not
execute a Scala callback to establish the claim. Scala ranks an operator by
its first character, so `==>` binds like `==` and `<=>` like `<`, both tighter than `&&` and `||`:
`p && q ==> r` means `p && (q ==> r)`. The alphanumeric aliases `implies` and `iff` bind loosest.

The nested syntax for `∀z ∀x. x ≠ 0 ⇒ ∃y. x·y > z` is:

```scala
forAll[BigInt](z => forAll[BigInt](x =>
    (x !== BigInt(0)) ==> exists[BigInt](y => x * y > z)
))
```

Binder bodies are statements, so this nests as written (§3.2). Without the `x ≠ 0` guard the
claim is false at `x = 0, z = 0`, which makes it a good negative control.

`existsLet` is the explicit-witness form of an existential. Its signature can be read as:

```scala
inline def existsLet[A: Quantifiable](inline witness: A)(inline body: A => Prop | Boolean): Prop
```

- `A: Quantifiable` says that values of type `A` may be used as quantified values.
- `witness` is a term that supplies one particular value of type `A`. It may refer to
  variables bound by surrounding `forAll`/`exists` expressions.
- `body` describes what must hold for that supplied value: a statement, or a Boolean test.

Logically, `existsLet(w)(body)` means `body(w)`: it proves an existential by naming the witness
instead of asking the backend to search for one. For example:

```scala
forAll[BigInt](x =>
    (x > 0) ==> existsLet(x + 1)(y => y > x)
)
```

states that for every positive `x`, the explicitly supplied witness `x + 1` satisfies the body.
By contrast, `exists[BigInt](y => y > x)` leaves the witness to the verification backend.

### 3.2 Scala syntax and the runtime object

The Scala lambdas of `forAll`, `exists`, `existsLet` and a call's continuation are source
notation for binders, and their bodies are statements. The lambdas are not stored in `Prop`.
Capture (§4.3) makes two changes:

- each binder becomes a `PropExpr.Ident` with its `SIRType`;
- each leaf becomes a SIR term that refers to the binders it uses. A leaf is a Boolean test, the
  operand of `denotes` or `equal`, a call's argument, or an `existsLet` witness.

The quantifier stays in `Prop`; the computation in each leaf is SIR.

A binder's body is either a statement or a Boolean. A Boolean body is one test, whatever it
contains, so `forAll[Boolean, BigInt]((flag, x) => if flag then x > 0 else x < 0)` is a single
test, and so is a body with a `match` or local `val`s. A Boolean operand of a connective also
becomes one test, through an implicit conversion: in `(lo <= hi) ==> call(...)`, `lo <= hi` is a
test.

In a statement body, a binder variable used outside a leaf would make the shape of the statement
depend on its value, so it is a compile error. For example, an `if` or `match` that chooses
between statements is outside the subset of §4.1; state the cases with `==>` instead.

The explicit representation gives the verifier stable scoping, serialization and backend access.
How the macros build it is described in
[statement capture](verification-details/prop-capture.md).

**`@Compile` code has SIR at runtime.** The Scalus plugin compiles every `@Compile` object to SIR
and stores it in the object itself, in the generated `sirModule` and `sirDeps` fields
(`scalus-plugin/.../SIRPreprocessor.scala:33-34`). The linker can compile any of its definitions
to UPLC on demand. So:

- a function defined in a `@Compile` object is not opaque. It has typed SIR at runtime, and
  `blaster-uplc` can verify its compiled code directly, with no hand-written export (§3.6);
- a leaf that calls such a function keeps a reference into that SIR, which a backend resolves
  from its function table (§3.8);
- in-body contracts are to be stored with that SIR too (§3.7, §4.3).

### 3.3 Semantics

In full, with examples: [statement semantics](verification-details/prop-semantics.md).

- **`∀ (x : A)` ranges over values of `A` as a Scalus program sees them.** For `BigInt`,
  `ByteString`, `Boolean` and `Data` that means every value. For a case class it means every value
  built from its constructors: the image of `FromData`, not arbitrary `Data`. The difference
  matters for validators. A validator must be safe on *all* `Data`, so quantifying over the raw
  input is written explicitly as `forAll[Data]`.
- **A test holds iff it evaluates to `true`.** Evaluation uses the on-chain semantics of the
  compiled code. An error or non-termination is *not* true.
- **`denotes(e)` holds iff `e` evaluates without error.** It is a separate predicate, so totality
  claims are stated explicitly instead of being implied by tests.
- **Connectives are classical**, as they are in Lean and in SMT.

One consequence users must know: when `e` can fail, `!Bool(e)` is not `Bool(!e)`. For example,
`!(xs.head > 0)` holds on the empty list, while `xs.head <= 0` does not. `!` applied to a `Prop`
stays at the `Prop` level. `!` applied to a `Boolean` stays inside the test. Capture keeps
exactly the distinction the user wrote.

### 3.4 Quantifiable types

`Quantifiable[A]` marks a type that can be quantified over. It contains no generators, edge
cases or sampling configuration. The static information comes from the type itself at capture:
the `SIRType`, the Lean type, and how a Lean value is lifted to a UPLC term. A testing backend can
supply its own generator registry without changing this marker.

| Version | Types |
|---|---|
| v1 | `BigInt`, `Boolean`, `ByteString`, `Data` |
| v2 | case classes and enums deriving `ToData`/`FromData` (Lean `structure`/`inductive` plus `IsData` are generated); `List` and `Option` of quantifiable types |
| later | ledger types (`ScriptContext`, …) with the ledger validity predicate (§6.2) |

Type parameters and function-typed binders are out of scope for quantifiers. Function types appear
only as targets (§3.6).

### 3.5 Backend interpretation

The frontend captures a Scala statement as an explicit `Prop` (§4). `Prop` defines the logical structure;
it does not choose an evaluator or a testing library. Formal proof backends translate the IR
and its expressions to their own representation.

ScalaCheck may also be offered as an optional backend. That backend owns its evaluator, generators,
sample counts and seeds, using ScalaCheck directly. None of those belong in `Prop`, `Quantifiable`
or the core function table. JVM sampling provides evidence about Scala execution, and cannot
replace replay of a counterexample about compiled bytes (§5.2).

### 3.6 Targets: what a statement is about

A statement quantifies over values, but it is *about* functions. The functions a statement is
about are its **targets**. Source lambdas introduce their binders, which become explicit variables in `Prop`:

```scala
val verifier = Verifier.default
val absNonneg = verifier.statement("abs_nonneg", Math.abs) { f =>
    forAll[BigInt](x => f(x) >= 0)
}
val result = verifier.verify(absNonneg, blasterUplc(budget = 40))
```

Here `verifier` is an instance of `Verifier`, and `statement` is its method. The verifier owns the
function table and the available lemmas. Calling `statement` registers a named statement and its
target in that context; calling `verify` later runs the selected tactic. A proof can be added to
the verifier as a lemma only after its proof dependencies have been checked.

`statement(...)` declares the claim. `verify(statement, tactic)` runs verification with the
chosen tactic. Only a successful verification produces a `Theorem`, pairing that statement with
its `Proof`. The same statement can be checked by several tactics without being declared again.

A target is one of two things.

- **A `@Compile` function**, named directly (`Math.abs`, `VestingValidator.linearVesting`). Its SIR
  is available (§3.2), so no export step is needed. `blaster-uplc` compiles it standalone, as
  `FunctionDef(Math.clamp)` does, and embeds that program verbatim. `lean-direct` translates its SIR. An optional testing backend supplies
  any additional representation it needs.
  An `inline def`, such as `Math.abs`, `Math.min` and `Math.max`, has no SIR definition of its
  own. A target that names one stands for its eta-expansion (`x => Math.abs(x)`), which is
  compiled the same way. Today such a function is registered under a synthetic name, with
  `FunctionDef.named("abs", (x: BigInt) => Math.abs(x))`.
- **A compiled script**, such as a `PlutusV3[A]` value or a contract's compiled validator. This
  is what ties a claim to a **script hash**, so claims about a deployed contract name the script.

Each tactic instantiates a target with its own view of the same definition. The claim is therefore
about the target, whichever tactic proves it.

**Nested propositions.** A formula is a runtime `Prop` object built from explicit logical
constructors. In `forAll[BigInt](x => Prop(f(x) >= 0))`, the lambda is source notation; compilation
produces a quantifier node whose body contains a variable reference for `x`.

The Boolean expression is stored as a typed SIR term in the test node. The proof backend reasons
about that term (or its compiled UPLC), with no opaque Scala function involved.

An optional ScalaCheck backend may interpret the same `Prop` object by sampling, but its
generators and evaluator are backend concerns and are not part of the `Prop` model.

| Scala expression | Representation inside the statement |
|---|---|
| `x` | A variable occurrence (`SIR.Var`) referring to a binder in scope. |
| `r.amount` | A field selection (`SIR.Select`) on the expression for `r`. |
| `f(x)` | A function application (`SIR.Apply`) inside a value expression. |
| `Prop(r.amount >= 0)` | A `Prop.Bool` containing `PropExpr[Boolean]`, whose payload is the Boolean SIR expression. |
| `call(fn, x)(r => Prop(r.amount >= 0))` | A `Prop.Call` naming a function-table entry, with a result binder and a test in its continuation (§3.8). |

The outer formula handles logical connectives and quantifiers; SIR handles the computation inside
each test. A Boolean conditional such as `if flag then x > 0 else x < 0` stays inside the
test as `SIR.IfThenElse`. A conditional returning `Prop` is outside the accepted subset (§4.1).

A helper call such as `Math.abs(x)` can therefore occur inside the test's expression even when
`Math.abs` is not a named target. In that case the helper is compiled together with the test, so
the optimizer may inline or specialize it. Naming the function as a target instead preserves the
standalone compiled program at the application boundary (§6.2). Use that form when the claim is
about that particular compiled function.

### 3.7 Function contracts: pre- and postconditions

The most common statement about a function has one shape: for all arguments that satisfy a
precondition, the result satisfies a postcondition. It is first-class, and it can be written in two
equivalent forms.

**In the function's body**, with the `spec` clauses of `feature/verification-blaster`:

```scala
@Compile
object Math {   // illustrative: the prelude's clamp carries no clauses today
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}
```

**Next to the function**, as a function-property statement. This form suits functions you do not
own, or a contract that belongs to a proof suite rather than to the code. It is implemented:

```scala
val clamp = FunctionDef(Math.clamp)
val clampInRange = verifier.contract(
  "clamp_in_range",
  contract(clamp)(
    expects = (x, lo, hi) => lo <= hi,
    ensures = (x, lo, hi) => r => lo <= r && r <= hi
  )
)
// Proven, in a Lean server that `lean` gives
verifier.verify(clampInRange, UplcBlaster(Budget.LeanSteps(120), lean))
```

`Props.contract` builds the statement below, with `whenReturns`; `Props.totalContract` builds it
with `call`. Both return a `Contract`: the function, its variables, the precondition and the
guarantees, from which its statement is derived.
`verifier.contract` registers it as the function's contract (`Origin.Contract`), and
`verifier.contracts(clamp.ref)` finds it. The parameters must
be quantifiable. How the two lambdas come to speak of the same variables is in
[statement capture](verification-details/prop-capture.md#contracts).

Both forms elaborate to the same statement, with `f` bound to the target:

```
∀ args. expects(args) ⇒ ( denotes(f(args)) ⇒ ensures(args)(f(args)) )
```

- **Partial correctness is the default.** The reading is "if `f` returns, the result is good".
  For a validator handler it is exactly "if the script succeeds, `P` holds", because rejection
  *is* an error. Totality is a separate, explicit clause (`spec.total`, or `totalContract`), which
  adds `expects(args) ⇒ denotes(f(args))`.
- **The outcome can be stated by region.** `contract(…).returnsWhen(c)` says that `f` returns
  where `c` holds, and `.failsWhen(c)` that it fails there; arguments that satisfy neither are
  left open. A total contract is `.returnsWhen(_ => true)`. See
  [statement semantics](verification-details/prop-semantics.md#6-contracts).
- **In-body clauses cost nothing on-chain.** They are erased before the code reaches UPLC. On
  `feature/verification-blaster`, `VestingValidator` compiled to the same 2046 bytes and the same
  script hash with and without them. That was measured on an older master and needs re-checking.
- **Contracts compose.** When a `@Compile` function `g` calls `f`, `f`'s precondition at that call
  site is an obligation of `g`, and `f`'s proved postcondition can be assumed about the call. That
  modular use needs a verification-condition generator over `g`'s SIR, and a backend that can
  treat `f` abstractly. `lean-direct` can do that (and later `smt` and `kernel`). `blaster-uplc`
  cannot abstract a callee inside compiled code, so it checks each contract as a statement about
  the function's whole compiled program. The caller's side is implemented for it:
  `verifier.obligations(g)` declares, for each call in `g` of a function with a contract, the
  statement that the call's arguments satisfy the precondition
  ([statement semantics](verification-details/prop-semantics.md#call-site-obligations)).

### 3.8 Calls and the function table

A statement calls a function explicitly, with `call(f, arg)(r => P)` (the call returns and `P(r)`
holds: total correctness) or `whenReturns(f, arg)(r => P)` (if it returns, `P(r)` holds: partial
correctness, the contract form of §3.7). Here `f` can be a method of a `@Compile` object or a
`FunctionDef`. Use `callRef` or `whenReturnsRef` when `f` is already a `FunctionRef`.
The call node stores only a typed name, a
`FunctionRef[A, R]`, and each proof method looks the function up in the verifier's
**function table**.

- **The name is the function's identity**: for a `@Compile` definition, the fully-qualified name
  SIR uses (`…prelude.Math$.clamp`), taken from a method reference by `FunctionRef(Math.clamp)`
  or `FunctionDef(Math.clamp)`; otherwise a synthetic name without dots. The macro accepts a
  direct eta-expanded method reference and rejects arbitrary lambdas and methods outside a
  `@Compile` object. It is never a file name or a Lean identifier.
- **An entry holds one representation per proof method**, and each method takes the one it needs:
  the compiled UPLC for `blaster-uplc`, and the SIR (or a declared Lean mapping) for `lean-direct`.
  `FunctionDef(Math.clamp)` provides SIR and UPLC. A backend can add a representation under its own
  `Representation.custom` key, for example a Scala callback for a testing backend. A method that
  finds its representation missing fails with an error naming the function.
- **An entry records its calling convention.** `FunctionDef.fromCompiled` adds a `UplcSignature`:
  the representation in which the V3 lowering passes each parameter and the result, computed with
  the options the program was compiled with. `blaster-uplc` checks it against the one its tests
  call the function with.
- **A function of several parameters takes them as one tuple.** An entry records its `arity`, and
  a call passes a tuple written out as `(a, b, ...)`. Its compiled programs are curried, so
  `blaster-uplc` applies the program to each component in turn. A function of one parameter whose
  type is a tuple takes the whole tuple.

Implemented in `scalus-verification/.../scalus/verify/Function.scala`.

For example, `FunctionRef(Math.clamp)` obtains the SIR method name without compiling a UPLC
program. `call(Math.clamp, (x, lo, hi))(r => r >= lo)` captures the same reference, compiles the
argument to SIR, and binds the result `r` in the continuation, which is a statement. `callRef(ref, arg)(r => condition)` uses a
reference already in hand. The verifier still needs a matching `FunctionDef` in its function
table to analyze the named method.

---

## 4. Syntax capture: Scala expression → `Prop`

### 4.1 The accepted subset

```
prop  ::= forAll[T](x => prop) | exists[T](x => prop) | existsLet[T](term)(x => prop)
        | call(function, term)(result => prop) | whenReturns(function, term)(result => prop)
        | prop && prop | prop || prop | prop ==> prop | prop <=> prop | !prop
        | denotes(term) | equal(term, term) | { val v = term; prop } | bool
bool  ::= a Boolean expression in the @Compile subset
term  ::= an expression in the @Compile subset
T     ::= a Quantifiable type, statically known
```

The free variables of `bool` and `term` must be binders in scope, target binders, or anything
`compile` already accepts (constants, `@Compile` definitions).

The capture macros do not accept `{ val v = term; prop }` yet, where `prop` is a statement. A
binder variable used in `term` is outside every leaf, and `v` is not a binder, so leaves cannot
close over it. `existsLet(term)(v => prop)` states the same thing. When the block is a Boolean, it
is one test and is accepted.

These are rejected with a compile error:

- a quantifier inside a test, for example inside `xs.forall(...)`;
- `if` or `match` that produces a `Prop` (write `==>`; case analysis is a v2 item);
- recursion at the `Prop` level (use lemmas and induction tactics instead).

### 4.2 Algorithm

Capture works over Scala's typed tree, in the macros behind the combinators (§4.3).

1. **Recognize the combinators** (`scalus.verify.forAll`, …). Each one is a macro.
2. **Turn every binder into an SIR binder** with its `SIRType`, and keep the list of binders in
   scope, in order. Target binders come first. A call adds its result binder
   only to the call's continuation; its argument uses the enclosing scope.
3. **Capture every leaf (`bool` or `term`) as SIR** under the variables in scope. Under the scope
   `[z, x, y]`, the test `x * y > z` becomes an SIR expression with references to those three
   Prop variables. The macro compiles it as a closed lambda over the variables it uses, and at
   runtime the lambda's parameters become those references. The quantifier nodes remain in
   `Prop`; SIR represents only the computation in the leaf.
4. **Produce the `Prop` value.** The backend receives the quantifier structure and the SIR
   expression of each leaf, and supplies any evaluation or translation needed to discharge it
   (§3.5).

### 4.3 Capture: macros for statements, the plugin for contracts

All three Scala versions the build supports, 3.3.8 (the default), 3.8.4 and 3.9.0, run the relevant
phases in this order (checked with `-Xshow-phases`):

```
typer → posttyper → [ScalusPrepare] → pickler → inlining (macros) → firstTransform → [Scalus] → patternMatcher
```

A statement mixes two kinds of parts. Its logical skeleton must stay `Prop`, and its computations
must become SIR that refers to the bound variables. Two phases matter, and each sees only one
of these parts:

- macros expand in `inlining` and never see SIR;
- the `Scalus` phase, which runs after `firstTransform` (`scalus-plugin/.../Plugin.scala:169`),
  produces SIR, but by then the skeleton is ordinary code that builds `Prop` values.

So either the statement is split before the plugin runs, or the plugin learns the combinators.

**Statements: macros (implemented).** `forAll`, `call`, `Prop(...)` and the other combinators are
macros. They build the skeleton in Scala, and emit a `compile(...)` call for each leaf. The plugin
compiles those calls like hand-written ones (`Plugin.scala:273-276`), so it needs no changes. The
compiler inlines from the inside out, so each leaf expands before the binders around it. It is
compiled as a closed lambda over the binder variables it uses; see
[statement capture](verification-details/prop-capture.md).

The same route can serve the target binders of §3.6: in `f => forAll(x => f(x) >= 0)`, the leaf
closes over `f` as well, and a backend substitutes the target for it.

**Specifications in `@Compile` bodies: SIR pseudo-functions (built for `Spec`).** A macro cannot capture
a clause written inside a `@Compile` function: the clause must be erased from the function's code,
yet travel with its SIR in `sirModule`, because verifying a caller in another module or jar needs
the callee's contract (§3.7). The earlier branch made its `spec` clauses `inline` no-ops, which
the inliner erased before the plugin saw them.

The clauses are calls to a marker object, `Spec.expects(c)` and `body.ensuring(r => c)`, the way
`UniversalDataConversion` works. The plugin compiles them like any call, so they stay in the
function's SIR. `Contract.inSource` reads them from the SIR a `FunctionDef` holds, and
`EraseSpecifications` drops them at the head of the lowering pipeline, so the on-chain bytes do
not change. Reading them from `sirModule`, for a whole object, is still proposed. The same marker approach
can capture whole statements, with `forAll` and the other combinators as Boolean pseudo-functions.
It needs no plugin change and no SIR type for `Prop`: the two costs this section once attributed
to taking statements from SIR. See
[statements in SIR](verification-details/prop-capture.md#statements-in-sir-boolean-pseudo-functions-proposed).
What the clauses mean, on chain and on the JVM, is in
[statement semantics](verification-details/prop-semantics.md#7-specifications-in-the-functions-body).

**What actually has to be SIR, and why.** Not the statement as a whole.

| Part | SIR? | Why |
|---|---|---|
| Tests and witnesses | **yes** | They are executable code. `blaster-uplc` compiles them to UPLC, and `lean-direct` translates them from SIR. Only the Scalus compiler can produce either. |
| SIR binder types | `SIRType`s | They decide the Lean binder type and how a value is lifted to a UPLC constant. The plugin computes them, from a compiled identity lambda. |
| The skeleton (quantifiers, connectives) | **no** | The macros build it from explicit `Prop` constructors. |
| Contracts | **stored with the function's SIR** | A contract is part of a function's interface. Verifying a caller, possibly in another module or another jar, needs the callee's contract (§3.7). Kept as `Spec` calls in the definition's SIR, it travels in `sirModule` with the code it describes. |

So cross-module use is the reason for exactly one case, contracts. Standalone statements in a proof
suite are consumed only by the runner, and need SIR only for their tests.

### 4.4 Runtime `Prop` representation

`Prop` is the intended first-order runtime representation consumed by the verifier. Its binders
use explicit identifiers and bodies; its value expressions are represented as SIR.

The earlier prototype called this same structure `PropIR`. That separate type is unnecessary when
`Prop` is already explicit, typed and serializable. Keeping both types would duplicate every
logical constructor and require a pointless conversion between them. A backend may still lower a
`Prop` into its own private form, but that lower form is not a second public proposition model.

```scala
enum Prop:
    case Forall[A](ident: PropExpr.Ident[A], body: Prop)
    case Exists[A](ident: PropExpr.Ident[A], witness: Option[PropExpr[A]], body: Prop)
    case Call[A, R](fn: FunctionRef[A, R], arg: PropExpr[A],
                    result: PropExpr.Ident[R], total: Boolean, body: Prop)
    case And(a: Prop, b: Prop)
    case Or(a: Prop, b: Prop)
    case Implies(a: Prop, b: Prop)
    case Iff(a: Prop, b: Prop)
    case Not(a: Prop)
    case Bool(expr: PropExpr[Boolean])
    case Denotes[A](expr: PropExpr[A])
    case Equal[A](a: PropExpr[A], b: PropExpr[A])

enum PropExpr[A]:
    case Ident(name: String, id: Long, tp: SIRType)
    case SIRExpr(sir: SIR)

// `Call` uses the same typed function name as `Prop.Call` (§3.8).
// `arg` closes over the enclosing binders; `result` is in scope only in `body`.
// `total = true` means `call`; `total = false` means `whenReturns`.

enum TargetRef:
    case Function(fullName: String, sir: SIR)   // a @Compile definition, or an inline def's eta-expansion
    case Script(program: Program)                // a compiled script, e.g. `PlutusV3[A].program`

enum Origin:
    case Explicit                                // statement(...) / refute(...)
    case Contract(contract: Contract[?, ?])      // contract(f)(...); later Spec.* clauses
    case Harvested                               // from a `require` (§8.4)

final case class Statement(
    name: String,
    targets: List[TargetRef],
    prop: Prop,
    origin: Origin,
    pos: SIRPosition
)
```

`SIR` and `SIRType` already have flat codecs. `Prop` needs its own codec for the logical nodes,
including `Call`; the call's `FunctionRef` is serialized by name, while `arg` and `result` retain
the SIR types of the call. A statement's content hash, together with
its targets' script hashes, is its identity in the proof cache (§5.4).

### 4.5 Runtime representation

Compilation produces a `CapturedStatement` containing the runtime `Prop` and the `Statement`
with its `Prop` for backends. Neither representation contains sampling configuration. The
combinators capture a statement where it is written (§4.3), and the `statement`, `refute` and
`contract` methods register it. Capture does not select a tactic or run a proof.
For batch runs, a `VerificationRequest` records a statement, a tactic and the expected outcome;
the runner passes its statement and tactic to `verify`.

```scala
final case class CapturedStatement(surface: Prop, statement: Statement)
enum ExpectedOutcome:
    case Valid, Falsified
final case class VerificationRequest(
    statement: CapturedStatement, tactic: Tactic, expected: ExpectedOutcome
)
```

The implemented `Verifier.empty` owns a function table, named declarations and available theorems.
`addFunction` and `addFunctions` populate its table; `statement(name, prop)` registers a runtime
`Prop`; `prove(statement, tactic)` (also named `verify`) passes the context to a tactic and returns
a theorem, a confirmed refutation, an unsupported report, an inconclusive result, or a backend
failure (§5.3). Successful theorems become
available as lemmas. `statement(prop)` generates a local name when the caller does not supply one;
these names depend on declaration order. `addTheorem` imports a theorem from another verifier. Source capture methods
such as `statement(name, target)`, `refute` and `contract` remain planned.

The compiler can emit verifier descriptors alongside `sirModule`: functions and in-body contracts
from a `@Compile` module, with their SIR names and source positions. A planned `Verifier.default` builds a
fresh verifier from the available descriptors when the application or `sbt verify` starts. A
project can extend that context with statement declarations, explicit function representations and
proved lemmas. Discovery and assembly run at runtime; Lean and SMT do not run during Scala
compilation.

---

## 5. Theorems and proofs in Scala

### 5.1 Syntax

The current runtime API accepts an already constructed `Prop` and registered `FunctionDef`s:

```scala
val verifier = Verifier.empty
val clamp = FunctionDef(Math.clamp)
verifier.addFunction(clamp)
val claim = verifier.statement(
    "clamp_example",
    callRef(clamp.ref, (BigInt(2), BigInt(0), BigInt(10)))(r => r == BigInt(2))
)
val result: VerificationResult = verifier.prove(claim, tactic)
```

Here `tactic` is a proof backend implementing `Tactic`. The following source syntax is planned;
target capture is not implemented yet.

`UplcBlaster`, in `scalus.verify.uplcblaster`, is the first `Tactic`. It accepts universal and
existential quantifiers over `BigInt`, `Boolean`, `ByteString`, `Data` and case classes of them.
`existsLet` is eliminated by applying the statement body to its supplied witness.

- Each leaf of the body (a Boolean test, a total call, `denotes`, `equal`) is compiled to its own
  UPLC predicate over the quantified values. A partial call, `whenReturns`, is
  `denotes(f(a)) ==> call(f, a)(…)`, the form of §3.7.
- The connectives become a Lean proposition that reads each leaf by the polarity rule of §6.2.
- The tactic replays a counterexample on the Scalus CEK before it reports `Refuted` (§5.2).
- A closed statement, one without quantified variables, is decided by evaluation in Lean instead
  of by Blaster (proof kind `LeanNative`).

See [the `blaster-uplc` tactic](verification-details/uplc-blaster.md) for how it works. `forAll`
accepts one, two or three binders, and its body is any statement of the fragment:

```scala
val clamp = FunctionDef(Math.clamp)
forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
    (lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
)
```

Common direct Lean generation lives in `scalus.verify.lean.LeanExporter`. It translates `Prop`
structure and SIR expressions over `Boolean` and `BigInt`; unsupported types and operations fail
explicitly. It is shared infrastructure for `lean-direct` and for generated proposition text used
by other Lean-backed tactics.

```scala
object MathProofs {                        // plain Scala; each statement is compiled to SIR (§4.3)
    val verifier = Verifier.default

    val absTotal = verifier.statement("abs_total", Math.abs) { f => forAll[BigInt](x => denotes(f(x))) }

    val absNonneg = verifier.statement("abs_nonneg", Math.abs) { f => forAll[BigInt](x => f(x) >= 0) }

    val minMaxSum = verifier.statement("min_max_sum", Math.min, Math.max) { (mn, mx) =>
        forAll[BigInt](x => forAll[BigInt](y => mn(x, y) + mx(x, y) === x + y))
    }

    val absPositive = verifier.refute("abs_positive", Math.abs) { f => forAll[BigInt](x => f(x) > 0) } // x = 0

    // the contract of §3.7, stated about clamp's compiled code
    val clampContract = verifier.contract(Math.clamp)

    // a claim about a deployed contract names its script, which fixes the script hash
    val vestingNeedsSignature = verifier.statement("vesting_needs_signature", VestingContract.compiled) { script => ... }

    val mulUnbounded = verifier.statement("mul_unbounded") {
        forAll[BigInt](z => forAll[BigInt](x =>
            (x !== BigInt(0)) ==> existsLet(x * (Math.abs(z) + 1))(y => x * y > z)
        ))
    }
}
```

For a direct run, supply the tactic at the call site:

```scala
val result = MathProofs.verifier.verify(MathProofs.absNonneg, blasterUplc(budget = 40))
```

For `sbt verify`, a suite supplies requests to its verifier, for example:

```scala
val requests = List(
    VerificationRequest(MathProofs.absNonneg, blasterUplc(budget = 40), ExpectedOutcome.Valid),
    VerificationRequest(MathProofs.absPositive, blasterUplc(budget = 40), ExpectedOutcome.Falsified)
)
```

These are data for the runner. It calls `verify(request.statement, request.tactic)` and checks
`request.expected` against the result; constructing the list does not execute a tactic.

`existsLet` removes the ∃ in Scala. What remains is a goal with only ∀, which avoids asking Z3
to handle alternating quantifiers. Here the remaining goal is still nonlinear, and Z3's nonlinear
integer arithmetic is incomplete. The witness helps, but it does not guarantee a verdict.

### 5.2 Expected outcomes

| Declaration | Expected verdict | On the opposite verdict |
|---|---|---|
| `statement` | Valid | fail; show the counterexample, replayed (below) |
| `refute` | Falsified, with a replayed counterexample | fail: the negative control did not bite |

Every counterexample is **replayed** in the semantics of its reported target. For a compiled program
claim, the runner evaluates the complete composed test and target UPLC on the Scalus CEK without
a step limit; evaluating only the target would miss differences between JVM and on-chain test
semantics. For a `Jvm` claim, replay belongs to the testing backend. A `Source` claim needs a matching
source-semantics replay or remains inconclusive. A counterexample that does not replay is reported
as spurious. §6.2 explains why budgeted backends can produce spurious ones.

### 5.3 Results

```scala
enum Verdict:
    case Valid
    case Falsified(counterexample: Map[String, Data])
    case Undetermined
    case Timeout
    case Unsupported(reason: String)

// The public result of Verifier.verify; Verdict is the lower-level backend result.
enum VerificationResult:
    case Proven(proof: Proof)
    case Refuted(proof: Proof)
    case Unsupported(report: CompatibilityReport)
    case Inconclusive(reason: String)
    case Failed(reason: String)

trait Tactic:
    type Prepared
    def prepare(goal: Goal): Either[CompatibilityReport, Prepared]
    def run(prepared: Prepared): ExecutionResult

trait ProofArtifact:
    def kind: ProofKind

final class Verifier {
    def addFunction(definition: FunctionDef[?, ?]): Unit
    def addFunctions(table: FunctionTable): Unit
    def statement(name: String, prop: Prop): Statement
    def statement(prop: Prop): Statement                  // generated local name
    def addTheorem(theorem: Theorem): Unit
    def prepare(statement: Statement, tactic: Tactic): Either[CompatibilityReport, PreparedRun]
    def prove(prepared: PreparedRun): ExecutionResult
    def prove(statement: Statement, tactic: Tactic): VerificationResult
    def verify(statement: Statement, tactic: Tactic): VerificationResult // alias
}

object Verifier {
    def empty: Verifier
}

enum ProofKind:
    case Blaster                    // goal closed by `axiom blasterProven`, not by a kernel proof
    case LeanKernel
    case LeanNative                 // decided by `native_decide`: trusts the Lean compiler

final class Theorem private[verify] (val statement: Statement, val proof: Proof)

final class Proof private[verify] (
    val artifact: ProofArtifact,
    val usedLemmas: List[Theorem]
)
```

Only the verifier constructs and registers a `Theorem`, from proof evidence returned by a tactic;
passing samples alone do not qualify. `VerificationResult.Proven` returns the `Proof`, while the
registered `Theorem` is available as a lemma for later goals. An artifact carries the backend's proof material and any target
identity needed to check what was verified. Tactics are responsible for validating artifacts and
deciding whether an artifact-backed lemma applies to a goal. The verifier checks that the statement
is registered and that used lemmas are available. Each tactic resolves the function references it
supports from the goal's function table. The proof retains its artifact and lemma dependencies. A spurious
model, timeout, or unsupported goal is `Inconclusive`. The result has explicit proven, refuted,
and inconclusive cases.

`VerificationResult.Refuted` carries a `Proof` of the negated claim. Its artifact may contain a
replayed, concrete falsifying execution or backend proof material for the negation. A concrete
input can refute a universal claim, but it cannot by itself refute an existential claim; that
requires proof of the negation. A passing `scalacheck` run likewise provides evidence, not a
`Proven` result.

A proof's trust level can then be checked by an ordinary test:

```scala
test("vesting authorisation is proved about the shipped script") {
    // cachedResults is the runner's map of statement names to VerificationResult.
    val proof = cachedResults("vesting_needs_signature") match
        case VerificationResult.Proven(value) => value
        case other => fail(s"expected a proof, got $other")
    assert(proof.artifact.kind == ProofKind.Blaster)
}
```

### 5.4 When proofs run

- **Not in scalac.** Lean runs take seconds to minutes, and Z3 is not fully deterministic. At
  compile time we only capture: statements become values.
- **`sbt verify`** discovers verifier instances and their batch requests the way test frameworks
  discover suites. It calls each verifier's `verify` method and stores results in a
  content-addressed cache. The key includes the statement IR, every referenced function
  representation and target script hash, the identities and trust metadata of used lemmas, the
  tactic and its configuration, and the backend pins (Lean toolchain, Blaster and PlutusCore
  revisions).
- **At test time**, `VerificationResult`s are read from the cache, and policy tests like the one
  above run.
- **In CI**, keep the split that `scalus-verification` already uses. At PR time, a cheap check that
  the statements and targets still match the cache. Nightly, the full proof run.

### 5.5 Lemmas and composition

- `verifier.verify(statement, leanDirect.using(lemmaA, lemmaB))` adds proved `Theorem`s as hypotheses. The resulting proof records the lemmas it used, and its `kinds` include their proof kinds. The tactic checks that each lemma artifact applies to the goal's target and semantics.
- Structural steps can run on the Scala side before a backend is called: `split` breaks ∧ into
  subgoals, `cases(x)` splits over constructors, `intro` introduces a binder. Each subgoal can use
  its own tactic, as in `verifier.verify(statement, split(blasterUplc(40), leanDirect))`.
- This is the seed of a Scala-side proof kernel. A full LCF-style kernel is deliberately not part
  of this design (§6.7).

---

## 6. Tactics

### 6.1 Interface

```scala
enum VerificationResult:
    case Proven(proof: Proof)
    case Refuted(proof: Proof)
    case Unsupported(report: CompatibilityReport)
    case Inconclusive(reason: String)
    case Failed(reason: String)

trait Tactic {
    type Prepared
    def name: String
    def prepare(goal: Goal): Either[CompatibilityReport, Prepared]
    def run(prepared: Prepared): ExecutionResult
}

final case class Goal(statement: Statement, functions: FunctionTable, lemmas: List[Theorem])
```

`PreparedRun` is opaque evidence tied to the verifier, statement, tactic, function-table snapshot
and available lemmas that were checked. `prove(prepared)` returns `ExecutionResult`, a subtype of
`VerificationResult` that excludes `Unsupported`. The one-step API maps a failed preparation to
`VerificationResult.Unsupported`; a backend process that fails after preparation is `Failed`, not
an incompatibility.

### 6.2 `blaster-uplc`: proofs about the compiled bytes

**Lowering.**

1. Compile every SIR term to UPLC, and every `@Compile` function target standalone. A compiled
   script target is taken as it is. Each program is identified by its **content hash**, which
   the proof artifact records. A function's name is its identity in the function table, never a
   file name. Today compilation uses
   `Options.releaseUntagged.copy(valueBuiltins = false)`, as `scalus-verification` does (see
   Limits). A script target compiled with other options cannot be checked until the Lean model
   gains those builtins.
2. Apply each test program to the target terms with a plain `Term.Apply`, and run **no
   optimization across that boundary**. The target's bytes then appear unchanged inside the
   program that Lean evaluates.
3. Lift bound variables to UPLC constants in `#prep_uplc_run`'s inputs function, chosen by binder
   type: `Integer`, `ByteString`, `Bool` and `Data` constants, and the constructor encoding for
   case classes.
4. Map quantifiers to Lean binders, writing `∃ x, p x` as the classically equivalent
   `¬ ∀ x, ¬ p x`.

**The polarity rule.** `#prep_uplc` evaluates with a step budget. An exhausted budget ends in the
same `State.Error` as a genuine failure. With ∃ and → in the language, how a test is read under
the budget must depend on its position.

| Position of the test | Reading at budget *b* |
|---|---|
| positive: a conclusion, under ∃ | strong, "halts within *b* steps with `true`" |
| negative: a premise, under ¬ | weak, "does not halt within *b* steps with a non-`true` result or an error" |

`denotes` is read the same way, with success in place of `true`. `<=>` is split into two
implications before translation, because its tests occur in both positions.

The weak reading needs a failure told apart from an exhausted budget, and PlutusCore's
`runSteps` returns `State.Error` for both. The tactic therefore runs each test with
`ScalusProofs.Run.runFor` (`#prep_uplc_run`), which makes the same steps but ends an exhausted run
in the state it reached. `State.Error` then means that the program failed within *b* steps. That
makes failure provable: `!denotes(e)`, and `!Bool(e)` for an `e` that fails, hold when the program
fails within the budget. With `runSteps` alone the weak reading of `denotes` would be `True`, and
a premise `denotes(f(x))`, which is the partial-correctness reading of §3.7, would carry no
information.

*Why this is sound.* Let T be the unbudgeted truth of a test, S_b the strong reading and W_b the
weak one. Then S_b ⊆ T ⊆ W_b. Replacing positive occurrences with something stronger and negative
occurrences with something weaker gives a formula that implies the original one. **A Valid verdict
at any budget therefore implies the statement with no budget.**

The reverse does not hold. A formula falsified under the budget may be true without it, because
running out of steps turns a strong reading false. That is why every counterexample is replayed
without a step limit (§5.2).

*Consequences.*

- A program whose step count does not depend on its input, such as `abs`, proves over the whole
  domain.
- A program whose step count grows with its input, such as `gcd`, does not prove over an unbounded
  domain. The user must write the restriction into the statement, for example
  `(Math.abs(x) < 65536) ==> …`. It then shows up in the report instead of hiding in a budget
  number.

**Budgets.** The user states the budget for each verification request. Proof cost grows steeply
with the budget ([measurements](verification-details/uplc-blaster.md#budgets)), so budgets have to
be measured, not guessed. The runner can measure a budget by running the composed program on the
Scalus CEK over generated samples. That first needs a calibration: Scalus meters CPU and memory,
while PlutusCoreBlaster counts machine steps.

**Limits.**

- Quantified variables are `BigInt`, `Boolean`, `ByteString`, `Data` or case classes of them for
  now; values inside tests, such as call arguments and results, can have any type. An enum or a
  list cannot be quantified over yet; quantify over `Data` with `FromData`.
- Programs must be compiled without the CIP-153 `Value` and CIP-138 array builtins, which the
  Lean model lacks. A ledger `Value` represented as `Data` is unaffected.
- Programs that read single bytes or reach the CIP-121/122 bitwise builtins cannot be proved over
  symbolic inputs.
- PlutusCore is pinned to a fork until PR #40 merges.
- Ordinary `exists`, emitted as `¬∀¬`, still requires existential SMT reasoning after
  normalization, so goals with alternating quantifiers can come back Undetermined. Prefer
  `existsLet`.

Details and upstream references are in
[the tactic's limits](verification-details/uplc-blaster.md#limits).

The artifact identifies the exact compiled bytes. **Proof kind:** `Blaster`.

### 6.3 `lean-direct`: mapping to functions and statements defined in Lean

Instead of evaluating compiled code, `lean-direct` maps each Scalus function a statement uses to a
**Lean function**, and the statement to a **Lean proposition** over those functions. The proof is
then ordinary Lean: `blaster`, or a Lean tactic script, with Lean's libraries available.

**Where the Lean function comes from.** There are two sources.

- **Generated** from the function's SIR, as a shallow translation over native Lean types:
  - `BigInt` → PlutusCore `Integer`, `Boolean` → `Bool`, `ByteString` → PlutusCore `ByteString`;
  - `List` and `Option` → their Lean counterparts;
  - case classes and enums → generated `structure` / `inductive`.

  Recursive definitions need a termination argument in Lean. How to produce one is an open
  question.
- **Declared.** The user maps a Scalus function to a Lean term of the matching type: a definition
  from Lean core or Mathlib, or a hand-written Lean specification.

  ```scala
  leanMapping(Math.gcd, "fun a b => (Int.gcd a b : Int)")
  ```

  Statements about `Math.gcd` can then use everything Lean already knows about `Int.gcd`
  (`Int.gcd_comm`, `Int.gcd_dvd_left`, …). A statement can be mapped the same way, to an existing
  Lean theorem whose proof then closes it.

**A declared mapping is a claim, and it can be proved.** `leanMapping(f, g)` asserts
`∀ args. f(args) = g(args)`, including that `f` returns at all. Whether that holds for every
input, for example at zero or for negative arguments, is exactly what proving the mapping settles.
It is never trusted silently:

- until it is proved, a statement that relies on it remains an open goal rather than a theorem;
- it is a statement like any other, so it can be proved. `blaster-uplc` is the natural tactic,
  because its generated Lean theorem can mention `g` directly: "the compiled `Math.gcd` applied to
  `a b` halts within the budget with `(Int.gcd a b : Int)`".

**Proving mappings with `blaster-uplc` combines the two tactics.** `blaster-uplc` ties the Lean
function to the compiled bytes, once. Lean then reasons about the Lean function, with induction
and its libraries. A theorem proved this way is about the compiled bytes, on the
domain the mapping was proved for. When a function's step count grows with its input, as `gcd`'s
does, that domain needs an explicit restriction (§6.2), and every theorem that goes through the
mapping inherits it.

**Partiality.** A generated function returns `Option` or `Except`, and SIR `Error` becomes `none`.
A test becomes `t = some true`, and `denotes` becomes `isSome`. A mapping to a total Lean
function also claims that the Scalus function is total on the mapped domain. There is no fuel, so
the polarity rule is not needed.

**Equality.** `===` maps to Lean `=`. This is sound because Scalus accepts only `Eq.derived` and
`Eq.structural` (`SIRCompiler.scala:1352`). `Eq.structural(f)` is itself an unchecked user claim.
Each use needs a separate proof obligation before it can support a theorem:
`forAll[A](a => forAll[A](b => f(a, b) <=> equal(a, b)))`.

**Discharge.** By `blaster`, or by a Lean tactic script given in the verification request, such as
`leanDirect(tactic = "induction xs <;> blaster")`. That script is the escape for the lack of
automatic induction in SMT.

**Trade-off.** It gets induction, quantifier-rich statements, no fuel, Lean's libraries, and
faster SMT on native integers. The price is trust in the generated translation, or in declared
mappings until they are proved.

The artifact identifies the compiled bytes when every function the statement uses goes through a
mapping proved by `blaster-uplc`; otherwise it identifies the SIR translation. Unproved mappings
leave the program claim open.
**Proof kind:** `Blaster`, or `LeanKernel` when the script does not use `blaster`.

### 6.4 `lean-manual`

The statement is generated by either lowering, and the proof is written by hand in a Lean file that
is never regenerated. The runner checks that the file compiles, and it classifies the result with
`#print axioms`: `LeanKernel` when neither `blasterProven` nor `sorry` appears.

### 6.5 `scalacheck`

This is an optional backend, implemented separately from the verifier core. It uses ScalaCheck
directly and owns generators, sampling settings, shrinking and its evaluator. Its dependencies
and configuration do not appear in `Prop` or `Quantifiable`. Any Scala function callbacks it needs
are attached under a backend-defined representation key.

The backend may check Scala semantics or evaluate compiled programs on the Scalus CEK; its artifact
identifies which target it checked. A passing sample run is evidence, not a proof. Existentials without a
verified witness remain undetermined when sampling cannot settle them.

Passing samples are **evidence, never proof**; the runtime verifier returns `Inconclusive` for
that outcome.

### 6.6 Explicit premises

A conditional claim states its premise in `Prop`, for example with `Implies`. A proved helper is
passed as a `Theorem` and recorded in `usedLemmas`. The current `Proof` has no free-form list of
assumptions: a string cannot establish what proposition was assumed or whether it was discharged.
Unproved premises leave the goal open.

### 6.7 Later

- **`smt`**: send the `lean-direct` fragment straight to Z3 through anthill, without Lean. It is
  faster to iterate with, but trusts a second translator.
- **`kernel`**: an LCF-style Scala kernel in which `Theorem` can be built only by inference rules and
  tactics are untrusted. It is justified only by a real backlog of lemmas that SMT cannot do,
  typically induction over lists and maps. Build it when that backlog exists, not before.

### 6.8 Comparison

| Tactic | Artifact target | What a Valid verdict rests on | Handles ∃ | Induction | Needs fuel |
|---|---|---|---|---|---|
| `blaster-uplc` | compiled bytes | Lean CEK model, Blaster axiom, Z3, polarity rule | poorly (use witnesses) | no | yes |
| `lean-direct` | source (SIR), or compiled bytes via proved mappings | Blaster axiom or Lean kernel, Z3; the generated translation, or the declared mappings (proved by `blaster-uplc`, or assumed) | yes | via Lean tactics | no |
| `lean-manual` | either | Lean kernel (plus Blaster if used) | yes | yes | depends |
| `scalacheck` | JVM or bytes | nothing: evidence only | witnesses only | no | no |

---

## 7. Trust model

- **A Valid verdict is never shown without its proof artifact and used lemmas.** "Proved" alone
  says nothing about whether the claim is about the shipped bytes, or whether Z3 produced it.
- **What is trusted in the Lean backends:**
  - the PlutusCoreBlaster CEK model, which is checked against the Plutus conformance suite
    (1134 cases);
  - Blaster's translation, which closes goals with `axiom blasterProven`;
  - Z3;
  - for a closed statement decided by `native_decide`, the Lean compiler, through the axiom
    `Lean.ofReduceBool`, instead of Blaster and Z3;
  - our own lowering: the polarity rule, the value lifting, the verbatim target embedding, and
    the `runFor` runner. Proving that a program fails takes the model's `State.Error` as a
    Plutus evaluation failure, so the model must not fail where Plutus returns a value. The
    conformance suite's success cases check exactly that; its failure cases run on `runSteps`,
    and cannot tell a failure from an exhausted budget;
  - and, for `lean-direct`, the generated translation, plus any declared mapping that has not
    been proved.
- **Negative controls are required.** A suite that uses a Lean tactic must contain at least one
  `refute` for each combination of target, tactic and budget, and the runner fails a suite that
  does not. A broken template or a vacuous encoding would otherwise report Valid for everything,
  indistinguishable from success. `PreludeProofsTest` follows this rule per function; the runner
  does not enforce it yet.
- **Counterexamples are replayed** (§5.2). A solver model alone is not a finding.

---

## 8. Relation to other work

### 8.1 The existing `blaster-uplc` suite

The `scalus-verification` module is the intended home for everything in this document. Its first
`blaster-uplc` suite was written by hand in Lean, over UPLC exported by a target catalogue. It is
now stated in Scala, in `PreludeProofsTest`, as `Prop` statements over `FunctionDef` targets. Each
function has samples, and each proved property has a negative control. The catalogue, the exporter
and the hand-written Lean files are gone. What the suite covers, and why the rest is not stated,
is described in
[the tactic's details](verification-details/uplc-blaster.md#what-preludeproofstest-covers).

### 8.2 `spec` clauses on `feature/verification-blaster`

That branch holds the `spec` clauses of §3.7 and an annotated `VestingValidator`, and nothing
else. §3.7 gives the clauses their meaning, and §4.3 how the plugin captures them. The branch is
about 308 commits behind master, and `VestingValidator` has been rewritten since, so the
annotations have to be redone against the current validator. The branch has no remote copy.

### 8.3 Obligations nobody has to write

Every `require(cond, msg)` in a validator compiles to `if cond then () else error`. These can be
harvested into statements automatically, for example "the script does not succeed unless `cond`".
Every existing contract then gets checkable obligations with no new syntax. The obligation model
above is designed so that harvested, `spec`-derived and hand-written statements all land in the
same place.

---

## 9. Plan

| Phase | Deliverables | Exit criterion |
|---|---|---|
| **0** | `scalus.verify` surface (`Prop`, `forAll`, connectives, `Quantifiable` for `BigInt`/`Boolean`/`ByteString`); verifier-owned `statement` / `refute` methods; generated verifier descriptors; statement capture by macros (§4.3); `Prop` with flat serialization; `@Compile` function and script targets; `blaster-uplc` for ∀-prefix statements with the polarity rule; `sbt verify` generating a Lean workspace and parsing verdicts | the `Math.lean` theorems and controls, restated in Scala over `Math.*` targets, give the same verdicts; generated files replace the hand-written ones |
| **1** | `exists` / `existsLet`, `denotes`, `equal`; `refute` with replay; `scalacheck`; external contracts (`verifier.contract(f)`); `Theorem`, report, cache; PR-time freshness check and nightly run | a suite without a negative control fails; a planted spurious counterexample is reported as spurious; `Math.clamp`'s contract proved about its compiled code |
| **2** | in-body `spec` capture in the plugin; `lean-direct` for integers, `Bool`, `ByteString`, `List`, `Option` and case classes; declared Lean mappings, proved by `blaster-uplc`; `using`, `split`, `cases`; contract VCG for modular proofs | adding clauses leaves the script hash unchanged; a list lemma proved with `induction xs <;> blaster`; a Mathlib `Int.gcd` theorem transferred to the compiled `Math.gcd` through a proved mapping; a caller's obligation discharged with its callee's contract |
| **3** | validators: `ScriptContext` targets with the CardanoLedgerApiBlaster validity predicates; bring `feature/verification-blaster`'s Vesting annotations forward | one real contract (Vesting) with authorisation and conservation properties proved about its script hash |
| **4** | a Scala kernel only if a lemma backlog demands it | representative lemmas elaborate and discharge through `Prop` |

---

## 10. Open questions

1. **Specification capture.** `Spec` clauses are read from SIR and erased by the lowering
   pipeline (§4.3). Still proposed: `Logic` pseudo-functions for whole statements. Open: how the
   runtime reaches an object's `sirDeps` to link a function, and when the reifier replaces the
   macros for statements in test code.
2. **Contract semantics.** Partial correctness by default, with totality opt-in (§3.7). Is
   `spec.total` the right spelling, and should validators default differently from helpers?
3. **`Prop` versus `Boolean` connectives.** `a && b` on two Booleans stays one test. Should
   `lean-direct` split it when both sides are total?
4. **Calling convention across representations.** For `blaster-uplc` this is settled: tests and
   programs both use each type's default representation, recorded as a `UplcSignature` and
   checked (see `verification-details/uplc-blaster.md`). Open: `lean-direct`'s mapping proofs
   and claims about a program's raw boundary need a Lean encoding per representation.
5. **Budget calibration** between Scalus CEK metering and PlutusCoreBlaster step counts.
6. **Commit policy.** Should the proof cache and the generated Lean input be committed, so Lean
   builds without a JVM?
7. **The default domain for validator inputs:** well-formed values or raw `Data` (§3.3).
8. **Where mappings are declared.** `leanMapping(f, "…")` in the verification module, or an
   annotation on the function itself, which would tie core code to Lean names?
9. **Naming:** `scalus.verify` and `statement` / `refute`. The precondition is `expects`, not
   `requires`, so that it does not read as the prelude's runtime `require`.
10. **Default verifier discovery.** How should compiler-generated descriptors from multiple jars
    be indexed and assembled without loading every `@Compile` object eagerly?
