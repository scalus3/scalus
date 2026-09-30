# Verification in Scalus — overview

Status: **draft design**. Date: 2026-09-25.

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

Three pieces of work exist, and none of them connects to the others yet.

- **`scalus-verification`** (formerly `scalus-lean-proofs`; branch `worktree-lean-proofs`). It
  compiles prelude functions to UPLC, exports them, and proves 17 theorems about the exported
  bytes with Blaster. It is the first instance of what this document calls the `blaster-uplc`
  tactic. Its statements, step budgets, anti-vacuity guards and negative controls are all
  **written by hand in Lean**. See
  `scalus-verification/README.md` and
  `docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md`.
- **`spec.requires` / `spec.ensures` / `spec.ensuresResult`** (local branch
  `feature/verification-blaster`, not pushed). These are in-body specification clauses. They are
  `inline` no-ops, so they erase completely from the compiled script. Nothing gives them meaning
  yet.
What is missing is the layer in the middle: one way to *state* a property in Scala that every
backend can consume.

---

## 2. Architecture at a glance

```
 Scala source     verifier.statement("abs_nonneg", Math.abs) { f => forAll[BigInt](x => f(x) >= 0) }
                  def clamp(...) = { spec.requires(lo <= hi); spec.ensuresResult(r => ...); ... }
                  (Scala syntax for building a typed logical statement)
      │
      │  reify — at compile time, from the SIR the Scalus plugin produces (§4.3)
      ▼
 Prop             ∀ (x : Integer). Bool(f x ≥ 0)   with f := target `Math.abs`
                  (first-order, typed binders, SIR tests, serializable)
      │
      │  tactic
      ├──► blaster-uplc  Lean workspace: #import_uplc / #prep_uplc + generated theorem + blaster
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
| `Verifier.verify` | Runs a tactic and returns a proof, a confirmed refutation, or an inconclusive result. |
| `Proof` | A backend-specific proof artifact and the proved lemmas it used. |

---

## 3. `Prop`: logical statements

### 3.1 Surface

```scala
package scalus.verify
import Props.*

enum Prop { ... }                   // explicit runtime logical object; see below

object Props {
    inline def forAll[A: Quantifiable](inline body: A => Boolean): Prop // implemented syntax
    def forAllSIR[A](ident: PropExpr.Ident[A], body: Prop): Prop       // runtime constructor
    inline def exists[A: Quantifiable](inline body: A => Boolean): Prop
    inline def existsLet[A: Quantifiable](inline witness: A)(inline body: A => Boolean): Prop
    def existsSIR[A](ident: PropExpr.Ident[A], witness: Option[PropExpr[A]], body: Prop): Prop
    inline def denotes[A](inline e: A): Prop
    inline def equal[A](inline a: A, inline b: A): Prop
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

The intended nested syntax for `∀z ∀x. x ≠ 0 ⇒ ∃y. x·y > z` is:

```scala
forAll[BigInt](z => forAll[BigInt](x =>
    (x !== BigInt(0)) ==> exists[BigInt](y => x * y > z)
))
```

The current Boolean-only macros cannot yet compile these nested `Prop` bodies. Without the
`x ≠ 0` guard the claim is false at `x = 0, z = 0`, which makes it a good negative control.

`existsLet` is the explicit-witness form of an existential. Its signature can be read as:

```scala
inline def existsLet[A: Quantifiable](inline witness: A)(inline body: A => Boolean): Prop
```

- `A: Quantifiable` says that values of type `A` may be used as quantified values.
- `witness: => A` is a term that supplies one particular value of type `A`. It may refer to
  variables bound by surrounding `forAll`/`exists` expressions.
- `body: A => Boolean` describes what must hold for that supplied value.

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

The Scala lambdas in `forAll(x => x > 0)` and `exists(x => x > 0)` are source notation. The
macros compile each Boolean lambda to SIR, extract its SIR parameter and body, and construct a
`Prop.Forall` or `Prop.Exists` with a `PropExpr.Ident` and a `Prop.Bool` body. `existsLet` also
compiles its witness to SIR. The quantifier stays in `Prop`; its Boolean computation is SIR.
`call` and `whenReturns` likewise compile the argument and Boolean continuation to SIR. General
`Prop` bodies and nested quantifiers remain design work.

The explicit representation gives the verifier stable scoping, serialization and backend access.

**`@Compile` code is the exception.** The Scalus plugin compiles every `@Compile` object to SIR and
stores it in the object itself, in the generated `sirModule` and `sirDeps` fields
(`scalus-plugin/.../SIRPreprocessor.scala:33-34`). The linker can compile any of its definitions
to UPLC on demand. So:

- a function defined in a `@Compile` object is not opaque. It has typed SIR at runtime, and
  `blaster-uplc` can verify its compiled code directly, with no hand-written export (§3.6);
- a statement compiled by the plugin is not opaque either. That covers contracts in `@Compile`
  objects, and statements passed to `statement` / `contract`, which hand them to `compileInline(...)`.
  Its Boolean and value expressions are SIR terms carrying the types of variables in scope. The
  quantifier structure remains in `Prop`; this is the capture route this design recommends (§4.3).

The Scalus frontend has the typed SIR it needs, so it does not need to recover these lambdas from
Scala's typed tree with a separate macro.

### 3.3 Semantics

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
stays at the `Prop` level. `!` applied to a `Boolean` stays inside the test. The reifier keeps
exactly the distinction the user wrote.

### 3.4 Quantifiable types

`Quantifiable[A]` marks a type that can be quantified over. It contains no generators, edge
cases or sampling configuration. The static information comes from the type itself at reification:
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
  is available (§3.2), so no export step is needed. `blaster-uplc` compiles it standalone, exactly as
  `ProofTargets` does by hand today with `PlutusV3.compile((x: BigInt) => Math.abs(x))`, and embeds
  that program verbatim. `lean-direct` translates its SIR. An optional testing backend supplies
  any additional representation it needs.
  An `inline def`, such as `Math.abs`, `Math.min` and `Math.max`, has no SIR definition of its
  own. A target that names one stands for its eta-expansion (`x => Math.abs(x)`), which is
  compiled the same way.
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
each test. A Boolean conditional such as `Prop(if flag then x > 0 else x < 0)` stays inside the
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
        spec.requires(lo <= hi)
        spec.ensuresResult[BigInt](r => lo <= r && r <= hi)
        if x < lo then lo else if x > hi then hi else x
    }
}
```

**Next to the function**, as a function-property statement. This form suits functions you do not
own, or a contract that belongs to a proof suite rather than to the code:

```scala
contract(Math.clamp)(
  requires = (x, lo, hi) => lo <= hi,
  ensures = (x, lo, hi) => r => lo <= r && r <= hi
)
```

Both forms elaborate to the same statement, with `f` bound to the target:

```
∀ args. requires(args) ⇒ ( denotes(f(args)) ⇒ ensures(args)(f(args)) )
```

- **Partial correctness is the default.** The reading is "if `f` returns, the result is good".
  For a validator handler it is exactly "if the script succeeds, `P` holds", because rejection
  *is* an error. Totality is a separate, explicit clause (`spec.total`, or `total = true`), which
  adds `requires(args) ⇒ denotes(f(args))`.
- **In-body clauses cost nothing on-chain.** They are erased before the code reaches UPLC. On
  `feature/verification-blaster`, `VestingValidator` compiled to the same 2046 bytes and the same
  script hash with and without them. That was measured on an older master and needs re-checking.
- **Contracts compose.** When a `@Compile` function `g` calls `f`, `f`'s precondition at that call
  site is an obligation of `g`, and `f`'s proved postcondition can be assumed about the call. That
  modular use needs a verification-condition generator over `g`'s SIR, and a backend that can
  treat `f` abstractly. `lean-direct` can do that (and later `smt` and `kernel`). `blaster-uplc`
  cannot abstract a callee inside compiled code, so it checks each contract as a statement about
  the function's whole compiled program.

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

Implemented in `scalus-verification/.../scalus/verify/Function.scala`.

For example, `FunctionRef(Math.clamp)` obtains the SIR method name without compiling a UPLC
program. `call(Math.clamp, (x, lo, hi))(r => r >= lo)` captures the same reference and compiles
the argument and Boolean result condition to SIR. `callRef(ref, arg)(r => condition)` uses a
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

These are rejected with a compile error:

- a quantifier inside a test, for example inside `xs.forall(...)`;
- `if` or `match` that produces a `Prop` (write `==>`; case analysis is a v2 item);
- recursion at the `Prop` level (use lemmas and induction tactics instead).

### 4.2 Algorithm

The reifier works over a typed tree. In the recommended route that tree is the statement's SIR; in
the alternative it is Scala's typed tree, inside a macro (§4.3). The steps are the same.

1. **Recognize the combinators** (`scalus.verify.forAll`, …) and the `spec` clauses.
2. **Turn every binder into an SIR binder** with its `SIRType`, and keep the list of binders in
   scope, in order. Target binders come first. A call adds its result binder
   only to the call's continuation; its argument uses the enclosing scope.
3. **Capture every leaf (`bool` or `term`) as SIR** under the variables in scope. Under the scope
   `[z, x, y]`, the test `x * y > z` becomes an SIR expression with references to those three
   Prop variables. The quantifier nodes remain in `Prop`; SIR represents only the computation in
   the leaf.
   - In the SIR route the leaf is already a SIR subterm.
   - In the macro route the plugin compiles the leaf expression and stores the resulting SIR.

   The backend receives the Prop quantifier structure and the SIR expression for each leaf.
4. **Produce the `Prop` value.** The core captures the formula and its SIR expressions. A
   backend supplies any evaluation or translation needed to discharge it (§3.5).

### 4.3 Two routes to a typed tree

All three Scala versions the build supports, 3.3.8 (the default), 3.8.4 and 3.9.0, run the relevant
phases in this order (checked with `-Xshow-phases`):

```
typer → posttyper → [ScalusPrepare] → pickler → inlining (macros) → firstTransform → [Scalus] → patternMatcher
```

**Route 1 (recommended): statements are compiled to SIR, and reified from it.** A statement
reaches the plugin in one of two ways, and neither needs a macro:

- **Contracts** sit in `@Compile` objects, next to the code they describe.
- **`Verifier.statement`, `Verifier.refute` and `Verifier.contract`** are `inline` methods that pass
  their statement lambda to `compileInline(...)`, which emits a plugin-intercepted `compile` call
  after inlining. The verifier instance itself stays plain Scala,
  so its tactic configuration never goes through the plugin.

Either way the plugin compiles the statement to SIR like any other code:

- the quantifier lambdas are `LamAbs` nodes with their `SIRType`s, and the tests are already SIR
  subterms;
- targets and helper functions resolve through the normal linker;
- in-body `spec` clauses are handled by the same plugin, in the same pass as the function they
  describe.

The verification runner takes the statement's SIR from the `compile` result, or from the object's
`sirModule` for in-body contracts, and reifies it. Code and statements share one representation.

The work lies in the plugin:

- **Combinators as intrinsics.** `forAll`, `==>`, `denotes` and the other combinators must be
  compiled as recognizable intrinsics, and lowering one to UPLC must be an error.
- **`Prop` in SIR.** `Prop` needs a SIR type.
- **`spec` clauses must reach the `Scalus` phase.** Today they are `inline` no-ops that `inlining`
  erases first. Two ways to change that:
  - `ScalusPrepare`, which runs before `inlining`, rewrites each clause into a marker call that
    survives it;
  - or the clauses stop being `inline`, and the plugin moves their arguments into the definition's
    `AnnotationsDecl.data` and deletes the call.

  Either way the clause is erased from the code's SIR, so the on-chain bytes do not change.

**Route 2: a macro over Scala's typed tree.** For a frontend that does not run the Scalus plugin:

- **Emitted compile calls are handled by the plugin.** Macros expand in `inlining`. The main
  `Scalus` phase runs after `firstTransform` (`scalus-plugin/.../Plugin.scala:169`), and it
  compiles `compile` and `compileWithOptions` calls itself (`Plugin.scala:273-276`). So the
  compile calls the macro emits become SIR with no plugin changes. A spike must confirm that they
  get their options and SIR exactly like hand-written calls.
**Recommendation.** Take Route 1 for Scalus: function contracts need the plugin anyway, and SIR
already carries everything reification needs.

**What actually has to be SIR, and why.** Not the statement as a whole.

| Part | SIR? | Why |
|---|---|---|
| Tests and witnesses | **yes** | They are executable code. `blaster-uplc` compiles them to UPLC, and `lean-direct` translates them from SIR. Only the Scalus compiler can produce either. |
| SIR binder types | `SIRType`s | They decide the Lean binder type and how a value is lifted to a UPLC constant. The plugin already computes them. |
| The skeleton (quantifiers, connectives) | **no** | It is represented by explicit `Prop` constructors. Capturing the whole statement through `compileInline(...)` is a way to obtain the SIR terms inside those constructors. |
| Contracts | **stored with the function's SIR** | A contract is part of a function's interface. Verifying a caller, possibly in another module or another jar, needs the callee's contract (§3.7). Kept in the definition's `AnnotationsDecl.data`, it travels in `sirModule` with the code it describes. |

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
    case Contract(inBody: Boolean)               // spec.* clauses, or contract(f)(...)
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
with its `Prop` for backends. Neither representation contains sampling configuration. The `statement`, `refute` and `contract`
methods capture it with `compileInline` (§4.3). Capture does not select a tactic or run a proof.
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
a theorem, a confirmed refutation, or an inconclusive result (§5.3). Successful theorems become
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
target capture and nested `Prop` bodies are not implemented yet.

The initial `UplcBlaster` implementation owns the backend's compile options, existing target
catalogue and `exportUplc` operation under `scalus.verify.uplcblaster`. It implements `Tactic` for
a universal prefix over `BigInt` and `Boolean` followed by a quantifier-free body. Each leaf of the
body (a Boolean test, a total call, `denotes`, `equal`) is compiled to its own UPLC predicate over
the quantified values. The connectives become a Lean proposition that reads each leaf by the
polarity rule of §6.2. The tactic runs Lean, and replays a counterexample on the Scalus CEK before
it reports `Refuted` (§5.2). `forAll` accepts one, two or three binders with a Boolean body, as in
`forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x)`. A body with `Prop` connectives still has
to be assembled from `Prop` nodes.

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
    case Inconclusive(reason: String)

trait ProofArtifact:
    def kind: ProofKind

final class Verifier {
    def addFunction(definition: FunctionDef[?, ?]): Unit
    def addFunctions(table: FunctionTable): Unit
    def statement(name: String, prop: Prop): Statement
    def statement(prop: Prop): Statement                  // generated local name
    def addTheorem(theorem: Theorem): Unit
    def prove(statement: Statement, tactic: Tactic): VerificationResult
    def verify(statement: Statement, tactic: Tactic): VerificationResult // alias
}

object Verifier {
    def empty: Verifier
}

enum ProofKind:
    case Blaster                    // goal closed by `axiom blasterProven`, not by a kernel proof
    case LeanKernel

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
  compile time we only reify: statements become values.
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
    case Inconclusive(reason: String)

trait Tactic {
    def name: String
    def discharge(goal: Goal): VerificationResult // run at runtime, never by scalac
}

final case class Goal(statement: Statement, functions: FunctionTable, lemmas: List[Theorem])
```

### 6.2 `blaster-uplc`: proofs about the compiled bytes

**Lowering.**

1. Compile every SIR term to UPLC, and every `@Compile` function target standalone. A compiled
   script target is taken as it is. Each program is exported under its **content hash**, and its
   Lean identifier is derived from the function's qualified SIR name. A function's name is its
   identity in the function table, never a file name. Today compilation uses
   `Options.releaseUntagged.copy(valueBuiltins = false)`, as `scalus-verification` does (see
   Limits). A script target compiled with other options cannot be checked until the Lean model
   gains those builtins.
2. Apply each test program to the target terms with a plain `Term.Apply`, and run **no
   optimization across that boundary**. The target's bytes then appear unchanged inside the
   program that Lean evaluates.
3. Lift bound variables to UPLC constants in `#prep_uplc`'s inputs function, chosen by binder
   type: `Integer`, `ByteString`, `Bool` and `Data` constants, and the constructor encoding for
   case classes.
4. Map quantifiers to Lean binders.

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
- The anti-vacuity guards of `scalus-verification` are no longer what soundness rests on. They stay
  useful as an early warning that a budget has become too small.

Compare with today's hand-written shape, `fromFrameToInt (p.prop x) = some r → r ≥ 0`. There the
premise is read strongly in a negative position, so the theorem says nothing about inputs that
need more steps than the budget.

**Budgets.** The user states the budget per verification request. Proof cost grows steeply with the budget. On a
`gcd` theorem whose worst input needs 203 steps, budget 250 proved in 2 s, 350 took 52 s, and 500
gave no result within 500 s. So budgets have to be measured, not guessed. The runner can measure
by running the composed program on the Scalus CEK over generated samples. That first needs a
calibration: Scalus meters CPU and memory, while PlutusCoreBlaster counts machine steps.

**Limits, as of 2026-09-23.**

- PlutusCoreBlaster `main` cannot decode the CIP-153 `Value` builtins or the CIP-138 array
  builtins; both are commented out of its flat decoder. The open PRs are
  input-output-hk/PlutusCoreBlaster#15 (Value; conflicting, unreviewed since 2026-05) and #12
  (arrays; mergeable). Until they land, compile with `valueBuiltins = false`. A ledger `Value`
  represented as `Data` is unaffected.
- Programs reaching the CIP-121/122 bitwise builtins cannot be proved over symbolic inputs,
  because Blaster cannot translate `BitVec`, whose width is a value index.
- PlutusCore is pinned to a fork until PR #40 merges. Without it, Blaster does not terminate on
  UPLC `case`.
- Blaster passes ∃ to Z3 as an SMT quantifier, so goals with alternating quantifiers can come back
  Undetermined. Prefer `existsLet`.

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
  indistinguishable from success. `scalus-verification` already follows this rule by hand.
- **Counterexamples are replayed** (§5.2). A solver model alone is not a finding.

---

## 8. Relation to other work

### 8.1 The existing `blaster-uplc` suite

The `scalus-verification` module is the intended home for everything in this document. Today it
holds the first `blaster-uplc` instance, with hand-written statements. Migrating that suite is Phase 0
of the plan:

- the 17 theorems and their negative controls are restated in Scala, and must produce the same
  verdicts;
- each `ProofTarget` becomes a `@Compile` function target (`Math.abs`, …), so the hand-written
  `PlutusV3.compile(...)` wrappers go away;
- its `samples` feed `scalacheck`;
- `Generated/` and the hand-written `.lean` files are produced by the runner.

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
| **0** | `scalus.verify` surface (`Prop`, `forAll`, connectives, `Quantifiable` for `BigInt`/`Boolean`/`ByteString`); verifier-owned `statement` / `refute` methods using `compileInline`; generated verifier descriptors; the SIR term capture; `Prop` with flat serialization; `@Compile` function and script targets; `blaster-uplc` for ∀-prefix statements with the polarity rule; `sbt verify` generating a Lean workspace and parsing verdicts | the `Math.lean` theorems and controls, restated in Scala over `Math.*` targets, give the same verdicts; generated files replace the hand-written ones |
| **1** | `exists` / `existsLet`, `denotes`, `equal`; `refute` with replay; `scalacheck`; external contracts (`verifier.contract(f)`); `Theorem`, report, cache; PR-time freshness check and nightly run | a suite without a negative control fails; a planted spurious counterexample is reported as spurious; `Math.clamp`'s contract proved about its compiled code |
| **2** | in-body `spec` capture in the plugin; `lean-direct` for integers, `Bool`, `ByteString`, `List`, `Option` and case classes; declared Lean mappings, proved by `blaster-uplc`; `using`, `split`, `cases`; contract VCG for modular proofs | adding clauses leaves the script hash unchanged; a list lemma proved with `induction xs <;> blaster`; a Mathlib `Int.gcd` theorem transferred to the compiled `Math.gcd` through a proved mapping; a caller's obligation discharged with its callee's contract |
| **3** | validators: `ScriptContext` targets with the CardanoLedgerApiBlaster validity predicates; bring `feature/verification-blaster`'s Vesting annotations forward | one real contract (Vesting) with authorisation and conservation properties proved about its script hash |
| **4** | a Scala kernel only if a lemma backlog demands it | representative lemmas elaborate and discharge through `Prop` |

---

## 10. Open questions

1. **Reification route.** This design recommends SIR (Route 1) for Scalus, and a macro only for
   ctproof. A spike must confirm two things: that the combinators compile to recognizable SIR
   intrinsics, and that `Verifier.statement` hands the statement lambda to `compileInline`
   intact.
2. **`spec` capture.** A marker call inserted by `ScalusPrepare` before inlining, or non-`inline`
   clauses that the plugin intercepts and moves into `AnnotationsDecl.data`?
3. **Contract semantics.** Partial correctness by default, with totality opt-in (§3.7). Is
   `spec.total` the right spelling, and should validators default differently from helpers?
4. **`Prop` versus `Boolean` connectives.** `a && b` on two Booleans stays one test. Should
   `lean-direct` split it when both sides are total?
5. **Calling convention for target binders** with non-primitive argument types. The test's call
   `f(x)` must use the same representation, `Data` or `UplcConstr`, as the target's parameter.
6. **Budget calibration** between Scalus CEK metering and PlutusCoreBlaster step counts.
7. **Commit policy.** Should the cache and the generated Lean workspace be committed, as
   `Generated/` is today, so Lean builds without a JVM?
8. **The default domain for validator inputs:** well-formed values or raw `Data` (§3.3).
9. **Where mappings are declared.** `leanMapping(f, "…")` in the verification module, or an
    annotation on the function itself, which would tie core code to Lean names?
10. **Naming:** `scalus.verify`, `statement` / `refute`, and `spec.requires` next to the prelude's
    `require`.
11. **Default verifier discovery.** How should compiler-generated descriptors from multiple jars
    be indexed and assembled without loading every `@Compile` object eagerly?
