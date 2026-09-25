# Verification in Scalus — overview

Status: **draft design**. Date: 2026-09-25.

This document describes how logical statements about Scalus code are written in Scala and how they
are proved. It fixes four layers and the contracts between them:

1. **`Prop`**, a notion of logical statement in Scalus: typed quantifiers, connectives and atoms.
2. **Reification**, a compile-time mapping from a subset of Scala expressions to `Prop`, using a
   higher-order abstract syntax (HOAS) embedding.
3. **Theorems**, Scala values that pair a statement with the tactic that must prove it.
4. **Tactics**, the pluggable backends that discharge a statement: `lean-uplc`, `lean-direct`,
   `scalacheck`, and later others.

The statement side lives in Scala, type-checked by scalac against the real definitions. The proof
side stays in the backends: Lean 4, Blaster and Z3 today.

---

## 1. Where we are

Three pieces of work exist, and none of them connects to the others yet.

- **`scalus-lean-proofs`** (branch `worktree-lean-proofs`). It compiles prelude functions to UPLC,
  exports them, and proves 17 theorems about the exported bytes with Blaster. It is the first
  instance of what this document calls the `lean-uplc` tactic. Its statements, step budgets,
  anti-vacuity guards and negative controls are all **written by hand in Lean**. See
  `scalus-lean-proofs/README.md` and
  `docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md`.
- **`spec.requires` / `spec.ensures` / `spec.ensuresResult`** (local branch
  `feature/verification-blaster`, not pushed). These are in-body specification clauses. They are
  `inline` no-ops, so they erase completely from the compiled script. Nothing gives them meaning
  yet.
- **ctproof**, a compile-time proof system: `Checked[A, P]` refinements, a type-level predicate
  AST, and given resolution plus anthill as the prover. Its prototype handles `Int` literals only,
  and its design has no quantifiers.

What is missing is the layer in the middle: one way to *state* a property in Scala that every
backend can consume.

---

## 2. Architecture at a glance

```
 Scala source     theorem("abs_nonneg", abs) { f => forAll[BigInt](x => f(x) >= 0) }
                  (HOAS surface: ordinary Scala, type-checked by scalac)
      │
      │  reify — macro at compile time; atoms compiled to SIR by the Scalus plugin
      ▼
 PropIR           ∀ (x : Integer). Atom(λ f x. f x ≥ 0)   with f := target `abs`
                  (first-order, typed binders, SIR atoms, serializable)
      │
      │  tactic
      ├──► lean-uplc    Lean workspace: #import_uplc / #prep_uplc + generated theorem + blaster
      ├──► lean-direct  Lean workspace: shallow translation to native Lean + blaster / Lean tactics
      ├──► scalacheck   runtime interpretation of the same HOAS term
      ▼
 Verdict ──► Thm (subject, oracles, assumptions) ──► report, proof cache, policy tests
```

| Term | Meaning |
|---|---|
| `Prop` | A logical statement. At the surface it is built from HOAS combinators; internally it is `PropIR`. |
| Atom | A quantifier-free Boolean expression in the `@Compile` subset. It is the only executable part of a `Prop`. |
| Binder | A quantified variable with a static type. |
| Target | A compiled program the statement is about, such as a `PlutusV3[A]`. |
| Theorem | A named statement, plus its expected outcome and the tactic that should discharge it. |
| Tactic | A backend that turns a statement into a verdict. |
| `Thm` | The result of a successful discharge. It carries its subject, its oracles and its assumptions. |

---

## 3. `Prop`: logical statements

### 3.1 Surface

```scala
package scalus.verify

sealed trait Prop

def forAll[A: Quantifiable](body: A => Prop): Prop
def exists[A: Quantifiable](body: A => Prop): Prop
def existsWith[A: Quantifiable](witness: A)(body: A => Prop): Prop
def denotes[A](e: A): Prop          // e evaluates without error
def equal[A](a: A, b: A): Prop      // logical (structural) equality

given Conversion[Boolean, Prop]     // a Boolean expression is an atom

extension (b: Boolean)
    def ==>(q: Prop): Prop          // so `cond ==> p` works without an explicit lift

extension (p: Prop)
    def &&(q: Prop): Prop
    def ||(q: Prop): Prop
    def ==>(q: Prop): Prop
    def <=>(q: Prop): Prop
    def unary_! : Prop
```

The example `∀x ∃y. x·y > z` is written:

```scala
forAll[BigInt](z => forAll[BigInt](x =>
    (x !== BigInt(0)) ==> exists[BigInt](y => x * y > z)
))
```

Without the `x ≠ 0` guard it is false at `x = 0, z = 0`, which makes it a good negative control.

### 3.2 Why HOAS

In a HOAS embedding, the binders of the object language (the quantifiers of `Prop`) are
represented by binders of the host language (Scala lambdas). This gives us:

- **Scoping and capture avoidance for free.** No variable names, no substitution code, no
  capture bugs. These have been a recurring source of defects in the SIR type machinery.
- **Typing for free.** The bound variable is an ordinary Scala value of type `A`, so the body
  `x * y > z` is plain Scala. scalac checks it, the IDE completes it, and renames reach it.
- **A runtime interpretation for free.** `forAll` can store its function and be run on generated
  values (§3.5).

The price is that a Scala function is opaque at runtime: you cannot look inside `A => Prop`. The
first-order structure therefore has to be recovered from the typed syntax tree **at compile time**.
That is the reification step of §4.

### 3.3 Semantics

- **`∀ (x : A)` ranges over values of `A` as a Scalus program sees them.** For `BigInt`,
  `ByteString`, `Boolean` and `Data` that means every value. For a case class it means every value
  built from its constructors: the image of `FromData`, not arbitrary `Data`. The difference
  matters for validators. A validator must be safe on *all* `Data`, so quantifying over the raw
  input is written explicitly as `forAll[Data]`.
- **An atom holds iff it evaluates to `true`.** Evaluation uses the on-chain semantics of the
  compiled code. An error or non-termination is *not* true.
- **`denotes(e)` holds iff `e` evaluates without error.** It is a separate predicate, so totality
  claims are stated explicitly instead of being implied by atoms.
- **Connectives are classical**, as they are in Lean and in SMT.

One consequence users must know: when `e` can fail, `!Atom(e)` is not `Atom(!e)`. For example,
`!(xs.head > 0)` holds on the empty list, while `xs.head <= 0` does not. `!` applied to a `Prop`
stays at the `Prop` level. `!` applied to a `Boolean` stays inside the atom. The reifier keeps
exactly the distinction the user wrote.

### 3.4 Quantifiable types

`Quantifiable[A]` marks a type that can be quantified over. It provides a ScalaCheck generator for
the runtime interpretation. The static information comes from the type itself at reification: the
`SIRType`, the Lean type, and how a Lean value is lifted to a UPLC term.

| Version | Types |
|---|---|
| v1 | `BigInt`, `Boolean`, `ByteString`, `Data` |
| v2 | case classes and enums deriving `ToData`/`FromData` (Lean `structure`/`inductive` plus `IsData` are generated); `List` and `Option` of quantifiable types |
| later | ledger types (`ScriptContext`, …) with the ledger validity predicate (§6.2) |

Type parameters and function-typed binders are out of scope for quantifiers. Function types appear
only as targets (§3.6).

### 3.5 Two interpretations of one term

A theorem expression is interpreted twice.

1. **Reified**, at compile time. It becomes `PropIR` (§4), which the proving tactics consume.
2. **Run**, on the JVM. The HOAS value is evaluated: `forAll` samples its generator,
   `existsWith` evaluates its witness, and `exists` without a witness is untestable. This powers
   the `scalacheck` tactic, and it is what replays counterexamples from the solvers (§5.2).

Both interpretations come from the same source expression, so they cannot drift apart.

### 3.6 Targets: what a statement is about

A statement quantifies over values, but it is *about* functions. An atom can mention a function in
two ways.

- **Directly.** In `Math.abs(x) >= 0`, `Math.abs` is compiled together with the atom. The claim is
  about the function as compiled in that context, and the optimizer may specialize it. This is
  fine for lemmas about the prelude.
- **Through a target binder.**

  ```scala
  val abs: PlutusV3[BigInt => BigInt] = PlutusV3.compile((x: BigInt) => Math.abs(x))
  theorem("abs_nonneg", abs) { f => forAll[BigInt](x => f(x) >= 0) }
  ```

  `f` is bound by HOAS like any other variable. Each tactic instantiates it with its own view of
  the same compiled program. `lean-uplc` uses the program's UPLC term, verbatim. `lean-direct`
  uses the translation of its SIR. `scalacheck` uses the JVM function, or the program run on the
  Scalus CEK.

**Rule: claims about contracts use targets.** Only a target ties a proof to a script hash.

---

## 4. Reification: Scala expression → `PropIR`

### 4.1 The accepted subset

```
prop  ::= forAll[T](x => prop) | exists[T](x => prop) | existsWith[T](term)(x => prop)
        | prop && prop | prop || prop | prop ==> prop | prop <=> prop | !prop
        | denotes(term) | equal(term, term) | { val v = term; prop } | bool
bool  ::= a Boolean expression in the @Compile subset
term  ::= an expression in the @Compile subset
T     ::= a Quantifiable type, statically known
```

The free variables of `bool` and `term` must be binders in scope, target binders, or anything
`compile` already accepts (constants, `@Compile` definitions).

These are rejected with a compile error:

- a quantifier inside an atom, for example inside `xs.forall(...)`;
- `if` or `match` that produces a `Prop` (write `==>`; case analysis is a v2 item);
- recursion at the `Prop` level (use lemmas and induction tactics instead).

### 4.2 Algorithm

The reifier is a macro over the typed tree.

1. **Recognize the combinators** by symbol (`scalus.verify.forAll`, …).
2. **Turn every binder into a `Binder(name, SIRType)`** with a fresh unique name, and keep the
   list of binders in scope, in order. Target binders come first.
3. **Lambda-lift every leaf (`bool` or `term`)** over the binders in scope and wrap it in a
   compile call. The atom `x * y > z` under the scope `[z, x, y]` becomes:

   ```scala
   compileWithOptions(opts, (z: BigInt) => (x: BigInt) => (y: BigInt) => x * y > z)
   ```

   Each leaf becomes a closed SIR lambda. Both Lean tactics can use that directly: `lean-uplc`
   applies it to lifted constants, and `lean-direct` instantiates its body with Lean variables.
4. **Emit one expression** that builds both the `PropIR` value (with the compiled atoms) and the
   original HOAS value (for §3.5).

### 4.3 Why a macro, and why it works with the existing plugin

Scala 3.3.8 runs its phases in this order:

```
typer → posttyper → [ScalusPrepare] → pickler → inlining (macros) → firstTransform → [Scalus] → patternMatcher
```

- Macros expand in `inlining`. The main `Scalus` phase runs after `firstTransform`
  (`scalus-plugin/.../Plugin.scala:169`), and it compiles `compile` and `compileWithOptions` calls
  itself (`Plugin.scala:273-276`). So the compile calls the reifier emits are turned into SIR by
  the existing plugin. **The first version needs no plugin changes.**
- A macro uses `quotes.reflect`, not the compiler's internal trees. That is what makes a frontend
  shared with ctproof possible (§8.3).
- `ScalusPrepare` runs *before* `inlining`, so it still sees `spec.*` calls that inlining later
  erases. That is where capturing `spec` clauses would happen (§8.2).

A spike must confirm step 3 end to end: that macro-generated compile calls get their options and
SIR exactly like hand-written ones.

### 4.4 `PropIR`

```scala
enum PropIR:
    case Forall(v: Binder, body: PropIR)
    case Exists(v: Binder, witness: Option[TermIR], body: PropIR)
    case And(a: PropIR, b: PropIR)
    case Or(a: PropIR, b: PropIR)
    case Implies(a: PropIR, b: PropIR)
    case Iff(a: PropIR, b: PropIR)
    case Not(a: PropIR)
    case Atom(t: TermIR)                 // Boolean-valued
    case Denotes(t: TermIR)
    case Equal(a: TermIR, b: TermIR)

final case class Binder(name: String, tp: SIRType)
final case class TermIR(params: List[Binder], body: SIR)   // closed lambda over `params`
final case class Statement(
    name: String,
    targets: List[TargetRef],
    prop: PropIR,
    pos: SIRPosition
)
```

`SIR` and `SIRType` already have flat codecs, so `PropIR` serializes without new encoding work. A
statement's content hash, together with its targets' script hashes, is its identity in the proof
cache (§5.4).

---

## 5. Theorems and proofs in Scala

### 5.1 Syntax

```scala
object MathProofs extends ProofSuite {
    val abs = PlutusV3.compile((x: BigInt) => Math.abs(x))
    val min = PlutusV3.compile((x: BigInt) => (y: BigInt) => Math.min(x, y))
    val max = PlutusV3.compile((x: BigInt) => (y: BigInt) => Math.max(x, y))

    theorem("abs_total", abs) { f => forAll[BigInt](x => denotes(f(x))) }
        .by(leanUplc(budget = 40))

    theorem("abs_nonneg", abs) { f => forAll[BigInt](x => f(x) >= 0) }
        .by(leanUplc(budget = 40))

    theorem("min_max_sum", min, max) { (mn, mx) =>
        forAll[BigInt](x => forAll[BigInt](y => mn(x)(y) + mx(x)(y) === x + y))
    }.by(leanUplc(budget = 40))

    refute("abs_positive", abs) { f => forAll[BigInt](x => f(x) > 0) }   // x = 0
        .by(leanUplc(budget = 40))

    theorem("mul_unbounded") {
        forAll[BigInt](z => forAll[BigInt](x =>
            (x !== BigInt(0)) ==> existsWith(x * (Math.abs(z) + 1))(y => x * y > z)
        ))
    }.by(leanDirect)
}
```

`existsWith` removes the ∃ in Scala. What remains is a goal with only ∀, which avoids asking Z3
to handle alternating quantifiers. Here the remaining goal is still nonlinear, and Z3's nonlinear
integer arithmetic is incomplete. The witness helps, but it does not guarantee a verdict.

### 5.2 Expected outcomes

| Declaration | Expected verdict | On the opposite verdict |
|---|---|---|
| `theorem` | Valid | fail; show the counterexample, replayed (below) |
| `refute` | Falsified, with a replayed counterexample | fail: the negative control did not bite |

Every counterexample is **replayed**: decoded into Scala values, then evaluated by the runtime
interpretation of the statement and by running the targets on the Scalus CEK without a step
limit. A counterexample that does not replay is reported as spurious. §6.2 explains why budgeted
backends can produce spurious ones.

### 5.3 Results

```scala
enum Verdict:
    case Valid
    case Falsified(counterexample: Map[String, Data], replayed: Boolean)
    case Undetermined
    case Timeout
    case Unsupported(reason: String)

enum Subject:
    case Script(hash: ScriptHash)   // the exact bytes (lean-uplc)
    case Source(sirHash: String)    // SIR semantics; trusts the compiler for the bytes (lean-direct)
    case Jvm                        // Scala semantics (scalacheck)

enum Oracle:
    case Blaster                    // goal closed by `axiom blasterProven`, not by a kernel proof
    case LeanKernel
    case Tested(samples: Int)       // evidence, not proof
    case Assumed(reason: String)
    case Admitted(reason: String)

final class Thm private[verify] (
    val statement: Statement,
    val subject: Subject,
    val oracles: Set[Oracle],
    val assumptions: List[String]    // domain restrictions, Eq.structural claims, assume(...)
)
```

Only the verification runner can construct a `Thm`, and only from a `Valid` verdict. Its trust
level can then be checked by an ordinary test:

```scala
test("vesting authorisation is proved about the shipped script") {
    val thm = VestingProofs.beneficiarySigns.result.get
    assert(thm.subject == Subject.Script(VestingContract.scriptHash))
    assert(!thm.oracles.exists(_.isInstanceOf[Oracle.Tested]))
}
```

### 5.4 When proofs run

- **Not in scalac.** Lean runs take seconds to minutes, and Z3 is not fully deterministic. At
  compile time we only reify: statements become values.
- **`sbt verify`** discovers `ProofSuite`s the way test frameworks discover suites. It runs each
  tactic and stores verdicts in a content-addressed cache. The key is the hash of the statement's
  IR, the targets' script hashes, the tactic and its configuration, and the backend pins (Lean
  toolchain, Blaster and PlutusCore revisions).
- **At test time**, `Thm`s are read from the cache, and policy tests like the one above run.
- **In CI**, keep the split that `scalus-lean-proofs` already uses. At PR time, a cheap check that
  the statements and targets still match the cache. Nightly, the full proof run.

### 5.5 Lemmas and composition

- `.by(leanDirect.using(lemmaA, lemmaB))` adds proved `Thm`s as hypotheses. The result's oracles
  and assumptions are the union over everything used. A lemma whose subject is `Source` cannot
  support a claim whose subject is `Script`. The result is downgraded to `Source`, or the
  combination is rejected.
- Structural steps can run on the Scala side before a backend is called: `split` breaks ∧ into
  subgoals, `cases(x)` splits over constructors, `intro` introduces a binder. Each subgoal can use
  its own tactic, as in `.by(split(leanUplc(40), leanDirect))`.
- This is the seed of a Scala-side proof kernel. A full LCF-style kernel is deliberately not part
  of this design (§6.7).

---

## 6. Tactics

### 6.1 Interface

```scala
trait Tactic {
    def name: String
    def discharge(goal: Goal): Verdict      // run by `sbt verify`, never by scalac
}

final case class Goal(statement: Statement, hypotheses: List[Thm], config: Map[String, String])
```

### 6.2 `lean-uplc`: proofs about the compiled bytes

**Lowering.**

1. Compile every `TermIR` to UPLC. Today this uses
   `Options.releaseUntagged.copy(valueBuiltins = false)`, as `scalus-lean-proofs` does (see
   Limits).
2. Apply each atom program to the target terms with a plain `Term.Apply`, and run **no
   optimization across that boundary**. The target's bytes then appear unchanged inside the
   program that Lean evaluates.
3. Lift bound variables to UPLC constants in `#prep_uplc`'s inputs function, chosen by binder
   type: `Integer`, `ByteString`, `Bool` and `Data` constants, and the constructor encoding for
   case classes.
4. Map quantifiers to Lean binders.

**The polarity rule.** `#prep_uplc` evaluates with a step budget. An exhausted budget ends in the
same `State.Error` as a genuine failure. With ∃ and → in the language, how an atom is read under
the budget must depend on its position.

| Position of the atom | Reading at budget *b* |
|---|---|
| positive: a conclusion, under ∃ | strong, "halts within *b* steps with `true`" |
| negative: a premise, under ¬ | weak, "does not halt within *b* steps with a non-`true` result or an error" |

`denotes` is read the same way, with success in place of `true`. `<=>` is split into two
implications before translation, because its atoms occur in both positions.

*Why this is sound.* Let T be the unbudgeted truth of an atom, S_b the strong reading and W_b the
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
- The anti-vacuity guards of `scalus-lean-proofs` are no longer what soundness rests on. They stay
  useful as an early warning that a budget has become too small.

Compare with today's hand-written shape, `fromFrameToInt (p.prop x) = some r → r ≥ 0`. There the
premise is read strongly in a negative position, so the theorem says nothing about inputs that
need more steps than the budget.

**Budgets.** The user states the budget per theorem. Proof cost grows steeply with the budget. On a
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
  Undetermined. Prefer `existsWith`.

**Subject:** `Script(hash)`. **Oracles:** `Blaster`.

### 6.3 `lean-direct`: proofs about the source semantics

**Translation.** `PropIR` and its SIR atoms are translated to Lean terms over native Lean types:

- `BigInt` → PlutusCore `Integer`, `Boolean` → `Bool`, `ByteString` → PlutusCore `ByteString`;
- `List` and `Option` → their Lean counterparts;
- case classes and enums → generated `structure` / `inductive`.

Scalus definitions reached from atoms are handled in one of two ways. They are mapped through a
**prelude bridge**, a table from known prelude functions to Lean definitions whose correspondence
is trusted. Or they are translated from their SIR into Lean `def`s. For recursive definitions,
termination is the open question.

**Partiality.** SIR `Error` translates to an `Option` or `Except` result. An atom becomes
`t = some true`, and `denotes` becomes `isSome`. There is no fuel, so the polarity rule is not
needed.

**Equality.** `===` maps to Lean `=`. This is sound because Scalus accepts only `Eq.derived` and
`Eq.structural` (`SIRCompiler.scala:1352`). `Eq.structural(f)` is itself an unchecked user claim.
Each use is recorded as an assumption on the `Thm`, and it is a natural obligation of its own:
`forAll[A](a => forAll[A](b => f(a, b) <=> equal(a, b)))`.

**Discharge.** By `blaster`, or by a Lean tactic script given in the theorem, such as
`leanDirect(tactic = "induction xs <;> blaster")`. That script is the escape for the lack of
automatic induction in SMT.

**Trade-off.** It gets induction, quantifier-rich statements, no fuel, and faster SMT on native
integers. The price is a trusted translator, and claims about the source rather than the bytes.

**Subject:** `Source`. **Oracles:** `Blaster`, or `LeanKernel` when the script does not use
`blaster`.

### 6.4 `lean-manual`

The statement is generated by either lowering, and the proof is written by hand in a Lean file that
is never regenerated. The runner checks that the file compiles, and it classifies the result with
`#print axioms`: `LeanKernel` when neither `blasterProven` nor `sorry` appears.

### 6.5 `scalacheck`

This is the runtime interpretation of §3.5. Atoms run on the JVM, or, with `scalacheck.onUplc`,
as the compiled atom programs on the Scalus CEK. The first gives evidence about the Scala
semantics, the second about the bytes. `exists` without a witness yields `Undetermined`. It is
cheap, so it is useful as a pre-check before Lean, and as a fallback when Lean is Undetermined.

**Oracles:** `Tested(n)`. This is **evidence, never proof**, and the report says so.

### 6.6 `assume` and `admit`

`assume(reason)` states something the project takes as given, such as a trusted external fact.
`admit(reason)` marks work in progress. Both produce a `Thm` whose oracles make the gap visible
everywhere it is used.

### 6.7 Later

- **`smt`**: send the `lean-direct` fragment straight to Z3 through anthill, without Lean. It is
  faster to iterate with, but trusts a second translator.
- **`kernel`**: an LCF-style Scala kernel in which `Thm` can be built only by inference rules and
  tactics are untrusted. It is justified only by a real backlog of lemmas that SMT cannot do,
  typically induction over lists and maps. Build it when that backlog exists, not before.

### 6.8 Comparison

| Tactic | Subject | What a Valid verdict rests on | Handles ∃ | Induction | Needs fuel |
|---|---|---|---|---|---|
| `lean-uplc` | script bytes | Lean CEK model, Blaster axiom, Z3, polarity rule | poorly (use witnesses) | no | yes |
| `lean-direct` | source (SIR) | the above minus the CEK, plus translator and prelude bridge; the compiler, for the bytes | yes | via Lean tactics | no |
| `lean-manual` | either | Lean kernel (plus Blaster if used) | yes | yes | depends |
| `scalacheck` | JVM or bytes | nothing: evidence only | witnesses only | no | no |

---

## 7. Trust model

- **A Valid verdict is never shown without its subject, oracles and assumptions.** "Proved" alone
  says nothing about whether the claim is about the shipped bytes, or whether Z3 produced it.
- **What is trusted in the Lean backends:**
  - the PlutusCoreBlaster CEK model, which is checked against the Plutus conformance suite
    (1134 cases);
  - Blaster's translation, which closes goals with `axiom blasterProven`;
  - Z3;
  - our own lowering: the polarity rule, the value lifting, the verbatim target embedding;
  - and, for `lean-direct`, the translator and the prelude bridge.
- **Negative controls are required.** A suite that uses a Lean tactic must contain at least one
  `refute` for each combination of target, tactic and budget, and the runner fails a suite that
  does not. A broken template or a vacuous encoding would otherwise report Valid for everything,
  indistinguishable from success. `scalus-lean-proofs` already follows this rule by hand.
- **Counterexamples are replayed** (§5.2). A solver model alone is not a finding.

---

## 8. Relation to other work

### 8.1 `scalus-lean-proofs`

This module is the first `lean-uplc` instance, with hand-written statements. Migrating it is Phase
0 of the plan:

- the 17 theorems and their negative controls are restated in Scala, and must produce the same
  verdicts;
- `ProofTarget` becomes a target;
- its `samples` feed `scalacheck`;
- `Generated/` and the hand-written `.lean` files are produced by the runner.

### 8.2 `spec` clauses

A method's `spec` clauses become one generated statement whose target is the method itself:

```scala
def linearVesting(cfg: Config, t: BigInt): BigInt = {
    spec.requires(cfg.duration > 0)
    spec.ensuresResult[BigInt](r => r >= 0 && r <= cfg.initialAmount)
    ...
}
// ≡ theorem("linearVesting.ensures", linearVesting) { f =>
//       forAll[Config](cfg => forAll[BigInt](t =>
//           (cfg.duration > 0) ==> { val r = f(cfg, t); r >= 0 && r <= cfg.initialAmount }))
//   }
```

- For a validator handler, `ensures(P)` reads as `succeeds(ctx) ==> P(ctx)`.
- On that branch, the clauses were measured to erase completely: `VestingValidator` compiled to the
  same 2046 bytes and the same script hash with and without them. That measurement was taken on an
  older master and needs re-checking after a rebase.
- Capturing the clauses needs a hook that sees them before inlining erases them. That is
  `ScalusPrepare` (§4.3), or a macro annotation on the method.

### 8.3 ctproof: a common frontend

What can be shared:

1. the surface combinators and the reifier;
2. `PropIR`, made generic in the leaf representation, so that one structure serves both
   (`PropIR[Leaf]`, with `Leaf = TermIR` over SIR for Scalus, and ctproof's `Expr` AST for
   ctproof);
3. the semantics of atoms and `denotes`.

The reifier takes an **atom elaborator** as a parameter. Scalus's emits the compile calls of §4.2.
ctproof's translates the atom's tree into its type-level `Expr` at macro-expansion time.

A refinement `Checked[A, P]` is a `Prop` with one free variable, which in HOAS is `A => Prop`.
ctproof's quantifier-free type-level AST is the quantifier-free fragment of `PropIR`.

The shared piece must not depend on Scalus, so it lives in its own small module. On surface syntax:
in-body `spec.*` clauses are the canonical form, and ctproof's `@pre` / `@post` can be sugar over
the same structure.

### 8.4 Obligations nobody has to write

Every `require(cond, msg)` in a validator compiles to `if cond then () else error`. These can be
harvested into statements automatically, for example "the script does not succeed unless `cond`".
Every existing contract then gets checkable obligations with no new syntax. The obligation model
above is designed so that harvested, `spec`-derived and hand-written statements all land in the
same place.

---

## 9. Plan

| Phase | Deliverables | Exit criterion |
|---|---|---|
| **0** | `scalus.verify` surface (`Prop`, `forAll`, connectives, `Quantifiable` for `BigInt`/`Boolean`/`ByteString`); macro reifier; `PropIR` with flat serialization; `lean-uplc` for ∀-prefix statements with the polarity rule; `sbt verify` generating a Lean workspace and parsing verdicts | the `Math.lean` theorems and controls, restated in Scala, give the same verdicts; generated files replace the hand-written ones |
| **1** | `exists` / `existsWith`, `denotes`, `equal`; `refute` with replay; `scalacheck`; `Thm`, report, cache; PR-time freshness check and nightly run | a suite without a negative control fails; a planted spurious counterexample is reported as spurious |
| **2** | `lean-direct` for integers, `Bool`, `ByteString`, `List`, `Option` and case classes; prelude bridge; `using`, `split`, `cases` | a list lemma proved with `induction xs <;> blaster` from a Scala statement |
| **3** | validators: `ScriptContext` targets with the CardanoLedgerApiBlaster validity predicates; `spec` capture; bring `feature/verification-blaster` forward | one real contract (Vesting) with authorisation and conservation properties proved about its script hash |
| **4** | ctproof on the shared frontend; a Scala kernel only if a lemma backlog demands it | ctproof's `@pre` / `@post` examples elaborate through `PropIR` |

---

## 10. Open questions

1. **Reifier placement.** A macro gives portability and sharing with ctproof. A plugin phase would
   see more. This design recommends the macro, pending the spike in §4.3.
2. **`spec` capture.** `ScalusPrepare`, or a macro annotation?
3. **`Prop` versus `Boolean` connectives.** `a && b` on two Booleans stays one atom. Should
   `lean-direct` split it when both sides are total?
4. **Calling convention for target binders** with non-primitive argument types. The atom's call
   `f(x)` must use the same representation, `Data` or `UplcConstr`, as the target's parameter.
5. **Budget calibration** between Scalus CEK metering and PlutusCoreBlaster step counts.
6. **Commit policy.** Should the cache and the generated Lean workspace be committed, as
   `Generated/` is today, so Lean builds without a JVM?
7. **Where the ctproof-shared module lives**, and under which name.
8. **The default domain for validator inputs:** well-formed values or raw `Data` (§3.3).
9. **Naming:** `scalus.verify`, `theorem` / `refute`, and `spec.requires` next to the prelude's
   `require`.
