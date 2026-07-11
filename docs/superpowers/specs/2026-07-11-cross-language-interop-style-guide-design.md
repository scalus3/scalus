# Scalus Cross-Language Interop Style Guide — Design

Status: design approved in brainstorming; ready for implementation-plan phase.
Date: 2026-07-11

## Goal

Establish a grounded, enforceable Scala 3 coding style for Scalus so that the
designated public surface is **either directly usable from Java / Kotlin /
JavaScript, or cheap to write a facade for**. The style targets the domain
objects (`Transaction`, `TransactionOutput`, `Value`, …), `Data` +
`ToData`/`FromData`, and the workflow entry points (`TxBuilder`, `Emulator`,
`PlutusVM`).

Non-goal: making the *entire* codebase interop-friendly. Internal machinery (SIR
compiler, plugin, `prelude`, ledger internals) is out of scope and stays fully
idiomatic Scala.

## Target platforms

- **Java** — primary. Consumes Scala bytecode; static forwarders already exist
  (verified via `javap`), so factory *access* is fine; friction is argument/return
  types, default args, givens, `Future`/`Either`.
- **Kotlin** — "Java on the JVM": same bytecode consumption, same friction, plus
  it cannot use Scala `$default$` encoding either. Served by the same rules as Java.
- **JavaScript (Scala.js)** — first-class. Structurally different: nothing is
  visible without `@JSExport`; `Long`/`BigInt`, Scala collections and `Option`
  are opaque blobs in JS; default-param exports are buggy (use overloads).
- **Native** — compiles (scalus-core cross-compiles to it) but is not an interop
  target; its per-class `Platform` traits are empty.

## Architecture — Hybrid (Tier 0 unified + per-platform mixin)

Two mechanisms, split by cost. The authoring decision rule:

> **If the interop-friendly form is equally good Scala, make it the only form
> (Tier 0). If it degrades Scala ergonomics, put it in the per-platform
> `Platform` trait.**

### Tier 0 — Unified rules (authored directly into the shared public type)

These help all platforms with zero Scala cost, so there is one real API:

1. **No default parameters on public APIs — provide explicit overloads.** Java &
   Kotlin cannot use `$default$`; Scala.js default-param export is buggy. Single
   biggest rule.
2. **`@scala.annotation.varargs`** on every public vararg method.
3. **Named aliases beside symbolic operators** — keep `+` / `unary_-` for Scala,
   add `def plus` / `def negate`. (Kotlin can remap `plus` → `+`.)
4. **Public factories return concrete types** — no `Product`, path-dependent, or
   bare `IterableOnce` returns.
5. **Non-implicit entry point for every public `given`** — for each
   `ToData`/`FromData`/`Encoder`/`Decoder` on an interop type, expose
   `Foo.toData(x)` / `Foo.fromData(d)` / `Foo.encode` / `Foo.decode`. Givens stay
   for Scala.
6. **Public callback types are `@FunctionalInterface` single-method** (already
   true for `ToData`/`FromData`; codify it). Enables Java/Kotlin lambdas + clean JS.
7. **No `using`/context params on public entry points** — take them as ordinary
   parameters, with a Scala overload that supplies the `given` (as `fromCbor`
   already does).

Tier 0 is what keeps the per-platform traits thin.

### Tier 1 — Per-platform `<ClassName>Platform` mixin

`scalus-core` and `scalus-cardano-ledger` are cross-compiled, and a `shared/`
class cannot gain instance methods from a `jvm/`-only file (no partial classes).
So divergent, platform-specific members are contributed via a **trait of the same
FQN with a different body per platform** — the established Scala.js cross-project
idiom (same pattern as `scalus.uplc.builtin.platform`).

Structure, one trait per interop class, one file per platform:

```scala
// shared/…/Transaction.scala
case class Transaction(...) extends TransactionPlatform

// jvm/…/TransactionPlatform.scala   — full Java idiom
private[ledger] trait TransactionPlatform extends InteropApi { self: Transaction =>
    def getAuxiliaryDataOrNull: AuxiliaryData = auxiliaryData.map(_.value).orNull
    // @BeanProperty-style getters, java.util.List accessors, builder entry points…
}

// js/…/TransactionPlatform.scala     — full JS idiom, mixed INTO the type
private[ledger] trait TransactionPlatform extends InteropApi { self: Transaction =>
    @JSExport def auxiliaryDataOrUndefined: js.UndefOr[AuxiliaryData] = …
    @JSExport("fee") def feeStr: String = body.value.fee.value.toString  // Long→String
}

// native/…/TransactionPlatform.scala — empty
private[ledger] trait TransactionPlatform extends InteropApi { self: Transaction => }
```

Rules:
- Self-type `self: Transaction =>` gives the trait access to the class fields
  without redeclaring them.
- Every platform trait `extends InteropApi`, a **shared empty marker** used for
  test enumeration.
- Naming convention: `<ClassName>Platform`, one file per platform.
- The trait must exist on **every** platform the class compiles to (empty is
  fine) or that platform will not compile.

This collapses what would otherwise be a separate JVM mixin tier and a separate
JS facade layer into one uniform mechanism. Full-fidelity JS idiom is mixed into
the type instead of hand-written in a parallel facade.

#### JS constructor entry — the one exception

`@JSExportTopLevel` cannot be inherited through a mixed-in trait nor placed in
`shared/`. JS-side **construction** therefore stays a small `js/`-only
`@JSExport`ed factory object (instance accessors mix in via the trait; only the
constructor entry is a separate factory).

## Interop surface & MiMa stability (lean)

- **Package = the normative boundary.** The designated interop packages are the
  stable surface; churn-prone additions go into `*.internal` subpackages. Do not
  relocate existing domain types (that is itself a break).
- **MiMa allow-list.** A custom filter predicate suppresses every incompatibility
  whose symbol is not under a stable interop-package prefix (`*.internal` and
  non-interop excluded). Cheap, no annotation scanning. This gives the desired
  "few types stable, most free to break within a version" shape.
- **`InteropApi` marker trait** = interop mechanism marker, test-enumerable.
- **Lean:** no `@Stable` annotation for now. Add it only if an intra-package
  stable/unstable conflict forces finer granularity.

## Enforcement — `InteropSurfaceTest`

A mechanical test over the interop packages (not docs-only):
- No `$default$` synthetic methods on public members of interop classes.
- Symbolic operators on interop types have named aliases.
- Every public `given` codec on an interop type has a non-implicit entry point.
- `InteropApi` coverage: types needing divergent members mix in
  `<ClassName>Platform extends InteropApi`; JVM and JS expose the agreed accessor
  set (guards against the three per-platform bodies drifting).

## Rollout

Incremental, not big-bang. Apply to the designated types in order:
`Transaction`, `TransactionOutput`, `Value`, `Data` + codecs, `TxBuilder`,
`Emulator`, `PlutusVM`. Each type: apply Tier 0 rules in the shared class, add
`<ClassName>Platform` traits per platform where divergent members are needed,
extend `InteropSurfaceTest` coverage, update the MiMa allow-list.

## Non-goals / YAGNI

- No `@Stable` annotation yet (package boundary does the job).
- No macro-annotation code generation in this phase (revisit if the per-platform
  boilerplate becomes painful; the earlier research doc
  `docs/internal/JAVA_INTEROP_RESEARCH.md` covers the macro path).
- No separate `scalus-java` facade module — the per-platform mixin replaces it.
- Lombok / JSR-269 processors: rejected (cannot see Scala sources).

## Risks

- **Per-platform boilerplate**: up to 3 small trait files per interop class
  (jvm/js/native for scalus-core; jvm/js for cardano-ledger). Bounded (~7–15
  types) but real; `InteropSurfaceTest` mitigates drift.
- **`@JSExport` on trait members**: verify member export works when the trait is
  mixed into a plain Scala.js class (expected to work; confirm in the plan).
- **MiMa allow-list predicate**: must be written against `Problem#matchName`
  prefixes; confirm the API shape when implementing.
- **Kotlin SAM over Scala-defined functional interfaces**: expected to work for
  single-abstract-method JVM interfaces; verify with a Kotlin smoke test if Kotlin
  becomes a supported consumer.

## Reference

- Prior research: `docs/internal/JAVA_INTEROP_RESEARCH.md` (Lombok rejection,
  static-forwarder facts, macro-annotation state of the art).
