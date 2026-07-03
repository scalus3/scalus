# Java Interop for Scalus Domain Objects — Feasibility Research

Status: research / recommendation (not yet implemented)
Scope decisions captured up front:
- Targets: construct/read domain objects (`Transaction`, `TransactionOutput`, `Value`, `Data`, …) **and** drive workflows (`TxBuilder`, `Emulator`, `PlutusVM`).
- Free to modify/annotate `scalus-core`.
- Experimental Scala features acceptable (gated to a single module).

## Bottom line

1. **Lombok is a dead end for Scala sources** — it is a `javac`-only AST processor; `scalac` never runs it. It can only help on hand-written `.java` files. There is no "point Lombok at `Transaction.scala`" path. The same is true for *any* JSR-269 annotation processor (MapStruct, Immutables, AutoValue, Dagger) — they all run inside `javac` and never see Scala sources.
2. **The Scala-native Lombok analog is the experimental `scala.annotation.MacroAnnotation`** (re-introduced in 3.3, still `@experimental`). It can generate builders, bean getters, and static codecs — but its sweet spot is **case-class-shaped domain types**, not the workflow APIs.
3. **The friction splits into two layers that want different solutions:**
   - **Domain construct/read** → annotation/codegen-driven **builders + bean getters + explicit codecs**. Automation pays off here.
   - **Workflows** (`TxBuilder`, `Emulator`, `PlutusVM`) → not case-class-shaped (implicits, `Future`, `Either`, by-name `Transaction => Data`). No annotation fixes them; a **hand-written thin facade** is the right tool — and there is a working template in `bloxbean-cardano-client-lib`.
4. **Recommended shape: a separate `scalus-java` facade module**, *not* in-place annotation of core — keeps the idiomatic Scala API clean and sidesteps the macro's same-compilation-unit visibility limitation.

## 1. Why Lombok / annotation processors cannot work

Lombok hooks `com.sun.tools.javac` and mutates the Java compiler's parse tree before bytecode. The domain types are compiled by the **Scala 3 compiler**, which never invokes `javac`'s annotation-processing round for `.scala` files and has its own typer/erasure. So `@Data`/`@Builder`/`@Value` on a Scala class are inert. The only place such processors apply is the existing `.java` files under `bloxbean-cardano-client-lib/src/main/java/` and `scalus-examples/jvm/src/main/java/`.

## 2. The actual friction in the codebase (grounded)

Severity is from a *Java caller's* seat.

| Friction class | Where it bites | Why Java chokes |
|---|---|---|
| Default/named args on big case classes | `TransactionBody` (14 defaulted fields), `TransactionWitnessSet` (8), `Certificate.PoolRegistration` (9) | Java has no named/default args → must pass every positional arg |
| Opaque wrapper types | `KeepRaw[A]`, `TaggedSortedSet/Map`, `TaggedOrderedStrictSet`, `OriginalCborByteArray` | Constructors hidden behind `.from()` factories, some needing `using ProtocolVersion` / `KeyOf` evidence |
| `Option` / `Either` / Scala collections | most body fields; `Emulator.submitSync: Either[…]`; `IndexedSeq`, `SortedMap` | Java sees `scala.Option`, `scala.collection.*` → needs `CollectionConverters`/`OptionConverters` everywhere |
| `given` codecs + context params | `Transaction.fromCbor(using ProtocolVersion, OriginalCborByteArray)`; every `Encoder`/`Decoder` | Java can't summon givens; must locate the static forwarder (e.g. `given_Encoder_Transaction()`) and thread instances manually |
| `ToData`/`FromData` typeclasses + extension methods | `.toData`, `data.to[T]`, `Data.fromData[T]`, macro `derived` | `summon` + `inline` are compile-time only; extension methods invisible from Java; derivation can't run for Java types |
| Singleton values passed as arguments | `NoBudgetSpender`, `NoLogger` in `PlutusVM` | Java must write `NoBudgetSpender$.MODULE$` — this is a *value*, not a method call, so no static forwarder helps |
| Scala 3 enums / sealed traits | `Certificate` (12+ cases), `Credential`, `DatumOption`, `TransactionOutput` | No pattern matching → `instanceof` + cast |
| `Future` returns + by-name function params | `TxBuilder.complete: Future[…]`, `Emulator.submit`, `spend(utxo, tx => Data)` | `scala.concurrent.Future` ≠ `CompletableFuture`; `Function1` lambdas awkward |
| Infix operators | `Value`, `Coin` (`+`, `-`, `unary_-`) | Compile to `$plus` / `unary_$minus` |

Two distinct shapes: **(A) data/domain types** (case-class-shaped, amenable to mechanical generation) and **(B) workflow services** (not).

### What is NOT friction: static factory access

A common misconception (corrected after inspecting bytecode): **Scala 3 auto-generates `public static` forwarders** on the companion *class/interface* for every companion-object method. Java can already call factories directly, no `$.MODULE$` and no `@static` needed:

```java
TransactionOutput.apply(addr, value);          // static forwarder on the interface
Transaction.empty();
Transaction.fromCbor(bytes, protocolVersion);
Value.lovelace(5_000_000L);
```

Verified via `javap -p` on `scala-3.3.8/classes`: `Transaction`, `Value` (case classes) and `TransactionOutput` (sealed trait) all carry `public static apply(...)`, `empty()`, `fromCbor(...)`, `lovelace(...)`, and even `given_Encoder_Transaction()` / `decoder(...)` forwarders. So `@static` is redundant for these. Caveats: forwarders are only emitted for top-level objects and are skipped on a name clash with an instance member.

The friction is therefore **arguments and returns, not reaching the factory**:
- **Default args still don't auto-apply.** `new Value(coin)` won't compile from Java; the default is a separate static `Value$lessinit$greater$default$2()` the caller must invoke and pass manually. Only hand-written overloads (`Transaction.apply(body)`, `apply(body, ws)`) give Java a no-defaults path.
- Argument/return types (`KeepRaw`, `Option`, `Tagged*`, `scala.collection.*`, `Future`, `Either`) still need construction/conversion.
- Singleton *values* used as arguments (`NoBudgetSpender`, `NoLogger`) genuinely need `…$.MODULE$`.

## 3. State of the art in the Scala ecosystem

### 3.1 `scala.annotation.MacroAnnotation` — the real Lombok analog
- `@experimental` feature, re-introduced in 3.3.0-RC2, present through current 3.x. Subclass `MacroAnnotation`, override `transform`, which receives the annotated symbol/AST and returns a list of definitions — may add new members, and via the `companion` parameter add/transform companion members. Enough to synthesize bean getters, a Java builder class, `@varargs` overloads, and static codec methods.
- Limitations to design around:
  - **Experimental ⇒ viral within the *defining* module.** Applying the annotation requires experimental scope (`-experimental` or `@experimental`). Generated members are ordinary, so **downstream consumers are unaffected** — cost confined to whichever module carries the annotated sources.
  - **Generated members are added after typer.** Callable from **separately-compiled** code (a different module) but **not visible to type-checking within the same compilation unit**, and **IDE support (Metals/IntelliJ) for them is weak**. Favours generating into a separate facade module rather than annotating `Transaction` and expecting `scalus-core` Scala to use the generated bits.
  - Cannot create brand-new top-level classes unrelated to the annotee (a nested Java `Builder` in the companion is fine).
- Real usage exists but niche (`@ExtendWith`-style model extension; ongoing design in scala3#19676).

### 3.2 Stable stdlib annotations (no experimental flag)
- `@scala.beans.BeanProperty` → `getX()`/`setX()`; in Scala 3 **visible only from Java**, so it doesn't clutter the Scala API. Good for "read domain objects from Java."
- `@scala.annotation.varargs` → Java `T...` overload alongside the Scala `Seq` one.
- `@scala.annotation.static` → real JVM statics (in the companion object of a class/trait). Lets Java call `Transaction.empty()` not `Transaction$.MODULE$.empty()`.
- `scala.jdk.CollectionConverters` / `OptionConverters` / `FunctionConverters` / `FutureConverters` → standard bridges (`.asJava`, `OptionConverters.toJava`, `FutureConverters.asJava` → `CompletableFuture`). Already used in the bloxbean module.
- `@FunctionalInterface` — already applied to `ToData`/`FromData`; correct (lets Java pass a lambda).

### 3.3 Build-time source generation
- sbt `Compile / sourceGenerators` can emit `.scala`/`.java` before compilation. Since the domain is defined by Cardano's **CDDL**, a schema-driven generator (CDDL → Java-facade builders/codecs) is a credible fully-stable alternative to macros — more upfront machinery, no experimental flag, full IDE visibility, single source of truth. ScalaPB/Avro4s-style.

### 3.4 Prior art already in the repo
`bloxbean-cardano-client-lib` is a working Java-interop facade: hand-written `.java` statics (`ScalusScriptUtils`, `EvaluatorMode`), Scala converters (`Interop.scala`, 1200+ lines) using `CollectionConverters`/`OptionConverters`, `@BeanProperty` on bridge classes, Scala classes implementing Java interfaces (`ScalusTransactionEvaluator extends TransactionEvaluator`). This is the proven template; the open question is how much to automate.

## 4. Approaches compared

| Approach | Construct/read domain | Drive workflows | Stability | IDE/tooling | Effort | Verdict |
|---|---|---|---|---|---|---|
| Lombok / JSR-269 processors | ✗ (Scala invisible) | ✗ | n/a | n/a | n/a | Rejected |
| Stable annotations (`@BeanProperty`, `@varargs`, `@static`) on core | ✓ getters/statics; ✗ builders/Optional | ~ minor | stable | full | Low | Adopt as baseline |
| `MacroAnnotation` (`@JavaApi`) generating builder+beans+codecs | strong | ✗ (not case-class-shaped) | experimental, viral-in-module | generated members weak | Med | Adopt for domain layer, in a facade module |
| sbt source generator (from CDDL or case classes) | strong, single source of truth | ~ | stable | full | High upfront | Strong alternative to macros |
| Hand-written facade module (bloxbean style) | ✓ but boilerplate-heavy | the only good answer | stable | full | Med-High ongoing | Adopt for workflow layer; seed domain layer |
| Do nothing, document converters | ~ | ~ | stable | full | None | Insufficient |

## 5. Recommendation — layered, separate-module

**Layer 0 — stable quick wins on core (low risk, do first)**
- `@varargs` on vararg factories (`MultiAsset.assets`, witness-set builders).
- Add a few **no-defaults `apply` overloads** where the common Java call would otherwise have to fill in defaulted params by hand (this is the real win — static forwarders already exist, but Java can't use the defaults).
- `@BeanProperty` where it helps Java reading (Java-only-visible in Scala 3, so safe). Optional.
- Note: `@static` is **not** needed — Scala 3 already emits static forwarders for companion-object methods (see §2). Skip it.

**Layer 1 — `scalus-java` facade module (core of the work)**
- Hand-written, modeled on `Interop.scala`. Per major domain type: a **fluent Java builder** wrapping the defaulted Scala constructor, accepting `java.util.List`/`Optional`, hiding `KeepRaw`/`Tagged*` behind `.from(...)`, threading `ProtocolVersion`/`OriginalCborByteArray`.
- Workflows: thin wrappers returning `CompletableFuture` (`FutureConverters`), throwing exceptions instead of `Either`, taking `@FunctionalInterface` callbacks instead of by-name `Transaction => Data`. Stays hand-written — annotations buy nothing here.
- `Data`: don't expose typeclasses. Expose Java-callable `Data.Constr/I/B/List/Map` constructors plus generated `static Data encodeX(X)` / `static X decodeX(Data)` per domain type.

**Layer 2 — automate the domain facade boilerplate** (pick one engine)
- Option A — `@JavaApi` macro annotation (experimental). Annotate domain case classes (or thin facade stubs); generate builder/getters/codec statics. Fastest to a PoC; accept weak IDE view + `-experimental` on that module only.
- Option B — sbt source generator from CDDL. More upfront; stable, IDE-visible, spec as single source of truth. Better long-term if the facade grows.

Advice: start with Layer 0 + a hand-written Layer 1 facade for 2–3 representative types (`Transaction`/`TransactionOutput`; `Emulator.submit` + `PlutusVM.evaluateScript`). Validate from a real Java test. Then decide A vs B with a concrete generation target in hand.

## 6. What "good" looks like (sketch)

Today (static access already works — the pain is the argument types: `Option`, `KeepRaw`, no defaults):
```java
TransactionOutput out = TransactionOutput.apply(addr, value, Option.apply(datum), Option.empty());
```
With a facade builder:
```java
TransactionOutput out = ScalusJava.output()
    .address(addr).value(value).inlineDatum(datum)   // Optional handled inside
    .build();
Coin fee = out.getValue().getLovelace();             // @BeanProperty getters
```
Workflows:
```java
CompletableFuture<TransactionHash> h = ScalusJava.emulator(emu).submit(tx); // not Future/Either
Data d = ScalusData.encodeMyRedeemer(r);             // not .toData / summon
```

## 7. Risks & caveats

- Macro annotations are experimental and evolving — possible API churn between minors; `-experimental` confined to the facade module; pin the Scala version and budget for occasional macro maintenance.
- Generated-member IDE blindness is the main practical annoyance — another reason to generate into a facade module that Java consumes via bytecode.
- `Data` typeclass derivation can't be triggered for Java-defined types — Java users must use Scalus-defined domain types or call explicit `encodeX/decodeX` statics; document this boundary.
- Maintenance surface: a facade must track core API changes. CDDL-driven generation (Option B) minimizes drift; hand-written maximizes control. mima won't cover the facade — add Java-level API tests.
- Enums/sealed traits (`Certificate`, `Credential`) still need Java-friendly `isX()/asX()` accessors or visitor helpers; generation can emit these too.

## 8. Suggested PoC

1. Add `scalus-java` JVM-only module.
2. Layer 0 annotations on `Transaction`, `Value`, `PlutusVM` factories.
3. Hand-write facade builders for `Transaction`/`TransactionOutput` + workflow wrappers for `Emulator.submit` and `PlutusVM.evaluateScript`, with a Java test.
4. Prototype one `@JavaApi` macro annotation generating a builder + bean getters for `TransactionOutput`, to measure ergonomics and the experimental-flag/IDE cost.

## Sources

- MacroAnnotation API — https://www.scala-lang.org/api/3.x/scala/annotation/MacroAnnotation.html
- Macro Annotations docs — https://docs.scala-lang.org/overviews/macros/annotations.html
- scala3#19676 (companion transform) — https://github.com/scala/scala3/issues/19676
- Macro-annotation model extension deep dive — https://medium.com/@zainalpour_79971/harnessing-scala-macros-to-extend-models-a-deep-dive-cef769ff18fd
- Contributors: macro annotations & codegen — https://contributors.scala-lang.org/t/scala-3-macro-annotations-and-code-generation/6035
- BeanProperty — https://www.scala-lang.org/api/3.x/scala/beans/BeanProperty.html
- Interacting with Java (Scala 3 book) — https://docs.scala-lang.org/scala3/book/interacting-with-java.html
- Java/Scala interop best practices — https://reintech.io/blog/understanding-java-scala-interoperability
