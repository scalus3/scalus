# Scala.js TypeScript Definitions Generator — Design

**Date:** 2026-08-03
**Status:** Design approved; ready for implementation-plan phase.
**Goal:** Generate `scalus.d.ts` automatically from the compiled Scala.js facades, replacing the hand-written file, so the JVM + JS + TypeScript surface derives from a single Scala source.

## Background and problem

Scalus ships an npm package (`scalus`) built from Scala.js: ESModule linker output bundled by esbuild into a single `scalus.js` (`build.sbt` `prepareNpmPackage`). Its TypeScript types live in a **hand-written** 332-line `scalus-cardano-ledger/js/src/main/npm/scalus.d.ts`. That file is a parallel, manually-synced mirror of the Scala facades (`JScalus.scala`, `JEmulator.scala`, `SlotConfig.scala`) and drifts:

- It nests `ExUnits`, `Result`, `Redeemer` inside `namespace Scalus`, but Scala exports them as **top-level** exports (`@JSExportTopLevel("ExUnits")` etc.). The declared shape does not match the runtime module.
- Every facade change requires a manual d.ts edit; nothing enforces consistency.

### Survey of existing tools (researched 2026-08)

No maintained tool generates `.d.ts` from Scala 3 / Scala.js exported APIs:

- **scala-ts** (active, Scala 3) and **scala-tsi** (frozen 2024): data-model-only — case classes/sealed traits to TS interfaces for JSON interchange. Blind to `@JSExport`, classes, methods, functions.
- **swachter/scala-ts** (`ScalaTsPlugin`): the exact right design — SemanticDB-driven, honors `@JSExportTopLevel`/`@JSExport`/`@JSExportStatic`, emits `.d.ts` beside the Scala.js ESM output. Scala 2.12/2.13 only, dormant since 2023. Used here as the design reference.
- **Scala.js core**: issue scala-js/scala-js#3836 closed as "separate repo" — the team wants this as a community tool. No public linker API exposes typed export info; IR-level export signatures are erased toward `any`, so the `.sjsir`/linker route alone cannot produce well-typed definitions. Type information must come from TASTy.
- **Kotlin/JS** (prior art): generates `.d.ts` natively from `@JsExport` with a restricted "exportable types" subset and compiler errors/warnings on non-exportable members. We adopt the same strictness model.

## Decisions (made with the user, 2026-08-03)

1. **Scope: generator first.** Build the generator against the existing four facades. Exporting `TxBuilder`/`Transaction`/`Address`/`Value` to JS (the 2026-07-11 interop-spec Tier-1 rollout) is a separate follow-up; the generator will cover those exports automatically once they exist.
2. **Mechanism: TASTy, via `scala3-tasty-inspector`.** Post-compilation analysis over the JS sourceset's TASTy. The inspector gives the full `Quotes` reflection API plus `Symbol.docstring`. Compiler-version coupling is acceptable for an in-repo tool.
3. **Packaging: in-repo module first.** Extract to a standalone sbt plugin later if it proves generic. The core stays free of Scalus-specific logic to keep that path open.
4. **Output fidelity: emit runtime truth.** Top-level exports are declared top-level (no compat `namespace Scalus` nesting of classes). npm tests and docs are fixed in the same change.
5. **Doc comments in v1.** Scaladoc transfers to TSDoc.
6. **Flatten the JS API to canonical ESM shape** (added 2026-08-03). The ES module is the namespace in modern TypeScript; the `namespace` keyword is legacy (typescript-eslint `no-namespace`), and flat named exports tree-shake better. Therefore:
   - The four `JScalus` functions (`applyDataArgToScript`, `evaluateScript`, `evaluateScriptProfile`, `evalPlutusScripts`) gain `@JSExportTopLevel`, so `import { evaluateScript } from "scalus"` works.
   - The exported `Scalus` object stays exported as a **deprecated alias** for one release window (Scala.js allows exporting the same def both top-level and as a member).
   - Generic class names are tightened while the window is open: `Result` → `EvaluationResult`, `Redeemer` → `RedeemerBudget`. The old names stay as additional deprecated top-level exports (a class may carry multiple `@JSExportTopLevel` annotations).
   - Deprecated entries are marked `@deprecated` in the generated d.ts (see Doc comments).

## Architecture

### Module

New JVM-only sbt module **`scalus-ts-exporter`** (`scalusTsExporter`): a library plus a small CLI main. Dependencies: `scala3-tasty-inspector` (matching the build's Scala version) and nothing Scalus-specific.

### Inputs

- An ordered list of TASTy roots to scan (class directories or jars) — for Scalus: the `Compile/classDirectory` of `scalusJS` and `scalusCardanoLedgerJS` (the npm bundle spans both; `SlotConfig` lives in scalus-core).
- The full dependency classpath (so the inspector resolves `scala.scalajs.js.*` and library types).
- Output file path.

### Pipeline

1. **Collect** symbols annotated `@JSExportTopLevel`, `@JSExport`, `@JSExportStatic` across the TASTy roots.
2. **Model** an export AST: exported classes (constructor, members), exported objects (as namespaces of functions/consts), static members, overload sets, getters/setters, top-level export names (annotation argument wins over Scala name).
3. **Chase referenced types** transitively: non-exported `js.Object` traits/classes reachable from exported signatures (e.g. `JEmulatorInitialState`, `JStakeRegistration`) are emitted as `export interface`. References to exported classes resolve to their exported names across roots.
4. **Map types** to TypeScript (table below).
5. **Emit** a single `.d.ts` with TSDoc comments, deterministic ordering (stable across runs for clean git diffs).

### Type mapping

| Scala | TypeScript |
|---|---|
| `Boolean` | `boolean` |
| `Byte`/`Short`/`Int`/`Float`/`Double` | `number` |
| `String` | `string` |
| `Unit` | `void` |
| `Null` / `Nothing` | `null` / `never` |
| `js.BigInt` | `bigint` |
| `js.Array[T]` | `T[]` |
| `js.Promise[T]` | `Promise<T>` |
| `js.UndefOr[T]` | `T \| undefined`; as a trailing parameter: optional `x?: T` |
| Scala.js union `A \| B` | `A \| B` |
| `js.FunctionN[..., R]` / `js.ThisFunction` | arrow function types |
| `js.Dictionary[T]` | `{ [key: string]: T }` |
| Typed arrays (`Uint8Array`, …), `js.Date`, `js.RegExp` | same-named DOM/ES types |
| `js.Any` / `js.Object` | `any` / `object` |
| Exported class/object reference | the exported name |
| Non-exported `js.Object` type in a signature | generated `export interface` |
| Generic type params with bounds | TS generics (`<T extends U>`) |

Member-level rules:

- Default parameters → optional parameters (`x?: T`).
- `@JSExportStatic` → `static` members on the exported class.
- Overloaded exports → multiple TS overload signatures.
- `val` / parameterless `def` getter → `readonly` property / `get` accessor; `var` / setter pairs → mutable property.

### Unsupported-type policy (strict, Kotlin-style)

An exported member whose signature references a non-exportable type (`Long`, Scala collections, `Option`, tuples, any opaque Scala class not itself exported) is a **hard error** that names the member and the offending type. No silent `any`.

Escape hatches (three tiny annotations in `scalus.interop`, scalus-core JS sourceset):

- `@TsType("...")` overrides the emitted TS type for one member or parameter. It also serves precision the Scala types cannot express — e.g. re-tightening today's hand-written `"key" | "script"` literal union on `credentialType`.
- `@TsName("...")` overrides the emitted declaration name for chased (non-exported) traits/classes — e.g. `@TsName("SubmitResult")` on `trait JSubmitResult`, so the Scala `J` prefix does not leak into the d.ts. For exported declarations the `@JSExport*` annotation name wins.
- `@TsIgnore` omits the annotated member from the d.ts (it still exists at JS runtime). For Scala-facing members of `js.Object` classes that cross-platform Scala code needs — e.g. `SlotConfig.slotToInstant`/`instantToSlot`, which use `java.time.Instant` (added 2026-08-03 when the strict checker flagged them).

### Doc comments

Scaladoc → TSDoc: body text kept, `@param`/`@return` kept, `[[Foo]]` → `{@link Foo}`. Scala `@deprecated("msg", "version")` annotations map to the TSDoc `@deprecated` tag. Source: `Symbol.docstring` from the inspector. This requires the doc-retention compiler flag on the inspected modules (exact flag verified during implementation; fallback if TASTy lacks docstrings: read the source file via TASTy position info, as scaladoc does).

### sbt wiring

- `generateDts` task in `scalusCardanoLedgerJS` jsSettings: depends on `Compile/compile` of both JS modules, runs the exporter (forked JVM run of `scalusTsExporter`), writes `js/src/main/npm/scalus.d.ts`.
- `prepareNpmPackage` depends on `generateDts`.
- The generated file stays **committed** (same policy as `scalus.js`). CI (`ci-js`) regenerates it and fails if `git diff` is non-empty — this is the drift gate.

## Migration (consumer-visible, one-time)

The first generated file will differ from the hand-written one:

- The API flattens per decision 6: the evaluation functions become top-level named exports; `import * as Scalus from "scalus"` replaces the `Scalus` namespace object for consumers who want grouping. The `Scalus` object and the old `Result`/`Redeemer` names remain as `@deprecated` exports for one release window.
- `ExUnits`, `EvaluationResult`, `RedeemerBudget` are top-level exports (runtime truth) instead of `namespace Scalus` members.
- Hand-tightened literal unions widen to `string` unless `@TsType` is applied (apply it where the hand-written file had them).
- `ReadonlyArray<T>` becomes `T[]` (or `@TsType` where readonly matters).
- Doc text becomes the Scaladoc text.

The migration commit reconciles npm `__tests__`, the TS example project (`scalus-examples/js/src/main/ts`), and site docs (`js-emulator.mdx`) with the new declarations.

## Testing

1. **Unit:** pure tests of the type mapper and TSDoc converter.
2. **Golden:** a test-only Scala.js fixtures project (`scalus-ts-exporter-fixtures`) containing every supported export shape — overloads, statics, defaults, `UndefOr`, unions, generics, nested objects, referenced non-exported traits, `@TsType` overrides, and error cases. Generate, compare to committed golden `.d.ts`, and run `tsc --noEmit` over the golden plus a consumer `.ts` that exercises the declarations.
3. **Integration:** regenerate the real `scalus.d.ts`; the existing vitest suite and TS type-checks must pass; CI diff check as above.

## Error handling

- Unsupported type in an exported signature: fail with member FQN, offending type, and a hint (`@TsType` or restructure).
- Name collisions among top-level exports: fail (mirrors a Scala.js linker error, but check defensively across roots).
- Missing docstrings: not an error; emit member without TSDoc.
- Inspector/classpath failures: fail the sbt task with the underlying diagnostics.

## Out of scope

- Exporting `TxBuilder` / ledger domain types to JS (separate effort per the interop style guide; the generator picks them up automatically once exported).
- Standalone OSS sbt-plugin extraction (later, after the in-repo tool stabilizes).
- Scala 2 / SemanticDB support.
- npm publish automation.

## References

- Interop style guide: `docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md`
- Facades: `scalus-cardano-ledger/js/src/main/scala/scalus/uplc/eval/JScalus.scala`, `.../cardano/node/JEmulator.scala`, `scalus-core/js/src/main/scala/scalus/cardano/ledger/SlotConfig.scala`
- Hand-written target: `scalus-cardano-ledger/js/src/main/npm/scalus.d.ts`
- Design reference: https://github.com/swachter/scala-ts (Scala 2, SemanticDB)
- Scala.js position: https://github.com/scala-js/scala-js/issues/3836
- Kotlin/JS prior art: https://kotlinlang.org/docs/js-to-kotlin-interop.html
