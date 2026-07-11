# Scalus Compiler Plugin Audit

Date: 2026-07-11.
Scope: `scalus-plugin/src/main/scala/scalus/compiler/plugin/` (9 files, ~9,500 lines), reviewed for bugs, duplication, and alignment with dotty compiler-plugin best practices (evidence gathered from the Scala 3 compiler sources).

Findings marked **[verified]** were confirmed by direct code reading during the audit; the rest were reported by review passes over the full sources.

---

## 1. Error handling: `report.error` vs exceptions

### Current state (four coexisting styles)

| Style | Where | Effect |
|---|---|---|
| `report.error(...)` + continue with fallback value | ~30 sites in SIRCompiler, PatternMatchingCompiler, Plugin | Error accumulates; compilation of the unit continues |
| `error(err, defaultValue)` helper (`SIRCompiler.scala:1133`) — reports **and** throws `RuntimeException` | ~79 call sites | `defaultValue` is `@unused` and dead; the throw escapes to dotty as a crash |
| bare `throw TypingException` | 17 sites in SIRTyper (+1 in PatternMatchingCompiler) | Caught at `SIRCompiler.sirTypeInEnv:3606`, re-reported, then **re-thrown** (`if true then throw e` at :3613) |
| bare `throw RuntimeException` / `IllegalStateException` / `???` | SIRCompiler ×3, PatternMatchingCompiler ×5, SIRHelper:102, SIRTyper `???` ×3 | Escapes as an unpositioned compiler crash |

The phase entry points (`ScalusPhase.prepareForUnit:191`, `transformApply:355`) catch `NonFatal`, call `ex.printStackTrace()`, and **rethrow**. So any thrown error produces three outputs: the clean positioned `report.error` (when one was emitted), a raw stack trace on stderr, and dotty's crash banner. Since the phase is a `PluginPhase`, dotty at least attributes the crash correctly — `report.scala:156-168` in the compiler prints *"An unhandled exception was thrown in the compiler plugin… Please report the issue to the plugin's maintainers"* — but the user still sees a crash dump, and only the first error per file is ever reported.

### How dotty itself does it (evidence from the compiler sources)

- **`report.error` never throws** — it hands a diagnostic to `ctx.reporter` and returns `Unit` (`compiler/.../report.scala:67-70`).
- **You do not need to throw to prevent bad codegen.** Once any error is reported, `Phase.isRunnable` (`Phases.scala:313`) returns false for all later phases — the backend simply never runs. The idiom is `report.error` + return a plausible sentinel (`errorTree`/`ErrorType`, `typer/ErrorReporting.scala:22-39`).
- **Macros are the canonical precedent** for "user errors + internal errors in one phase" (`transform/Splicer.scala:69-88`):
  - user abort = control-flow exception `StopMacroExpansion`, **caught at the phase boundary**, never escapes; error already reported (backs `report.errorAndAbort`);
  - internal bug = `catch NonFatal(ex)` → `report.error` with class, message, trimmed stack trace, and a source position, then return an error tree — never a compiler crash;
  - `CompilationUnit.SuspendException` is always re-thrown.
- **Scala.js backend convention** (`backend/sjs/JSCodeGen.scala`): `report.error` for anything the user could have caused; `throw FatalError` only for illegal-tree invariants that indicate a compiler bug.

### Pros/cons for our use case

**`report.error` + continue (accumulate):**
- (+) Multiple independent errors per file reported in one compile — much better iteration UX.
- (+) No crash banner; IDE/BSP diagnostics stay clean.
- (−) Every error site must produce a *plausible* fallback SIR value so the rest of the phase doesn't NPE; sloppy fallbacks cause confusing cascade errors.

**Exceptions (current dominant behavior):**
- (+) Trivially correct locally — no fallback value needed, no cascades.
- (−) First error aborts the whole unit; user fixes errors one at a time.
- (−) Raw `RuntimeException`/`???` without a prior `report.error` surfaces as an unpositioned crash that looks like a Scalus bug even when it's a user error.
- (−) Stack traces + crash banner pollute output and IDE diagnostics.

### Recommendation: the macro model (two lanes + phase boundary)

1. **User errors** (unsupported constructs): construct a `CompilationError` case class (the hierarchy in `CompilationError.scala` is already good), then `report.error(err.message, err.srcPos)` and throw a single private control-flow exception:
   ```scala
   class ScalusCompilationAborted extends Exception with scala.util.control.NoStackTrace
   ```
   This preserves today's "first error aborts the expression" semantics without inventing fallback values everywhere.
2. **Catch it at the phase boundary** (`transformApply` / `compileModule` per-definition loop): swallow it — the error was already reported. Result: no crash banner, and *per-definition* error recovery in `compileModule` means multiple errors per file get reported.
3. **Internal errors** (plugin bugs): keep `IllegalStateException`/`assert` at the throw site, but at the same phase boundary `catch NonFatal(ex)` and convert to `report.error` with "this is likely a bug in Scalus, please report it" + class/message/trimmed stack trace + the current tree's position (exactly `Splicer.scala:80-87`). Never let it escape as a crash.
4. **Never generically catch** `CompilationUnit.SuspendException` — `NonFatal` excludes nothing here, so match it explicitly and rethrow before the `NonFatal` case.
5. Migration path: change `error[A](err, default)` to `error(err: CompilationError): Nothing` (report + throw `ScalusCompilationAborted`); `Nothing` is a subtype of every `A`, so the 79 call sites only need their dead second argument deleted. Convert SIRTyper's `TypingException` handling in `sirTypeInEnv` to report + `ScalusCompilationAborted` too. Replace the three `???` in SIRTyper and the raw throws in PatternMatchingCompiler with either lane.

Steps 1–2 + 5 are mechanical and mostly low-risk; step 3 is a small wrapper. This can land incrementally: the phase-boundary catch alone (step 2 + 3) already removes the crash-banner UX for the existing throws and is a good 0.18.3 candidate.

---

## 2. Bugs by severity

### High

| # | Location | Finding | Fix risk |
|---|---|---|---|
| H1 | `PatternMatchingCompiler.scala:1748` **[verified]** **[FIXED]** | `throw new RuntimeException("NonWildcardInnerPattern")` (preceded by 3 debug `println`s) makes the following `report.error` + `???` unreachable. Follow-up investigation: this exact branch is unreachable from surface Scala on 3.3.8 (typed-pattern inners parse only to Wildcard/Constructor); it was replaced defensively with `ErrorPattern`. The *reachable* crash of the same family was an extractor-arity mismatch (boolean `unapply` → `Constructor` with 0 subpatterns → `IllegalStateException: columnBinding.size != row.patterns.size` at `:95`); fixed with an arity guard in `parseConstructorPattern` returning a positioned `ErrorPattern`, plus `ErrorPattern` tolerance in decision-tree specialization. Regression test: `CustomUnapplyErrorTest`. | Fixed |
| H2 | `SIRTyper.scala:636-646` **[verified]** **[FIXED]** | Two-parameter-list constructor handling: line 645 duplicated line 643 (dead branch), and lines 644/646 were swapped relative to the `(typeParamSymbols, paramSymbols)` return contract — for `case class Wrap()(val y: BigInt)` the field `y` became a bogus SIR type parameter and the constructor lost all fields. Fixed: branches corrected, duplicate deleted. Regression test: `SecondaryParamListCaseClassTest`. | Fixed |

### Medium

| # | Location | Finding | Fix risk |
|---|---|---|---|
| M1 | `SIRCompiler.scala:1375` **[verified]** | `val debug = env.debug \|\| dd.symbol.fullName.toString == "b"` — leftover debug trigger: any user method named `b` at root level floods stdout with compile traces. | **Low** — delete the `\|\| … == "b"` |
| M2 | `PatternMatchingCompiler.scala:1870, 1885` **[verified]** | Inline/by-reference embedding heuristic multiplies size by the loop **index** `i` instead of the usage `count` (computed on the line above and only used for `count <= 1`). Action/guard #0 always inlines regardless of size; later ones get an index-dependent, meaningless decision. Script-size regression, not a semantic bug. | **Low** — change `i` to `count`. Note: re-measure pinned ExUnits baselines on both compiler generations after this |
| M3 | `SIRCompiler.scala:3969-3984` | `extractResultType` doesn't recurse through curried `MethodType`s (the recursive version is commented out with a TODO), so `FromData`/`ToData`/functional-interface result detection inspects the wrong type for multi-parameter-list methods. | **Medium** — needs recursion + regression check of annotation-driven dispatch |
| M4 | `Plugin.scala:238-243` | Blueprint manifest read swallows *any* `Exception` → returns empty set → one transient read failure silently wipes manifest entries contributed by all other source files. | **Low** — narrow catch, log a warning |
| M5 | `Plugin.scala:204-251` | Deleted-source pruning never happens (doc comment claims it does): entries are only pruned when a file is *recompiled*; deleting a Contract source leaves a stale manifest line forever. | **Invasive** — needs reconciliation on full builds |
| M6 | `SIRTyper.scala:229, 474, 1060` | Three `???` landmines (`SuperType`, `makeSIRFunType` fallback, `makeFunTypeLambda`) crash with unpositioned `NotImplementedError` instead of `unsupportedType`. | **Low** |
| M7 | `SIRTyper.scala:375` | `TypeVar` symbol code is `binder.typeSymbol.hashCode() + idx` — distinct type params can collide, making unification treat two different type vars as equal. Known (adjacent `TODO: make SymCode long`; related to the deferred flat-serialization TypeVar fix). | **Invasive** — widen to Long across all TypeVar construction |
| M8 | `SIRCompiler.scala:747-762` **[verified]** | `writeModule` opens `output` with no `try/finally`: if encode/roundtrip-verify/write throws, the stream leaks and a truncated `.sir` may be left for downstream units to link against. (Contrast the manifest write at `Plugin.scala:246-250`, which does use `try/finally`.) | **Low** — open the stream after encoding succeeds, wrap write/close in `try/finally` |

### Low

| # | Location | Finding | Fix risk |
|---|---|---|---|
| L1 | `Plugin.scala:309`, `SIRCompiler.scala:3602`, `SIRCompiler.scala:3613`, `SIRTyper.scala:179` | Dead conditionals: `if true then … else flatTree`; `trace = if false then env.debug else false` (permanently disables the TypingException stack-trace diagnostic); `if true then throw e` (makes the `TypeNothing` recovery unreachable); `if true then` wrapper. | Low |
| L2 | `Plugin.scala:468-798, 343-351` | ~330 lines of commented-out dead code (old `retrieveCompilerOptions`, `createSirLoader`), containing `throw new RuntimeException("QQQ")` and typos. Delete; git history preserves it. | Low |
| L3 | many | `println` on production/error paths instead of `report.*` or debug gating: `SIRCompiler.scala:999-1007, 2235-2236, 2898-2900, 3383-3385, 3565, 1462-1491`; `Plugin.scala:58-60, 124-135, 320-322`; `SIRTyper.scala:838-842`; `PatternMatchingCompiler.scala:689-693, 870-871, 1743-1747`. | Low |
| L4 | `Plugin.scala:192-194, 356-357` | `ex.printStackTrace()` + rethrow in phase entry points — superseded by the §1 recommendation. | Low |
| L5 | `SIRCompiler.scala:2176-2211` | `EQ`/`NE` branches in `compileBigIntOps` are unreachable — `compileEquality` matches first at :2941. | Low — delete or comment |
| L6 | `SIRCompiler.scala:1133` | `error` helper: dead `@unused defaultValue` evaluated eagerly at every call site (e.g. :1774 runs a full recursive `assembleMethodWithTypeFromBody` and discards it). Superseded by §1 step 5; as a stopgap, make it by-name. | Low |
| L7 | `SIRCompiler.scala:571-575, 1088, 3184-3187, 333-334`; `Macros.scala:36-41` | Fragile string-based symbol identification (`fullName.show == "scala.Tuple2$.apply"`, Map-companion names, etc.) — breaks silently on renames. The `ArrowAssoc` fix (symbol identity) is the model. | Low, per-site |
| L8 | `SIRCompiler.scala:3188, 3353` | Tuple2 constructor path passes un-widened `tree.tpe` where the sibling case at :3185 widens. | Low |
| L9 | `SIRTyper.scala:608-610` | `report.warning` with no `srcPos` (points nowhere) + typos "marded"/"FuctionalInterface". Also `SIRPreprocessor.scala:179-181` — `report.error` without position, then a `.head` that can itself throw. | Low |
| L10 | `PatternMatchingCompiler.scala:1999` vs `:2322` | Action inlining uses `SIR.renameFreeVarsInExpr`; guard inlining uses `SIR.renameFreeVars(...).asInstanceOf[AnnotatedSIR]` — two APIs + unchecked cast for the same operation. Reconcile after verifying semantics. | Low |
| L11 | `PatternMatchingCompiler.scala:2217` | By-reference let declares `SIRType.Fun(Unit, decisions.tp)` while the reference site (:2196) uses `sir.tp`; coincidentally equal today. | Low |
| L12 | `PatternMatchingCompiler.scala:80-88, 1892-1913` | `_decisionTreeRefsUsageCount` computed but never consumed (embedding decided by `treeSize >= 10` alone). | Low — delete or use |
| L13 | `PatternMatchingCompiler.scala:2312, 2319-2321` | Guard binding lookups (`bindingMap(b.variableKey)`) crash with `NoSuchElementException` on identity drift instead of a clean internal error. | Low — guard with clear message |
| L14 | various | Typos in user-facing/exception strings and identifiers: `compileMartch`, `scroutineType`, `mwthod`, `bingingMap`, "ptrimary constryctir", "parametes", "aum", "shoule", "promitives", `SIRPreprocessor` redundant re-check at :115-117, misleading comment `Plugin.scala:452`. | Low |

---

## 3. Duplication / unification / refactoring

| # | Finding | Effort / risk |
|---|---|---|
| R1 | **Per-Apply symbol resolution, duplicated across phases.** `Plugin.scala:117-121` and `:265-271` both resolve `compiler$package` + 4-5 `compile*` methods on **every `Apply` node** (and the two lists have drifted). Dotty phases cache these in `prepareForUnit`-initialized fields (`Instrumentation.scala:43-55`) or null-guarded lazies (`CapturedVars.scala:52-55`). Extract one lazily-initialized symbol holder shared by both phases. | Small / low |
| R2 | **SIRTyper misses the symbol cache SIRCompiler already has.** 17 `requiredClass` calls inside per-type-node methods (`SIRTyper.scala:438, 490-496, 511-544, 601-603, 1004, 1040`) re-resolve ByteString/Data/BuiltinList/… on every type; SIRCompiler caches the same set as `private val`s (:100-278). Hoist into vals or share one `PluginSymbols` bundle. | Small / low |
| R3 | **Unify error raising** per §1: one `error(err): Nothing` lane for user errors, `IllegalStateException` + boundary conversion for internal ones; fold `TypingException` reporting (formatted ad hoc at `SIRCompiler.scala:3610-3611`) into the same helper. | Medium / medium |
| R4 | **Raw-string errors → `CompilationError` case classes**: module-not-found at `SIRCompiler.scala:4435` (natural sibling of `SymbolNotFound`), PatternMatchingCompiler `:337, :939, :1179, :1693, :2312`, Plugin `:379, :387`. `GenericError` exists as the escape hatch but is under-used. | Medium / low |
| R5 | **`SIRModuleWithDeps` tree-building idiom repeated 4×**: `Plugin.scala:410-423` and `SIRCompiler.scala:4453-4477`. Extract `mkModuleWithDeps`/`mkModuleWithDepsList` into `SIRHelper.scala` (which already hosts tree builders). | Medium / low |
| R6 | **Giant methods** (defer to a minor release): `SIRCompiler.compileExpr2` (~667 lines, natural split by case groups), `PatternMatchingCompiler.createLeaf` (~433), `compileDecisions` (~252), `applyStaticInheritanceInAnnotatedSIR` (~233), `sirTypeInEnvWithErr` (~223). Only the first two are worth splitting now. | Large / medium |

---

## 4. Dotty best-practices deltas (beyond error handling)

1. **The `ctx.phase != this` workaround** (`Plugin.scala:101-106, 178-182, 253-257`, commented "bug in dotty") appears **nowhere** in the dotty codebase itself. If fusion places the phase in a group whose context phase is the group's superPhase, the supported mechanism is `runsAfterGroupsOf` (`MegaPhase.scala:40`; asserted in `Phases.scala:95-97`), not a runtime phase check. Worth investigating whether `runsAfterGroupsOf` removes the need — but keep the workaround until proven (it's harmless).
2. **Auxiliary output files**: the canonical precedent is `ExtractSemanticDB` — a plain `Phase` overriding `runOn` (not a MiniPhase hook), `isCheckable = false`, gated by `isRunnable`, writing via `java.nio.file` into `ctx.settings.outputDir.value`, once per run. Our blueprint manifest instead does a read-modify-write of the whole file from *every* unit's `prepareForUnit` (O(n²), two different path-resolution mechanisms for read vs write — `Plugin.scala:229-246`). Restructure as collect-during-run, write-once (also fixes M4 and enables M5).
3. **`StandardPlugin` scaladoc requires freshly-constructed phases per `init` call** — we comply (`Plugin.scala:38`). The pre-3.5/3.5+ `init`/`initialize` split handled by `PluginCompat` matches current dotty API.
4. **Flags**: `ScalusPreparePhase` correctly declares `changesMembers = true` (it adds the SIR vals; dotty asserts this at `Symbols.scala:224`). `allowsImplicitSearch = true` is needed by `retrieveCompilerOptions` on both phases — correct as-is. `ScalusPhase` rewrites `compile*` calls and TypeDefs but adds no members after pickling — no extra flags needed.

---

## 5. Proposed 0.18.3 (low-risk) change list

Ordered by value; all are small, behavior-narrow diffs:

1. **H1** — ~~remove `RuntimeException("NonWildcardInnerPattern")` + printlns~~ **done** (incl. the reachable extractor-arity crash).
2. **H2** — ~~fix swapped/duplicate branches in `SIRTyper.scala:636-646`~~ **done**.
3. **M1** — remove the `== "b"` debug trigger.
4. **M6** — replace the three `???` in SIRTyper with `unsupportedType`.
5. **M8** — `try/finally` (encode-before-open) in `writeModule`.
6. **M4** — narrow the manifest-read catch + warn.
7. **M2** — `i` → `count` in embedding heuristics. *Caveat: may change generated script sizes → re-measure dual ExUnits baselines (pre-3.8 and 3.8+) before merging.*
8. **L1-L4** — delete dead conditionals, the 330-line dead block, printlns on error paths, `printStackTrace` calls.
9. **L9** — add positions to the unpositioned `report.warning`/`report.error` sites; fix user-visible typos (L14).
10. **R1 + R2** — cache symbol lookups (pure perf, no behavior change).
11. **§1 steps 2-3** — phase-boundary catch converting escaping `NonFatal` into a positioned "likely a Scalus bug, please report" `report.error` (crash-banner removal). Steps 1+5 (the `Nothing`-returning `error` + 79-site cleanup) can follow in 0.19 with R3/R4.

Defer to 0.19+: M3 (curried `extractResultType`), M5 (manifest lifecycle → write-once `runOn` restructure, §4.2), M7 (TypeVar Long codes), R3-R6, and the `runsAfterGroupsOf` investigation (§4.1).
