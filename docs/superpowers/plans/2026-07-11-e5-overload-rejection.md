# E5: Reject Overloaded Definitions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Reject overloaded methods in a compiled object with a clear compile error instead of silently colliding (audit finding E5).

**Architecture:** Module bindings are keyed by `FullName` with no signature (`SIRCompiler.scala:394-398`), so two overloads of `f` produce two `Binding`s with the same name; the second silently overwrites the first or dies later at lowering with a baffling `Cannot unify result type of apply`. The fix groups the compiled `LocalBinding`s by `fullName.name` and reports a positioned error for any name with more than one binding. Synthetic `val` getters are already filtered in `compileTreeInModule` (`:3854`), so a plain `val` yields exactly one binding — no false positives. Testing reuses the E2 negative-compilation infrastructure (drive `dotty.tools.dotc.Driver` with the packaged plugin jar + scalus-core classpath), extracted into a shared `SnippetCompilation` trait.

**CORRECTION (execution):** Extension methods legitimately share a name (different receivers, resolved at the call site) — the prelude's own `Ord` has two `equiv`/`nonEquiv` extension methods that collide on the binding key today and work fine. The check MUST exempt `Flags.ExtensionMethod`, or it breaks the prelude. Also: do NOT dedup/reorder `bindings` — keep construction exactly as before (only ADD the error path), because changing which colliding binding survives could alter existing (benign) prelude behavior.

**Tech Stack:** Scala 3 compiler plugin (dotty API), sbt, ScalaTest.

## Global Constraints

- Production change confined to the binding-construction block in `SIRCompiler.scala:394-398`.
- Detect overloads by grouping compiled `LocalBinding`s on `fullName.name` (the exact `Binding` key). Report at `lb.pos` (a `SourcePosition`, usable as `SrcPos`). Error text must contain `Overloaded definitions`.
- Report-and-continue via `report.error` (not the throwing `error` helper), and dedup bindings (keep first per name) so lowering doesn't emit the baffling secondary error.
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `PatternMatchAuditTest.scala` stay untracked — never `git add`. Its 2 failing tests are by-design (M4-family repros), not regressions.
- Commit to `master`, `scalafmtAll` before commit, `git pull --rebase` before push, no `Co-Authored-By` trailer.

---

### Task 1: Extract shared snippet-compilation trait + RED test

- [ ] Extract `SnippetCompilation` trait (the `compileSnippet(source): List[String]` helper + `ErrorCollector`) from `ByNameParamErrorTest` into `scalus-core/jvm/src/test/scala/scalus/compiler/plugin/SnippetCompilation.scala`; refactor `ByNameParamErrorTest` to `extends AnyFunSuite with SnippetCompilation`.
- [ ] Create `OverloadedDefRejectionTest` with: a control (`def f`, `def g` → compiles clean) and a repro (`def f(a: BigInt)`, `def f(a: BigInt, b: BigInt)` → error contains `Overloaded definitions`).
- [ ] Run `sbt "scalusJVM/testOnly scalus.compiler.plugin.OverloadedDefRejectionTest scalus.compiler.plugin.ByNameParamErrorTest"`; verify RED (repro fails: no error emitted; ByName + control pass).

### Task 2: The fix (GREEN)

- [ ] In `SIRCompiler.scala:394-398`, collect `localBindings`, filter out `Flags.ExtensionMethod`, group by `fullName.name`, `report.error` at `lbs.last.pos` for any group > 1. Build `bindings` exactly as before (no dedup/reorder).
- [ ] Run the two suites; verify GREEN.

### Task 3: Regression

- [ ] `sbt "scalusJVM/Test/clean; scalusJVM/test"` — pass except the 2 intentional `PatternMatchAuditTest` failures.
- [ ] `sbt "scalusExamplesJVM/clean; scalusExamplesJVM/test; scalusCardanoLedgerJVM/test; scalusPlugin/test"` — all pass (no false positives on real `@Compile` code).
- [ ] `sbt scalafmtAll; sbt quick`.

### Task 4: Docs + commit

- [ ] Mark E5 **FIXED** in the untracked audit doc (section + table row).
- [ ] Commit + push (plugin, tests, plan).
