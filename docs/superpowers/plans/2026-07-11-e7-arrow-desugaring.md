# E7: `->` Desugaring Over-Broad Match Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development or superpowers:executing-plans. Steps use checkbox (`- [ ]`).

**Goal:** Make the `a -> b` special-case fire only for stdlib `Predef.ArrowAssoc.->`, not for any method named `->` returning a `Tuple2` (audit finding E7).

**Architecture:** `SIRCompiler` special-cases `a -> b` to `Tuple2(a, b)` because `scala.Predef.ArrowAssoc` has no TASTy (its body can't be compiled). The two arrow match arms guarded on `name.show == "->" && tree.tpe =:= Tuple2`, then blindly unwrapped one `Apply` layer off the receiver (assuming it was `ArrowAssoc(a)`). A user-defined member `->` on `Foo(x)` therefore compiled silently to `Tuple2(x, b)` — receiver dropped, and mistyped (`x` where `Foo`'s field is expected). The fix keys the guard on the `ArrowAssoc.->` **method symbol** (there is no `StdNames` constant for `->`), so a user `->` falls through to normal compilation instead of being silently mis-desugared.

**Tech Stack:** Scala 3 compiler plugin (dotty API), sbt, ScalaTest.

## Global Constraints

- Production change confined to the two arrow arms in `compileExpr2` (`SIRCompiler.scala` ~`:3166-3177`) plus one symbol definition. Match on `sel.symbol == ArrowAssocArrowMethod` where `ArrowAssocArrowMethod = requiredClass("scala.Predef.ArrowAssoc").requiredMethod("->")`. There is NO `nme.*` constant for `->`; symbol identity is the correct check.
- Dropping the `=:= Tuple2` guard is safe — `ArrowAssoc.->` always returns `Tuple2`.
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `PatternMatchAuditTest.scala` stay untracked. Its 2 failing tests are by-design.
- Commit to `master`, `scalafmtAll` before commit, `git pull --rebase` before push, no `Co-Authored-By` trailer.

## Execution notes (discovered)

- Scalus does NOT compile instance methods on case classes (`compileTreeInModule` skips non-module `TypeDef`s), so a member `->` is uncompilable. The buggy path *hid* this by short-circuiting to `Tuple2(x,y)`; the fixed path correctly falls through and surfaces a clear "not found" error naming `Foo.->`. That is the right outcome (loud error > silent wrong), so the repro is a **negative-compilation** test, not a runtime one.
- Extension-method `->` would not reproduce E7 anyway: it is typed as `->(receiver)(arg)`, not `Select(receiver, ->)`, so it never matched the arrow arm.

---

### Task 1: RED test
- [ ] `scalus-core/jvm/src/test/scala/scalus/compiler/plugin/ArrowDesugaringTest.scala` extends `SnippetCompilation`: (a) runtime control — stdlib `x -> (x+1)` evaluates correctly; (b) repro via `compileSnippet` — a user member `->` on a case class must produce a diagnostic (was silently accepted). Verify RED: repro fails with `got: List()` (no error) without the fix.

### Task 2: Fix (GREEN)
- [ ] Add `ArrowAssocClass` + `ArrowAssocArrowMethod` symbols; change both arrow arms to `case Apply(sel @ Select(qual, _), List(rhs)) if sel.symbol == ArrowAssocArrowMethod` (and the `TypeApply` variant). Verify GREEN.

### Task 3: Regression
- [ ] `sbt "scalusJVM/Test/clean; scalusJVM/test"` (only the 2 `PatternMatchAuditTest` failures), `sbt "scalusExamplesJVM/clean; scalusExamplesJVM/test; scalusCardanoLedgerJVM/test; scalusPlugin/test"` (confirms existing `a -> b` usage still compiles/works), `sbt scalafmtAll; sbt quick`.

### Task 4: Docs + commit
- [ ] Mark E7 **FIXED** in the untracked audit doc. Commit + push (plugin, test, plan).
