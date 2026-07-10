# X1 Fix: Strict Match Scrutinee Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make pattern-match scrutinees evaluate strictly (exactly once), matching Scala semantics, so `(BigInt(1) / BigInt(0)) match { case _ => BigInt(42) }` fails on-chain instead of silently returning 42.

**Architecture:** Audit finding X1 (docs/internal/UPLC_CORRECTNESS_AUDIT.md): `PatternMatchingCompiler.compileDecisionTree` binds a non-variable scrutinee with `SIR.LetFlags.Lazy`. Lazy lets lower to "emit only if used" (`Lowering.lowerLet` → `ScopeBracketsLoweredValue` in the V3 backend; unused-lazy elimination in `LetFloating` for the simple backends), so a decision tree that never inspects the scrutinee drops its evaluation — and a used scrutinee can be *re-evaluated* at each use site by the V3 sharing heuristic (audit L7). The fix is one flag change at the single binding site: `LetFlags.Lazy` → `LetFlags.None`. A strict let lowers to `(λs. body) scrutinee` on every backend, which evaluates the scrutinee exactly once, before the match — Scala semantics. Variable scrutinees (`needsScrutineeLet == false`, PatternMatchingCompiler.scala:648-655) are unaffected. Actions, guards, and column bindings keep their `Lazy` flags — those MUST stay lazy (they correspond to code Scala only evaluates on the taken path).

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest (`AnyFunSuite`), Scalus compiler plugin + UPLC CEK evaluator.

## Global Constraints

- Commit directly to `master` — no branches, no PRs (user preference). Rebase before push if pushing.
- NEVER add `Co-Authored-By: Claude` or any Claude/Anthropic trailer to commits.
- Conventional commit style (`fix:`), 1-2 short paragraphs.
- Run `sbtn scalafmtAll` before every commit (CI runs `scalafmtCheckAll`; one unformatted file fails the lts job).
- `git add` every newly created file.
- The plugin change affects ALL compiled test fixtures — incremental compilation can lie. Use `clean` on affected test projects before trusting green results.
- Do NOT modify or commit `scalus-core/jvm/src/test/scala/scalus/compiler/PatternMatchAuditTest.scala` — its 5 failures are intentional repros of audit findings M1–M3.

---

### Task 1: Failing tests for scrutinee strictness

**Files:**
- Create: `scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala`
- Test: same file

**Interfaces:**
- Consumes: `scalus.compiler.compile`, `Options`, `TargetLoweringBackend`, `Term.$`, `Term.evaluate`/`evaluateDebug` (returns `scalus.uplc.eval.Result` with `.isFailure: Boolean` and `.logs: Seq[String]`), `Builtins.trace[A](s: String)(a: A): A`.
- Produces: the regression test suite Task 2 must turn green.

- [ ] **Step 1: Write the failing tests**

Create `scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala`:

```scala
package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Builtins
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Audit finding X1 (docs/internal/UPLC_CORRECTNESS_AUDIT.md): a match whose decision
  * tree never inspects the scrutinee must still evaluate it, matching Scala's strict
  * scrutinee semantics. The scrutinee must be evaluated exactly once.
  */
class MatchScrutineeStrictnessTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("wildcard-only match evaluates erroring scrutinee (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        // Scala: 1/0 throws before the match — the script must fail
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match emits scrutinee trace exactly once (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("scrutinee-evaluated")(x + BigInt(1)) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(result.logs.count(_.contains("scrutinee-evaluated")) == 1)
    }

    test("used scrutinee is evaluated exactly once, not per use (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("scrutinee-evaluated")(x + BigInt(1)) match
                case y if y > BigInt(10) => y + y
                case y                   => y
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val big = compile { BigInt(100) }.toUplc()
        val small = compile { BigInt(1) }.toUplc()
        val rBig = (uplc $ big).evaluateDebug
        assert(rBig.isSuccess)
        assert(rBig.logs.count(_.contains("scrutinee-evaluated")) == 1)
        val rSmall = (uplc $ small).evaluateDebug
        assert(rSmall.isSuccess)
        assert(rSmall.logs.count(_.contains("scrutinee-evaluated")) == 1)
    }

    test("tuple match with all-wildcard components evaluates erroring scrutinee (V3 backend)") {
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x, BigInt(2)) match
                case (_, _) => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match evaluates erroring scrutinee (SumOfProducts backend)") {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SumOfProductsLowering,
          generateErrorTraces = true,
          optimizeUplc = false,
          debug = false
        )
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 42.asTerm)
        assert((uplc $ zero).evaluateDebug.isFailure)
    }

    test("wildcard-only match evaluates erroring scrutinee with optimizeUplc = true") {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true,
          optimizeUplc = true,
          debug = false
        )
        val compiled = compile { (x: BigInt) =>
            (BigInt(1) / x) match
                case _ => BigInt(42)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val zero = compile { BigInt(0) }.toUplc()
        assert((uplc $ zero).evaluateDebug.isFailure)
    }
}
```

Note: the scrutinee expressions are written inline (not via `val`) on purpose — a `val` would introduce its own strict let and a `SIR.Var` scrutinee, bypassing the code path under test (`needsScrutineeLet` is false for `SIR.Var` scrutinees, PatternMatchingCompiler.scala:648-655).

- [ ] **Step 2: Add the file to git**

```bash
git add scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.MatchScrutineeStrictnessTest"`

Expected: FAIL. Specifically the `isFailure` asserts fail (script evaluates to `42` because the scrutinee was dropped) and the trace-count asserts fail with count 0. The `(uplc $ one).evaluate == 42` sanity asserts and possibly the "used scrutinee" test may already pass — that is fine; at least the four dropped-scrutinee assertions must fail. If ALL tests pass, STOP: the premise is wrong, re-read the audit entry and investigate before touching the compiler.

---

### Task 2: The fix — bind the scrutinee strictly

**Files:**
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/PatternMatchingCompiler.scala:1911-1925`
- Test: `scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala` (from Task 1)

**Interfaces:**
- Consumes: `SIR.LetFlags.None` (`scalus-plugin/src/main/shared/scala/scalus/compiler/sir/SIR.scala:334`, value `0`; NOT `scala.None`).
- Produces: pattern matches on non-variable scrutinees now emit a strict `SIR.Let`, which every backend lowers as `(λs. decisions) scrutinee`.

- [ ] **Step 1: Change the let flag**

In `scalus-plugin/src/main/scala/scalus/compiler/plugin/PatternMatchingCompiler.scala`, the `decisionsWithScrutinee` binding currently reads (lines 1911-1925):

```scala
        val decisionsWithScrutinee =
            if needsScrutineeLet then
                // For non-variable scrutinees, create a let binding
                SIR.Let(
                  List(
                    Binding(
                      ctx.scrutineeName,
                      parsedMatch.scrutineeTp,
                      parsedMatch.scrutinee
                    )
                  ),
                  decisions,
                  SIR.LetFlags.Lazy,
                  AnnotationsDecl.fromSrcPos(ctx.topLevelPos)
                )
```

Change `SIR.LetFlags.Lazy` to `SIR.LetFlags.None` and replace the comment so the constraint is documented at the site:

```scala
        val decisionsWithScrutinee =
            if needsScrutineeLet then
                // Scala evaluates the scrutinee strictly, exactly once, even when no
                // pattern inspects it — a Lazy let would let the lowering drop or
                // re-evaluate it (audit finding X1).
                SIR.Let(
                  List(
                    Binding(
                      ctx.scrutineeName,
                      parsedMatch.scrutineeTp,
                      parsedMatch.scrutinee
                    )
                  ),
                  decisions,
                  SIR.LetFlags.None,
                  AnnotationsDecl.fromSrcPos(ctx.topLevelPos)
                )
```

Do NOT touch any other `LetFlags.Lazy` in the file — the action/guard/decision-tree-ref lets (`addActionsGuardsDecRefsLet`) and column bindings must remain lazy.

- [ ] **Step 2: Run the new tests to verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.MatchScrutineeStrictnessTest"`

Expected: PASS (all 6 tests). sbt rebuilds the plugin and recompiles the new test file with it automatically. If a test still fails, capture the generated UPLC (`println(uplc.show)` temporarily) and diagnose before proceeding — do not weaken an assertion.

- [ ] **Step 3: Run the pattern-matching and plugin test suites**

Run:
```bash
sbtn "scalusJVM/testOnly scalus.compiler.*"
sbtn scalusPluginTests/test
```

Expected: everything green EXCEPT `PatternMatchAuditTest` (5 pre-existing intentional failures — findings M1-M3, unrelated to this change; leave them). If any *other* test fails, inspect it: a test that asserted the old dropped-scrutinee behavior or pinned exact budgets/terms for a match on a non-variable scrutinee may legitimately need its expectation updated — update it and note it for the commit message. A test failing on semantics (wrong value) means the fix broke something: stop and investigate.

---

### Task 3: Full regression sweep and ExUnits baselines

**Files:**
- Possibly modify: example tests that pin exact `ExUnits` (found via grep below).

**Interfaces:**
- Consumes: green suites from Task 2.
- Produces: fully green `sbtn quick` + example tests; updated baselines if generated code cost shifted.

- [ ] **Step 1: Clean-build the example tests (they JIT from SIR; incremental builds lie after plugin changes)**

```bash
sbtn scalusExamplesJVM/clean
sbtn scalusExamplesJVM/test
```

Expected: PASS. Matches on non-variable scrutinees now always emit `(λs. body) scrutinee`, so validators' ExUnits/script sizes can shift slightly in either direction.

- [ ] **Step 2 (conditional): Update pinned ExUnits baselines**

Only if Step 1 failed on exact-ExUnits assertions:

```bash
grep -rn "ScalaCompilerVersion.baseline" scalus-examples/
```

Each pinned test carries two baselines (`pre38`, `since38`) — one per Scala compiler generation (3.3.x and 3.8.x). Update the baseline for the generation the build currently runs on from the observed values in the failure message, then re-run the failing suites. The OTHER generation's baseline must be re-measured too before release: run the same tests under the other cross Scala version (check `build.sbt` for the cross versions; `sbtn "++<version>; scalusExamplesJVM/testOnly <failing suites>"`). If the other generation cannot be run in this session, update what can be measured and flag the remaining re-measurement explicitly in the commit message and to the user — do not guess values.

- [ ] **Step 3: Full quick pipeline**

```bash
sbtn quick
```

Expected: format + compile + jvm/testQuick green (modulo the intentional `PatternMatchAuditTest` failures — if `testQuick` picks them up, run the remaining suites explicitly and confirm they are the only failures).

---

### Task 4: Documentation and commit

**Files:**
- Modify: `docs/internal/UPLC_CORRECTNESS_AUDIT.md` (X1 entry and executive-summary row)

- [ ] **Step 1: Mark X1 fixed in the audit report**

In `docs/internal/UPLC_CORRECTNESS_AUDIT.md`:

1. In the executive summary table, change the X1 row status by appending ` — **FIXED**` to the finding text:

```markdown
| X1 | Wildcard-only match drops effectful scrutinee (both backends) — **FIXED** | major | confirmed (executed) |
```

2. In section "X. Cross-cutting", at the end of the X1 entry (after the paragraph ending "…silently deletes what users may rely on as a runtime check."), append:

```markdown
**Status: FIXED** — the scrutinee let is now strict (`SIR.LetFlags.None`) in
`PatternMatchingCompiler.compileDecisionTree`; regression suite:
`scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala`.
```

- [ ] **Step 2: Format, stage, commit**

```bash
sbtn scalafmtAll
git add scalus-plugin/src/main/scala/scalus/compiler/plugin/PatternMatchingCompiler.scala \
        scalus-core/jvm/src/test/scala/scalus/compiler/MatchScrutineeStrictnessTest.scala \
        docs/internal/UPLC_CORRECTNESS_AUDIT.md
git status   # verify nothing unexpected is staged; PatternMatchAuditTest.scala must NOT be staged
git commit -m "fix(plugin): evaluate match scrutinee strictly, matching Scala semantics

A non-variable scrutinee was bound with a Lazy let, so a match whose
decision tree never inspects it (e.g. wildcard-only) silently dropped
the scrutinee's evaluation, and a used scrutinee could be re-evaluated
per use site. Bind it strictly instead: (lam s decisions) scrutinee,
evaluated exactly once before the match, on all lowering backends.
Audit finding X1 in docs/internal/UPLC_CORRECTNESS_AUDIT.md."
```

Include any baseline/test-expectation updates from Task 2 Step 3 / Task 3 Step 2 in the same commit (add those files to the `git add` list) and mention them in the commit body if present.

- [ ] **Step 3: Verify working tree state**

Run: `git status`
Expected: clean except pre-existing untracked/intentional files (`PatternMatchAuditTest.scala`). Do not push unless asked; if pushing, rebase first (user commits to master in parallel).
