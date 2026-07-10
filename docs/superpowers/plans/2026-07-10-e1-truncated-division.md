# E1: `BigInt./` Truncated Division Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make on-chain `BigInt./` match Scala semantics by mapping it to `quotientInteger` (truncated, toward zero) instead of `divideInteger` (floored, toward −∞) — audit finding E1.

**Architecture:** The Scalus compiler plugin translates Scala's `BigInt` operators to Plutus builtins in `SIRCompiler.compileExpr` (one `case nme.DIV` site). Today `a / b` becomes `divideInteger(a, b)`, which floors: `-20 / 3` yields −7 on-chain but −6 on the JVM. Scala's `BigInt./` truncates — exactly Plutus's `quotientInteger`. The existing `%` → `remainderInteger` mapping already matches Scala, so switching `/` to `quotientInteger` also restores the Euclidean identity `(a/b)*b + a%b == a`, which the current mixed pair violates. TDD: write a semantics regression suite first (RED against the floored mapping), apply the one-word fix, verify GREEN, then run full clean-build regression.

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest + ScalaCheck, Scalus compiler plugin, UPLC evaluation via `PlutusVM`.

## Global Constraints

- The ONLY production change is `SIRBuiltins.divideInteger` → `SIRBuiltins.quotientInteger` at the `case nme.DIV` site in `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala` (~line 2042). Do NOT touch `nme.MOD` (→ `remainderInteger`, already correct), `Builtins.divideInteger`/`Builtins.modInteger` (explicit builtins stay floored), or any lowering/optimizer code.
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `scalus-core/jvm/src/test/scala/scalus/compiler/PatternMatchAuditTest.scala` are intentionally untracked. NEVER `git add` or commit them. `PatternMatchAuditTest` has 5 intentionally failing tests (audit findings M1–M3) — they are expected failures, not regressions; do not fix them.
- Commit directly to `master` (no branches/worktrees). Run `sbtn scalafmtAll` before committing. Rebase (`git pull --rebase`) before any push — the user commits in parallel.
- Never add `Co-Authored-By: Claude` or any similar trailer to commit messages.
- Cost sanity: `quotientInteger` and `divideInteger` have identical CPU/memory cost parameters in all shipped cost models (`builtinCostModelA..E.json`; only the diagonal-model type tag differs), so pinned ExUnits baselines are NOT expected to shift. If an ExUnits-pinning test fails anyway, re-measure on a clean build and update the baseline — and if the affected test uses `ScalaCompilerVersion.baseline(pre38, since38)`, re-measure BOTH generations (default Scala and `++3.8.4`).
- Example validator tests JIT from SIR and lie under incremental compilation after a plugin change: always `clean` the affected test projects before trusting results (`scalusExamplesJVM/clean`, and `scalusJVM/Test/clean` for core tests when in doubt).

---

### Task 1: Semantics regression suite (RED)

Write the failing test suite first, against the current (floored) mapping. This is the same TDD-RED pattern as `MatchScrutineeStrictnessTest.scala` (X1) — the file compiles fine; the assertions fail because on-chain results differ from Scala.

**Files:**
- Create: `scalus-core/jvm/src/test/scala/scalus/compiler/IntegerDivisionSemanticsTest.scala`

**Interfaces:**
- Consumes: `compile {…}` / `.toUplc()` / `Term.$` / `.evaluate` / `Term.asTerm` (via `Constant.LiftValue[BigInt]`), `PlutusVM.makePlutusV2VM()` — all existing.
- Produces: the regression suite Task 2 must turn GREEN without modifying its assertions.

- [ ] **Step 1: Write the failing tests**

```scala
package scalus.compiler

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Regression suite for audit finding E1: `BigInt./` must have Scala semantics on-chain.
  *
  * Scala's `BigInt./` truncates toward zero (= Plutus `quotientInteger`); Plutus
  * `divideInteger` floors toward negative infinity. The two differ whenever the
  * operands have opposite signs. `%` maps to `remainderInteger`, which already
  * matches Scala — so `/` and `%` together must satisfy the Euclidean identity
  * `(a/b)*b + a%b == a` on-chain, like they do on the JVM.
  */
class IntegerDivisionSemanticsTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    private lazy val divUplc = compile { (a: BigInt, b: BigInt) => a / b }.toUplc()
    private lazy val modUplc = compile { (a: BigInt, b: BigInt) => a % b }.toUplc()
    private lazy val euclidUplc = compile { (a: BigInt, b: BigInt) =>
        (a / b) * b + a % b
    }.toUplc()

    // All four sign combinations, exact division, and the audit's counterexample (-20, 3)
    private val cases: Seq[(BigInt, BigInt)] = Seq(
      (BigInt(7), BigInt(2)),
      (BigInt(-7), BigInt(2)), // floored: -4, Scala: -3
      (BigInt(7), BigInt(-2)), // floored: -4, Scala: -3
      (BigInt(-7), BigInt(-2)),
      (BigInt(-20), BigInt(3)), // audit E1: floored -7 vs Scala -6
      (BigInt(20), BigInt(-3)),
      (BigInt(-20), BigInt(4)), // exact division: both conventions agree
      (BigInt(0), BigInt(5))
    )

    test("BigInt./ matches Scala (truncated) division on-chain") {
        for (a, b) <- cases do
            assert(
              (divUplc $ a.asTerm $ b.asTerm).evaluate == (a / b).asTerm,
              s"$a / $b: expected ${a / b}"
            )
    }

    test("BigInt.% matches Scala remainder on-chain") {
        for (a, b) <- cases do
            assert(
              (modUplc $ a.asTerm $ b.asTerm).evaluate == (a % b).asTerm,
              s"$a % $b: expected ${a % b}"
            )
    }

    test("(a/b)*b + a%b == a holds on-chain (Euclidean identity)") {
        for (a, b) <- cases do
            assert(
              (euclidUplc $ a.asTerm $ b.asTerm).evaluate == a.asTerm,
              s"identity broken for ($a, $b)"
            )
    }

    test("BigInt./ and % match Scala for arbitrary operands (property)") {
        val operands = Gen.chooseNum(-1000L, 1000L).map(BigInt(_))
        forAll(operands, operands) { (a, b) =>
            whenever(b != 0) {
                assert((divUplc $ a.asTerm $ b.asTerm).evaluate == (a / b).asTerm)
                assert((modUplc $ a.asTerm $ b.asTerm).evaluate == (a % b).asTerm)
            }
        }
    }
}
```

- [ ] **Step 2: Run and verify RED**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.IntegerDivisionSemanticsTest"`
Expected: the `/`, Euclidean-identity, and property tests FAIL on opposite-sign cases (e.g. `-7 / 2` evaluates to −4 on-chain vs Scala's −3; `-20 / 3` gives −7 vs −6). The `%` test PASSES (mapping already correct). Record the exact failure output in the task report — this is the RED evidence.

- [ ] **Step 3: git add the new file** (do not commit yet — the fix commit in Task 2 carries it)

```bash
git add scalus-core/jvm/src/test/scala/scalus/compiler/IntegerDivisionSemanticsTest.scala
```

---

### Task 2: The fix + mapping-pin update (GREEN)

**Files:**
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala:2039-2050` (the `case nme.DIV`)
- Modify: `scalus-core/jvm/src/test/scala/scalus/compiler/CompilerPluginBuiltinsToSIRTest.scala:856` (pins the operator mapping)

**Interfaces:**
- Consumes: `SIRBuiltins.quotientInteger` (already defined in `scalus-plugin/src/main/shared/scala/scalus/compiler/sir/SIRBuiltins.scala`).
- Produces: `compile(a / b)` now emits `QuotientInteger`; Task 1's suite must pass unmodified.

- [ ] **Step 1: Apply the one-word fix**

In `SIRCompiler.scala`, `case nme.DIV` (~line 2042), change the builtin and leave a semantics comment:

```scala
            case nme.DIV =>
                // Scala's BigInt./ truncates toward zero = quotientInteger.
                // divideInteger floors toward -infinity and would diverge from the
                // JVM for opposite-sign operands (audit finding E1). Paired with
                // %/remainderInteger this preserves (a/b)*b + a%b == a.
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.quotientInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
```

- [ ] **Step 2: Update the pinned operator-mapping test**

In `CompilerPluginBuiltinsToSIRTest.scala` line 856, inside `test("compile BigInt ops")`:

```scala
        assert(compile(BigInt(1) / 2) ~=~ (QuotientInteger $ 1 $ 2))
```

(`QuotientInteger` comes from the already-imported `DefaultFun` values in that file — check the file's imports; line 872 in the same file already uses `QuotientInteger` for the explicit builtin, so it is in scope.)

Do NOT change line 870 (`Builtins.divideInteger` → `DivideInteger`) — explicit builtins keep floored semantics.

- [ ] **Step 3: Run the new suite and the mapping tests — verify GREEN**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.IntegerDivisionSemanticsTest scalus.compiler.CompilerPluginBuiltinsToSIRTest"`
Expected: PASS (all tests; Task 1's assertions unmodified). If the plugin change is not picked up, run `sbtn "scalusJVM/Test/clean"` first — the plugin jar feeds test compilation.

---

### Task 3: Full clean-build regression

The plugin change alters generated UPLC for every use of `/`, so incremental builds cannot be trusted (see Global Constraints). Cost parameters for the two builtins are identical, so ExUnits baselines should NOT move — any shift is a finding to investigate, not blindly re-pin.

**Files:**
- Modify (only if a pinned baseline legitimately shifts): the affected test file(s), re-measured per the dual-generation rule in Global Constraints.

- [ ] **Step 1: Clean-build core and run the full JVM core suite**

Run: `sbtn "scalusJVM/Test/clean" "scalusJVM/test"`
Expected: all pass EXCEPT the 5 intentional `PatternMatchAuditTest` failures (M1–M3 repros — expected, do not touch).

- [ ] **Step 2: Clean-build and run example tests (ExUnits/hash pins live here)**

Run: `sbtn "scalusExamplesJVM/clean" "scalusExamplesJVM/test"`
Expected: all pass. No example validator uses bare `/` (verified during planning), and cost params are identical, so no baseline churn is expected. If an ExUnits pin fails: confirm the delta is division-related (validator transitively uses a prelude helper with `/`), re-measure on the clean build, update the pin — dual generations (`sbtn "++3.8.4" ...`) if the test uses `ScalaCompilerVersion.baseline`.

- [ ] **Step 3: Run plugin tests**

Run: `sbtn "scalusPlugin/test"`
Expected: all pass.

- [ ] **Step 4: Format and quick-gate**

Run: `sbtn scalafmtAll` then `sbtn quick`
Expected: format applied; compile + testQuick green (modulo the 5 intentional audit-test failures).

---

### Task 4: Docs + commit

**Files:**
- Modify: `scalus-site/content/language-guide/constants-primitives.mdx` (~line 67 — the arithmetic example block)
- Modify: `docs/internal/UPLC_CORRECTNESS_AUDIT.md` — E1 section: append a Status line (this file stays UNTRACKED — edit it, never `git add` it)

**Interfaces:**
- Consumes: the committed fix from Tasks 1–3.
- Produces: the final commit on `master`.

- [ ] **Step 1: Add a semantics callout to the language guide**

In `constants-primitives.mdx`, after the arithmetic code block (the one showing `BigInt(20) / BigInt(4)`), add:

```mdx
<Callout type="info">
`/` and `%` follow Scala's semantics on-chain: truncated division (rounding toward zero),
compiled to the `quotientInteger` and `remainderInteger` builtins. They satisfy
`(a / b) * b + a % b == a`. If you need floor division (rounding toward negative infinity),
use `Builtins.divideInteger` and `Builtins.modInteger` explicitly.
</Callout>
```

(Match the exact `Callout` component usage already present in `builtin-functions.mdx:49-51`.)

- [ ] **Step 2: Mark E1 fixed in the (untracked) audit doc**

Append to the E1 section of `docs/internal/UPLC_CORRECTNESS_AUDIT.md`:

```markdown
**Status: FIXED** — `nme.DIV` now maps to `quotientInteger` (Scala truncated semantics,
coherent with `%` → `remainderInteger`) in `SIRCompiler.compileExpr`; regression suite:
`scalus-core/jvm/src/test/scala/scalus/compiler/IntegerDivisionSemanticsTest.scala`.
```

Do NOT `git add` this file.

- [ ] **Step 3: Commit and push**

```bash
git add scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala \
        scalus-core/jvm/src/test/scala/scalus/compiler/IntegerDivisionSemanticsTest.scala \
        scalus-core/jvm/src/test/scala/scalus/compiler/CompilerPluginBuiltinsToSIRTest.scala \
        scalus-site/content/language-guide/constants-primitives.mdx \
        docs/superpowers/plans/2026-07-10-e1-truncated-division.md
git status   # verify UPLC_CORRECTNESS_AUDIT.md and PatternMatchAuditTest.scala are NOT staged
git commit -m "fix(plugin): compile BigInt./ to quotientInteger, matching Scala semantics

BigInt./ previously mapped to divideInteger, which floors toward negative
infinity, so opposite-sign division gave different results on-chain than on
the JVM (-20 / 3 was -7 on-chain vs -6 in Scala). quotientInteger truncates
toward zero, exactly like Scala's BigInt./, and together with the existing
% -> remainderInteger mapping restores (a/b)*b + a%b == a.

Builtins.divideInteger / Builtins.modInteger remain available for explicit
floor-division semantics. Generated UPLC changes (and thus script hashes)
for any contract using /. Cost parameters of the two builtins are identical,
so budgets are unaffected.

Audit finding E1; regression suite IntegerDivisionSemanticsTest covers the
sign matrix, the Euclidean identity, and a randomized property check."
git pull --rebase && git push
```

---

## Notes for the release changelog (not part of this change)

When 0.18.3+ notes are written: call out that `/` on `BigInt` now compiles to `quotientInteger` (Scala-truncated) instead of `divideInteger` (floored). Behavior changes only for opposite-sign operands; script hashes change for any contract using `/`; use `Builtins.divideInteger` to keep floored semantics.
