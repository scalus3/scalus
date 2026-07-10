# E3: `copy` Drops the Receiver Expression Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop `x.copy(...)` from silently dropping the receiver expression when every field is overridden explicitly — audit finding E3.

**Architecture:** The `copy` desugaring is compiled in `SIRCompiler` by two match arms (`:3346-3349`) that call `compileNewConstructor` on the copy's arguments and never compile the receiver `obj`. When some fields are defaulted, the receiver survives because the Scala compiler passes `obj.copy$default$N` getters (compiled to `Select(objSir, field)` at `:3269-3293`) — and for an impure receiver dotty first lifts it to a synthetic stable val, so `obj` at the copy site is a stable identifier. But when **every** field is explicit, there are no `copy$default$N` getters, dotty does not lift the receiver, and it is dropped entirely: `trace("recv")(Box(x)).copy(a = x+1).a` logs nothing on-chain; `validate(datum).copy(a = 1)` where `validate` throws succeeds instead of failing. The fix compiles the receiver and, when it is **not pure** (`tpd.isPureExpr`), sequences it before the `Constr` with a strict `SIR.Let` — the same mechanism the X1 fix used, which `LetNonRecLoweredValue.termInternal` emits as `(λ_. constr) receiver`, evaluating the receiver exactly once even though the body never references it. Pure receivers (the overwhelmingly common `localVal.copy(...)`, and dotty's synthetic stable receiver in the defaulted case) are left untouched, so no code-size/budget regression on ordinary copies.

**Tech Stack:** Scala 3 compiler plugin (dotty API), sbt (`sbt`), ScalaTest, Scalus V3 lowering, UPLC evaluation via `PlutusVM`.

## Global Constraints

- The production change is confined to the two `copy` match arms in `compileExpr2` (`SIRCompiler.scala:3346-3349`) plus one new private helper. Do NOT change the `copy$default$N` handling (`:3269-3293`), `compileNewConstructor`, or any lowering code.
- Sequence the receiver ONLY when `tpd.isPureExpr(obj)` is false. `tpd.isPureExpr` is `dotty.tools.dotc.ast.tpd.isPureExpr(tree)(using Context)` (from `TreeInfo`); the plugin already imports `dotty.tools.dotc.ast.tpd`. If the exact name differs in the pinned compiler, fall back to a manual stable-receiver check: `obj.isInstanceOf[tpd.Ident] || obj.isInstanceOf[tpd.This]` (sequence otherwise). Erring toward sequencing is safe (only a minor size cost); erring toward dropping reintroduces the bug.
- The strict let must use `SIR.LetFlags.None` (strict, non-recursive) — NOT `Lazy` (a lazy unused binding is dropped by the lowering, reintroducing E3, exactly the X1 failure mode).
- Never name the synthetic binding `"args"` — `SIR.Let` throws `RuntimeException("QQQ")` on a single binding named `args` (debug landmine at `SIR.scala:350-352`). Use `"_copyReceiver"` (optionally suffixed with the source line).
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `scalus-core/jvm/src/test/scala/scalus/compiler/PatternMatchAuditTest.scala` are intentionally untracked. NEVER `git add` or commit them. `PatternMatchAuditTest`'s 5 failing tests are by-design M1–M3 repros — expected, do not fix.
- This changes generated UPLC (and script hashes) only for `copy` with a non-pure receiver — rare. Any ExUnits pin that shifts must be re-measured on a clean build (dual generations via `sbt "++3.8.4 …"` where the test uses `ScalaCompilerVersion.baseline`).
- Commit directly to `master`. Run `sbt scalafmtAll` before committing. Rebase (`git pull --rebase`) before pushing. No `Co-Authored-By: Claude` trailer.
- Example validator tests JIT from SIR and lie under incremental compilation after a plugin change: `clean` the affected test projects before trusting results.

---

### Task 1: Reproduce E3 (RED)

**Files:**
- Create: `scalus-core/jvm/src/test/scala/scalus/compiler/CopyReceiverEvalTest.scala`

**Interfaces:**
- Consumes: `compile {…}.toUplc(generateErrorTraces = true)`, `Term.$`, `.evaluateDebug` → `Result` (`.isSuccess/.isFailure/.logs`), `Builtins.trace[A](s: String)(a: A): A`, `PlutusVM.makePlutusV3VM()`. Same surface as `MatchScrutineeStrictnessTest`.

- [ ] **Step 1: Write the failing tests**

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

/** Audit finding E3: `x.copy(...)` must evaluate the receiver `x` exactly once (Scala semantics),
  * even when every field is overridden explicitly. The receiver's traces/errors must not be
  * dropped.
  */
class CopyReceiverEvalTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    case class Box(a: BigInt, b: BigInt)

    // All fields explicit → no copy$default getter references the receiver → the receiver was
    // dropped. Scala evaluates it once, so the trace must appear exactly once.
    test("copy with all fields explicit evaluates the receiver exactly once") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("recv")(Box(x, x)).copy(a = x + BigInt(1), b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(
          result.logs.count(_.contains("recv")) == 1,
          s"expected receiver evaluated once, logs=${result.logs}"
        )
        assert((uplc $ one).evaluate == 2.asTerm) // a = 1 + 1
    }

    // All fields explicit, receiver expression itself errors (impure, not hoisted to a val). Scala
    // evaluates it eagerly, so the script must fail. The bug drops the receiver, so it succeeds.
    // NOTE (execution): a `val boom: Box = throw ...; boom.copy(...)` form is a WEAK test — the
    // error fires at the val binding and `boom` is a pure Ident, so it passes without exercising
    // E3. Put the throw directly in receiver position so the receiver is impure.
    test("copy with all fields explicit fails when the receiver errors") {
        val compiled = compile { (x: BigInt) =>
            ((throw new RuntimeException("boom")): Box).copy(a = x, b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluateDebug.isFailure)
    }

    // Guard: a defaulted field keeps the receiver referenced via copy$default; the fix must NOT
    // double-evaluate it. Trace count stays exactly 1. (Passes before and after — regression guard.)
    test("copy with a defaulted field evaluates the receiver exactly once") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("recv")(Box(x, x)).copy(a = x + BigInt(1)).b
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(
          result.logs.count(_.contains("recv")) == 1,
          s"expected receiver evaluated once, logs=${result.logs}"
        )
    }

    // Control: pure receiver (a local val) needs no sequencing and behaves correctly.
    test("copy on a pure receiver works") {
        val compiled = compile { (x: BigInt) =>
            val box = Box(x, x)
            box.copy(a = x + BigInt(1), b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 2.asTerm)
    }
}
```

- [ ] **Step 2: Run and verify RED**

Run: `sbt "scalusJVM/testOnly scalus.compiler.CopyReceiverEvalTest"`
Expected: the "all fields explicit" trace test FAILS (`recv` count 0 — receiver dropped) and the erroring-receiver test FAILS (script succeeds instead of failing). The defaulted-field guard and the pure-receiver control PASS. Record the exact counts/outcomes as RED evidence. If the erroring-receiver snippet doesn't compile (the `throw`-typed-as-`Box` form is rejected by the plugin), substitute an equivalent erroring receiver — e.g. wrap a failing branch: `Builtins.trace("recv")(if x == BigInt(0) then Box(x, x) else Box(x, x)).copy(...)` is not erroring; instead use `scalus.cardano.onchain.fail("boom"): Box` if available — and note the substitution.

- [ ] **Step 3: git add the new file**

```bash
git add scalus-core/jvm/src/test/scala/scalus/compiler/CopyReceiverEvalTest.scala
```

---

### Task 2: Sequence the receiver (GREEN)

**Files:**
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala` (add a helper near `compileNewConstructor` ~`:968`; rewrite the two copy arms at `:3346-3349`)

**Interfaces:**
- Consumes: `compileNewConstructor`, `compileExpr`, `tpd.isPureExpr`, `Binding`, `SIR.Let`, `SIR.LetFlags.None`, `AnnotationsDecl.fromSrcPos` (all already imported/in scope).
- Produces: `copy` on a non-pure receiver sequences the receiver before the `Constr`; Task 1's tests pass unmodified.

- [ ] **Step 1: Add the helper**

Near `compileNewConstructor` (e.g. after `:1002`):

```scala
    /** Compile `obj.copy(args)`. The Scala compiler desugars copy to pass every field (explicit
      * and defaulted) as an argument, so we build the new instance with `compileNewConstructor`.
      * When every field is explicit there is no `copy$default$N` getter referencing `obj`, so the
      * receiver would be dropped — but Scala evaluates it once, eagerly. Sequence a non-pure
      * receiver before the Constr with a strict let so its traces/errors are preserved (audit
      * finding E3). Pure receivers (local vals, and the synthetic stable receiver dotty lifts when
      * default getters are used) need no sequencing.
      */
    private def compileCaseClassCopy(
        env: Env,
        obj: Tree,
        nakedType: Type,
        fullType: Type,
        args: List[Tree],
        tree: Tree
    ): AnnotatedSIR = {
        val constr = compileNewConstructor(env, nakedType, fullType, args, tree)
        if tpd.isPureExpr(obj) then constr
        else
            val objSir = compileExpr(env, obj)
            SIR.Let(
              List(Binding(s"_copyReceiver_${tree.srcPos.line}", objSir.tp, objSir)),
              constr,
              SIR.LetFlags.None,
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
    }
```

- [ ] **Step 2: Route both copy arms through the helper**

Replace `SIRCompiler.scala:3346-3349`:

```scala
            case Apply(TypeApply(Select(obj, nme.copy), targs), args) if isCaseClassInstance(obj) =>
                compileCaseClassCopy(env, obj, obj.tpe.widen.dealias, tree.tpe.widen, args, tree)
            case Apply(Select(obj, nme.copy), args) if isCaseClassInstance(obj) =>
                compileCaseClassCopy(env, obj, obj.tpe.widen.dealias, tree.tpe.widen, args, tree)
```

(`targs` is unused in the helper — the copy result type comes from `tree.tpe.widen`, matching the pre-fix `compileNewConstructor` calls.)

- [ ] **Step 3: Run the reproducer — verify GREEN**

Run: `sbt "scalusJVM/testOnly scalus.compiler.CopyReceiverEvalTest"`
Expected: 4/4 PASS (receiver traced once; erroring receiver fails; defaulted guard and pure control still pass) with Task 1's assertions unmodified. If the plugin change isn't picked up, run `sbt "scalusJVM/Test/clean"` first.

---

### Task 3: Full clean-build regression

The change alters generated UPLC only for `copy` with a non-pure receiver. `.copy` on plain vals (pure) is unchanged, so broad baseline churn is not expected.

- [ ] **Step 1: Clean-build core and run the full JVM core suite**

Run: `sbt "scalusJVM/Test/clean; scalusJVM/test"`
Expected: all pass EXCEPT the 5 intentional `PatternMatchAuditTest` failures.

- [ ] **Step 2: Clean-build and run example + ledger tests (ExUnits/hash pins)**

Run: `sbt "scalusExamplesJVM/clean; scalusExamplesJVM/test; scalusCardanoLedgerJVM/test"`
Expected: all pass. If an ExUnits pin shifts, confirm it's a contract with a non-pure copy receiver, re-measure on the clean build, update the pin (dual generations if it uses `ScalaCompilerVersion.baseline`).

- [ ] **Step 3: Run plugin tests + quick gate**

Run: `sbt scalafmtAll` then `sbt "scalusPlugin/test; quick"`
Expected: format applied; plugin tests pass; compile + testQuick green (modulo the 5 intentional audit-test failures).

---

### Task 4: Docs + commit

**Files:**
- Modify: `docs/internal/UPLC_CORRECTNESS_AUDIT.md` — E3 section + summary-table row (UNTRACKED — edit, never `git add`).

- [ ] **Step 1: Mark E3 fixed in the (untracked) audit doc**

Append to the E3 section:

```markdown
**Status: FIXED** — `SIRCompiler` now compiles the copy receiver and sequences it before the
`Constr` with a strict let when it is not `tpd.isPureExpr` (audit finding E3); regression suite:
`scalus-core/jvm/src/test/scala/scalus/compiler/CopyReceiverEvalTest.scala`.
```

Add `— **FIXED**` to the E3 row in the summary table (matching the X1/E1/E2 rows).

- [ ] **Step 2: Commit and push**

```bash
git add scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala \
        scalus-core/jvm/src/test/scala/scalus/compiler/CopyReceiverEvalTest.scala \
        docs/superpowers/plans/2026-07-10-e3-copy-receiver.md
# plus any ExUnits baseline files legitimately re-measured in Task 3
git status   # verify UPLC_CORRECTNESS_AUDIT.md and PatternMatchAuditTest.scala are NOT staged
git commit -m "fix(plugin): evaluate copy receiver, matching Scala semantics

x.copy(...) compiled only the copy's field arguments and never the receiver
obj. When some fields are defaulted, obj survives via copy\$default\$N getters
(and dotty lifts an impure receiver to a stable val first), but when every
field is overridden explicitly there is no getter referencing obj and it was
dropped entirely: trace(\"recv\")(Box(x)).copy(a = x+1) logged nothing, and a
copy whose receiver throws succeeded on-chain instead of failing.

compileCaseClassCopy now sequences a non-pure receiver (tpd.isPureExpr false)
before the Constr with a strict let, so it is evaluated exactly once, matching
Scala. Pure receivers (local vals, dotty's synthetic stable receiver) are
untouched, so ordinary copies are unaffected.

Audit finding E3; regression suite CopyReceiverEvalTest."
git pull --rebase && git push
```

---

## Notes

- Out of scope: `x.copy(a = 1)` with **multiple** defaulted fields on an impure-but-not-lifted receiver could evaluate the receiver once per `copy$default$N` getter — a duplicate-evaluation concern in the L3/L7 family, not the drop bug E3 targets. If Task 1's defaulted guard ever shows a count > 1, report it as a separate finding rather than widening this fix.
- Release notes (later): generated UPLC / script hashes change for contracts that `copy` a non-pure receiver with all fields explicit; such copies previously dropped the receiver's effects.
