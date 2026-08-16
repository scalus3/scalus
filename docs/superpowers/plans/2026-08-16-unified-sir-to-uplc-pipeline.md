# Unified SIR -> UPLC Pipeline Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One SIR->UPLC pipeline function used by both `CompiledPlutus.toUplc`
and the `sir.toUplc`/`lowerToUplc` extensions, fixing the four drift bugs and
hoisting `MutualRecursionElimination` so SAT runs after it.

**Architecture:** New `object UplcPipeline` in `scalus.compiler.sir.lowering`
with `run(sir, options, language, optimizer): Term` implementing
removeTraces? -> MutualRecursionElimination -> StaticArgumentTransformation?
-> lower(backend) -> optimize? -> fill positions. All existing entry points
delegate to it. Backends keep their internal MRE calls as safety nets.

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest. Spec:
`docs/superpowers/specs/2026-08-16-unified-sir-to-uplc-pipeline-design.md`.

## Global Constraints

- Commit directly to `master`; no branches, no PR.
- Run `sbtn scalafmtAll` before every commit.
- Conventional commit messages. NEVER add a `Co-Authored-By: Claude` trailer.
- Public signatures of `sir.toUplc`, `toUplcOptimized`, `lowerToUplc`, and
  the `CompiledPlutus` constructor/fields must not change (MiMa; baseline
  1.0.0, zero filters - keep it that way).
- `optimizeUplc` default stays `false` - the flip is explicitly out of scope.
- Backends keep their internal `MutualRecursionElimination` calls
  (`SirToUplcV3Lowering.scala:28`, `BaseSimpleLowering.scala:52`) - do NOT
  remove them.
- `RemoveRecursivity` stays in `SIRLinker` - do NOT touch the linker.
- Test commands: `sbtn "scalusJVM/testOnly <classes>"`, full run `sbtn quick`.
  Known infra quirk: background `sbtn` runs can show zero output for hours -
  run foreground with a `timeout`, and if the server disconnects, retry with
  plain `sbt`.

## Reference facts (verified 2026-08-16)

- `Options` fields (`scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala`):
  `targetLoweringBackend`, `targetLanguage: Language = Language.PlutusV3`,
  `targetProtocolVersion`, `generateErrorTraces`, `removeTraces`,
  `optimizeUplc`, `uplcOptimizers: Seq[Optimizer]`, `cseIterations: Int = 2`,
  `cceEnabled: Boolean = false`, `debugLevel`, `debug`, `addScalusTag`,
  `warnListConversions`, `noWarn`.
- `class V3Optimizer(cseIterations: Int = 2, cceEnabled: Boolean = false)
  extends Optimizer`; `class V1V2Optimizer extends Optimizer` - both in
  `scalus-core/shared/src/main/scala/scalus/uplc/transform/OptimizerPipelines.scala`.
- `ScottEncodingLowering(sir, generateErrorTraces = false, targetLanguage =
  Language.PlutusV3, targetProtocolVersion = vanRossemPV)`; same shape for
  `SumOfProductsLowering`. Their `targetLanguage` default equals
  `Options.targetLanguage`'s default, so passing it explicitly is
  byte-identical for default-options callers.
- The reference implementation to copy verbatim is
  `CompiledPlutus.toUplc` (`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala:89-135`),
  including the position back-fill comment block.
- SAT emits an inner binding named `<name>$sat`; MRE emits peers named
  `<name>$mutrec`. UPLC name sanitization maps `$` to `_`, so rendered
  terms show `_sat` / `_mutrec`.

---

### Task 1: UplcPipeline + CompiledPlutus delegation (byte-identical refactor)

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/UplcPipeline.scala`
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala:88-135` (replace `toUplc` body)
- Create: `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/UplcPipelineTest.scala`

**Interfaces:**
- Produces: `UplcPipeline.run(sir: SIR, options: Options, language: Language,
  optimizer: Optimizer): Term` and
  `UplcPipeline.defaultOptimizer(language: Language, options: Options): Optimizer`.
  Task 2 calls exactly these.

- [ ] **Step 1: Write the failing differential test**

```scala
package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Language
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.uplc.transform.V3Optimizer

class UplcPipelineTest extends AnyFunSuite {

    private val releaseNoTag = Options(
      generateErrorTraces = false,
      removeTraces = true,
      optimizeUplc = true
    )

    test("CompiledPlutus.program and UplcPipeline.run produce the same term") {
        given Options = releaseNoTag
        val compiled = PlutusV3.compile { (d: Data) =>
            val x = d.to[BigInt]
            scalus.cardano.onchain.plutus.prelude.require(x > BigInt(0))
        }
        val direct = UplcPipeline.run(
          compiled.sir,
          compiled.options,
          Language.PlutusV3,
          new V3Optimizer(compiled.options.cseIterations, compiled.options.cceEnabled)
        )
        assert(compiled.program.term == direct)
    }
}
```

Adjust the compiled body to whatever compiles cleanly in this test scope
(any small validator-shaped function works; `prelude.require` import per
other shared tests). The assertion that matters: `program.term == direct`
with `addScalusTag = false`.

- [ ] **Step 2: Run, verify it fails**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.UplcPipelineTest"`
Expected: compile error "not found: UplcPipeline".

- [ ] **Step 3: Create UplcPipeline**

Copy the body of `CompiledPlutus.toUplc` verbatim, then add the MRE step:

```scala
package scalus.compiler.sir.lowering

import scalus.cardano.ledger.Language
import scalus.compiler.Options
import scalus.compiler.sir.lowering.simple.{ScottEncodingLowering, SumOfProductsLowering}
import scalus.compiler.sir.{
    MutualRecursionElimination,
    RemoveTraces,
    SIR,
    StaticArgumentTransformation,
    TargetLoweringBackend
}
import scalus.uplc.Term
import scalus.uplc.transform.{Optimizer, V1V2Optimizer, V3Optimizer}

/** The single SIR -> UPLC pipeline, shared by [[scalus.uplc.CompiledPlutus]] and the
  * `sir.toUplc` extensions:
  *
  * removeTraces? -> MutualRecursionElimination -> StaticArgumentTransformation?
  * -> lower(backend) -> optimize? -> fill positions
  *
  * MutualRecursionElimination is unconditional (backends reject multi-binding recursive
  * lets); running StaticArgumentTransformation after it lifts the peers-as-params static
  * arguments MRE introduces. The backends keep their own MRE calls as safety nets for
  * direct construction - MRE is idempotent, so the second call is a no-op walk.
  *
  * ScalusTag is NOT applied here - it is a program-level concern
  * ([[scalus.uplc.CompiledPlutus.program]]).
  */
object UplcPipeline {

    def run(sir: SIR, options: Options, language: Language, optimizer: Optimizer): Term = {
        val sir1 = if options.removeTraces then RemoveTraces.transform(sir) else sir
        val sir2 = MutualRecursionElimination(sir1)
        val sirToLower =
            if options.optimizeUplc then StaticArgumentTransformation(sir2) else sir2
        val uplc = options.targetLoweringBackend match
            case TargetLoweringBackend.ScottEncodingLowering =>
                ScottEncodingLowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion
                ).lower()
            case TargetLoweringBackend.SumOfProductsLowering =>
                SumOfProductsLowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion
                ).lower()
            case TargetLoweringBackend.SirToUplcV3Lowering =>
                SirToUplcV3Lowering(
                  sir = sirToLower,
                  generateErrorTraces = options.generateErrorTraces,
                  debug = options.debug,
                  warnListConversions = options.warnListConversions,
                  noWarn = options.noWarn,
                  targetLanguage = language,
                  targetProtocolVersion = options.targetProtocolVersion,
                  intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
                  supportModules = IntrinsicResolver.defaultSupportModules
                ).lower()
        val optimized =
            if options.uplcOptimizers.nonEmpty then
                options.uplcOptimizers.foldLeft(uplc)((term, opt) => opt(term))
            else if options.optimizeUplc then optimizer(uplc)
            else uplc
        // Give every still-position-less node a source location, so profiling and
        // source-traces can attribute the cost of generated/optimized spines (the UPLC
        // optimizer rebuilds Apply/Case/Constr nodes without positions). Run on the FINAL
        // term, after optimization: bottom-up so a spine node inherits the location of the
        // leaf it operates on, then top-down to fill any node with no positioned descendant
        // from its nearest positioned ancestor. Positions never affect flat encoding,
        // budget, or evaluation - only diagnostics.
        optimized.fillEmptyPosBottomUp._1.fillEmptyPosTopDown(scalus.utils.ScalusSourcePos.empty)
    }

    /** The optimizer `run` should use when the caller has no version-specific one:
      * V1/V2 -> [[V1V2Optimizer]] (V3Optimizer's CaseConstrApply emits Case/Constr
      * terms that are illegal before Plutus V3); otherwise [[V3Optimizer]] configured
      * from the options.
      */
    def defaultOptimizer(language: Language, options: Options): Optimizer =
        language match
            case Language.PlutusV1 | Language.PlutusV2 => new V1V2Optimizer()
            case _ => new V3Optimizer(options.cseIterations, options.cceEnabled)
}
```

- [ ] **Step 4: Replace `CompiledPlutus.toUplc` body with delegation**

In `Compiled.scala`, replace the whole `protected def toUplc: Term = { ... }`
body (lines 89-135) with:

```scala
    /** Lowers the SIR to UPLC using the configured backend and applies optimization if enabled. */
    protected def toUplc: Term =
        scalus.compiler.sir.lowering.UplcPipeline.run(sir, options, language, optimizer)
```

Remove now-unused imports from `Compiled.scala` (`RemoveTraces`,
`StaticArgumentTransformation`, `ScottEncodingLowering`,
`SumOfProductsLowering`, `SirToUplcV3Lowering` - keep what's still used).

- [ ] **Step 5: Run the differential test + pin-heavy guards**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.UplcPipelineTest scalus.uplc.eval.ExprSizeAndBudgetTest scalus.compiler.sir.lowering.MutualRecursionTest scalus.compiler.sir.lowering.SelfApplicationRecursionTest"`
Expected: all PASS. The Compiled path gained the MRE-before-SAT ordering,
but `PlutusVX.compile` output previously went backend-MRE-then-SAT-never...
note: SAT ran before backends already, so the ONLY change for Compiled is
that SAT now sees MRE output for mutual-recursion code. Prelude-heavy code
has zero mutual groups (measured), so these suites must not move. If
`ExprSizeAndBudgetTest` moves, something is wrong - stop and investigate.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add -A scalus-core
git commit -m "refactor(sir): extract UplcPipeline, delegate CompiledPlutus.toUplc"
```

---

### Task 2: Rewire package.scala extensions (fixes the 4 bugs)

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/package.scala:36-120`
- Modify: `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/UplcPipelineTest.scala`

**Interfaces:**
- Consumes: `UplcPipeline.run` / `UplcPipeline.defaultOptimizer` from Task 1.
- Produces: unchanged public signatures; `lowerToUplc` deprecated.

- [ ] **Step 1: Write the failing bug tests**

Append to `UplcPipelineTest`. Fixtures: build small SIR via `compile {}`
so all backends can lower it (an integer function with a `Boolean` match
exercises encoding differences).

```scala
    import scalus.compiler.compile
    import scalus.compiler.sir.TargetLoweringBackend

    private def fixtureSir = compile { (x: BigInt) => if x > BigInt(0) then x else -x }

    test("bug 1: the backend parameter switches the backend") {
        val v3 = fixtureSir.toUplc()(backend = TargetLoweringBackend.SirToUplcV3Lowering)
        val scott = fixtureSir.toUplc()(backend = TargetLoweringBackend.ScottEncodingLowering)
        assert(v3 != scott, "backend parameter must not be ignored")
    }

    test("bug 2: generateErrorTraces parameter reaches the V3 backend") {
        val sirWithReq = compile { (x: BigInt) =>
            scalus.cardano.onchain.plutus.prelude.require(x > BigInt(0), "positive")
        }
        val traced = sirWithReq.toUplc()(generateErrorTraces = true)
        val untraced = sirWithReq.toUplc()(generateErrorTraces = false)
        assert(traced != untraced, "generateErrorTraces must not be ignored on V3 backend")
    }

    test("bug 3: V1-targeted optimization uses V1V2Optimizer (no Case/Constr terms)") {
        import scalus.uplc.Term
        def containsCaseOrConstr(t: Term): Boolean = t match
            case Term.Case(scrut, cases) => true
            case Term.Constr(_, _)       => true
            case Term.Apply(f, a)        => containsCaseOrConstr(f) || containsCaseOrConstr(a)
            case Term.LamAbs(_, b)       => containsCaseOrConstr(b)
            case Term.Force(b)           => containsCaseOrConstr(b)
            case Term.Delay(b)           => containsCaseOrConstr(b)
            case _                       => false
        val opts = Options(
          targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
          targetLanguage = scalus.cardano.ledger.Language.PlutusV1,
          targetProtocolVersion = scalus.cardano.ledger.MajorProtocolVersion.plominPV
        )
        val term = fixtureSir.toUplc(using opts)(optimizeUplc = true)
        assert(!containsCaseOrConstr(term), "V1 scripts must not contain Case/Constr")
    }

    test("bug 4: given Options.release strips traces on the toUplc path") {
        val sirWithLog = compile { (x: BigInt) =>
            scalus.cardano.onchain.plutus.prelude.log("marker-string")
            x
        }
        val plain = sirWithLog.toUplc(using Options())()
        val release = sirWithLog.toUplc(using Options.release)()
        assert(plain.show.contains("marker-string"))
        assert(!release.show.contains("marker-string"), "removeTraces must be honored")
    }

    test("optimized mutual recursion gets its peer parameters lifted") {
        // reuse the hand-built even/odd fixture pattern from
        // scalus-core/shared/src/test/scala/scalus/compiler/sir/MutualRecursionEliminationTest.scala
        // (evenOddGroup / callIsEven) - copy those helpers here.
        val sir: SIR = evenOddGroup(callIsEven(4))
        val optimized = sir.toUplc()(optimizeUplc = true)
        val plain = sir.toUplc()(optimizeUplc = false)
        // MRE peer binding survives in both; the SAT fixpoint only in the optimized one
        assert(optimized.show.contains("_sat") || optimized.show.contains("$sat"))
        assert(!(plain.show.contains("_sat") || plain.show.contains("$sat")))
        optimized.evaluateDebug match
            case s: scalus.uplc.eval.Result.Success =>
                assert(s.term == scalus.uplc.Term.Const(scalus.uplc.Constant.Bool(true)))
            case f => fail(s"optimized mutual recursion failed: $f")
    }
```

Match `Term` constructor arities to the actual ADT
(`scalus-core/shared/src/main/scala/scalus/uplc/Term.scala`) - adjust the
`containsCaseOrConstr` patterns as needed. The mutual-recursion test needs
`given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)`
in the suite for `evaluateDebug`, and the SirDSL helpers copied from
`MutualRecursionEliminationTest.scala:14-52`.

- [ ] **Step 2: Run, verify bugs 1-4 fail and the mutual-rec test fails**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.UplcPipelineTest"`
Expected: bug 1 FAILS (backend ignored -> terms equal), bug 2 FAILS on the
V3 path, bug 3 FAILS (V3Optimizer used -> Constr present) or errors, bug 4
FAILS (traces kept), mutual-rec FAILS (no `$sat` - SAT ran before MRE).

- [ ] **Step 3: Rewire the extensions**

Replace `toUplc` and `lowerToUplc` bodies in `package.scala` (keep exact
signatures); delete the now-unused backend imports:

```scala
        def toUplc(using
            options: Options = Options()
        )(
            generateErrorTraces: Boolean = options.generateErrorTraces,
            backend: TargetLoweringBackend = options.targetLoweringBackend,
            optimizeUplc: Boolean = options.optimizeUplc,
            debug: Boolean = options.debug
        ): Term = {
            val eff = options.copy(
              generateErrorTraces = generateErrorTraces,
              targetLoweringBackend = backend,
              optimizeUplc = optimizeUplc,
              debug = debug
            )
            UplcPipeline.run(
              sir,
              eff,
              eff.targetLanguage,
              UplcPipeline.defaultOptimizer(eff.targetLanguage, eff)
            )
        }

        @deprecated("use toUplc instead", "1.0.0")
        def lowerToUplc(using options: Options = Options()): Term = toUplc(using options)()
```

`toUplcOptimized` stays as-is (already delegates to `toUplc`). Import
`scalus.compiler.sir.lowering.UplcPipeline`. Check the latest git tag with
`git describe --tags --abbrev=0` and use it (without `v`) as the deprecation
version if it is no longer `v1.0.0`.

- [ ] **Step 4: Run the suite, verify all green**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.UplcPipelineTest scalus.compiler.sir.StaticArgumentTransformationTest scalus.compiler.sir.lowering.MutualRecursionTest scalus.compiler.sir.lowering.SelfApplicationRecursionTest scalus.compiler.sir.MutualRecursionEliminationTest"`
Expected: all PASS. Note the SAT gating test in
`StaticArgumentTransformationTest` ("SAT is applied only when optimizeUplc
is on") must stay green - the pipeline preserves that gate.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -A scalus-core
git commit -m "fix(sir): route sir.toUplc/lowerToUplc through UplcPipeline

Fixes four drift bugs: ignored backend parameter, generateErrorTraces
ignored on the V3 backend, hardcoded V3Optimizer (wrong for V1/V2 and
ignoring cseIterations/cceEnabled/uplcOptimizers), and missing
removeTraces + position back-fill. SAT now runs after
MutualRecursionElimination, lifting peers-as-params static arguments."
```

---

### Task 3: Full verification, re-pins, docs

**Files:**
- Modify: budget-pin literals (driven by failures)
- Modify: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (T1 limitation note)
- Modify: `docs/superpowers/specs/2026-08-16-unified-sir-to-uplc-pipeline-design.md` (status line)

- [ ] **Step 1: Clean full run**

```bash
sbtn clean
sbtn quick
```

Expected mismatches are confined to: the ~6 files calling `toUplc` under a
`given Options.release` (now trace-stripped), any V1/V2-optimized `toUplc`
users (optimizer switch), and mutual-recursion tests (peer lifting).
Bare-default `toUplc()` output must be byte-identical - a broad wave of
mismatches means a pipeline bug, not a re-pin situation: stop and compare
`sirToLower` between old and new paths on one failing case.

- [ ] **Step 2: Re-pin moved budgets**

```bash
python3 scripts/update-budgets.py
```

Manual tail for what the script cannot parse (known gaps): `Coin` fee
literals, `assertResult`-style pins, script-size pins, ping-ponging shared
literals in `ListTest`/`SortedMapTest` (fix by exact line). Verify with a
second `sbtn quick`.

- [ ] **Step 3: Dual-baseline check**

If any `ScalaCompilerVersion.baseline(pre38, since38)` file moved, re-measure
the since38 arm for the affected suites:

```bash
sbt -Dsbt.supershell=false "++3.8.4 scalusExamplesJVM/test"
```

and update the since38 values from actual measurements (never copy pre38).

- [ ] **Step 4: Update docs**

- `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`, T1 "Known limitation"
  paragraph: mark the mutual-recursion half resolved - "Resolved 2026-08-16:
  the unified `UplcPipeline` runs SAT after `MutualRecursionElimination`, so
  peers-as-params arguments are lifted." Keep the `genArrayToList`
  (lvLetRec runtime helpers) half as still-open.
- Spec status line: `Status: implemented`.

- [ ] **Step 5: Format and final commit**

```bash
sbtn scalafmtAll
git add -A
git commit -m "refactor(sir): unified pipeline - budgets re-pinned, docs updated"
```

---

## Self-review notes

- Spec coverage: pipeline object + step order incl. MRE hoist (Task 1),
  CompiledPlutus delegation (Task 1), extension rewiring + 4 bug fixes +
  lowerToUplc deprecation (Task 2), differential + bug + mutual-rec-lift
  tests (Tasks 1-2), Scott/SoP targetLanguage verification (resolved:
  defaults align; guarded by Task 1 step 5 pin suites), re-pin + dual
  baselines + doc updates (Task 3). Backends keep MRE; linker untouched
  (Global Constraints).
- No placeholders: all code blocks are concrete; the two "adjust to actual
  ADT" notes point at exact files and are shape-adaptations, not omissions.
- Type consistency: `UplcPipeline.run(sir, options, language, optimizer)`
  used identically in Tasks 1 and 2; `defaultOptimizer(language, options)`
  defined in Task 1, consumed in Task 2.
