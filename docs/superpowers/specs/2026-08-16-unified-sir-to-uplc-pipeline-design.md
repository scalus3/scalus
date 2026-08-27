# Unified SIR -> UPLC pipeline

Date: 2026-08-16
Status: implemented
Scope: unification only. Flipping `optimizeUplc` to true by default is
explicitly out of scope (measured data recorded below for when it happens).

## Problem

Three copies of the SIR -> UPLC pipeline exist and have drifted:

1. `CompiledPlutus.toUplc` (`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala:89-135`)
   - the user-facing path; the most complete.
2. `sir.toUplc(...)` extension (`scalus-core/shared/src/main/scala/scalus/package.scala:36-68`)
   - the test-facing path; stale.
3. `sir.lowerToUplc` (`package.scala:95-120`) - a third copy with zero users
   outside the file itself.

The drift left four bugs in the `package.scala` side:

1. The `backend` parameter of `toUplc` is silently ignored - shadowed by
   `val backend = options.targetLoweringBackend` on the first line.
2. The `generateErrorTraces` parameter is ignored for the V3 backend
   (`SirToUplcV3Lowering.fromOptions` reads `options.generateErrorTraces`);
   it only works for the Scott/SoP backends.
3. The optimizer is hardcoded `V3Optimizer()`: `options.cseIterations`,
   `options.cceEnabled` and `options.uplcOptimizers` are ignored, and
   `V3Optimizer`'s `CaseConstrApply` emits `Case`/`Constr` terms that are
   illegal in Plutus V1/V2 - optimizing a Scott-lowered V1/V2 script through
   this path can produce an invalid script. `CompiledPlutus` correctly
   selects `V1V2Optimizer` per language.
4. `options.removeTraces` is not honored and the position back-fill
   (`fillEmptyPosBottomUp`/`fillEmptyPosTopDown`, needed for profiling
   attribution) is missing.

## Solution

One pipeline function, one place. New object
`scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/UplcPipeline.scala`
(name bikesheddable at review):

```scala
package scalus.compiler.sir.lowering

object UplcPipeline {

    /** The single SIR -> UPLC pipeline:
      * removeTraces? -> StaticArgumentTransformation? -> lower(backend)
      * -> optimize? -> fill positions.
      *
      * Every step is driven by `options`; `language` selects the backend's
      * target language and the default optimizer; `optimizer` is what runs
      * when `options.optimizeUplc` is set and `options.uplcOptimizers` is
      * empty (uplcOptimizers, when non-empty, replace it - same semantics
      * as CompiledPlutus today).
      */
    def run(sir: SIR, options: Options, language: Language, optimizer: Optimizer): Term

    /** V1/V2 -> V1V2Optimizer; otherwise V3Optimizer(cseIterations, cceEnabled). */
    def defaultOptimizer(language: Language, options: Options): Optimizer
}
```

Step order:

1. `RemoveTraces.transform` when `options.removeTraces`.
2. `MutualRecursionElimination` - unconditional (correctness, not
   optimization: backends reject multi-binding recursive lets). Hoisted
   into the pipeline from the backend entry points, where all three
   backends currently duplicate the call (`SirToUplcV3Lowering.scala:28`,
   `BaseSimpleLowering.scala:52`).
3. `StaticArgumentTransformation` when `options.optimizeUplc`. Running it
   *after* MutualRecursionElimination closes the T1 known limitation: the
   peers-as-params arguments MRE emits (`fip f1 .. f(i-1)` on every
   self-call) are textbook static arguments and now get lifted. Caveat: this
   only applies to peers that are *also* self-recursive; a purely
   cross-recursive peer's self-reference is a bare argument, which SAT
   refuses to lift by design (see the delta bullet below).
4. Backend lowering per `options.targetLoweringBackend`, passing
   `targetLanguage = language` and all V3 options (`generateErrorTraces`,
   `debug`, `warnListConversions`, `noWarn`, default intrinsic/support
   modules).
5. `options.uplcOptimizers` fold when non-empty; else `optimizer` when
   `options.optimizeUplc`; else nothing.
6. Position back-fill on the final term.

**The backends keep their internal `MutualRecursionElimination` calls** as
safety nets: the backend classes are public API and are constructed
directly by `sir.toLoweredValue()` and many lowering tests, which would
otherwise crash on mutual-recursion input. MRE is idempotent (it only
rewrites `isRec && bindings.size >= 2` lets and emits single-binding
ones), so the second call on pipeline-fed input is a no-op tree walk -
negligible next to lowering cost. The backend-internal calls can be
dropped in a later major version.

ScalusTag stays in `CompiledPlutus.program` - it is a program-level concern,
not part of term generation.

### Call-site changes

- `CompiledPlutus.toUplc` becomes
  `UplcPipeline.run(sir1?, options, language, optimizer)` - purely
  mechanical, byte-identical output. The stored `optimizer` field is kept
  (public API).
- `sir.toUplc(params...)` keeps its exact signature (MiMa + source compat)
  and folds its parameters into the options before delegating:

  ```scala
  def toUplc(using options: Options = Options())(
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
      UplcPipeline.run(sir, eff, eff.targetLanguage,
        UplcPipeline.defaultOptimizer(eff.targetLanguage, eff))
  }
  ```

  This fixes bugs 1-4 as a side effect: the parameters now actually reach
  the pipeline.
- `toUplcOptimized` keeps delegating to `toUplc` (unchanged).
- `lowerToUplc` gets `@deprecated("use toUplc instead", "1.0.0")` and
  delegates to `toUplc` (zero users; signature kept for MiMa). Side effect:
  the old body hardcoded `debug = false` (it called
  `SirToUplcV3Lowering.fromOptions(sirToLower, options)` without a `debug`
  argument, falling through to that method's own `debug: Boolean = false`
  default, never threading `options.debug` through); delegating to `toUplc`
  means it now honors `options.debug` like every other path.
  Diagnostics-only delta.

## Considered and rejected: moving RemoveRecursivity out of SIRLinker

`RemoveRecursivity` looks similar (a SIR pass invoked from a fixed spot) but
is the opposite case. The linker marks every linked top-level def
`Recursivity` conservatively (`SIRLinker.scala:385`) and the final
`RemoveRecursivity(linked)` call (`:480`) strips the flag where the binding
is not actually self-referencing - the linker cleaning up its own
conservative over-flagging. Moving it into the pipeline would hand every
*other* consumer of linked SIR dirty flags: `compile{}` results and the
public `CompiledPlutus.sir` would show `rec let` on every top-level def,
and `sir.toLoweredValue()` (which bypasses the pipeline) would lower every
non-recursive def through a dead self-application fixpoint. Rule of thumb:
the pipeline owns consumer-side normalization (MRE, SAT); the linker owns
producing clean SIR (accurate rec flags). A pipeline-side RemoveRecursivity
safety net was also rejected: SAT already skips spuriously-rec lets, and
both producers (linker, plugin) emit accurate flags.

## Expected behavioral deltas (unification only)

Bounded and intentional:

- The ~6 test files calling `toUplc` under a `given Options = Options.release`
  start honoring `removeTraces` - their budgets/pins may move (down).
- `toUplcOptimized`/`toUplc` callers whose `given Options` sets
  `generateErrorTraces = true` while passing `generateErrorTraces = false` as
  the explicit parameter (bug 2) now get the parameter honored on the V3
  backend instead of the `given Options` value winning - traces are no
  longer generated where the caller asked for them to be suppressed; pins
  may move (down). This bucket, not peer lifting, is what moved all 8
  `ClausifyTest`/`KnightsDataTest` pins measured in Task 3: both suites do
  exactly this, and neither contains any mutual recursion.
- V1/V2-targeted optimization through `toUplc` switches from `V3Optimizer`
  to `V1V2Optimizer` - a correctness fix; affected pins may move.
- `toUplc` output gains source positions (diagnostics only; no effect on
  flat encoding, budget, or evaluation).
- Bare `sir.toUplc()` under default `Options()` stays byte-identical:
  `removeTraces = false`, `uplcOptimizers = empty`, and `V3Optimizer()`
  defaults equal `V3Optimizer(cseIterations = 2, cceEnabled = false)`.
  (MRE moving before the pipeline's SAT step is invisible here: with
  `optimizeUplc = false` the pipeline's MRE output is what the backend's
  internal MRE call produced anyway.)
- Optimized code containing top-level mutual recursion changes: SAT now
  lifts the peer parameters MRE introduces, but only for peers that are
  *also* self-recursive - a purely cross-recursive peer's self-reference is
  a bare argument, which SAT refuses to lift by design. Measured
  2026-08-16: six prelude-heavy programs contain zero multi-binding
  recursive lets, so the movement is expected to be limited to
  mutual-recursion tests (`MutualRecursionTest` budget ceiling and
  similar). Confirmed in Task 3: `ClausifyTest`/`KnightsDataTest` contain no
  mutual recursion at all, so their 8 pin movements are entirely the
  `generateErrorTraces` bucket above, not this one.
- One thing to verify during implementation, not assume: the Scott/SoP
  backends previously used their default `targetLanguage` parameter on this
  path; the unified pipeline passes `options.targetLanguage` explicitly.
  If the defaults differ, Scott/SoP `toUplc` output changes - check and
  pin with a differential test.

## Testing

- New `UplcPipelineTest`:
  - differential: `PlutusV3.compile(code).program.term` vs
    `sirOf(code).toUplc(...)` under identical options - identical terms;
  - `backend` parameter actually switches the backend (was bug 1);
  - `generateErrorTraces = true` reaches the V3 backend (was bug 2);
  - optimized V1-language output contains no `Case`/`Constr` terms (was
    bug 3);
  - `given Options.release` strips traces on the `toUplc` path (was bug 4);
  - an optimized mutual-recursion program (even/odd) shows lifted peers
    (a `$mutrec` binding whose `$sat` fixpoint takes only the changing
    param) and evaluates correctly on all three backends.
- Full `sbtn quick` + re-pin the small set of moved budgets;
  re-measure both compiler generations where dual baselines are touched.

## Out of scope, recorded for later: optimize-by-default

Measured 2026-08-16: 897 bare `.toUplc()` calls across 249 test files; 116
files already pin `optimizeUplc = false` explicitly; `PlutusVX.compile`
takes `using opts: Options` with no default given, so users always choose
explicitly there. The flip is one line (`SIRDefaultOptions.optimizeUplc`,
plugin copy auto-synced by the build) plus a shape-test audit (structure
assertions break semantically, unlike budget pins) plus a full re-pin.
