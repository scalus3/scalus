# T1: Static-Argument Transformation for recursive functions

Date: 2026-08-05
Status: approved design, pending implementation
Task: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` T1

## Problem

After T2 (self-application recursion encoding), a recursive call `f a b c`
still pays one Apply + one Var evaluation per argument on every iteration,
even for arguments that never change. TM byte-parsing loops re-pass the
invariant `rawTx`/params each call; list folds re-pass the folding function.
Aiken lifts loop-invariant parameters out (`recursive_nonstatic_params`) and
this showed up as ~15% machine-step advantage on parsing paths with
identical builtin work.

`ScalusRuntime.genMapList` already does this transformation by hand
(`f` and `nil` captured in closure, only `lst` passed recursively). T1
automates it for all user code.

## Solution

A new SIR-to-SIR pass `StaticArgumentTransformation`, applied only when
optimization is enabled (`Options.optimizeUplc = true`, e.g.
`Options.release`), not unconditionally. It runs in `Compiled.toUplc`
(`uplc/Compiled.scala`) on the SIR before backend dispatch, next to the
existing `RemoveTraces` step:

```scala
val sir1 = if options.removeTraces then RemoveTraces.transform(sir) else sir
val sirToLower =
    if options.optimizeUplc then StaticArgumentTransformation(sir1) else sir1
```

The extension methods in `scalus-core/shared/src/main/scala/scalus/package.scala`
get the same gating, applied to the SIR before the backend match:

- `sir.toUplc(...)`: gate on its `optimizeUplc` parameter (callers can
  override the `Options` default), e.g.
  `val sirToLower = if optimizeUplc then StaticArgumentTransformation(sir) else sir`.
  `toUplcOptimized` delegates to `toUplc`, so it is covered.
- `sir.lowerToUplc(...)`: gate on `options.optimizeUplc`.

Debug builds (`optimizeUplc = false`) keep the source-shaped recursion.

File: `scalus-core/shared/src/main/scala/scalus/compiler/sir/StaticArgumentTransformation.scala`,
`def apply(sir: SIR): SIR`, structural recursion over the whole tree
(covers top-level linked defs and local `let rec`s embedded in bodies).

### Transformation rule

For each single-binding recursive let `let rec f = λp1…pn. body`:

1. Collect all self-occurrences of `f` inside the rhs. Both `SIR.Var` and
   `SIR.ExternalVar` forms, shadow-aware (the
   `MutualRecursionElimination.rewriteExpr` traversal pattern).
2. Skip the binding entirely if any self-occurrence is not a call saturated
   to full arity `n` (bare reference or partial application).
3. Position `i` is **static** iff every self-call passes exactly
   `SIR.Var(pi)` at position `i` and `pi` is not shadowed at that call
   site. A repeated constant does not count (GHC SAT definition).
4. If all `n` positions are static, demote the last one to changing
   (a nullary strict letrec diverges).
5. If zero positions are static: leave the binding unchanged.

Rewrite (changing params `q1…qk` keep their original relative order and
names; `f$sat` is the fresh inner name):

```
let f = λp1…pn.
    let rec f$sat = λq1…qk. body[ f e1…en := f$sat e_q1…e_qk ]
    in f$sat q1…qk
```

The outer binding keeps the original name, signature, and `@UplcRepr`
annotations, so all external uses (partial applications, higher-order
uses, eta-lets) are untouched. The outer let is non-recursive; only the
inner let carries the rec flag.

Polymorphic bindings: `typeParams` stay on the outer wrapper's `LamAbs`;
the inner letrec is a plain lambda over the changing params with free type
vars (same approach as `MutualRecursionElimination.pTp`). This covers
`List.foldLeft`-style folds, where the folding function is invariant.

### Interaction with the T2 closed-fixpoint constraint

T2 requires the self-application fixpoint to stay a closed subterm in
argument position for `PartialEvaluator` constant folding. After SAT the
inner fixpoint captures the static params, but the outer wrapper redex is
still a plain application: a fully closed recursive computation remains
foldable as the whole redex. Gate:
`SelfApplicationRecursionTest."optimizer can still constant-fold closed
recursive computations"` must stay green. Fallback if it regresses: skip
SAT when the rec rhs is closed (those computations fold away anyway).

### Known limitation (documented, follow-up)

The pass runs before `MutualRecursionElimination` (backend entries), so
the peers-as-params static arguments MRE introduces for mutual-recursion
groups are not lifted by this pass. Multi-binding rec lets are skipped.
Follow-up option: run a second SAT application after MRE, or emit
SAT-shaped output from MRE directly.

Runtime helpers built directly with `lvLetRec` at the LoweredValue level
(e.g. `genArrayToList`) are out of scope.

## Testing

- `StaticArgumentTransformationTest` (template:
  `MutualRecursionEliminationTest`): SIR-shape assertions + evaluation on
  all three backends. Cases: basic lift; interleaved static/changing
  params; all-static demotion of the last param; undersaturated self-call
  skip; bare-reference skip; shadowed param not static; `ExternalVar`
  self-calls; polymorphic (typeParams) binding; zero-static no-op;
  non-rec and multi-binding lets untouched.
- Proof-record microbenchmark in `ExprSizeAndBudgetTest`: hand-written
  SAT vs non-SAT counting loop with 2 invariant args, pinned per-call
  cpu/mem delta (template: the T2 `compareEncodings` record at
  `ExprSizeAndBudgetTest.scala:221-333`).
- Existing guards stay green: `SelfApplicationRecursionTest`,
  `MutualRecursionTest`, `PlutusConformanceTest`.
- Budget re-pins across pinned suites via `scripts/update-budgets.py`
  plus the known manual tail; re-measure dual `ScalaCompilerVersion`
  baselines (pre38/since38) where used. Expected: drops on
  Knights/Clausify and prelude fold-heavy tests. Only suites compiling
  with `optimizeUplc = true` move; debug-mode pins are unaffected.
- Gating test: same program compiled with `Options.debug` shows no SAT
  wrapper in the SIR/UPLC; with `Options.release` it does.

## Validation

ExUnits regression on recursive-heavy benchmarks (list folds, bytestring
parsing). CPU should drop several percent, more with many invariant
params. The pinned proof-record quantifies the per-call delta exactly.
