# T2: Self-application recursion encoding (design)

Date: 2026-08-03. Branch: `feature/codegen-improvements`.
Task T2 of `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`.

## Goal

Replace the shared Z-combinator encoding of single self-recursion in the V3
lowering with direct self-application. Every recursive call currently pays
6 extra machine steps through the Z fixpoint dispatch.

## Scope decisions (agreed)

- V3 lowering only (`SirToUplcV3Lowering` / `LetRecLoweredValue`). The
  legacy simple backend (`BaseSimpleLowering`) keeps its Z encoding.
- Default on, no option flag. The full test corpus plus budget re-pin is
  the validation.
- Mutual recursion is NOT in scope. It becomes a follow-up task (T2b):
  a self-applied dispatcher with a Scott-encoded chooser, which also lifts
  the `Lowering.scala:588-591` restriction and the sum-helper limitation
  noted in `SirToUplcV3Lowering.scala:56-59`.

## Measured proof

`ExprSizeAndBudgetTest` ("T2 proof" tests, commit `4cb129bf9`) compares
both encodings on identical raw UPLC terms at PV11 mainnet costs:

- 6 machine steps saved per recursive call: 96,000 cpu / 600 mem.
- Countdown loop n=100k: -13.8% cpu, -20.7% mem. 2-arg sum loop: -10.6%
  cpu, -16.6% mem.
- Cheaper even at n=0 and when the body references the function twice.
- Script size: -12 bytes (the Z term disappears).
- Results are term-identical in every case.

## Encoding

Current (`LetRecLoweredValue.termInternal`, `LoweredValue.scala:997-1016`):

```
(λf. body) (__Z (λf. rhs))          -- __Z bound once at program root
```

New (v2, as implemented):

```
(λf. body) ((λf. f f) (λf. rhs[Var(f) := f f]))
```

- The same binder name (`newVar.id`) is reused for all three lambdas.
  UPLC named-DeBruijn conversion resolves the shadowing. No fresh-name
  machinery.
- `rhs[Var(f) := f f]`: every free occurrence of the recursive variable in
  the generated rhs term is replaced by `Apply(Var(f), Var(f))`.
- `body` still receives the fixed function as a plain value, bound once.

**Why v2 and not the originally-presented
`(λf. (λf. body) (f f)) (λf. rhs')`:** implementation surfaced a real
regression with the v1 shape. Under Z, `(λf. body) (Z rhs)` lets the
Inliner inline the single-use fixpoint application, producing a closed,
trace-free redex that `PartialEvaluator` constant-folds (e.g.
`List.single(1).last` folded to a constant: 2128 mem / 691881 steps).
In v1 the fixpoint variable is used twice inside the wrapper, the
Inliner declines, and the fold never fires (+142% mem on that test).
v2 keeps the fixpoint `(λf. f f) rhs'` as a self-contained closed
subterm in argument position - the same top-level shape as Z - so the
fold still fires. Guarded by the "optimizer can still constant-fold
closed recursive computations" test in `SelfApplicationRecursionTest`.

## Implementation

1. **`LetRecLoweredValue.termInternal`** (`LoweredValue.scala`): emit the
   new shape. Keep `generatedVars` handling as is.
2. **Substitution helper**: private `substituteVar(term: Term, name:
   String, replacement: Term): Term` in `LoweredValue.scala`. Structural
   recursion; stops descending under a `LamAbs` that rebinds `name`.
   Capture-safe: `newVar.id` is globally unique and the replacement
   contains only that variable.
3. **Delete dead Z machinery** in the V3 path:
   - root wrapper `SirToUplcV3Lowering.scala:113` and the
     `zCombinatorNeeded = false` init at `:128`;
   - `LoweringContext.zCombinatorNeeded` (`LoweringContext.scala:9`);
   - all writes: `Lowering.scala:554`,
     `ScalusRuntime.scala:24,70,76,91,750,936` (conservative flags only -
     every runtime helper defines its recursion via `lvLetRec`, so they
     inherit the new encoding automatically).
   - Verified: the only `"__Z"` references in the V3 path are the two
     sites above.
4. **Untouched**: `ExprBuilder.ZTerm`/`Z`/`z`/`rec` (public API, MiMa,
   used by the legacy backend); `lvLetRec` builder signature; the legacy
   simple backend and its own `zCombinatorNeeded` flag.

## Semantics

No observable change. `(f f)` at let-entry unrolls the rhs lambda once -
the same eager step Z performs today. A non-lambda recursive rhs diverges
under both encodings. Evaluation order, errors, traces unchanged; only
machine-step counts drop.

## Validation plan (TDD order)

1. New lowering test first: compile a recursive function via the V3
   pipeline; assert no `__Z` in the emitted term, self-application shape
   present, evaluation result unchanged, budget lower than the old pinned
   value.
2. `ExprSizeAndBudgetTest."Recursion cost"` pin (cpu=128352, mem=702 per
   recursion) must drop; re-pin.
3. `sbtn quick`; then `scripts/update-budgets.py` to re-pin ExUnits
   literals repo-wide. Dual-baselines policy applies: re-measure on both
   Scala 3.3.x and 3.8.x generations.
4. Record before/after Knights/Clausify/CAPE budgets in the commit
   message; every budget change must be a decrease.
5. `sbtn mima` - expected clean (no public signature changes).

## Risks

- Substitution bug: caught by the full corpus - every recursive function
  in every test exercises the new path.
- CSE hoisting the repeated `f f`: would surface as a budget regression in
  step 4, not a correctness issue; investigate only if seen.

## Implementation outcome (2026-08-03, commit 9a32b5227)

- Landed with encoding v2 (see above). All JVM suites green on Scala
  3.3.8; MiMa clean with zero filters - `LoweringContext.zCombinatorNeeded`
  kept as a deprecated no-op first ctor param because removing it shifted
  all 17 default-argument accessors (29 MiMa errors).
- Headline improvements: Knights 8x8 -19.8% mem / -16.1% cpu; CAPE
  fibonacci_25 fee -23%; two_party_escrow fees -4..5%; HelloCardano and
  HTLC scripts -11 B each; prelude List/SortedMap/Math ops -5..12% cpu.
- Accepted regressions: unoptimized MintingPolicy script 885 -> 903 B
  (+2%, per-call-site `f f` growth; the optimized variant did not grow),
  and one escrow refund path +0.6% cpu alongside -4.7% mem (net fee
  still lower).
- `scripts/update-budgets.py` gaps found while re-pinning (worth fixing
  in T11e): rewrites ExUnits literals BY VALUE across all files, so
  different call sites sharing one old value get stomped to one site's
  new value and the loop ping-pongs; cannot parse `Coin(...)` fee pins,
  `assertResult(ExUnits(...))`, `had size X` on indirect literals,
  KnightsTest's `assertBudgetClose` tolerance message; does not cover
  `bloxbean-cardano-client-lib`.
