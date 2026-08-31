# T5: let-chain regrouping (and the measured case for deferring T18)

Design for `CODEGEN_IMPROVEMENT_PLAN.md` task **T5** ("Let-chain regrouping into
case-constr headers"), plus the measurement that reorders T5 and **T18**
("Let-sinking of multi-use top-level bindings").

Status: design approved in chat 2026-08-31, not yet implemented.

## 1. Decision

Build **T5 first, alone, flag-gated**. Defer **T18** until T5 is measured.

The plan's suggested order put T18 first. A measurement over 11 validators
(section 3) inverts that: T5 is worth 5-25x more on large validators, and it is
the simpler pass — it needs no purity analysis, no evaluation-order change, and
no shared path machinery.

The `TermPaths` extraction (folding the duplicated placement machinery out of
`CommonSubexpressionElimination` and `CommonContextExtraction`) moves to the T18
step, which is the pass that actually needs it. T5 is spine-local.

`ForcedBuiltinsExtractor` stays. See section 7.

## 2. What T5 does

Aiken's `split_body_lambda`
(`aiken/crates/uplc/src/optimize/shrinker.rs:1520`, v1.1.23) — "the ultimate
function when used in conjunction with case_constr_apply". There is no Plutus
counterpart; the `case (constr 0 [...])` application encoding is a V3 trick, not
a PIR pass. T5 is **not** let-floating in either direction.

A chain of nested lets is re-associated into one multi-argument application:

```
-- before: 3 nested lets
[(lam a [(lam b [(lam c body) ec]) eb]) ea]

-- after T5: one 3-argument application
[[[(lam a (lam b (lam c body))) ea] eb] ec]

-- after the existing CaseConstrApply pass (unchanged, still last)
(case (constr 0 [ea, eb, ec]) [(lam a (lam b (lam c body)))])
```

T5 itself only re-associates. The `case`/`constr` encoding is left to
`CaseConstrApply`, which already fires on any application chain with 3+
arguments. That keeps T5 small and keeps the encoding decision in one place.

### Cost model

For a group of N bindings, counting CEK machine steps (100 mem / 16,000 cpu
each, per `CaseConstrApply`'s docstring and the `ecd` measurement recorded in
the plan under T5):

| form | steps |
|---|---|
| N nested lets | 2N — N `Apply` + N `LamAbs` |
| flattened + case-constr | N + 2 — `Case` + `Constr` + N `LamAbs` |

So a group of N saves **N − 2 steps per execution of that chain**: nothing at
N ≤ 2, and it can never lose. Script size is unchanged by the re-association
itself; the case-constr encoding costs about 1 byte per group (measured on
`ecd`, recorded in the plan).

## 3. Measured evidence

Method: compile each validator with its production `Options`, undo
`CaseConstrApply` on the result to recover the term the new passes would see,
then (a) partition every nested let chain into maximal contiguous
scope-independent runs and sum `max(0, N−2)`, and (b) list bindings whose uses
all sit below a `Case` branch. The probe is kept in the session scratchpad, not
in the repo; it is ~200 lines and easy to reconstruct from this document.

| validator | nodes | T18 candidates | crossing ≥2 branches | T5 steps saved |
|---|---:|---:|---:|---:|
| cape/linear_vesting | 678 | 2 | 1 | 3 |
| cape/htlc | 572 | 3 | 0 | 2 |
| cape/two_party_escrow | 817 | 3 | 2 | 1 |
| escrow | 1068 | 7 | 0 | 10 |
| betting | 2072 | 6 | 2 | 33 |
| auction | 2336 | 5 | 1 | 19 |
| editable_nft | 1040 | 6 | 1 | 3 |
| linked_list | 4060 | 5 | 0 | 72 |
| upgradeable_proxy | 665 | 4 | 2 | 2 |
| payment_splitter | 1357 | 5 | 0 | 6 |
| binocular/oracle | 6838 | 18 | 11 | 68 |
| **total** | | **64** | **20** | **219** |

The binocular oracle (`../binocular`, `BitcoinValidator`, scalus 1.1.1) is the
strongest witness for both passes, being the only validator in the corpus with a
real multi-action dispatch. Its longest chain partitions as
`2,2,1,2,6,6,6,6,2,2,2,2,1,9` — 23 steps from that chain alone.

At 100 mem / 16,000 cpu per step:

- `linked_list`: 72 steps = 7,200 mem / 1.15M cpu
- `binocular/oracle`: 68 steps = 6,800 mem / 1.09M cpu
- `betting`: 33 steps = 3,300 mem / 528,000 cpu

For scale, `linear_vesting`'s pinned partial-unlock budget is 63,888 mem /
31M steps, so a betting-sized win is roughly 5% memory and 1.7% steps.

**Two caveats.** These are node-count model numbers, not measured ExUnits; the
model's basis (case+constr = 2 steps regardless of arity) is the same one
`CaseConstrApply` already relies on and which the plan's `ecd` measurement
confirmed, but the pass must still be validated by a real before/after run. And
the totals are upper bounds: only chains on the executed path pay off.

## 4. Algorithm

A single top-down traversal. At any node matching `Apply(LamAbs(x, body), rhs)`:

1. **Collect the chain.** Walk down through `body` while it keeps matching the
   same shape, producing `[(x1, e1), (x2, e2), …, (xn, en)]` outermost-first,
   plus the final `body`.
2. **Partition into maximal contiguous runs.** Extend the current run with
   `(xi, ei)` while both hold:
   - `freeVars(ei)` contains none of the binders already in the run;
   - `xi` does not repeat a binder name already in the run.

   Otherwise close the run and start a new one at `(xi, ei)`. **No reordering** —
   Aiken moves bindings between groups to grow them, which changes evaluation
   order; the plan's research item 5 explicitly warns against copying that.
3. **Rebuild**, outermost run first:
   - run of 1 or 2: emit the original nested shape (no gain, so no churn);
   - run of N ≥ 3: emit `[[[…[(lam x1 (lam x2 … (lam xN inner))) e1] e2] … eN]`.
4. Recurse into every `ei` and into the final body.

Annotations on reused `Apply`/`LamAbs` nodes are preserved; new nodes are left
position-less, which `UplcPipeline.run` fills in afterwards via
`fillEmptyPosBottomUp` / `fillEmptyPosTopDown`.

### Why it is correct

**Evaluation order is preserved without any purity requirement.** The CEK
machine evaluates `[f a]` function-first, then argument. So
`[[[F e1] e2] e3]` evaluates `F` (a lambda — effect-free), then `e1`, then the
beta-reduction yields the next lambda (effect-free), then `e2`, then `e3` —
exactly the order of the nested form. Under the subsequent `case (constr 0
[e1,e2,e3])` encoding, `Constr` evaluates its fields left to right, so the order
holds there too. An `ei` that errors, traces, or diverges does so at the same
point relative to the others in both forms.

This is the key difference from T18: T5 needs no `isPure` guard, so it applies to
statement-lets (`require(...)` lowered to a binding with zero uses) as readily as
to value bindings.

**No capture, in either direction.** In the input, `ei` sits inside the scope of
`x1..x(i−1)`; in the output it sits outside. A free occurrence of `xj` (j < i) in
`ei` therefore refers to the let binder, and the dependency test rejects exactly
that case. If `ei` does not mention any run binder, moving it out of their scope
cannot change what any name resolves to. The repeated-name test removes the
remaining subtlety where two binders in one run share a name.

**Idempotent.** A run of 1 rebuilds to the identical term, so re-running the pass
is a no-op, and a chain that is already flat is left alone.

## 5. Pipeline and flag

`V3Optimizer` only. At V1/V2 the re-association is exactly step-neutral (N Apply
+ N LamAbs either way) because `CaseConstrApply` cannot run — `Case`/`Constr` are
illegal before Plutus V3.

```
Phase 4:  withCce |> letChainRegroup.apply |> caseConstr.apply
```

- `Options.letChainRegroup: Boolean = false` — flag-gated, default off, matching
  the `cceEnabled` precedent. This keeps the pinned-ExUnits baselines untouched
  until the numbers justify flipping it, which is a separate change that must
  re-measure on both compiler generations (3.3.7 and 3.8.4).
- `V3Optimizer` gains an **overloaded constructor** rather than a third
  parameter, so the existing 2-argument signature stays binary-compatible.
  Adding the `Options` field will still need a MiMa filter, as `cceEnabled` did.
- Plumbed through `UplcPipeline.defaultOptimizer` and `Compiled.scala:427`.

## 6. Testing

- `LetChainRegroupTest` (shared) — unit shapes, in the style of
  `CaseConstrApplyTest`: three independent lets flatten; a dependency splits the
  run; a repeated binder name splits the run; runs of 1 and 2 are returned
  unchanged; the pass is idempotent; nested chains inside `ei` are regrouped too.
- `LetChainRegroupCekConformanceTest` (jvm) — property-based differential test on
  the `EtaReduceCekConformanceTest` pattern: random closed terms biased toward let
  chains, evaluated before and after under a budget cap. Properties:
  - results are identical (not merely compatible — T5 preserves semantics
    exactly);
  - budget after ≤ budget before;
  - budget is *equal* when no run reaches 3.
- Corpus measurement, flag on vs off: the 10 scalus example/CAPE validators above
  plus `binocular/oracle`. Record ExUnits and script bytes. This is the number
  that decides whether the default flips.

## 7. Out of scope

- **T18 (let-sinking).** Deferred, with its measurement in section 3. Its rule is
  "sink only into positions entered at most once per execution" — `Case`
  branches, branch lambdas, let bodies, and the program's root parameter lambdas,
  but never a lambda in value position (a fixpoint, callback, or argument, where
  the work would be re-run per call) and never a `Delay` body. Its guard is
  `rhs.isPure`. Worth ~3 steps (300 mem / 48,000 cpu) per binding, on the paths
  that skip it: 1-2 bindings on small validators, 11 on the binocular oracle.
  It needs the `TermPaths` extraction first.
- **`ForcedBuiltinsExtractor` deletion.** FBE does something no other pass does:
  it hoists a forced builtin *out of a lambda body*, counting in-lambda uses as
  two. CSE cannot replace that — for two uses inside one lambda body, CSE's
  LCA placement rebinds per call (7 steps/call versus FBE's 2), an active
  regression. `V1V2Optimizer` also uses FBE and has no CSE at all. Revisit only
  as a measured phase 3.
- **Aiken-style reordering** to grow groups (see section 4, step 2).
- **Impure sinking**, which needs the plan's T6 `relaxedEvaluationOrder`.

## 8. Risks

- The step model could be wrong about `case (constr 0 [...])` arity costs. The
  corpus ExUnits run in section 6 is the check; if it disagrees, the pass stays
  off and this document records why.
- T5 makes every group one byte larger. On a validator with many groups this
  could offset the fee win through the Conway reference-script fee. Measured as
  script bytes in the same run.
- Flipping the default later churns every pinned ExUnits baseline. The
  `update-budgets` script cannot rewrite `Coin` fees, `assertResult`, or size
  pins, so that change needs a manual pass.
