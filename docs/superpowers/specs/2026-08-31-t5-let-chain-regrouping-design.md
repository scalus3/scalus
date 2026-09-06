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

T5 is the *regrouping* that feeds an encoding Scalus already has. Keeping the two
apart matters, because they have different origins — see section 9.

- The **`case (constr 0 [...])` application encoding** (`CaseConstrApply`) is
  Alexander Nemish's, published 2025-01-02.
- The **let-chain regrouping that feeds it** is Aiken's `split_body_lambda`
  (`aiken/crates/uplc/src/optimize/shrinker.rs:1520`, v1.1.23), added 2025-01-11
  — "the ultimate function when used in conjunction with case_constr_apply".
  That is what T5 implements.

There is no Plutus counterpart to either: the encoding is a Plutus V3 trick, not
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

So a group of N saves **N − 2 steps per execution of that chain**.

**Steps are not the whole fee, and this sets the threshold.** The encoding also
changes script size, and below N = 7 the two pull in opposite directions.

In the flat encoding an application chain is pure tags, `4N` bits. The
case-constr form pays fixed framing first — `Case` tag 4, `Constr` tag 4,
constructor index 8, field-list framing `N+1`, branch-list framing 2 — so
`19 + N` bits:

```
Δbits = (19 + N) − 4N = 19 − 3N
```

Measured against the encoder for N = 1..12, which reproduces every row exactly:

| N | Δ bits | steps saved | net @1 exec/tx | net @3 exec/tx |
|---:|---:|---:|---:|---:|
| 2 | +13 | 0 | −24.38 | −24.38 |
| **3** | **+10** | **1** | **−11.83** | **+2.02** |
| 4 | +7 | 2 | +0.72 | +28.42 |
| 5 | +4 | 3 | +13.27 | +54.81 |
| 6 | +1 | 4 | +25.82 | +81.21 |
| **7** | **−2** | **5** | **+38.37** | **+107.60** |
| 10 | −11 | 8 | +76.01 | +186.79 |

(lovelace; mem 0.0577, steps 0.0000721, reference script 15/byte)

So a machine step is worth 6.92 lovelace *per execution* while a byte costs 15
lovelace *per transaction*, and the break-even depends on how often the chain
runs. For a script executed once it sits just under N = 4. From N = 7 the
encoding is smaller *and* faster, so no analysis is needed.

`LetChainRegroup.MinRunSize` is **5**, not the theoretical 4, because the N = 4
margin (+0.72) does not survive measurement — flat is bit-packed, so 7
theoretical bits round up to a whole byte in the encoded script. Section 3.1 has
the corpus comparison.

The full cost model — size derivation, the fee break-even in executions per
transaction, the rounding argument, and the decision rule for all arities — lives
in `docs/internal/CASE_CONSTR_COST_MODEL.md`, which is the canonical source. It
applies to `CaseConstrApply` as much as to this pass.

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

### 3.1 Post-implementation measurement (2026-09-01)

The implemented pass reproduces the probe's prediction exactly — 151 steps across
the ten examples at a threshold of 3 — which cross-validates the model. Real
ExUnits agree too: with the flag on, `linear_vesting`'s CAPE scenarios move
63,888 → 63,588 mem and 31,013,223 → 30,965,223 steps (exactly the 3 predicted
steps), and `full_unlock` moves 23,422 → 23,322 mem (1 step).

Measuring script size then showed the model was **incomplete**, and the fix is
the N ≥ 5 threshold:

Measured on master `853b589ec` (rebased after the CSE/CCE determinism fix, which
shifted the surrounding encoding and made the groups cheaper than the first
measurement suggested):

| threshold | steps saved | bytes added | net fee | validators made worse |
|---|---:|---:|---:|---:|
| 3 | 151 | +64 | +85 lovelace | **7 of 10** |
| 4 | 115 | +19 | +511 lovelace | 1 of 10 |
| **5** | **77** | **+0** | **+533 lovelace** | **0 of 10** |

Threshold 4 is where the model and the measurement part company: the per-group
cost is 7 bits in theory but rounds up to a whole byte in a bit-packed script, so
`payment_splitter` pays 1.5 bytes per group and regresses by 17 lovelace.
Threshold 5 nets more in total, costs no bytes at all across the corpus, *and*
leaves every validator no worse off — the property worth having for a default.

At a threshold of 3 the pass is a net loss on `htlc`, `two_party_escrow`,
`escrow`, `editable_nft`, `upgradeable_proxy` and `payment_splitter`: each pays
more in reference-script bytes than it saves in execution. Raising the threshold
keeps half the steps and drops 92% of the bytes, because the long runs turn out
to be nearly byte-free (`betting`: 5 groups, 23 steps, **+0 bytes**).

Per-validator at threshold 5 — only four validators have any run long enough to
qualify, and none regress:

| validator | groups | steps | bytes | net lovelace/tx |
|---|---:|---:|---:|---:|
| linked_list | 10 | 38 | +1 | +248 |
| betting | 5 | 23 | **−1** | +174 |
| auction | 3 | 12 | +0 | +83 |
| escrow | 1 | 4 | +0 | +28 |
| the other six | 0 | 0 | 0 | 0 |

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
   - run below `MinRunSize` (5): emit the original nested shape, so the pass adds
     no churn where it would not pay;
   - run of N ≥ 5: emit `[[[…[(lam x1 (lam x2 … (lam xN inner))) e1] e2] … eN]`.
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

**Idempotent.** A short run rebuilds to the identical term, so re-running the
pass is a no-op, and a chain that is already flat is left alone.

**The budget guarantee holds on the pair of passes, not on this one alone.**
Re-association by itself can cost extra steps when a bound expression fails:
`[[[F e1] e2] e3]` enters all three `Apply` nodes before evaluating `e1`, where
the nested form enters one. The `case (constr 0 [...])` encoding removes that,
being two steps at any arity. So `LetChainRegroup` must never ship without
`CaseConstrApply` after it — which the pipeline placement guarantees, and the
conformance test asserts on the composed pair.

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

**What was actually built** (all green, 418 tests in `scalus.uplc.transform.*`):

- `LetChainRegroupTest` (shared, 12 cases) — the shape rules, plus two cases
  pinning the `MinRunSize` threshold and two pinning the `V3Optimizer` wiring in
  both flag states.
- `LetChainRegroupCekConformanceTest` (jvm, 3 cases) — the two properties, plus a
  **vacuity guard** asserting that the generator actually produces terms the pass
  regroups (≥20 of 200) and terms that get cheaper (≥5 of 200). Without it both
  properties could pass on a generator the pass never touches.
- Corpus measurement: reproduce by compiling each validator twice under
  `Options.release` and `Options.release.copy(letChainRegroup = true)`, comparing
  `program.cborByteString.size` and summing the pass's own log lines. Real ExUnits
  come from flipping the `Options.letChainRegroup` default to `true` and reading
  the pinned-budget failures in `scalusExamplesJVM/testOnly scalus.examples.cape.*`.

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

## 9. Provenance of the case-constr application encoding

Recorded here because the record otherwise lives only in a tweet, and because
section 2 would otherwise credit the wrong project.

The technique — applying an N-argument function as `(case (constr 0 [a1..aN]) f)`
instead of `(apply .. (apply f a1) .. aN)`, cheaper for N >= 3 because `Case` and
`Constr` cost two machine steps at any arity — was published by Alexander Nemish
on 2025-01-02 at 20:01 UTC
([@atlanter](https://x.com/atlanter/status/1874909022056505670)):

> I've discovered an interesting optimization in Plutus V3 using Sums Of
> Products. In UPLC you call a function with 2 arguments like this:
> `(apply (apply f a1) a2)`. But with Sums of Products in Plutus V3 you can also
> call it like this: `(case (constr 0 [a1, a2]) f)`

Sums-of-products had been available since the Chang hard fork in September 2024.
The three implementations, normalized to UTC:

| when (UTC) | what |
|---|---|
| 2025-01-02 20:01 | the disclosure above |
| 2025-01-02 22:15 | Scalus `48b81870b` "test: Plutus V3 SoP optimization evaluation" — both encodings benchmarked for flat size and CEK budget, with the threshold recorded in comments: "apply is more efficient for n=1", "same efficiency for n=2", "sop is more efficient for n=3 and more" |
| 2025-01-03 03:21 | Plutarch `ba8dc235` (Seungheon Oh) "Optimize applications with SOP, expand application inlining rule"; PR [#795](https://github.com/Plutonomicon/plutarch-plutus/pull/795) opened 03:28. `Plutarch/Internal/Term.hs`: `length args <= 2` -> applies, `otherwise` -> `UPLC.Case () (UPLC.Constr () 0 args)` |
| 2025-01-09 10:45 | Aiken `33392f15` (microproofs) "Add case constr for applies greater than 2 optimization", shipped in v1.1.10 on 2025-01-21 |
| 2025-01-11 | Aiken `09ddec6b` `split_body_lambda` — the regrouping T5 implements, built on top of the encoding |

All three use the same `N > 2` threshold, which is not a fingerprint: it falls
directly out of the cost model, so anyone doing the arithmetic arrives at it.
Neither the Plutarch nor the Aiken repository carries any attribution — checked
by grepping commit messages, changelogs, code comments and PR bodies for
"scalus", "nemish", "atlanter".

The dates establish sequence and opportunity, not derivation; what each author
had read is not something the artifacts can settle. The purpose of this section
is only to put the primary sources somewhere durable and greppable.
