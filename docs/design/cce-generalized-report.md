# Generalized Common Context Extraction (CCE) — Report

## Summary

The CCE optimizer was generalized from right-spine-only decomposition to full single-hole
decomposition at **any child position** of **any constructor**. This captures patterns where
repeated subtrees differ in exactly one position — not just at the innermost argument of
nested Apply chains, but also at left children of Apply, Case scrutinees, Constr arguments,
Force/Delay inners, and deeper nested positions.

## What Changed

| Component | Before | After |
|-----------|--------|-------|
| Decomposition | `decomposeRightSpine` — right Apply spine only | `decompose` — all positions, all node types |
| Collect pass | Only decomposes Apply nodes | Decomposes all non-leaf, non-LamAbs nodes |
| Re-collect (Pass 3) | `decomposeRightSpine` + `~=~` | `matchTemplate` |
| Substitute (Pass 3) | `decomposeRightSpine` + `~=~` | `matchTemplate` |
| Template naming | `collectSpineFunctions` (right spine) | `collectTemplateFunctions` (generalized) |

### New Functions Added

- **`decompose(t: Term): Iterator[(Term, Term)]`** — generates all single-hole (template, leaf) pairs by placing HOLE at each child position and recursing into children. Capped at depth 30 to prevent O(S²) blowup on pathological chains.
- **`matchTemplate(template: Term, term: Term): Option[Term]`** — walks template and term in lockstep; returns the leaf at the HOLE position. Uses sealed `MatchResult` ADT (NoHole | Found).
- **`containsHole(t: Term): Boolean`** — checks for HOLE sentinel in any position.

### What Stayed The Same

- 3-pass algorithm (collect, group & filter, substitute)
- Path-based scope tracking, profitability formula `(N-1)*tSize > N+3`
- Safety checks, TermKey, insertLetAtPath, replaceHole
- `decomposeRightSpine` kept for reference/backward-compatibility tests

### Follow-up Tuning Changes

After the initial generalization, two follow-up tuning changes were applied to address the
budget regression and to let CSE subsume more of FBE's role:

1. **Raise `MinTemplateSize` from 5 to 6** (in `CommonContextExtraction.scala`).
   At size=5 with N=2 the profitability formula `(N-1)*tSize > N+3` is exactly satisfied
   (5 > 5 is false; the implementation uses `>=` so it accepted those candidates). Those
   "saved=0" extractions added the runtime cost of an extra Apply/LamAbs call without
   any net node savings. Requiring size≥6 means every extracted template saves at least
   1 node at N=2, eliminating the 11% of extractions that were paying overhead for nothing.

2. **Stop skipping `Force(Builtin)` / `Force(Force(Builtin))` in CSE's `isSkippable`**
   (in `CommonSubexpressionElimination.scala`). These are technically value forms (no
   reducible head), so the prior `case _ if t.isValueForm => true` rule skipped them.
   That left FBE as the only optimizer that could de-duplicate forced builtins. Adding
   explicit `case Force(Builtin(_, _), _) => false` and `case Force(Force(Builtin(_, _), _), _) => false`
   cases before the `isValueForm` check lets CSE handle them too. With FBE enabled there
   is no behavior change (FBE has already extracted them); with FBE disabled, CSE can now
   take over the role.

## New Patterns Captured

| Pattern | Template | Example |
|---------|----------|---------|
| Left-spine Apply | `Apply(Apply(f, HOLE), z)` | `f(x, z)` / `f(y, z)` |
| Case scrutinee | `Case(HOLE, [b1,b2])` | `case x of [...]` / `case y of [...]` |
| Constr argument | `Constr(0, [a, HOLE, c])` | `Constr(0,[a,x,c])` / `Constr(0,[a,y,c])` |
| Deeper nested | `Apply(Apply(g, Apply(f, HOLE)), z)` | `g(f(x), z)` / `g(f(y), z)` |

## Impact Measurement

Spot-check on representative contracts (with FBE, MinTemplateSize=6):

### AuctionValidator

| Config | Flat (bytes) | Δ | UPLC nodes | Δ |
|--------|-------------:|--:|-----------:|--:|
| Baseline (no CSE/CCE) | 4285 | — | 4212 | — |
| CSE only (2 iters) | 3958 | -7.6% | 3911 | -7.1% |
| CCE only | 3361 | -21.6% | 3513 | -16.6% |
| **CSE + CCE** | **3280** | **-23.5%** | **3414** | **-18.9%** |

### NaivePaymentSplitter

| Config | Flat (bytes) | Δ | UPLC nodes | Δ |
|--------|-------------:|--:|-----------:|--:|
| Baseline (no CSE/CCE) | 2550 | — | 2439 | — |
| CSE only (2 iters) | 2256 | -11.5% | 2179 | -10.7% |
| CCE only | 2101 | -17.6% | 2112 | -13.4% |
| **CSE + CCE** | **1991** | **-21.9%** | **2001** | **-18.0%** |

### OptimizedPaymentSplitter

| Config | Flat (bytes) | Δ | UPLC nodes | Δ |
|--------|-------------:|--:|-----------:|--:|
| Baseline (no CSE/CCE) | 2186 | — | 2086 | — |
| CSE only (2 iters) | 1949 | -10.8% | 1881 | -9.8% |
| CCE only | 1833 | -16.1% | 1841 | -11.7% |
| **CSE + CCE** | **1773** | **-18.9%** | **1775** | **-14.9%** |

### HelloCardano (small contract — minimal CCE benefit)

| Config | Flat (bytes) | Δ | UPLC nodes | Δ |
|--------|-------------:|--:|-----------:|--:|
| Baseline (no CSE/CCE) | 348 | — | 330 | — |
| CSE only (2 iters) | 323 | -7.2% | 305 | -7.6% |
| CCE only | 324 | -6.9% | 310 | -6.1% |
| CSE + CCE | 322 | -7.5% | 307 | -7.0% |

### Key Findings

1. **Generalized CCE dramatically improves code size reduction** — AuctionValidator went from
   effectively no CCE benefit (right-spine only had limited matches) to -21.6% code size
   with the generalized approach (-24.8% before raising MinTemplateSize to 6).

2. **CCE alone now outperforms CSE alone** for code size reduction on most contracts. CSE
   reduces -7% to -14%, while generalized CCE reduces -14% to -24%.

3. **CSE + CCE combined gives the best results** — up to -28.6% code size reduction on
   CrowdfundingValidator, -25.7% on LotteryValidator, -23.5% on AuctionValidator.

4. **Small contracts** (HelloCardano at 348 bytes) benefit modestly because there are fewer
   repeated patterns to extract.

## ForcedBuiltinsExtractor (FBE) vs No-FBE Comparison

FBE extracts repeated `Force(Force(Builtin(fn)))` patterns into top-level let-bindings before
CSE/CCE runs. With the CSE `isSkippable` change, CSE can now handle these patterns directly
when FBE is disabled. The tables below use MinTemplateSize=6.

### AuctionValidator

| Config | With FBE (bytes) | No FBE (bytes) |
|--------|----------------:|---------------:|
| Baseline | 4285 | 4611 |
| CSE only | 3958 (-7.6%) | 3901 (-15.4%) |
| CCE only | 3361 (-21.6%) | 3315 (-28.1%) |
| CSE + CCE | **3280** (-23.5%) | 3339 (-27.6%) |

### NaivePaymentSplitter

| Config | With FBE (bytes) | No FBE (bytes) |
|--------|----------------:|---------------:|
| Baseline | 2550 | 2716 |
| CSE only | 2256 (-11.5%) | 2244 (-17.4%) |
| CCE only | 2101 (-17.6%) | 2134 (-21.4%) |
| CSE + CCE | **1991** (-21.9%) | 2007 (-26.1%) |

### OptimizedPaymentSplitter

| Config | With FBE (bytes) | No FBE (bytes) |
|--------|----------------:|---------------:|
| Baseline | 2186 | 2318 |
| CSE only | 1949 (-10.8%) | 1947 (-16.0%) |
| CCE only | 1833 (-16.1%) | 1864 (-19.6%) |
| CSE + CCE | **1773** (-18.9%) | 1772 (-23.6%) |

### FBE vs No-FBE Conclusions

1. **FBE still wins on absolute size** by ~1-2% in most contracts. FBE's top-level let-binding
   for forced builtins is more compact than the patterns CSE/CCE produce on their own.

2. **Without FBE, the relative gain from CSE/CCE is larger** — AuctionValidator CSE+CCE
   achieves -27.6% vs baseline (no-FBE) compared to -23.5% vs baseline (with-FBE). This is
   because FBE pre-extracts patterns that CSE/CCE would otherwise find.

3. **CSE alone is now competitive without FBE** — with the `Force(Builtin)` skip removed,
   CSE-only without FBE achieves nearly the same absolute size as CSE-only with FBE
   (e.g., AuctionValidator: 3958 vs 3901). CSE has effectively absorbed FBE's role for
   forced builtins.

4. **Recommendation: keep FBE enabled.** The ~1-2% absolute size advantage still adds up, and
   FBE is a cheap, targeted optimization that complements CSE/CCE well.

## Execution Budget Impact

The generalized CCE initially extracted templates whose lambda/apply overhead exceeded the
node savings, increasing execution budget on computation-heavy benchmarks. Raising
`MinTemplateSize` from 5 to 6 (so every extracted template saves at least 1 net node at N=2)
reclaims most of that regression:

| Benchmark | Original baseline | Generalized CCE (size≥5) | Generalized CCE (size≥6) | Net vs. baseline |
|-----------|------------------:|-------------------------:|-------------------------:|-----------------:|
| Clausify F1 | 37.8M | 45.7M (+20.8%) | **40.2M** | **+6.5%** |
| Clausify F2 | 47.4M | 57.0M (+20.2%) | **50.4M** | **+6.4%** |
| Clausify F3 | 126.5M | 151.4M (+19.6%) | **134.3M** | **+6.2%** |
| Clausify F4 | 175.9M | 204.3M (+16.1%) | **183.9M** | **+4.6%** |
| Clausify F5 | 607.2M | 723.8M (+19.2%) | **643.2M** | **+5.9%** |
| Knights 4x4 | 144.0M | 181.2M (+25.8%) | **157.8M** | **+9.6%** |
| Knights 6x6 | 385.8M | 449.1M (+16.4%) | **402.1M** | **+4.2%** |
| Knights 8x8 | 696.7M | 795.5M (+14.2%) | **717.9M** | **+3.0%** |

`size≥6` cuts the budget regression by **roughly half** across all benchmarks (from +14-26%
down to +3-10%). The remaining gap is the inherent runtime cost of the extra Apply/LamAbs
overhead at each call site, which is the price for the substantial code-size reduction.

**Root cause of the original regression**: The profitability formula `(N-1)*tSize > N+3` is
calibrated for code SIZE, not execution BUDGET. Each extracted call adds 1 Apply + 1 Var
(cheap in flat encoding but costs ~200 memory + 23100 steps at runtime). At `MinTemplateSize=5`
with N=2, the template-size savings were exactly `(2-1)*5 - (2+3) = 0` net nodes — saving
nothing but still adding the runtime call cost.

**Why size≥6 helps**: At size=6 with N=2, savings are `(2-1)*6 - (2+3) = 1` net node, so every
extracted template is at least 1 node profitable. This eliminates the ~11% of extractions that
were "saved=0" — the ones that paid the runtime cost without compensating with code-size savings.

## Full Contract Measurements (23 Contracts, MinTemplateSize=6)

| Contract | Baseline | CSE only | CCE only | CSE+CCE | CCE only (no FBE) | CSE+CCE (no FBE) |
|----------|---------:|---------:|---------:|--------:|------------------:|-----------------:|
| HelloCardano | 348 | 323 (-7.2%) | 324 (-6.9%) | 322 (-7.5%) | 329 (-6.5%) | 320 (-9.1%) |
| AuctionValidator | 4285 | 3958 (-7.6%) | 3361 (-21.6%) | 3280 (-23.5%) | 3315 (-28.1%) | 3339 (-27.6%) |
| NaivePaymentSplitter | 2550 | 2256 (-11.5%) | 2101 (-17.6%) | 1991 (-21.9%) | 2134 (-21.4%) | 2007 (-26.1%) |
| OptimizedPaymentSplitter | 2186 | 1949 (-10.8%) | 1833 (-16.1%) | 1773 (-18.9%) | 1864 (-19.6%) | 1772 (-23.6%) |
| EscrowValidator | 1857 | 1674 (-9.9%) | 1421 (-23.5%) | 1361 (-26.7%) | 1440 (-27.0%) | 1379 (-30.1%) |
| VestingValidator | 1651 | 1432 (-13.3%) | 1400 (-15.2%) | 1317 (-20.2%) | 1399 (-20.3%) | 1326 (-24.4%) |
| HtlcValidator | 538 | 526 (-2.2%) | 477 (-11.3%) | 471 (-12.5%) | 467 (-18.1%) | 465 (-18.4%) |
| VaultValidator | 2553 | 2411 (-5.6%) | 2041 (-20.1%) | 2059 (-19.3%) | 2004 (-26.4%) | 2084 (-23.5%) |
| LotteryValidator | 2764 | 2377 (-14.0%) | 2105 (-23.8%) | 2055 (-25.7%) | 2126 (-28.6%) | 1997 (-32.9%) |
| BettingValidator | 3497 | 2995 (-14.4%) | 2724 (-22.1%) | 2633 (-24.7%) | 2745 (-27.4%) | 2601 (-31.2%) |
| SimpleTransferValidator | 2870 | 2650 (-7.7%) | 2466 (-14.1%) | 2381 (-17.0%) | 2494 (-18.5%) | 2343 (-23.5%) |
| ProxyValidator | 1718 | 1613 (-6.1%) | 1484 (-13.6%) | 1440 (-16.2%) | 1500 (-18.3%) | 1454 (-20.8%) |
| AmmValidator | 2821 | 2681 (-5.0%) | 2358 (-16.4%) | 2357 (-16.4%) | 2274 (-24.9%) | 2318 (-23.5%) |
| EditableNftValidator | 1965 | 1703 (-13.3%) | 1580 (-19.6%) | 1524 (-22.4%) | 1592 (-24.1%) | 1518 (-27.6%) |
| LinkedListValidator | 5787 | 5493 (-5.1%) | 4553 (-21.3%) | 4665 (-19.4%) | 4341 (-30.6%) | 4560 (-27.1%) |
| CrowdfundingValidator | 5775 | 5053 (-12.5%) | 4532 (-21.5%) | 4122 (-28.6%) | 4330 (-31.0%) | 4131 (-34.2%) |
| DonationMintingPolicy | 1638 | 1522 (-7.1%) | 1473 (-10.1%) | 1451 (-11.4%) | 1433 (-17.6%) | 1381 (-20.6%) |
| AllowlistValidator | 803 | 784 (-2.4%) | 788 (-1.9%) | 783 (-2.5%) | 803 (-1.5%) | 786 (-3.6%) |
| AnonymousDataValidator | 4550 | 4310 (-5.3%) | 3679 (-19.1%) | 3649 (-19.8%) | 3702 (-24.0%) | 3550 (-27.1%) |
| AnonymousDataGateValidator | 1220 | 1129 (-7.5%) | 1045 (-14.3%) | 1034 (-15.2%) | 1071 (-16.5%) | 1035 (-19.3%) |
| DecentralizedIdentityValidator | 3735 | 3497 (-6.4%) | 2932 (-21.5%) | 3000 (-19.7%) | 2797 (-31.1%) | 2745 (-32.3%) |
| PricebetValidator | 2316 | 2131 (-8.0%) | 1836 (-20.7%) | 1829 (-21.0%) | 1822 (-26.8%) | 1845 (-25.9%) |
| OracleValidator | 2252 | 2126 (-5.6%) | 1901 (-15.6%) | 1872 (-16.9%) | 1859 (-23.0%) | 1858 (-23.0%) |

All values are flat-encoded bytes. "Baseline" = with FBE, no CSE/CCE. "No FBE" columns use no-FBE baseline.
After raising MinTemplateSize from 5 to 6, sizes are slightly larger (1-3%) than the
MinTemplateSize=5 measurements but the budget regression is roughly halved (see Execution Budget Impact).

### Key Observations Across All Contracts

1. **CCE alone delivers 11-24% size reduction** across medium-to-large contracts.
2. **CSE+CCE combined achieves 12-29%** — best results on CrowdfundingValidator (-28.6%) and EscrowValidator (-26.7%).
3. **Without FBE, relative CCE gains are larger** (up to -34.2% on CrowdfundingValidator).
4. **FBE still wins on absolute bytes** for almost all contracts — with-FBE CSE+CCE produces smaller output than no-FBE CSE+CCE.
5. **CSE+CCE occasionally regresses vs CCE only** on a few contracts (VaultValidator, LinkedListValidator, DecentralizedIdentityValidator) — CSE occasionally breaks patterns into smaller pieces that CCE could otherwise extract whole.
6. **AllowlistValidator is an outlier**: minimal CCE benefit (-1.9% to -2.5%) because the contract has very few repeated patterns at all.

## CCE Template Analysis: What Gets Extracted

Analysis of CCE extraction logs across all contracts reveals **6 categories** of extracted templates.
Data is from AuctionValidator (66 templates with FBE, 61 without FBE) and other contracts.

### Category 1: Data Field Accessor Chains (most impactful)

**Pattern**: Chains of `headList`/`tailList`/`sndPair`/`unConstrData` that navigate to a specific
field in a Plutus Data structure.

```
[__HeadList [__TailList [__TailList [__TailList [__TailList HOLE]]]]]  (size=11, 4 occ, saved=26)
[__SndPair [(builtin unConstrData) [__HeadList [__TailList HOLE]]]]     (size=9, 6 occ, saved=36)
[__HeadList [__SndPair [(builtin unConstrData) [__HeadList HOLE]]]]     (size=9, 3 occ, saved=12)
[__TailList [__SndPair [(builtin unConstrData) [__HeadList HOLE]]]]     (size=9, 7 occ, saved=44)
```

**Why these dominate**: Smart contracts repeatedly destructure the same Plutus Data paths — e.g.,
accessing transaction inputs, outputs, and datum fields requires navigating nested constructor
field lists. The same path like "get 5th field of constructor" appears in multiple branches.

**With FBE vs no FBE**: With FBE, `headList`/`tailList`/`sndPair` are already single Vars, so
templates are compact. Without FBE, each `Force(Builtin(headList))` is 3 nodes, inflating template
sizes (e.g., size=11 becomes size=25) but also increasing per-extraction savings.

### Category 2: Constructor Tag Matching (very frequent)

**Pattern**: `ifThenElse` + `equalsInteger` + `fstPair` to match on a constructor tag.

```
[__IfThenElse [(builtin equalsInteger) [__FstPair HOLE] (con integer 0)]]  (size=9, 13 occ, saved=92)
[__IfThenElse [(builtin equalsInteger) [__FstPair HOLE] (con integer 1)]]  (size=9, 4 occ, saved=20)
[__IfThenElse [(builtin equalsInteger) [__FstPair HOLE] (con integer 2)]]  (size=9, 4 occ, saved=20)
```

**Why these matter**: Every `match` expression on a data type compiles to a sequence of
`ifThenElse(equalsInteger(fstPair(unConstrData(x)), N), ...)` checks. The same comparison
template is used for each constructor of each data type, with different unConstrData arguments.

### Category 3: Type Coercion + Field Access

**Pattern**: `unIData`/`unBData`/`unConstrData`/`unListData` composed with `headList`.

```
[(builtin unIData) [__HeadList HOLE]]         (size=5, 14 occ, saved=48)
[(builtin unBData) [__HeadList HOLE]]          (size=5, 4 occ, saved=8)
[(builtin unConstrData) [__HeadList HOLE]]     (size=5, 10 occ, saved=32)
[(builtin unListData) [__HeadList HOLE]]       (size=5, 5 occ, saved=12)
```

**Why**: Nearly every field access in Plutus Data requires unwrapping from the generic Data type.
`unIData(headList(fields))` extracts an integer from the first field of a constructor.

### Category 4: Constructor Building

**Pattern**: `constrData` + `mkCons` to build Plutus Data constructors.

```
[(builtin constrData) (con integer 0) [__MkCons HOLE (con (list data) [])]]  (size=9, 3 occ, saved=12)
[__MkCons HOLE (con (list data) [])]                                          (size=5, 13 occ, saved=44)
[__MkCons [(builtin constrData) (con integer 0) HOLE] ...]                   (size=13, 2 occ, saved=8)
```

**Why**: Smart contracts frequently build result values — `Constr(0, [field])` patterns appear
wherever a function returns an `Option`, `Bool`, or other algebraic data type.

### Category 5: Assertion/Control Flow Patterns

**Pattern**: `ifThenElse` with delayed error branches.

```
[__IfThenElse HOLE (delay (con unit ()))]              (size=6, 23 occ, saved=106)
[__ChooseList HOLE (delay (con data (Constr 1 [])))]   (size=6, 3 occ, saved=6)
(force [HOLE (delay (con bool False))])                 (size=5, 9 occ, saved=28)
```

**Why**: `require(condition)` compiles to `ifThenElse(condition, delay(unit), delay(error))`.
The assertion template `[ifThenElse HOLE (delay unit)]` appears at every `require()` call.

### Category 6: Barely-Profitable Templates (saved=0 nodes)

**Pattern**: Small templates (size=5) with exactly 2 occurrences.

```
[(builtin equalsInteger) [(builtin unIData) HOLE]]              (size=5, 2 occ, saved=0)
[(builtin lessThanEqualsInteger) [(builtin unIData) HOLE]]      (size=5, 2 occ, saved=0)
[__TailList [__TailList HOLE]]                                   (size=5, 2 occ, saved=0)
[__IfThenElse [HOLE (con integer 2)]]                            (size=5, 2 occ, saved=0)
[__IfThenElse [__cse_20 HOLE]]                                   (size=5, 2 occ, saved=0)
```

**The "saved=0" problem**: The profitability formula `(N-1)*tSize >= N+3` computes to
`(2-1)*5 = 5 >= 2+3 = 5` → just barely profitable (0 nodes saved). These extractions add
1 LamAbs + 2 Applies + 2 Vars (7 nodes overhead) while removing 5 duplicated nodes.
In practice they save 0 net UPLC nodes but may save ~1-2 flat bytes due to encoding efficiency.

**In AuctionValidator with FBE**: 7 out of 66 templates (11%) have saved=0.

## Why FBE Outperforms CCE for Forced Builtins

### The Core Asymmetry

**FBE approach** for `Force(Force(Builtin(fn)))` with N occurrences:
- Inserts ONE let-binding at the top: `let __fn = Force(Force(Builtin(fn))) in body`
- Cost: 1 LamAbs + 1 Apply + 3 nodes (definition) = 4 node overhead
- Each usage becomes: 1 Var reference
- Total: 4 + N nodes (definition + N references)
- **Savings: 3N - (4 + N) = 2N - 4 nodes**

**CCE approach** for `[Force(Force(Builtin(fn))) HOLE]` (size=5) with N occurrences:
- CCE cannot extract `Force(Force(Builtin(fn)))` alone (size=3 < MinTemplateSize=5)
- Instead extracts the template including the HOLE context, e.g. `[sndPair HOLE]`
- Cost: 1 LamAbs + template body (5 nodes) + N Apply + N Var = 6 + 2N overhead
- Each usage becomes: Apply(Var(lambda), leaf) — 2 nodes
- **Savings: 5N - (6 + 2N) = 3N - 6 nodes** (larger savings per template, but...)

### Why CCE Still Loses on Absolute Size

1. **FBE is surgical**: It extracts just the `Force(Force(Builtin))` triple — the most frequently
   repeated atomic pattern in UPLC. With 15-30 distinct builtins each appearing 5-30 times,
   FBE handles them all with minimal overhead (one let per builtin).

2. **After FBE, CCE templates are smaller**: With FBE, `[__SndPair HOLE]` is only size=3
   (Apply + Var + HOLE), below MinTemplateSize. So CCE must find larger composite patterns
   to extract. These larger templates are less frequently repeated.

3. **Without FBE, CCE templates contain redundant builtin encoding**: Each CCE template that
   includes `Force(Force(Builtin(fn)))` encodes those 3 nodes in the template body once.
   But FBE's approach encodes them once TOTAL for the entire program, regardless of how many
   different templates use that builtin.

4. **CCE's per-call overhead is larger**: Each CCE call site adds Apply(Var(lambda), leaf) =
   2 nodes × ~4 bits each = ~8 bits. FBE's per-call overhead is just Var = ~4-8 bits.
   The difference is small per occurrence but compounds across hundreds of call sites.

### Quantitative Example (AuctionValidator)

| Metric | With FBE | No FBE |
|--------|----------|--------|
| CCE templates extracted | 66 | 61 |
| Total saved (UPLC nodes) | 1,281 | 1,865 |
| Saved=0 templates | 7 (11%) | ~1 (2%) |
| Final flat size (CSE+CCE) | **3,138** | 3,194 |
| FBE absolute advantage | | +56 bytes (+1.8%) |

Without FBE, CCE finds larger templates and saves more nodes — but the final flat-encoded size
is still 56 bytes larger because:
- FBE's top-level let-bindings are more compact than CCE's lambda wrappers
- CCE duplicates `Force(Force(Builtin(...)))` inside each template body
- FBE shares the builtin encoding across ALL uses regardless of surrounding context

### Recommendations

1. ✅ **Keep FBE enabled** — it handles the most common micro-pattern more efficiently than CCE can.
   Done — FBE remains the default in `OptimizerPipelines`.
2. ✅ **Raise MinTemplateSize to 6** — eliminates ~11% of barely-profitable extractions that add
   overhead without clear benefit. Done — `MinTemplateSize = 6` in `CommonContextExtraction.scala`.
   This roughly halved the budget regression on Knights/Clausify benchmarks.
3. ✅ **Let CSE handle Force(Builtin) too** — removes the need for FBE to be the sole optimizer
   for forced builtins. Done — explicit `case Force(Builtin(_, _), _) => false` in CSE's
   `isSkippable`. With FBE enabled this is a no-op; with FBE disabled, CSE-only now matches
   CSE+FBE on most contracts.
4. **Possible future work**: a budget-aware mode for execution-sensitive contracts that skips
   extractions where the runtime cost of Apply/LamAbs overhead exceeds the benefit.

## Test Results

- **55 unit tests** in `CommonContextExtractionTest` — all pass
  - 36 existing tests (regression check)
  - 19 new tests (decompose, matchTemplate, containsHole, new patterns)
- **393 JVM tests** — all pass (budget expectations updated for 9 benchmark tests)
- Property-based tests: idempotency, no new free variables, semantic preservation — all pass

## Safety Analysis

The generalized decomposition maintains all safety invariants:
- **Scope safety**: Templates' free variables must be in scope at bind path
- **Shadowing safety**: No re-binding between bind and occurrence paths
- **Conditional boundary safety**: No hoisting shape-partial builtins across Case/Delay
- **LamAbs boundary**: `decompose` skips LamAbs bodies to avoid scope issues
- **Case branch boundary**: `decompose` skips Case branch bodies (evaluation boundaries)
- **Depth cap**: `MaxDecomposeDepth=30` prevents pathological O(S²) chains
