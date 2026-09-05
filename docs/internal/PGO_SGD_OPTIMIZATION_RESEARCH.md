# Profile-guided + gradient-guided UPLC optimization: feasibility research

Date: 2026-09-02. Status: research only, no code written.

Question asked: collect a CEK evaluation profile, take a user-supplied loss function
(minimum fee, minimum memory), use the exact cost models to do backpropagation, and
run SGD to find a locally optimal combination of optimization passes and their
parameters.

---

## Verdict

1. **The SGD half does not work, and it is not a tooling problem.** The map
   `configuration -> fee` is piecewise constant in every knob. Its derivative is
   zero on a set of full measure and undefined at the jumps. There is nothing for a
   gradient to hold on to. This is proved in section 1 and confirmed numerically.

2. **The PGO half is right, and the exact cost model makes it stronger than what
   LLVM/GCC can do.** Scalus has something no mainstream compiler autotuner has: a
   deterministic machine, an exact integer cost model, and a closed-form fee. Every
   heavyweight technique in the literature (Bayesian optimization, learned cost
   models, RL policies, LLM pass prediction) exists to work around noise, expensive
   measurement, or the need to skip search. Scalus has none of those problems, so
   the simple methods are not a fallback: they are strictly better here.

3. **The intuition behind "compute the derivative" is correct in a different form,
   and it has already worked twice in this repo.** The fee is affine in per-site
   execution counts and in script bytes. The profile supplies the coefficients. That
   gives an exact marginal-cost formula per rewrite, which is what produced
   `k* = sqrt(N·F/(15·B))` for loop unrolling and `MinRunSize = 5` for let-chain
   regrouping. That is the sound version of the idea and the one worth building on.

Expected payoff, stated up front: knob tuning on this codebase measured **-5.07%
(FibUnfold), -1.71% (Knights), -0.88% fee (BitcoinValidator), 0% (Clausify)**. The
literature's exhaustive study (Kulkarni, TACO 2009) puts a well-iterated fixed
pipeline **4.8% off the per-function optimum on average, already optimal on 42% of
functions**, with a fat tail up to 50%. New rewrites in this repo gave 7.7x (natural
merge sort) and 13-75x (Value builtins). Search over knobs is worth single digits;
new rewrites are worth multiples. Budget accordingly.

---

## 1. Where the derivative dies

### 1.1 The chain

```
config --Phi--> Term --E(x)--> Trace --B--> (cpu, mem, bytes) in Z^3 --fee--> lovelace
```

`Phi` is the optimizer pipeline; `Term` is a discrete algebraic structure with no
vector space. `E(x)` is the CEK machine, a deterministic transition system. `B` sums
integer step costs. Only the final arrow is differentiable, and its domain is `Z^3`,
so its "derivative" is a marginal price (6.92 lovelace per CEK step, 15 lovelace per
reference-script byte), not something that composes backwards.

### 1.2 The knobs, by kind

- **Categorical** (which passes run, pass order). No topology. A derivative is not
  definable, and there is nothing to relax: `0.3·inline(t) + 0.7·cse(t)` is not a term.
- **Integer** (`cseIterations`, unroll factor `k`). The only derivative-like object is
  the forward difference `L(k+1) - L(k)`. That is a discrete gradient, and coordinate
  descent on it is exactly what `Autotune` already does.
- **Real-looking thresholds** (inline size limit, use-count limit). Piecewise constant,
  proved below.

### 1.3 Step-function theorem

Let `tau` be a threshold the optimizer uses only in comparisons `m <= tau`, `m < tau`,
`m >= tau`, `m > tau`, where `m` ranges over integer measurements of terms (bit sizes,
occurrence counts, depths). Then `L(tau)` is constant on every open interval `(n, n+1)`.

Proof: for integer `m` and `tau` in `(n, n+1)`, `m <= tau` iff `m <= n`, and likewise
for the other three forms. Every comparison outcome is therefore constant on the
interval, so `Phi(c, tau)` is one fixed term throughout, and everything downstream
depends on `tau` only through `Phi`. Hence `dL/dtau = 0` on every `(n, n+1)`, and at
integers where adjacent plateaus differ the one-sided limits differ, so the derivative
does not exist. The distributional derivative is a train of Dirac impulses. A finite
difference with step `h < 1` returns exactly 0 unless the stencil straddles an integer,
which happens with probability at most `2h`.

For the non-strict form actually in the code (`flatConstant.bitSize(c) <= 64`,
`Inliner.scala:150`), this sharpens to `L(tau) = L(floor(tau))`.

### 1.4 Worked example: the inline-size threshold

Take a binding `(lam x body) c` where `c` is an `s`-bit constant used `u` times, and the
binding site executes `n_bind` times in the profile. Inlining removes the wrapper:
Apply + Lam + Const evaluation = 3 CEK steps per execution.

- Steps: `-3·n_bind` steps, worth `20.8·n_bind` lovelace saved.
- Bytes (flat encoding): a `Var` is about 12 bits, a `Const` about `9 + 8·ceil(s/7)` bits.
  - `s = 64, u = 3`: `+134` bits = `+17` bytes = **+251 lovelace per tx**.
  - `s = 8, u = 3`: `+6` bits, about `+1` byte = **+15 lovelace per tx**.

Along `tau`, with both constants present, `L` has three plateaus: `tau < 8` (nothing
inlined), `8 <= tau < 64` (small one), `tau >= 64` (both). For `n_bind = 1` the `>= 64`
plateau loses about 230 lovelace per tx against the `>= 8` plateau; for `n_bind >= 13`
it wins. `dL/dtau = 0` at `tau = 30`, at `tau = 63.9`, and at `tau = 64.1`. The gradient
sees none of this.

Actionable consequence: the global `bitSize(c) <= 64` rule for `Many`-use constants at
`Inliner.scala:150` looks fee-negative for once-executed sites. **Measure before acting**;
eta-reduce and CSE interactions can move the numbers.

### 1.5 Numerical confirmation

Throwaway toy (`scratchpad/toy_landscape.py`, not kept): two knobs, an inline threshold
`tau` in `[0, 256]` over nine bindings and an unroll factor `k` in `[1, 64]`, real fee
constants. Landscape: 10 plateaus over 257 integer values of `tau`, three local minima,
global at 128. Central finite difference with `h = 1e-3` is non-zero at **1 of 2000**
random `tau`.

| Method | Evals | Gap to exhaustive optimum (lovelace) |
|---|---|---|
| SGD, finite diff `h = 1e-3` | 600 | +2260 / +2682 / +10923 / +3281 (never moves) |
| SGD, finite diff `h = 1` | 600 | 0 by luck, else +449 / +8518 (tau always stuck) |
| Simulated annealing, 10 seeds | 300 | 0 in 10/10 from every start |
| GP with LCB acquisition, 10 seeds | 40 | 0 in 9/10, +19 in the tenth |
| Analytic `k*` then check `{10, 11}` | 2 | 0 (matches brute force over all 64 `k`) |

With `h < 1` SGD never moves. With `h = 1` the "gradient" in `k` is the discrete forward
difference, which descends the convex `k` direction: that is coordinate descent, not SGD.

### 1.6 Nothing here is stochastic in the SGD sense

SGD's noise comes from minibatch sampling of a data distribution. Here the distribution
is over `(datum, redeemer, ScriptContext)` triples, and the profile is an empirical
sample of it. With 1-20 scenarios (this repo's normal case) the expectation
`sum_x w_x · fee(x)` is computed **exactly** in `S` CEK runs of a few ms each. A
stochastic estimator adds variance and buys nothing. Compilation dominates evaluation,
not the other way round.

Two real caveats that survive:
- The **budget cap is a max, not a mean**. `maxTxExUnits` binds the worst input; the fee
  objective is a mean. Sampling degrades exactly the tail that must not be missed.
- **Overfitting to the profile** is the genuine risk: unroll factors and strict-if
  conversions tuned to test scenarios can regress unprofiled inputs. `Autotune`'s
  1.10x constraint gate is the current mitigation; a held-out scenario set is the other.

---

## 2. The three derivatives Scalus already has

The user's instinct is not wrong, it is aimed at the wrong arrow. Three exact
derivative-like objects exist today.

**(a) The profile is the gradient.** `d(fee)/d(execute node n once more) = stepCost(n)`,
exactly. Per-node marginal cost attribution is what backpropagation *is*: credit
assignment. `ProfilingData.bySourceLocation` already computes it. Backprop through the
**fee** is trivial and yields the profile. Backprop through the **compiler** is
impossible.

**(b) Exact finite differences on integer knobs.** Because the CEK is deterministic,
`delta_fee(k -> k+1)` is exact and costs one compile. This is a discrete gradient and it
is sound. `Autotune.optimize` already descends it for unroll factors.

**(c) Analytic per-rewrite marginal cost.** Writing the fee in per-site quantities:

```
fee = F · sum_s count(s) · c(s) + R(bytes)
```

it is affine in `(count, c)` and convex piecewise-linear in `bytes`. So for any local
rewrite `r` at site `s`:

```
delta_fee(r, s) = F · sum_x w_x · count_x(s) · delta_steps_r  +  R'(bytes) · delta_bytes_r
```

This is the **discrete directional derivative along the rewrite**, with profile counts
playing the role activations play in backprop. It is *exact* whenever the rewrite changes
the per-execution step cost of `s` by a constant, leaves other sites' counts unchanged,
and crosses no size tier. It produced `k* = sqrt(N·F/(15·B))` (unrolling, net saving
`15·B·(k*-1)^2`) and `MinRunSize = 5` (T5 let-chain regroup).

Which passes admit a closed form:

| Pass | Closed form given the profile? | Shape |
|---|---|---|
| Loop unroll (factor `k`) | Yes | `C(k) = 15·B·(k-1) + 13.85·ceil(N/k)`, convex |
| Let-chain regroup (run `N`) | Yes | `(N-2)` steps vs about 1 byte, threshold 5 |
| Per-site inline | Yes | `-3·n_bind` steps vs `(u-1)·bytes(c)`, per-site inequality |
| Per-candidate CSE | Yes | saves `(sum_i count_i - count_at_binding)` evals of `e`, costs lam+apply+var |
| Forced-builtin extraction | Yes | per use, 2 Force steps -> 1 Var step, one lam/apply once |
| Case-constr-apply | Yes, parameter-free | saves Apply steps per call, apply whenever legal |
| Eta-reduce | Not a decision | strictly removes steps and bytes when legal |
| Strict-if | Yes, input-dependent | saves Delay+Force per taken branch, pays `E[cost of untaken branch]` |
| Common-context extraction | Yes, CSE shape | |
| Partial evaluation | **No** | effect is the result of computation; measure, with a size guard |
| Pass order, iteration counts | **No** | run to fixpoint under a cap; order is a small search |

Where the model leaks (remainder iterations, body growth when the inliner runs after
unroll, tier crossings, count changes from duplicated control flow) the fix is to
evaluate the exact objective at the few integer neighbours the model proposes. That is
already the `Autotune` pattern.

**Coupled knobs stay tractable.** Multi-loop unrolling is
`min_k sum_i 13.85·N_i/k_i + R(S0 + sum_i B_i·(k_i - 1))`. Each `1/k_i` is convex, `R` is
convex and non-decreasing, and `R` of an affine function is convex, so the continuous
relaxation is a **convex program with a unique minimum**. Rounding is not exact when the
`B_i` differ, but the model prunes `5^10` to a handful of neighbours to enumerate exactly.

---

## 3. What "best in the common path, compact in the rare path" actually needs

This is the sharpest part of the request and it does not need a second objective. It
falls straight out of the formula in 2(c). For a rewrite at site `s`:

```
inline/unroll/strictify at s  iff  F · count(s) · delta_steps  >  15 · delta_bytes
```

- `count(s) = 0` or `1` (error branches, one-shot setup, cold validators): the step term
  vanishes and **size wins automatically**.
- `count(s)` large (loop bodies, hot folds): the step term dominates and **speed wins
  automatically**.

One caveat this framing creates: "compact in the rare path" means *slower* in the rare
path, and the rare path is often the one nearest `maxTxExUnits`. So the constraint gate
must check cold scenarios against the **absolute** budget cap, not the 1.10x relative
tolerance the `Autotune` branch uses today (`BudgetTolerance`, gate at `Autotune.scala:224-242`).
That gap is already listed as a known limitation on the branch; this is where it bites.

There is one knob, `count(s)`, and the profile supplies it. No hot/cold mode flag, no
per-path objective selection, no user-facing "optimize this function for size". The
break-even is `count(s) = 15·delta_bytes / (6.92·delta_steps)`, roughly **2.2 executions
per byte added** for a reference-script-carried script, or **6.4 per byte** for a
witness-carried one (`minFeeA = 44` lovelace per byte).

This is the single highest-value change identified by this research, because today
**every pass in the pipeline is profile-blind**: `Inliner`, `CSE`, `CCE`, `StrictIf`,
`ForcedBuiltinsExtractor` and `CaseConstrApply` all decide per site using a fixed
syntactic predicate with no execution count in scope. `LoopUnroll` on the unmerged
branch is the only pass that takes a per-site decision from outside.

Prerequisite, and it is the real work: **profile-to-IR-node attribution**. The profiler
keys on `(file, line)` only (`Cek.scala:1032`), column and `endLine` are deliberately
dropped, and `UplcPipeline.scala:75-81` fills position-less optimizer-created nodes from
their neighbours, so hoisted CSE lets inherit someone else's line. There are no node ids.
Threading a stable site id from SIR through lowering into UPLC annotations is the
enabling step for everything in this section.

---

## 4. State of the art, and what it says about this plan

### 4.1 Does anyone backprop through a compiler? No.

Every "differentiable compiler" system differentiates a **surrogate** or a **proposal
distribution**, then re-discretizes and re-checks with the real evaluator.

- **TerpreT** (Gaunt et al. 2016, arXiv 1608.04428) is the controlled experiment:
  gradient descent through a relaxed interpreter versus ILP/SMT/Sketch. Gradient
  descent scored **0 of 3** on Boolean circuits at 35-80x the SMT time; Sketch scored
  12 of 12. They prove exponentially many bad local optima for the parity chain.
- **DiffTune** (Renda et al., MICRO 2020) backprops through a learned surrogate of
  `llvm-mca` to fit **cost model parameters**, not decisions. Error improved 30.0% ->
  24.6%, but Kendall's tau **dropped on all four microarchitectures**. Scalus's cost
  model is exact, so there is nothing to fit, and the tau regression is a warning about
  optimizing a surrogate.
- **SmoothE** (ASPLOS 2025) is the one real continuous relaxation of an extraction
  problem, and it requires the cost to be a **sum over selected nodes**. CEK steps are
  an interpreter output, not a node cost, so there is nothing for the relaxation to
  attach to.
- **Felix** (ASPLOS 2024) relaxes numeric knobs (tile sizes, unroll factors in log
  domain) through symbolic feature formulas into a pretrained MLP, then rounds and
  re-measures. Its win is fewer **expensive** measurements; the knobs it cannot relax
  (pass on/off, order) are exactly Scalus's knobs. For `k` in `{2,4,8,16}` its
  relax-round-verify loop degenerates to four exact evaluations.
- **DARTS-style relaxation over passes**: no primary source exists, 2016-2026. It is
  ill-posed for the reason in 1.2, and DARTS itself was beaten by random search at equal
  budget (Zela et al. ICLR 2020) and collapses to skip connections.

### 4.2 Where learned methods do win, and why that does not apply

- **MLGO** (Trofin et al. 2021, in LLVM): inlining-for-size as an MDP over call sites,
  PPO then Evolution Strategies, reward is exact binary size. Gains 3.7-5.9% on the
  training corpus, up to 7%. Requirements: **28k modules**, 60-150 h on 488 processors.
  They had an exact objective too, and still used RL, for three reasons that do not hold
  here: decisions are per-callsite and sequential, online search at compile time was an
  explicit anti-goal (determinism, compile time), and the policy amortizes over 30k
  modules. Scalus's corpus is about 20 examples plus CAPE, and the tuner runs offline.
- **CompilerGym** (CGO 2022) is the direct negative result: on an exact IR-instruction-count
  objective, **every learned policy scored at or below 60 seconds of random search**
  (PPO 0.964x, worse than `-Oz`; random at 60 s 1.045x).
- **AutoPhase** (MLSys 2020): per-program RL search 28% over `-O3`; the **transferred**
  policy without search 3-6%. Search is where the value is.
- **Cummins et al.** (2023, arXiv 2309.07062): the LLM's training oracle was random
  search costing 9,016 CPU-days. Autotuner +5.03%, model +3.01%. The **LLM Compiler**
  (2024) reaches 77% of the autotuner. In every case the search is the ceiling and the
  model approximates it. Scalus can just run the oracle.

### 4.3 What actually wins on exact objectives

- **Cooper, Schielke, Subramanian** (LCTES 1999) is the closest analog on record: a GA
  over pass **sequences** with an exact static objective, 10 passes at length 12, `10^12`
  space, population 20, hash-memoized fitness. Best found in under 100 generations,
  199-432 unique evaluations against random's 352-3,331. Size 0% to -40.8%. Key lesson:
  **most of the gain was captured by one better fixed default sequence**; per-module GA
  added only 0.6-1.5%.
- **Kulkarni et al.** (CGO 2006, CGO 2007, TACO 2009) enumerated the space exhaustively
  by hashing terms after each pass and pruning dormant passes, collapsing `15^12`-`15^44`
  sequences to `10^4`-`10^5` distinct instances. Results: the fixpoint batch compiler is
  4.8% off optimal on average, optimal on 42% of functions, worst case 50%. Against that
  known optimum: **hill climbing best-of-100 restarts came within 0.02%**, SA 0.15%,
  GA 0.43%, greedy 1.1%. Roughly 45% of local minima are global.
- **Redundancy filtering** (Kulkarni TACO 2005) removed **87.7%** of GA evaluations at
  zero quality loss, cutting search time 62%. Cooper 1999 saw 45% hash hits.
- **Random search is a strong floor.** Chen et al. (TACO 2012): 300 random draws are
  statistically indistinguishable from 8,000 over ~100 flags; "only a handful of options
  have a significant performance impact". Bergstra and Bengio (JMLR 2012) measured
  effective dimensionality between 1 and 4 in a 7-dimensional space.
- **BO does not fit.** GP-BO is wrong on three independent grounds here: evaluations are
  cheap, the space is discrete and partly a permutation, and GPs propose duplicates
  (ytopt finished 66 of 200 iterations because 134 proposals were duplicates). Frazier's
  own framing scopes BO to objectives "taking minutes or hours" in "less than 20
  dimensions". Bischl et al. state the rule directly: cheap evaluations plus a small
  space means fall back on minimal-overhead methods.

### 4.4 E-graphs: strong for size, unproven for steps

Equality saturation dissolves phase ordering by applying all rewrites at once, and
UPLC's **size** term qualifies: flat encoding pays every occurrence, so scripts are
trees, `tree_cost` is exactly solvable bottom-up in linear time, and the NP-hardness of
DAG extraction does not bite. One caveat: flat encodes de Bruijn indices in 7-bit chunks
(`FlatCodec.scala:64-67`), so a `Var` costs 1 byte below index 128 and 2 below 16,384;
a rewrite that changes binder distance changes the per-node constant.

The **execution** term does not qualify. Every extractor surveyed consumes a local cost
that is a function of the selection vector; none consumes a dynamic trace.
**Profile-weighted extraction is unpublished** (nearest ancestors: Tate's `k^depth`
static weighting in Peggy, TENSAT's measured constants, Denali's profile-supplied
latencies, STOKE's post-hoc rerank). Building it would be novel, and the known failure
mode is severe: SmoothE Table 2 shows proxy-plus-greedy up to 46% off optimal.

Binders are the first hard wall. Sketch-Guided Equality Saturation measured named
variables blowing past 2 GB in 16 s, and one unguided saturation taking **over an hour
and 35 GB**; De Bruijn plus human sketches brought it to 4-7 s. Slotted e-graphs (PLDI
2025) report 1.83M e-nodes / 5.1 GB / 52 s versus 214 e-nodes / 6 MB / 0.22 s on the
same goal.

Verdict: worth it for a size-only sub-problem, or as a **proposal generator** feeding a
search whose acceptance test uses the true fee. That last shape needs no local cost at
all and has a 2026 precedent (Hong et al., EGRAPHS 2026: MCMC over rewrites is a peer of
equality saturation, "neither technique comes out to be Pareto optimal over the other").

### 4.5 Blockchain compilers: no precedent to copy

Every Cardano compiler runs a hand-tuned fixed pipeline with syntactic heuristics and no
cost model in the decisions.

- **Aiken**: fixed pipeline, uncapped node-count fixpoint loop. Inliner rule is
  `occurrences == 1 and (must execute or cannot throw)`. No optimization level, no
  size-vs-speed knob, no cost-model decision anywhere.
- **plutus-tx / Plinth**: the richest knob set on Cardano and every default hand-set.
  PIR simplifier exactly 12 iterations with no change detection, UPLC 12, CSE 4,
  `inline-unconditional-growth 1`, `inline-callsite-growth 5`. Grepping `CostModel|ExBudget`
  over both Transform directories hits only constant folding. `profile-all` plus
  `traceToStacks` is a manual human flamegraph loop.
- **Plutarch**: no optimizer of its own, calls plutus-core at defaults.
- **solc / Yul** is the one partial exception, and it is instructive twice over.
  `libevmasm/Inliner.cpp:195` decides inlining by
  `runs · uninlinedExecutionCost + uninlinedDepositCost > inlinedDepositCost`, which is
  structurally the same trade Scalus faces, with `--optimize-runs` (default 200) as a
  **hand-set** stand-in for what Scalus can read from protocol prices. And `tools/yulPhaser`
  ran a GA over the pass-order string offline, size-only, over a corpus: hand-written
  sequence 97% of original size, GEWEP 92.6-94.2%. Its README warns about corpus overfitting.
- **EVM superoptimizers** (ebso, syrup, GASOL) use exact gas cost for **proofs of
  optimality** on tiny straight-line blocks: syrup optimized 46.06% of 61,217 blocks and
  proved 34.28% optimal, at 2 s to 1 h per block. Exactness spent on proof, not search.

The Scalus `Autotune` branch (coordinate descent over unroll factors against the exact
fee) is **already past the state of practice in this space**.

Reusable: UPLC-CAPE's fee arithmetic and scenario set are exactly this objective,
community-specified. Use them as the evaluation harness.

---

## 5. Where Scalus stands today

**Objective** (mainnet PV11): `fee = 0.0577·mem + 0.0000721·cpu + tieredRefScriptFee(size)`,
tier 15 lovelace/byte with a 1.2x multiplier per 25,600-byte stride
(`MinTransactionFee.scala:108-109`). One CEK step is 16,000 cpu / 100 mem = **6.92
lovelace**; one script byte is **15 lovelace**, so **1 byte is worth about 2.2 CEK steps
per tx**. Eliminating one recursive call saves exactly 2 steps = 13.85 lovelace.

**Global knob space is tiny.** Pure cost knobs today are `cseIterations {0..4}` x
`cceEnabled {2}` = **10 configurations**. Under a minute to enumerate exhaustively.
`valueBuiltins` is *not* a pure cost knob: the CIP-153 lowering fails on non-canonical
values where the portable lowering tolerates them, so it must sit behind the constraint
gate or be excluded. `Options.uplcOptimizers` already accepts an arbitrary pass list, so
**pass order is exposable with no new plumbing**.

**Per-site space is where the mass is**, and none of it is exposed: `2^(#beta-redexes)`
inlining decisions x `2^(#CSE candidates)` x `2^(#CCE templates)` x `2^(#if sites)`.
Validators run 315 B (HTLC) to 3,247 B (Crowdfunding) flat, on the order of `10^2`-`10^3`
UPLC nodes.

**One evaluation costs 20-300 ms** for a typical validator (18 ms HelloCardano, ~240 ms
Auction for the optimizer alone), **dominated by compilation, not by the CEK run**
(warm raw CEK is 227-934 us/op). A full `Autotune` run measured 0.84 s (FibUnfold),
2.3 s (Clausify), 8.5 s (Knights 4x4).

**Existing `Autotune`** (unmerged, `worktree-unrolling-fee-tradeoff`): greedy coordinate
descent, top 4 hottest loops in order, `k` in `{2,4,8,16}`, accept only on strict
improvement, no revisiting, no cross-loop interaction, ~18 compiles per run. Constraint
gate applies to the winner only and falls **straight back to baseline** on violation,
where the spec says to fall back to next-best.

**Two concrete defects found while surveying:**

1. **Reproducibility hazard – CONFIRMED and FIXED (2026-09-05).** `enum DefaultFun extends
   Enum[DefaultFun]` (`DefaultFun.scala:45`), so `bn.hashCode` was the `java.lang.Enum` identity
   hash. `TermKey.structuralHash` used it (`CSE:363`), so the iteration order of `counts`
   (`CSE:63`) and `templateOccurrences` (`CCE:75`) followed identity hashes, and the stable
   `sortBy((-size, key.toString))` kept that order for candidates whose 60-char `showShort`
   prefixes coincide. A 12-line validator (two field-access chains over two parameters in one
   scope, each used twice) compiled to two different scripts in different JVM runs; the 22
   example validators never moved because their tied chains sit in different scopes. Fixed by
   `LinkedHashMap` for both candidate maps plus `bn.ordinal` in the hash, guarded by a cross-JVM
   test (`CseDeterminismCrossJvmTest`, child JVMs under `-XX:hashCode=2` and `3`). Note that
   HotSpot's default identity hashing is deterministic for a fixed program in a fixed
   environment, so plain reruns cannot detect this class of bug; sweep `-XX:hashCode=N`.
   Full record: `docs/internal/UPLC_OPTIMIZER_DETERMINISM.md`. The pinned-config precondition
   is met.

2. **One dead pass.** `AbbreviateErrorTraces.scala` exists and has no non-test caller.
   It is a free knob. (`BooleanOptimizer.scala` was in the same state during this
   survey, but was made sound and wired behind `optimizeUplc` in `ebecd98a7` while this
   document was being written; see `docs/internal/BOOLEAN_OPTIMIZER_REVIEW.md`. Note it
   is gated by a separate `optimizeBooleans` parameter at `UplcPipeline.scala:55`, so it
   is a genuine knob for the grid in step 1, not a fixed part of the pipeline.)

---

## 6. Recommendation

Ordered by payoff per unit of work. Steps 0-2 need no new theory.

**Step 0. Land the `Autotune` branch, and settle reproducibility.** Everything below
builds on it. Verify the `DefaultFun` hash hazard first: a pinned config is worthless if
the same config can produce two script hashes. Note the memory's existing caveat that
`unrollFactors` keys are per-compile symbol names and do not survive recompiles, so the
pinning story on the branch is itself incomplete.

**Step 1. Enumerate the global knob grid.** 10-20 configurations, exact objective, under
a minute per contract. No search algorithm needed. This is strictly better than anything
BO or RL would do at this scale, and the literature is unambiguous about it. Add the two
dead pass and `optimizeBooleans` as knobs while you are there.

**Step 2. Strengthen coordinate descent cheaply.** Add 2-3 restarts from different loop
orderings and one pairwise refinement pass over the two hottest loops: about 25-40
evaluations, all memoized on a hash of the post-pass term. Kulkarni measured hill
climbing with 100 restarts at 0.02% off the true optimum, and redundancy filtering
removing 87.7% of evaluations at zero loss. Seed the candidate ladder from the analytic
`k*` instead of the fixed `{2,4,8,16}`.

**Step 3. Thread stable site ids from SIR through lowering into UPLC annotations.** This
is the enabling work for section 3 and the largest single item. Today the profile joins
to code by `(file, line)` spans with documented blind spots (one-line recursive defs are
invisible; helpers inside a span inflate counts).

**Step 4. Make the passes profile-aware, one at a time, each gated by its own marginal
formula from section 2(c).** Start with per-site inlining, since section 1.4 already
flags the global `<= 64` rule as suspect. Then CSE candidates and forced-builtin
extraction. This is where "best in the common path, compact in the rare path" is
delivered, and it does not need a mode flag: `count(s)` decides.

**Step 5. Only if phase ordering becomes the bottleneck:** open `uplcOptimizers` and
search over **distinct terms** (hash after each pass, prune dormant passes, stop at
fixpoint leaves), never over raw sequences. Multi-restart hill climbing over leaves.
Cooper's finding applies: expect most of the win to come from shipping one better fixed
default, not from per-contract order search.

**Not recommended:** SGD or backprop over configurations (section 1); Bayesian
optimization (the space is enumerable and evaluations are cheap); RL policy learning
(needs a corpus 3 orders of magnitude larger than Scalus has, and CompilerGym measured
learned policies below 60 s of random search); a learned surrogate cost model (the true
cost model is exact, so a surrogate can only add error).

**Deferred, genuinely interesting:** profile-weighted e-graph extraction. It is
unpublished, it would be a real contribution, and the binder problem plus the
non-locality of CEK cost make it a research project rather than a feature. The MCMC
variant, which uses the true fee as the acceptance test and needs no local cost, is the
cheaper way in.

---

## 7. Honest ROI ceiling

Any search over knobs is bounded above by the best knob setting's fee. Measured on this
codebase that is **0 to 5%** per contract. Kulkarni's exhaustive study says a
well-iterated pipeline is already within 4.8% of optimal on average and **exactly
optimal on 42% of functions**. Compare against what new rewrites bought here: natural
merge sort 7.7x, Value builtins 13-75x, `Order` as native constr -20%.

The strategic conclusion: build the marginal-cost model (section 2c) as the **gate for
evaluating new rewrites**, not primarily as a search engine over existing knobs. The
formula is the durable asset; the search is a modest bonus on top.

---

## Sources

Codebase, surveyed at master `4fcf2b0ee`, checked against `ecdffe9b4`: `compiler.scala:8-46`, `OptimizerPipelines.scala:27-61`,
`UplcPipeline.scala:38-82`, `Inliner.scala:142-153`, `CommonSubexpressionElimination.scala:139-169`,
`CommonContextExtraction.scala:403-412`, `Cek.scala:993-1130`, `ProfilingData.scala:6-70`,
`MinTransactionFee.scala:62-121`, `FlatCodec.scala:64-67`, `DefaultFun.scala:45`.
Branch `worktree-unrolling-fee-tradeoff`: `autotune/Autotune.scala`, `sir/transform/LoopUnroll.scala`,
`docs/superpowers/specs/2026-08-31-profile-guided-autotune-design.md`.
Prior findings: `docs/internal/UNROLLING_REF_SCRIPT_FEE_TRADEOFF.md`.

Literature: Cooper et al. LCTES 1999; Kulkarni et al. CGO 2006 / CGO 2007 / TACO 2005 /
TACO 2009; Touati and Barthou CF 2006; Pan and Eigenmann CGO 2006; Fursin et al. IJPP 2011;
Ansel et al. PACT 2014; Chen et al. TACO 2012; Agakov et al. CGO 2006; Bergstra and Bengio
JMLR 2012; Hutter et al. LION 2011; Chen et al. ICSE 2021 (BOCA); Frazier arXiv 1807.02811;
Bischl et al. arXiv 2107.05847; Trofin et al. arXiv 2101.04808 (MLGO); Cummins et al. CGO 2022
(CompilerGym) / arXiv 2309.07062 / arXiv 2407.02524; Haj-Ali et al. MLSys 2020 (AutoPhase);
Mammadli et al. arXiv 2008.08951; Gaunt et al. arXiv 1608.04428 (TerpreT); Renda et al. MICRO 2020
(DiffTune); Cai et al. ASPLOS 2025 (SmoothE); Zhao et al. ASPLOS 2024 (Felix); Liu et al. ICLR 2019
(DARTS) and Zela et al. ICLR 2020; Willsey et al. POPL 2021 (egg); Tate et al. POPL 2009 (Peggy);
Goharshady et al. OOPSLA 2024; Koehler and Steuwer arXiv 2111.13040; Schneider et al. PLDI 2025
(slotted e-graphs); Cao et al. POPL 2023 (babble); Hong et al. EGRAPHS 2026 arXiv 2605.19005;
Schkufza et al. ASPLOS 2013 / PLDI 2014 (STOKE); Bunel et al. ICLR 2017; Joshi et al. PLDI 2002
(Denali); Nagele and Schett arXiv 2005.05912 (ebso); Albert et al. CAV 2020 (syrup).
Compilers read directly: aiken `@49a2ced`, plutus `@1.63.0.0`, UPLC-CAPE `@b8b165a`, solidity `develop`.
