# Verification and proposed response to the CSE/CCE review

Historical investigation before implementation. See the [resolution](CSE_CCE_REVIEW_RESOLUTION_2026-09-11.md) for completed fixes and validation.

Fix semantic safety and exponential traversal before changing profitability. Most substantive findings in [the review](CSE_CCE_REVIEW_2026-09-11.md) are correct, but several diagnoses, remedies, and priorities need qualification.

Verified against `d315bb9b0` in `worktree-cce-bit-pricing`. This investigation changes no production code or tests. Temporary probes and baseline optimizer copies have been removed from the repository; their sources and logs remain in `/tmp/scalus-review-verification/`.

## Method and limits

Eight focused probes cover F1-F7, F12 and F13. A second run compares relevant cases with optimizer implementations from `c8a247859`. Two additional probes count Data size calculations and exercise comma-decimal log parsing. All probes assert or report the existing behavior; they are diagnostic reproductions, not proposed regression tests.

The default Nix shell and test JVM use JDK 25, confirmed both by `java -version` and `System.getProperty("java.version")`. The review's claim that plain `nix develop` selects JDK 21 is incorrect: `flake.nix` selects 25 for `default`/`bench`, and 21 for `ci`. For a JDK 21 validation gate, use `nix develop .#ci` and verify the actual test JVM. A client can otherwise attach to an already running sbt server. This investigation did not reproduce the claimed `FromDataDerivationTest` environment failure.

The 23-blueprint corpus and historical validator budgets were not regenerated during this investigation. Existing measurements are evidence to revisit after fixes, not newly verified performance promises.

## Reproduced findings

| Finding | Independent evidence | Assessment |
|---|---|---|
| F1 | A 1,024-byte shared constant plus `appendByteString c c` grows from 1,049 to 3,100 CBOR bytes. The old inliner leaves it at 1,049. CEK results are equal. With four additional direct uses, 1,053 becomes 3,107 bytes. | Confirmed size regression. Our fixture/serialization counts differ from the review's 1,042/3,094; the growth mechanism agrees. This is S2 under the review's definitions, not a demonstrated wrong-result S1. |
| F2 | Retained-constant chains take 53 ms at k=12, 267 ms at k=16, 979 ms at k=18, and 4.14 s at k=20. | Confirmed exponential behavior on the default path. Fix before broader measurement. |
| F3 | Three heterogeneous delayed leaves: original succeeds with a false condition; CCE fails. The extracted template is 81 bits with claimed savings of 21.7 bits. | Confirmed semantic bug. The same fixture also fails under master's CCE: inherited, not introduced by this branch. CCE is optional, but this still deserves an immediate fix. |
| F4 | The repeated `map (lam x. addInteger x 1) xs` is 85 estimated bits with +78.6042 lovelace savings. Current CSE extracts nothing; master's CSE extracts it. | Confirmed regression caused by name-sensitive keys after global binder renaming. |
| F5 | Two equal G1 constants cause CSE to throw the Flat-encoding exception. | Confirmed public optimizer crash. Limited exposure through normal compilation does not justify postponing a small safety fix. |
| F6 | Two occurrences of `Apply(Var(length), Var(xs))` are 28 bits and score -28.2708 lovelace. | Confirmed rejection mechanism. Earlier controlled Knights measurements independently established its practical cost. Escrow's cited pin exists, but its runtime was not re-measured here. |
| F7 | One CSE pass over 50/100/200/400 distinct profitable groups takes 106/194/904/6,422 ms. | Confirmed scaling problem. Exact times depend on JVM warmup and hardware. |
| F13 | `fee(204800, 15)` returns 6,335,648; `fee(460800, 15)` returns 49,196,799; `fee(460801, 15)` throws on an intermediate rational numerator. | Confirmed. Independent unbounded rational arithmetic gives 49,197,199 for 460,801 bytes, which fits `Coin`. |
| F14 | A 2,102-byte Data list under a chain of 15 applications is sized 120 times in one CCE pass. Our four-arm variant causes 912 size calls. `Flat[Data].bitSize` CBOR-encodes on every call. | Confirmed repeated serialization. The review's exact four-arm count of 604 was not reproduced; its fixture is not specified sufficiently to establish an identical shape. |

The Data counter was added to a temporary copy of CCE and counted Data constants reached by each size traversal. It was not a timing measurement of a modified production encoder.

## Findings verified by source inspection, with qualifications

### F8: stale documentation

Confirmed: the site still says repeated constants inline below 64 bits; the placement document says CCE pricing is unchanged; traces are enabled by default. The changelog does disclose traced-build growth, so “hides” is too strong, but it should explicitly say that traced builds are the default.

Do not assume F1 causes HelloCardano's or AMM's recorded growth. That causal link has not been established. `containsTrace` prevents folding an entire trace expression, although foldable subexpressions can still be simplified before reaching it. Re-measure after fixes, then write the final numbers.

### F9: Knights comparison provenance

Confirmed: the source algorithm changed while the historical Plutus reference budgets stayed fixed. However, `docs/design/knights-htlc-measurements.md` contains no Scalus-versus-Plutus ratio; its tables compare Scalus variants. That specific accusation is false.

Restoring the name `quicksort` would not restore the old Plutus sorting algorithm: our `quicksort` already aliases merge sort. Nor do unchanged historical pins establish which computations the Plutus optimizer actually shared.

Keep the explicitly requested Knights optimization. Label the external comparison as a historical reference, with source/compiler provenance. If we need a compiler-only comparison, add a genuinely equivalent reference workload and regenerate both sides, rather than reverting the optimized example and calling it equivalent.

### F10: self-referential profitability assertion

Confirmed: the applied extraction and the test both trust `extractionSavingBits`; the log merely echoes the metric checked by the guard. A constant-positive pricing mutation is invisible to this assertion. It may catch removal of a guard, but it does not independently test profitability.

Under `Locale.GERMANY`, the probe emits one extraction log but the current regex parses zero savings, and `losers.isEmpty` is true. This directly confirms the vacuous locale case.

Add isolated output-based cases where a deliberately wrong price admits a growing extraction. Also measure whole-pass size changes on the corpus. A total decrease alone cannot prove that every individual extraction pays; use structured per-extraction observations if that stronger claim remains a requirement. Avoid test-only public API expansion and log parsing.

### F11: duplicated pricing machinery

Confirmed duplication. At current machine costs, three Apply charges and Apply+LamAbs+Var have the same numeric cost. This is a maintenance problem, not evidence of a present arithmetic discrepancy.

Share prices and per-node cost conversion, while retaining distinct CSE and CCE overhead formulas. Do not round each node's fee with `ExUnits.fee`: that method rounds up, whereas these profitability estimates currently retain fractional lovelace. `MinTemplateBits` already follows `StepBits`; the initialization-order warning describes a future refactor hazard, not a current bug.

### F12: encoder premise

Confirmed documentation error: unassigned index 0 is accepted. Named variables, lambda terms and hole-containing templates size successfully through `Flat[Term].bitSize` and agree with the estimate when their variable widths agree.

The actual reason for the approximation is fixed-width variables, not inability to size index 0. Keep the small estimator for now, correct its documentation, and add encoder-parity properties. A callback inside Flat's term-size traversal could remove duplication later. Simply wrapping the existing encoder does not override its recursive Var handling, and normalizing an entire tree for every candidate could worsen F7.

### F15: leaderboard duplicate

Confirmed: the leaderboard retains a separate tiered calculator and literal execution prices. Reuse the fee calculation. Choose the price policy explicitly: CAPE benchmark prices may need to remain fixed for comparability rather than silently following a newer repository mainnet snapshot.

Preserve one ceiling over the sum of execution units, not a sum of separately rounded case fees. Deduplicating the mechanism and selecting the benchmark's price snapshot are separate decisions.

### F16: punctuation

Confirmed, low priority. Fold this into documentation cleanup; do not put it ahead of semantic failures or exponential compilation.

## Important corrections to the suggested fixes

### F1: a local size guard is conservative, not globally optimal

A guard against growth relative to the original subterm prevents the reproduced duplication. It can also reject profitable folds when several results would together eliminate a large constant binding. The first patch should stop pathological growth and preserve existing small folds; follow with binding-aware pricing if needed, rather than expanding the folded result unconditionally or claiming a local guard minimizes total fees.

### F2: the “originalBody” is already optimized

`originalBody` comes from `inlinedF`, so it is already the result of the first traversal. The review's fallback advice to revisit the “already optimized body” describes what the code currently does. Handle syntactic let redexes before descending into the lambda, and separately test functions that simplify into lambdas. Merely renaming or substituting the body variable will not fix the repeated traversal.

### F4: preserve scope identity while adding alpha-equivalence

Keep unique outer binder identities for scope safety. Canonicalize binders internal to a candidate; free references retain their unique outer identity. A suitable key distinguishes `Bound(relativeIndex)` from `Free(uniqueName)`.

Do not directly reuse `DeBruijn.deBruijnTerm(subterm)` as a key: it retains binder names and assigns negative indices to free variable occurrences. Those indices are not a stable canonical identity for free names. Test equal lambdas, unequal captures, shadowing, and equal/hash consistency.

### F5: a large size sentinel reverses the intended result

For repeated terms, larger `termBits` means greater estimated savings. A large sentinel makes an unsupported constant more attractive, and may also overflow arithmetic. Prefer an explicit unavailable-size result that propagates to candidate rejection, or a cached serialization-preparation size where supported. Cover G1/G2 and BLS-containing composite constants, not only a standalone G1 case.

### F6: neither proposal is a complete cost model

For two `f(x)` calls, the current score is -28.2708 lovelace. At 6.9236 lovelace per CEK node, even crediting all three visible nodes adds only 20.7708: the score remains -7.5. Crediting only the Apply spine adds still less. The proposed spine-only formula therefore cannot make the review's own `length(xs)` test pass.

When starting from the current one-node-value overhead formula, the additional credit is based on avoided evaluation beyond the replaced variable lookup. Crediting the whole expression again double-counts that allowance. Optional descendant occurrences and repeated lambda execution also prevent syntactic occurrence counts from being exact runtime counts.

My recommendation: keep fee pricing for values, and give repeated computations a separate policy. Use known builtin costs where reliable; for opaque function calls, a runtime-sharing preference is a deliberate trade-off rather than a numerical prediction. The old filter should not become a safety rule again. Preserve safe placement independently of profitability.

Admitting size-increasing computational sharing also invalidates CSE's current termination argument based on monotonically decreasing `termBits`. Address termination/convergence before enabling that policy. Do not just remove the positive-size gate and re-pin budgets.

### F7/F14: cache first, batch later

Start with per-pass cached size/constant metadata and cached structural hashing, keeping extraction order unchanged. Alpha-aware keys need careful cache boundaries because bound/free identity depends on candidate scope.

Batching independent candidates changes selection, binding insertion, and potentially output layout. Treat it as a later optimization with separate safety and corpus checks. Replace strict two-second unit-test expectations with semantic tests and a reproducible performance benchmark; elapsed-time checks need generous limits and recorded JVM/hardware conditions.

### F13: use wider intermediate arithmetic

Use unbounded intermediate arithmetic and check the final `Coin` range explicitly. Imposing a 204,800-byte ledger-policy limit on a general calculator unnecessarily restricts off-chain analysis and does not by itself prevent overflow for arbitrary base prices.

## Other live items

| Item | Assessment / action |
|---|---|
| `cseIterations=0` with CCE enabled | Confirmed unconditional post-CCE CSE. Define the option's scope explicitly; prefer honoring zero globally if it means “disable CSE,” with a configuration test. |
| Deprecated forwarding helpers | Present for compatibility. Do not add broad MiMa filters merely to remove a few wrappers; establish release history and intended removal policy first. |
| `termSize` | No production callers, but multiple test callers exist. The review's “no callers” claim is inaccurate. Move test-only functionality or replace those assertions if deleting it. |
| Forced-builtin CCE exceptions | Their decompositions cannot reach the 40-bit threshold. The hole templates are 16/20 bits, not the original forced-builtin sizes 15/19. Remove the dead exceptions and update their tests. |
| Corpus script path | The default is version-specific, but `--root` already overrides it. Add an explicit version option or document the override; do not silently select a stale `scala-*` directory. |
| Prefix-based partial-builtin classification | CSE does not use it. Further CCE cleanup should avoid granting trust solely because a name starts with `__`; do not confuse the old prefix concern with the independently reproduced delayed-leaf bug. |

## Proposed implementation order

These are work stages, not an instruction to combine unrelated fixes into one commit.

1. **Safety:** fix F3 conservatively by stopping decomposition beneath Delay; add the heterogeneous success/failure reproduction. Handle F5's unavailable encoding sizes and add unsupported-constant coverage. Repair the relevant output-based tests from F10 alongside these fixes.
2. **Inliner:** fix F2's traversal first, then F1's uncontrolled growth. Preserve shadowing, success/failure, and the small folds that motivated constant propagation. Include direct and newly exposed lambda redexes.
3. **CSE equality and metadata:** fix F4 with scope-aware alpha keys, and address F7/F14 with caching that preserves selection order. Verify capture safety, hash consistency, and scaling. Defer batch extraction.
4. **Profitability decision:** unify pricing primitives under F11; correct/document F12 and test parity. Implement the chosen value/computation policy for F6 only with an explicit convergence argument. Re-measure Escrow and Knights rather than assuming either proposed formula recovers them.
5. **Fee utility:** fix F13 using wider intermediates, then deduplicate F15 while preserving the intended benchmark price policy and aggregate rounding.
6. **Configuration and evidence:** clarify F9's historical benchmark reference, CSE-disable semantics, locale formatting and test-only/deprecated helpers. Keep the optimized Knights implementation.
7. **Fresh release measurements:** clean only the affected example build, regenerate the 23-blueprint corpus, and run traced/default and release comparisons. Re-measure compiler-specific pins on 3.3.8 and 3.8.4. Update F8/F16 and the changelog with final results, not intermediate numbers.

Budget roughly 2-3 working days for the fixes and validation before attempting batched extraction. Re-estimate after the Inliner and alpha-key fixes reveal the remaining corpus deltas. The immediate patch should address safety and exponential traversal, not punctuation or a new global cost-model framework.
