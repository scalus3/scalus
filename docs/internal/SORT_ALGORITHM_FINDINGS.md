# Sorting in the Scalus on-chain stdlib – measurements and recommendation

**Status:** IMPLEMENTED. 2026-09-01. `List.sort` is now a stable natural merge sort and the
`UplcConstr` sort intrinsic has been deleted. Sections 1-5 are the measurements that led there;
§9 records what was implemented, what the measurements got wrong, and what is still open.

> **Reading note.** Sections 2-5 were measured with
> `Options(generateErrorTraces = false, optimizeUplc = true)`, which is NOT the production
> lowering – it leaves `removeTraces` off. Everything in §9 is re-measured under `Options.release`.
> Where the two disagree, §9 is correct. The rankings held under both; the absolute figures moved.

**Harness:** `scalus-core/jvm/src/test/scala/scalus/uplc/eval/SortBudgetTest.scala`,
`SortCandidates.scala`, `SortedMapFromListBudgetTest.scala`,
`SortedMapFromListCandidates.scala`. Re-run with
`sbtn "scalusJVM/testOnly scalus.uplc.eval.SortBudgetTest scalus.uplc.eval.SortedMapFromListBudgetTest -- -oD"`.
**Context:** extends `docs/internal/stdlib-research/00-RESEARCH-REPORT.md`; the cost rules it
cites live in `07-efficiency-constraints.md`.

---

## 1. Summary

Two prelude functions were quadratic on the input shape they most often see, and both are fixed.

**`List.sort` was a head-pivot quicksort** – Θ(n²) on already-sorted, reverse-sorted and all-equal
input. The ledger *guarantees* `tx.inputs` arrives ordered by `TxOutRef`, and `SortedMap` contents
are key-ordered, so the sort was worst-cased by its most common input. It is now a stable natural
merge sort.

**`SortedMap.fromList` folded left**, inserting into a growing sorted accumulator, which made
already-ascending keys its worst case for the same reason. At n=64 that cost 3.53 billion CPU units
– 88% of a whole transaction's memory budget – for one map. It now folds right.

### What shipped

| | before | after | at n=64 |
|---|---|---|---|
| `List.sort` | head-pivot quicksort | stable natural merge sort | **6.3x cheaper** |
| `SortedMap.fromList` | insertion sort, fold left | insertion sort, fold **right** | **28x cheaper on ascending keys** |
| `UplcConstr` sort intrinsic | 3 layers of machinery | **deleted** | it was making things *worse* |

New API: `List.insertionSort`, `List.sortWith(lt)`, `List.isSorted`, `List.isStrictlyAscending`,
`List.isSortedWith(lt)`, `SortedMap.fromLargeList`.

### The caveat that belongs next to the headline

`List.sort` is **not cheaper at every size**. Merge sort has a bigger body, and that is fixed
overhead paid even on an empty list:

| n | 0 | 2 | 4 | 5 | 8 | 16 | 32 | 64 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| old (lovelace) | 277 | 1,780 | 4,988 | 7,232 | 16,519 | 60,041 | 228,924 | 894,051 |
| new | 464 | 2,248 | 5,096 | 6,490 | 12,471 | 24,865 | 65,812 | 141,105 |
| | **1.7x worse** | 1.26x worse | 1.02x worse | 1.11x better | 1.3x | 2.4x | 3.5x | **6.3x** |

Break-even is **n = 5**.

Script size grew 169 → 277 bytes. `insertionSort` exists for callers whose length is bounded by
construction and ≤ 8, where it is up to 2.1x cheaper and a third the size.

The trade is accepted because the old worst case was reachable with ordinary ledger-shaped input,
and a validator must be provisioned for its worst case rather than its typical one.

### Memory, not CPU, is the metric

Fee is `priceMemory × memory + priceSteps × steps`, and a memory unit costs ~800x a CPU step. The
measured split is ~80% memory / ~20% steps for every candidate, and **memory binds first at every
size**: the old `List.sort` needed 75% of a transaction's memory allowance at n=64, and the old
`fromList` needed 88%. Ranking on CPU alone ranks on the cheap fifth. (§3.1)

### Sorting is usually the wrong tool

Of eight production Cardano codebases surveyed, **exactly one sorts on-chain**. Six verify an
order supplied off-chain instead. Plutarch ships no sort at all – only `pcheckSorted`. That is why
`isSorted` / `isStrictlyAscending` were added: verifying costs about a seventh of sorting at n=64,
and about a fortieth of the quicksort that used to be there. (§2.1)

## 2. What the ecosystem does

Read from source, not from memory. Three independent surveys; sources cited inline.

### 2.1 Nobody sorts on-chain

Across 8 production Cardano codebases and 4 on-chain standard libraries, **exactly one validator
calls a general-purpose sort**, and it sorts a fixed list of 11 integers.

| Protocol | Sorts on-chain? | What it does instead |
|---|---|---|
| SundaeSwap v3 | No | Scooper supplies the order in the redeemer; validator proves each index is used exactly once via an arithmetic bit-vector (`lib/calculation/shared.ak:74`) |
| Minswap v2 | No | Batcher supplies `input_indexes`; validator proves the permutation is a bijection with a 64-byte bitmap (`validators/pool_validator.ak:243`) |
| Splash, Spectrum | No | **Ship a merge sort and a tim sort in `PExtra/List.hs` and never call either one** |
| cip113, brale, ft-bifrost | No | Sorted linked list, `key < next` checked on insert |
| cardano-swaps, Sundae, Minswap | No | Canonical ordering of a *pair* of assets only |
| **binocular (Scalus)** | **Yes** | Hand-rolled insertion sort over 11 Bitcoin timestamps (`BitcoinValidator.scala:438`) |

Note the last row: the one Scalus protocol in the corpus that needs a sort **wrote its own
insertion sort instead of calling `List.sort`**.

SundaeSwap states the position most directly, in `lib/calculation/InputSorting.md`:

> One simple way to do this is to have the scoop redeemer have an array of integers, and just sort
> the inputs based on that order. **However, this is horribly inefficient because of the primitives
> we have available to us.** For example, with no random access, accessing the Nth input means we
> have to iterate from 0 to N. So, in total, we end up traversing the input list O(n²) times.

Ranked by number of distinct codebases, the actual ordering *needs* are:

| Rank | Need | Codebases |
|---|---|---|
| 1 | Verify an off-chain-supplied order | 6 |
| 2 | Sorted linked-list invariant `key < next` | 5 |
| 3 | Duplicate detection over a small index set | 4 |
| 4 | Canonical two-element pair ordering | 3 |
| 5 | **General-purpose sort of a list** | **1** |

### 2.2 What the reference implementations use

| Library | Sort | Algorithm | Verify-sortedness primitive |
|---|---|---|---|
| **Plutus-tx** 1.63.0.0 | `List.sortBy` | **natural bottom-up merge sort** (GHC's `Data.List.sortBy`, copied verbatim) | none |
| **Plutarch** (master) | **none at all** | – | `pcheckSorted`, O(n) |
| liqwid-plutarch-extra | `psort` | merge sort + 2/3/4-element sorting networks; **not adaptive** | `pisSortedBy`, `pallUniqueBy` |
| **Aiken stdlib** v3.0.0 | `list.sort` | **insertion sort** (`list.ak:1109`) | `dict.check_ascending_list` |
| Scalus today | `List.sort` | **head-pivot quicksort** | none |

Scalus is the only one of the five using quicksort.

### 2.3 IOG published this exact comparison, and it agrees

Kenneth MacKenzie (IOG), reviewing the benchmark PR that added these implementations
([plutus#4063, discussion_r723814528](https://github.com/IntersectMBO/plutus/pull/4063#discussion_r723814528),
2021-10-07). Each algorithm is run **on its own worst-case input**, which is the correct frame for
an adversarial execution budget:

| n = 1000 | CPU ExUnits | CEK steps | worst-case input used |
|---|---:|---:|---|
| GHC natural merge sort | 31,397,907,407 | 1,007,953 | max-interleaving |
| standard merge sort | 196,070,419,728 | 6,174,700 | max-interleaving |
| insertion sort | 1,087,200,622,538 | 34,056,106 | **already ascending** |
| head-pivot quicksort | 2,565,513,999,774 | 80,123,138 | **reverse-sorted** |

Natural merge sort is **82x** cheaper than head-pivot quicksort at n=1000, and the ranking is the
same one measured here from scratch on the Scalus pipeline. These are October 2021 numbers on
Scott-encoded lists and a pre-mainnet cost model, so treat the ranking as durable and the absolute
figures as not.

### 2.4 Aiken chose insertion sort on measurements, at small n

Worth stating precisely, because it is easy to read Aiken's choice as an oversight. The commit that
introduced `list.sort` (`fbe89ef`) says:

> I've tried three main approach: quicksort, mergesort and insert sort. The insert is the one which
> to gives the best results in all test bench.

Their test benches are ~10-element lists. That is consistent with everything above and with §4.1
here: insertion sort wins below the crossover, and Aiken measured only below the crossover. Note
also that Aiken's fold direction makes **ascending** input its best case, the opposite of the
plutus-benchmark variant – a detail to preserve if the shape is ever mirrored.

### 2.5 "Inputs arrive sorted" is a ledger promise, not an accident

This underpins the whole worst-case argument, so it is worth pinning down.
[plutus#4296](https://github.com/IntersectMBO/plutus/issues/4296), JaredCorduan (cardano-ledger):

> the ordering is the lexicographic ordering on pairs `(TransactionID, TransactionIndex)` … we are
> essentially promising that plutus scripts can depend on this. ie that we will never change it.
> with the advantage of making scripts more efficient.

Outputs, by contrast, keep the builder's order. So `ascending` is a guaranteed shape for inputs, and
the current sort is quadratic on exactly that shape. (Caveat for the future:
[CIP-0128](https://github.com/cardano-foundation/CIPs) "Preserving Order of Transaction Inputs" is
`Proposed`; if it lands, inputs to new-format transactions stop arriving sorted. That would remove a
best case, not create a new worst case, so it does not change the recommendation.)

Two quotes worth carrying into the scaladoc, both from `plutus-benchmark/lists/`:

> Experiments on random lists suggest that the behaviour for both ghcSort and mergeSort on the
> output of `mergeSortWorstCase` is only 3% or 4% worse than the average case.
> – `Sort/GhcSort.hs:61-72`

> The worst case is when the list is already sorted (or reverse sorted) because then if the list
> has n elements you have to recurse n times, scanning a list of length n-1, n-2, n-3, …
> – `Sort/QuickSort.hs:31-36`, describing the exact algorithm Scalus ships

---

## 3. Method

Each candidate is compiled once as `(d: Data) => sortX(fromData(d)).toData` and the list is applied
as a **UPLC argument**, not written as a literal inside the `compile {}` block – a literal lets the
optimizer fold the whole sort at compile time and every row reads zero. Lowering is
`Options(generateErrorTraces = false, optimizeUplc = true)`, i.e. production-like. Costs are mainnet
PV11, fees at mainnet `executionUnitPrices`.

Every candidate is generic in `A` with an `Ord[A]` context bound, exactly like the shipped `sort`,
so what differs between rows is algorithm shape and not calling convention.

**Input shapes.** `random`, `ascending`, `descending`, `allEqual`, `fewUnique`, `sawtooth`,
`organPipe`. Ranking is by the **worst** of these, because a validator must be provisioned for the
worst input an adversary can present. `ascending` is the ledger's own ordering; `sawtooth` and
`organPipe` are the classic shapes that defeat a fixed deterministic pivot rule.

**Correctness** of every candidate on every input is asserted by the harness; budgets are reported,
not pinned, so a compiler change reports new numbers rather than failing.

### 3.1 Rank on fee and memory, not on CPU

The ledger charges `fee = priceMemory * memory + priceSteps * steps`. At mainnet prices that is
`0.0577 * memory + 0.0000721 * steps`, so **one memory unit costs about 800x one CPU step**.
Measured across every candidate at n=64, the split is remarkably uniform:

| candidate | share of fee from memory | from steps |
|---|---:|---:|
| prelude (qsort) | 80.2% | 19.8% |
| natural merge | 80.6% | 19.4% |
| natural merge (lt) | 80.3% | 19.7% |
| insertion sort | 80.3% | 19.7% |
| merge (counted) | 78.5% | 21.5% |

**Roughly 80% of what a sort costs is memory.** The transaction limits are lopsided the same way:
10 billion steps against 16.5 million memory units, and in the headroom table every candidate at
every size hits the memory ceiling first, typically at ~3x the CPU percentage.

Two consequences for how to read this report:

1. Every table below reports **memory and fee** alongside CPU. Where only one number is shown, it is
   the fee.
2. CPU and memory turn out to be strongly correlated here, because both are driven by the same
   things: cons-cell allocation and `Order` construction per comparison. So the **ranking is the
   same** whichever of the three you sort by. What changes is the magnitude and the severity: the
   quadratic candidates look bad on CPU and alarming on memory.

---

## 4. Results: `List.sort`

### 4.1 Worst-case CPU across all input shapes

| n | prelude (qsort) | insertRight | insertLeft | mergeDeal | **mergeNatural** | qsortMedian3 | mergeGuarded | mergeCounted | hybrid |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 0.67M | 0.52M | 0.56M | 0.92M | 1.15M | 1.15M | 0.57M | 1.26M | 1.19M |
| 1 | 2.36M | 1.14M | 1.11M | 0.99M | 2.38M | 1.35M | 0.73M | 1.61M | 2.24M |
| 2 | 5.21M | 2.78M | **2.68M** | 4.64M | 6.11M | 9.83M | 5.46M | 6.94M | 4.36M |
| 4 | 14.42M | 9.11M | **8.89M** | 14.74M | 14.07M | 21.67M | 17.60M | 17.53M | 11.81M |
| 8 | 46.83M | 34.03M | **33.55M** | 40.78M | 34.95M | 56.16M | 45.91M | 41.75M | 39.52M |
| 16 | 167.64M | 132.84M | 131.85M | 104.30M | **69.74M** | 161.94M | 114.42M | 99.72M | 74.64M |
| 24 | 363.09M | 296.96M | 295.45M | 179.24M | **120.68M** | 321.37M | 193.97M | 161.11M | 125.58M |
| 32 | 633.18M | 526.38M | 524.36M | 254.46M | **185.82M** | 533.55M | 275.26M | 224.79M | 190.72M |
| 48 | 1397.29M | 1181.15M | 1178.11M | 427.30M | **290.68M** | 1116.12M | 458.16M | 365.63M | 295.58M |
| 64 | 2459.96M | 2097.16M | 2093.09M | 600.78M | **398.89M** | 1909.65M | 644.56M | 511.48M | 403.79M |

**The crossover is n ≈ 8.** Below it, insertion sort wins by up to 2.3x (at n=2). From n=16 up,
natural merge sort wins and the gap widens without bound.

### 4.1b Worst-case MEMORY (the binding resource)

| n | prelude (qsort) | insertLeft | mergeDeal | mergeNatural | qsortMedian3 | mergeCounted | **natural(lt)** | insertLeft(lt) |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 8 | 227,764 | 173,064 | 201,775 | 181,450 | 286,241 | 180,279 | 148,736 | **136,636** |
| 16 | 831,084 | 675,536 | 517,960 | 362,006 | 839,077 | 440,209 | **313,999** | 519,416 |
| 32 | 3,176,284 | 2,678,496 | 1,268,315 | 962,731 | 2,792,125 | 1,008,438 | **763,750** | 2,033,200 |
| 48 | 7,039,564 | 6,012,144 | 2,133,656 | 1,507,357 | 5,866,709 | 1,658,920 | **1,191,533** | 4,544,616 |
| 64 | 12,420,924 | 10,676,480 | 3,004,392 | 2,067,995 | 10,062,829 | 2,334,200 | **1,624,515** | 8,053,664 |

At n=64 the shipped sort needs 12.4M memory units against a 16.5M whole-transaction limit. Natural
merge with a boolean comparator needs 1.6M, a **7.6x** reduction, and is the only candidate still
comfortably inside the budget with room for the rest of the validator.

### 4.1c Worst-case FEE (lovelace)

| n | prelude (qsort) | insertLeft | mergeDeal | mergeNatural | qsortMedian3 | mergeCounted | **natural(lt)** | insertLeft(lt) |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 8 | 16,519 | 12,405 | 14,583 | 12,990 | 20,566 | 13,413 | 10,679 | **9,792** |
| 16 | 60,041 | 48,485 | 37,407 | 25,916 | 60,091 | 32,590 | **22,587** | 37,285 |
| 32 | 228,924 | 192,356 | 91,529 | 68,948 | 199,575 | 74,394 | **54,908** | 146,065 |
| 48 | 506,928 | 431,843 | 153,921 | 107,933 | 418,982 | 122,082 | **85,663** | 326,567 |
| 64 | 894,051 | 766,945 | 216,670 | 148,084 | 718,311 | 171,561 | **116,793** | 578,792 |

The fee ranking is identical to the CPU ranking at every size, which is what §3.1 predicts from the
uniform ~80/20 split. The crossover on fee is the same n ≈ 8-16: `insertLeft(lt)` is 8% cheaper at
n=8, and `natural(lt)` is 65% cheaper by n=16.

### 4.2 Growth law, confirmed by comparison counts

Comparison counts computed independently of the VM, from mirrors of the same algorithms
(`compare_counts.py`, worst case over the same input shapes):

| algorithm | n=8 | n=16 | n=32 | n=64 | n=128 | growth |
|---|---:|---:|---:|---:|---:|---|
| prelude (head-pivot qsort) | 28 | 120 | 496 | 2016 | 8128 | **O(n²)** – exactly n(n−1)/2 |
| insertion sort (either direction) | 28 | 120 | 496 | 2016 | 8128 | **O(n²)** |
| quicksort, median-of-3, 3-way | 32 | 96 | 320 | 1152 | 4352 | **O(n²)** |
| merge sort (deal) | 17 | 49 | 129 | 321 | 769 | O(n log n) |
| **natural merge sort** | 21 | 52 | 135 | 346 | 788 | O(n log n) |
| `isSorted` only | 7 | 15 | 31 | 63 | 127 | **O(n)** |

### 4.3 Which input shape is each algorithm's worst case

This is the part that matters for a validator's budget provisioning.

| algorithm | worst shape at n=64 | comment |
|---|---|---|
| prelude (qsort) | **allEqual** | also Θ(n²) on ascending and descending |
| insertLeft | **ascending** | the ledger's own ordering |
| insertRight | **descending** | mirror image of insertLeft |
| qsortMedian3 | **organPipe** | median-of-3 does **not** remove the quadratic case |
| **mergeNatural** | **random** | every *structured* shape is cheaper than its worst case |

`mergeNatural` is the only candidate whose worst case is the unstructured input. That is the
property you want: an attacker cannot construct a shape that is worse than noise.

### 4.4 Candidates that lost, and why

- **`qsortMedian3`** – median-of-three pivot plus a three-way partition. It fixes the sorted and
  all-equal cases but `organPipe` still drives it to O(n²) (1152 comparisons at n=64), it costs two
  extra passes per level to find the middle and last elements, and it is the **largest** script at
  365 bytes. There is no cheap patch that saves quicksort here: any fixed deterministic pivot rule
  has a constructible worst case, and on-chain there is no randomness to fall back on.
- **`hybrid`** (insertion below 8, natural merge above) – **worse than both** at every size. The
  bounded length check costs more than the algorithm switch saves: 33% worse than plain insertion at
  n=4, 7% worse than plain natural merge at n=16. A threshold is not worth paying for.
- **`mergeGuarded`** (an `isSorted` pre-pass, then merge sort) – dominated by `mergeNatural`
  everywhere. Natural merge already exploits ascending runs, and it exploits *partial* order too,
  which a boolean guard cannot.
- **`mergeCounted`** (count once, split by count, no nested list) – a real improvement over
  `mergeDeal` (511M vs 601M at n=64) but still 28% worse than `mergeNatural`. Run detection is worth
  more than avoiding the nested `List[List[A]]`.
- **`mergeDeal`** – best *comparison* count of all candidates, but not the best cost. Its
  alternating deal is also **not stable**, which disqualifies it as the basis for
  `SortedMap.fromList`.

### 4.5 Script size

| candidate | bytes |
|---|---:|
| `isSorted` | 79 |
| insertRight | 104 |
| insertLeft | 106 |
| prelude (qsort) | 169 |
| mergeDeal | 194 |
| mergeCounted | 261 |
| **mergeNatural** | 279 |
| hybrid | 385 |
| qsortMedian3 | 365 |

Natural merge sort costs 110 bytes more than the current quicksort. At n=16 it saves ~34,000
lovelace per call in the worst case, so the size is repaid by the first invocation many times over.

### 4.6 The floor: verifying beats sorting

If the caller can supply the list already sorted, checking the order is one pass and n−1
comparisons.

| n | `isSorted` | prelude sort, worst case | ratio |
|---:|---:|---:|---:|
| 8 | 6.38M | 46.83M | 7.3x |
| 16 | 13.07M | 167.64M | 12.8x |
| 32 | 26.45M | 633.18M | 23.9x |
| 64 | 53.20M | 2459.96M | **46.2x** |

Even against the *best* sort, verifying is 7.5x cheaper at n=64 (53.2M vs 398.9M). This is the
single largest lever available, and it is the one the whole ecosystem has already converged on
(§2.1). It is not currently offered by the Scalus stdlib in any form.

---

## 5. Results: `SortedMap.fromList`

> **Superseded.** The measurements below are sound, but the conclusion they were used to reach –
> make the merge-plus-dedup form the default – was reversed once it became clear that
> attacker-supplied maps never reach `fromList` at all. See §9.6.


The shipped implementation is `foldLeft` + sorted insert – an insertion sort whose worst case is
ascending keys, which is how ledger data arrives.

### 5.1 On ascending keys (the common case)

| n | today | fold right | stable merge + dedup |
|---:|---:|---:|---:|
| 8 | 62.87M | **15.25M** | 30.25M |
| 16 | 233.42M | **30.80M** | 60.59M |
| 32 | 899.60M | **61.92M** | 121.28M |
| 64 | 3532.20M | **124.15M** (28.4x) | 242.65M (14.6x) |

### 5.2 Worst case over all key shapes

| n | today | fold right | **stable merge + dedup** |
|---:|---:|---:|---:|
| 8 | 62.87M | **52.84M** | 64.56M |
| 16 | 233.42M | 206.36M | **132.79M** |
| 32 | 899.60M | 817.44M | **342.71M** |
| 64 | 3532.20M | 3255.70M | **737.22M** (4.8x) |

Folding right is a one-line change that makes the common case 28x cheaper, but it stays O(n²) – its
worst case merely moves from ascending to descending. Only the merge-based version bounds the worst
case, and it is the only one of the three that is worst-case safe under adversarial input.

### 5.3 Memory and fee, which is where this gets serious

Worst case over all key shapes:

| n | | today | fold right | **stable merge + dedup** |
|---:|---|---:|---:|---:|
| 32 | memory | 3,709,636 | 3,454,328 | **1,378,906** |
| 32 | fee | 278,907 | 258,253 | **104,273** |
| 64 | memory | **14,572,900** (88.3% of the tx limit) | 13,722,584 (83.2%) | **2,950,362** (17.9%) |
| 64 | fee | **1,095,528** | 1,026,530 | **223,390** |

On ascending keys, the shape the ledger actually produces:

| n | today | fold right | stable merge + dedup |
|---:|---:|---:|---:|
| 32 | 3,709,636 | **263,343** | 490,881 |
| 64 | 14,572,900 | **526,415** (27.7x) | 978,657 (14.9x) |

Two things stand out. First, **one `fromList` of 64 pairs consumes 88% of the entire transaction
memory budget** in its worst case, and costs over 1 ADA. A validator doing that has almost nothing
left for its actual logic. Second, the shipped implementation is no better on its *common* input
than on its worst, because ascending keys ARE its worst case: 14.5M memory either way.

Script sizes: 159 (today), 129 (fold right), 416 (merge + dedup). The 416 figure is measured
standalone; if `fromList` is implemented on top of the same natural merge sort that backs
`List.sort`, the merge code is shared and the marginal cost is only the dedup pass.

**Stability matters here.** `fromList` documents that the first occurrence of a duplicate key wins.
A stable sort makes the head of each equal-key run the first occurrence, so dedup is a single pass.
The natural merge sort is stable (runs preserve input order; the merge keeps the left list ahead on
ties). `mergeDeal` is **not**, which is a second reason to prefer natural merge as the one
underlying algorithm.

---

## 6. Recommendation (superseded)

This section proposed the changes; §9 records what was actually built. Three of its
recommendations did not survive measurement and are corrected in §9.5: the full GHC descending-run
shape, the `OrFail` twins, and keeping the `UplcConstr` intrinsic. Its `SortedMap.fromList`
recommendation was also overturned – see §9.6.

Read §9 for the normative outcome. This section is kept because the reasoning it records is what the
measurements were designed to test.

## 7. The comparator lever – worth another 20%, orthogonal to the algorithm

`Ord[A]` is `(A, A) => Order` (`Ord.scala:10`). Every single comparison therefore allocates an
`Order` constructor and immediately pattern-matches it back down to a boolean. liqwid-plutarch-extra
avoids this deliberately – its merge uses a boolean `pleqBy` rather than the three-way `pcompareBy`.

Measured by running the identical algorithms against a `(A, A) => Boolean` comparator, at
`BigInt`, where `<` takes the monomorphic fast path and lowers straight to `lessThanInteger`
(`Ord.scala:31-41`):

| n | mergeNatural (`Ord`) | natural(lt) | saving | insertLeft (`Ord`) | insertLeft(lt) | saving |
|---:|---:|---:|---:|---:|---:|---:|
| 8 | 34.95M | 29.08M | 17% | 33.55M | 26.45M | 21% |
| 16 | 69.74M | 61.99M | 11% | 131.85M | 101.45M | 23% |
| 32 | 185.82M | 150.33M | 19% | 524.36M | 398.73M | 24% |
| 64 | 398.89M | 319.81M | 20% | 2093.09M | 1582.46M | 24% |

Script size improves too: 252 bytes vs 279 for natural merge, 87 vs 106 for insertion.

Two effects are bundled here and the report should not oversell either: the boolean comparator
avoids the `Order` allocation, and at `BigInt` specifically the monomorphic `<` also skips the
generic `Ord[BigInt]` dispatch. Both savings are real and both are available to any caller who can
write a boolean test, but a caller whose comparator is genuinely three-way will see less.

**Best combination measured.** `natural(lt)` at n=64 costs 319.8M against today's 2460.0M –
**7.7x**, from the algorithm change and the comparator change together.

**API consequence.** Add **`List.sortWith(lt: (A, A) => Boolean)`** alongside `sort`, backed by the
same natural merge sort. It is the Scalus analogue of Aiken's `list.sort(compare)`, it is ~20%
cheaper whenever the caller has a cheap boolean test, and it costs no extra code because `sort`
becomes `sortWith((a, b) => (a <=> b).isLess)`. Note that `sort` itself does **not** inherit the
saving – the `Ord` round-trip is exactly what is being paid for – so the scaladoc must say plainly
that `sortWith` is the cheaper entry point when a direct comparator exists.

The same applies to `isSorted`: offer `isSortedWith(lt)` next to `isSorted(using Ord)`.

### 7.1 Match-clause ordering: no effect here (negative result)

On the PlutusTx code generator, reordering `merge` so the two-cons case is tested before the `Nil`
cases was worth **25% CPU** (same plutus#4063 discussion). Tested on the Scalus pipeline by compiling
the identical algorithm with the clause order reversed (`natural(lt,consFirst)`):

| n | natural(lt) | natural(lt, cons-first) |
|---:|---:|---:|
| 8 | 29,083,181 | 29,083,181 |
| 16 | 61,985,316 | 61,985,316 |
| 32 | 150,330,901 | 150,330,901 |
| 64 | 319,811,207 | 319,811,207 |

Identical to the unit at every size, and identical script size (252 bytes). The Scalus lowering
normalizes clause order, so this PlutusTx tuning knob does not transfer and should not be carried
into the implementation as a supposed optimization.

---

## 8. Lowering safety checks

The shipped `sort` carries an `INVESTIGATION` note: a `Tuple2`-returning partition triggers a
`Case index 2 out of bounds` lowering failure with `optimizeUplc = false`. Every candidate here
returns multiple values through a named case class (`Split`, `Parts3`, `Taken`) or a nested
`List[List[A]]`, which is the same shape class, so this had to be checked before recommending any of
them.

**Result: all candidates lower and sort correctly with `optimizeUplc = false`** as well as `true`,
across all 7 input shapes at n ∈ {0, 1, 2, 4, 8, 16}. The check is a permanent test in the harness
(`SortBudgetTest`, "every candidate lowers and sorts correctly with optimizeUplc = false"), so a
future lowering regression surfaces there.

**Not verified: the `UplcConstr` representation.** Everything above is measured on the default
Data-packed list path. Since `sort`'s only intrinsic is the `UplcConstr` one
(`UplcConstrListOperations.sort`), porting the winner into that intrinsic needs its own lowering
check before the implementation lands. This is the one axis from the measurement plan that is not
closed.


---

## 9. What was implemented, and where this report was wrong

Everything below is measured under `Options.release` – the production lowering, which sections 2-5
were not using.

### 9.1 Shipped

1. **`prelude.List.sort` is a stable natural merge sort** (`List.scala`). Ascending runs only,
   merged pairwise. Comparisons are a direct `match` on `Order`.
2. **The `UplcConstr` sort intrinsic is deleted** – the wrapper in `IntrinsicsUplcConstrList`, the
   body in `UplcConstrListOperations`, and its `Partition`/`Taken` carriers. Both representations
   now run the one prelude implementation. (§9.3)
3. **`SortedMap.fromList` folds right instead of left**, and `SortedMap.fromLargeList` adds the
   Theta(n log n) form for long inputs. (§9.6)
4. **New `List` API:** `insertionSort`, `sortWith(lt)`, `isSorted`, `isStrictlyAscending`,
   `isSortedWith(lt)`. No `OrFail` twins – `require(xs.isSorted, "...")` already says it.
5. New direct coverage: `UplcConstrSortTest` (the `UplcConstr` path had only Knights before, at a 5%
   budget tolerance), stability tests for all three sorts and both map constructors, on-chain
   lowering checks for every new method, and `SortBudgetTest` /
   `SortedMapFromListBudgetTest` as re-runnable harnesses.

Budget pins updated: 3 in `ListTest`, 1 in `SortedMapTest`, 6 in `ValueTest` (all downward, from
`Value.fromList` getting cheaper), 3 in `KnightsDataTest` (0.06-0.2% worse, it sorts short lists).

### 9.2 The honest before/after

Worst-case fee in lovelace over all input shapes, old head-pivot quicksort vs shipped, both under
`Options.release`. These are the final numbers, measured after the `inline` refactor of §9.8:

| n | 0 | 2 | 4 | 5 | 8 | 16 | 32 | 64 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| old | 277 | 1,780 | 4,988 | 7,232 | 16,519 | 60,041 | 228,924 | 894,051 |
| new | 464 | 2,248 | 5,096 | **6,490** | 12,471 | 24,865 | 65,812 | 141,105 |
| | 1.7x **worse** | 1.26x worse | 1.02x worse | 1.11x better | 1.3x | 2.4x | 3.5x | **6.3x** |

**The break-even is n = 5.** Below it the change is a regression, worst at n=0 where a merge sort's
larger body is pure fixed overhead. Script size grew 169 → 277 bytes. §1's original headline quoted
only the n=64 end of this range; that was the report's biggest presentational failure and this table
replaces it. The scaladoc on `sort` carries the same numbers.

The justification for accepting the small-n cost is unchanged: it removes a quadratic worst case
that ordinary ledger-shaped input triggers, and a validator is provisioned for its worst case.
`insertionSort` exists for callers who genuinely have a small bounded length.

### 9.3 Deleting the intrinsic – measured, not assumed

The `UplcConstr` intrinsic was not merely unprofitable, it was **harmful**. Same wrapper, same
inputs, n=32, intrinsic present vs deleted:

| shape | with intrinsic | deleted (falls back to prelude) | |
|---|---:|---:|---|
| random | 220.1M | 200.9M | 9% cheaper without |
| ascending | 183.8M | **63.9M** | **2.9x cheaper without** |
| allEqual | 183.8M | **63.9M** | **2.9x cheaper without** |
| descending | 174.5M | 170.1M | 2.5% cheaper without |
| fewUnique | 218.3M | 174.8M | 20% cheaper without |
| organPipe | 193.7M | 161.2M | 17% cheaper without |

Every shape is cheaper without it, because the prelude's natural merge exploits ascending runs and
the intrinsic's counted merge structurally cannot. Deleting it also removed three layers of
machinery: a registry entry, a delegating wrapper that patched up comparator representations, and a
support module with hand-annotated representations.

An earlier measurement in this session appeared to show the intrinsic winning 3.4x on ascending
input. That comparison was invalid – the fallback was still quicksort at the time, so it measured
merge-vs-quicksort, not intrinsic-vs-not.

### 9.4 Lowering constraints discovered (each cost a failed run)

These are undocumented elsewhere and will bite the next person:

1. `UplcConstr` list code only lowers inside `UplcConstrListOperations` /
   `UplcConstrOptionOperations` – the two module names hardcoded in
   `ScalusRuntime.initSupportBindings`, which lower with `inUplcConstrListScope = true`.
2. Support-module functions cannot be called directly from user code at all:
   `uplcConstrToBuiltinList: cannot convert with TypeVar element`.
3. **In a support module**, a list whose ELEMENT type is an annotated list cannot be lowered –
   construction needs a concrete element type and the type variable is unwrapped. Annotated list
   *fields* in a case class are fine. This is what forced the intrinsic onto counted merge. Note it
   is specific to the support modules: the same nested `List[List[A]]` lowers correctly in the
   prelude, which is why deletion works.
4. Local defs cannot forward-reference one another, so they cannot be mutually recursive. GHC's
   `sortBy` is three mutually-recursive functions and cannot be transcribed directly.

### 9.5 Corrections to earlier sections

- **§6.1's "implement the full GHC shape, detecting descending runs".** Tried and reverted.
  It requires the mutual recursion of constraint 4, cost 384 extra bytes of script, and measured no
  better from n=16 up – the ascending-only version had a slightly *better* worst case (389.1M vs
  392.8M at n=64). Without descending detection a reverse-sorted list is simply n singleton runs,
  i.e. ordinary bottom-up merge sort, never quadratic.
- **§7's boolean-comparator finding still stands** (~20%), but the cheaper half of it is available
  for free: writing comparisons as a direct `match` on `Order` instead of `(a <=> b).isLess`
  measured a consistent **2.5%** saving, and is applied in the shipped code. `isLess` is itself a
  match producing a `Boolean` that is then branched on.
- **§6.1's `OrFail` twins for `isSorted`.** Dropped. `require(l.isSorted, "...")` composes and costs
  nothing extra; the `OrFail` convention exists for finders returning `Option`, where it saves an
  allocation. A predicate has no such tax.

### 9.6 `SortedMap.fromList`: the report recommended the wrong default

§5 concluded that `fromList` should become a stable merge plus a dedup pass, on the grounds that it
bounds the worst case. That was implemented, measured, and **reversed**, because the argument rested
on an assumption that turns out to be false.

**Attacker-supplied maps never reach `fromList`.** A `SortedMap` decoded from `Data` goes through
`sortedMapFromData` → `unsafeFromList`, which does no sorting at all. `fromList` exists for maps a
contract builds itself, and the only on-chain caller in the codebase is `Value.fromList`
(`v1/Value.scala:191,195`), building token maps that are small and contract-authored. Every other
call site is a scaladoc example or an off-chain test fixture. So the adversarial-length argument
that justified the merge sort does not apply here.

What shipped instead is the same insertion sort **folded right rather than left**:

| worst-case fee | n=0 | n=4 | n=8 | n=16 | n=64 | ascending n=64 | script |
|---|---:|---:|---:|---:|---:|---:|---:|
| old (fold left) | 287 | 5,693 | 19,505 | 72,347 | 1,095,528 | 3.53B steps | 159 B |
| **shipped (fold right)** | **270** | **4,612** | **16,926** | 65,475 | 1,026,530 | **124M (28x)** | **129 B** |
| merge + dedup (`fromLargeList`) | 731 | 10,294 | 24,704 | **50,137** | **265,886** | 350M (10x) | 480 B |

Folding right is cheaper than the old implementation **at every size measured** and 30 bytes
smaller – there is no size at which it regresses, which is why it is the default despite staying
O(n²). The merge version survives as `fromLargeList`, worth using from about n=10.

Knock-on effect: `Value.fromList` calls `SortedMap.fromList` once for the policy map and again per
policy, so `Value` construction got 11-13% cheaper. Six `ValueTest` budget pins moved downward.

### 9.7 On removing `List.sort` entirely

Considered – force callers to pick `mergeSort` or `insertionSort` explicitly, as Plutarch does by
shipping no sort at all. Rejected:

- The hazard that would justify it is gone. Forcing a choice guards against a silently expensive
  default; the default is now the worst-case-safe one, and what remains is a ~2x difference on
  lists of ≤4 where the absolute cost is a few hundred lovelace.
- It would not improve outcomes. The one real call site in the survey – binocular's 11 timestamps –
  hand-rolled insertion sort *above* the crossover, i.e. chose wrong when left to decide. Removing
  the default produces more hand-rolling, not better choices.
- `List.sort` is public and MiMa-gated, so removal needs a `@deprecated` cycle regardless.

The nudge is delivered by scaladoc instead: `sort` carries the measured cost table and
`insertionSort` states the rule as "bounded by construction and at most 8".

### 9.8 One `inline` merge sort, shared by `sort` and `sortWith`

`sort` and `sortWith` began as two near-identical merge sort bodies differing only in how they
compare. Two ways to unify them were measured.

**Plain delegation fails.** Defining `sort = sortWith(lambda)` removes the duplication but costs
**4-8% at every size** (7.8% at n=64): the comparison becomes an indirect call returning a
materialized `Boolean` instead of one inlined `Order` match, giving back the 2.5% won in §7 plus a
closure hop.

**An `inline` helper with an `inline` comparator wins outright.** `List.mergeSortWith` is
`private inline def` taking `inline lt`, so a lambda literal is beta-reduced into the body instead
of being called through a closure. `sort` passes an `Order`-matching literal and gets it inlined as
if hand-written; `sortWith` passes its runtime function and gets an ordinary call. One definition,
two expansions, neither paying for the other:

| | duplicated bodies | plain delegation | **`inline` helper** |
|---|---:|---:|---:|
| n=8 fee | 12,575 | 13,489 | **12,471** |
| n=64 fee | 143,847 | 155,042 | **141,105** |
| script | 290 B | 279 B | **277 B** |
| source | ~80 lines, 2 copies | ~40 lines | **~40 lines, 1 copy** |

Better than the hand-written duplication on every axis – 1.9% cheaper at n=64 and 13 bytes smaller,
with half the code. The gain over hand-writing is small but consistent, and presumably comes from
the optimizer seeing one canonical shape.

The lesson generalizes: in this codebase an `inline` parameter is the way to share a higher-order
skeleton without paying for the closure. A plain function parameter is not free.

### 9.9 Checked and closed

**`AssocMap.fromList` is NOT the same defect.** It has the same `foldLeft` shape
(`AssocMap.scala:30`) and was the obvious next suspect, but it is a dedup that PRESERVES input
order, not an insertion sort. Its own scaladoc explains why that matters: *"Order is part of this
type's meaning - `AssocMap` is used for fields whose order the ledger fixes, such as
`TxInfo.redeemers`"*. It must not sort, so folding right would not help, and the `O(n^2)` is
inherent to "keep the first occurrence, preserve order" without a hash set. Only constant factors
are available. Left alone.

**Verification status.** MiMa passes on the branch: the six new methods are pure additions, and
removing the `UplcConstr` sort intrinsic falls under the existing `scalus.compiler.intrinsics`
wildcard exemption. `sbtn ci` reaches 12,140 passing tests across 17 suites with zero failures and
then aborts on a Scala.js linking error in `scalusTsExporterFixtures` (`java.time.Instant`,
duplicate `@JSExport`s). That failure reproduces on a clean `origin/master` worktree and this branch
touches no ts-exporter file, so it is pre-existing and unrelated.

### 9.10 Follow-ups for other people

- **binocular** hand-rolls an insertion sort over 11 Bitcoin timestamps
  (`BitcoinValidator.scala:438`, called at `:500`). n=11 is above the crossover: `List.sort` is now
  about 1.4x cheaper for them, or 1.1x against their comparator style. They also only need the
  median (`sortedTimestamps.at(5)`), so a selection algorithm would beat both – no stdlib case for
  one yet, at a single call site.

- **The `inline`-parameter finding (§9.8) partly generalizes – measured, see §9.11 – but is not a
  sweep.** Original note kept below for the hazards it lists.

- **The `inline`-parameter finding (§9.8) may generalize.** `map`, `flatMap`, `filter`, `filterMap`,
  `find`, `foldLeft`, `foldRight`, `exists`, `forall`, `count`, `takeWhile` and `dropWhile` all take
  plain function parameters and may be paying the same closure cost. Two cautions before anyone
  tries it:
  1. `mergeSortWith` was safe to inline because it is a PRIVATE helper with two internal call
     sites. These are public, so `inline` expands the body at every USER call site – trading
     closure cost for script size, the opposite of the trade made here.
  2. `map`, `filter`, `foldLeft` and `find` have intrinsics, and `IntrinsicResolver` dispatches by
     matching the prelude method name. Inlining the method away may leave no call to match,
     silently losing the intrinsic on the Data-packed path where it matters most.

  So this is one careful experiment on a non-intrinsified function with a small body – `count` or
  `forall` – measuring CPU and script size at several call-site counts. Not a sweep.

- **Sections 4 and 5 still report their original non-release numbers.** The rankings hold and §9.2
  supersedes the headline, but any absolute figure quoted from those sections is measured on a build
  that is not what ships.

### 9.11 Does the `inline` trick generalize to the other higher-order functions?

Measured on `List.forall`, chosen because it has NO intrinsic (the `forall` entries in the
intrinsics modules are `Option.forall`), so the intrinsic hazard cannot confound the result. A
throwaway harness compared a plain function parameter against an `inline` one, at one call site and
at three; it is not committed, since it guards nothing and the numbers below are the deliverable.

To reproduce: two `@Compile` definitions of `forall`, identical but for `inline p: A => Boolean`
versus `p: A => Boolean`, compiled under `Options.release` and applied to a `List[BigInt]`.

**CPU: a real saving, 12-19% per call.**

| n | 0 | 1 | 4 | 8 | 16 | 32 | 64 |
|---|---:|---:|---:|---:|---:|---:|---:|
| regular | 512,184 | 914,218 | 2,120,320 | 3,728,456 | 6,944,728 | 13,377,272 | 26,242,360 |
| inline | 416,184 | 770,218 | 1,832,320 | 3,248,456 | 6,080,728 | 11,745,272 | 23,074,360 |
| saving | 18.7% | 15.8% | 13.6% | 12.9% | 12.4% | 12.2% | **12.1%** |

**Script size: it depends on how many times you call it.**

| | 1 call site | 3 call sites |
|---|---:|---:|
| regular | 48 B | 84 B |
| inline | **40 B** | 112 B |

At one call site `inline` is smaller AND faster. At three it costs 28 bytes more – roughly 18 extra
bytes per additional site, because the loop body is duplicated rather than shared.

**Verdict: not a blanket change.** The trade flips with call-site count, which is a property of the
CALLER, not of the function, so the prelude cannot choose correctly on the caller's behalf. That is
the opposite of `mergeSortWith`, where the two call sites are internal, fixed and known.

Nothing in the prelude was changed on the strength of this. What it establishes for future work:
a plain function parameter costs about 12% against an inlined one, so the saving is worth chasing
for a function typically called once per script, and not worth it for one sprinkled everywhere.
The four intrinsified functions (`map`, `filter`, `foldLeft`, `find`) remain unmeasured and carry
the separate hazard that inlining may leave no call for `IntrinsicResolver` to match.
