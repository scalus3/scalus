# Knights sharing and sorting; HTLC fees

Measured on 2026-09-11, branch base `27ffbfdfb`, Scala 3.3.8, V3 lowering, van Rossem VM.

## Knights

All variants use the current optimizer. The baseline is the unchanged benchmark source. The shared variants bind `descAndNo` inside the existing `else` branch, then reuse it for filtering and sorting. No optimizer changes were made.

```scala
val descendantsWithCounts = descAndNo
val singles = descendantsWithCounts.filterMap(...)
singles match
    case List.Nil => descendantsWithCounts.insertionSort.map(...)
    // Existing handling of one or multiple singles remains unchanged.
```

The list has at most eight entries: `allDescend` maps over a filtered list of the eight knight directions. Both sorting implementations are stable. Every measured run passed the existing exact solution checks.

`quicksort` is an inline alias of `sort`, which uses merge sort. Switching the shared version from `quicksort` to `sort` produced identical sizes and budgets in every case. The tables therefore combine these two measurements.

Sizes are serialized CBOR bytes of each closed benchmark program, including its input and expected-result assertion; they are not standalone validator sizes.

### KnightsTest

| Board | Variant | Bytes | CPU | Memory |
|---|---|---:|---:|---:|
| 4×4 | Baseline | 3,837 | 31,177,071,655 | 147,065,632 |
| 4×4 | Shared + sort/quicksort | 3,824 | 25,136,072,733 | 118,004,221 |
| 4×4 | Shared + insertionSort | 3,662 | 24,795,623,419 | 116,736,784 |
| 6×6 | Baseline | 5,621 | 103,914,286,173 | 486,139,274 |
| 6×6 | Shared + sort/quicksort | 5,607 | 83,363,098,208 | 386,513,857 |
| 6×6 | Shared + insertionSort | 5,445 | 82,543,516,417 | 383,754,374 |
| 8×8 | Baseline | 5,962 | 220,570,353,446 | 1,032,866,464 |
| 8×8 | Shared + sort/quicksort | 5,948 | 162,391,918,886 | 746,731,118 |
| 8×8 | Shared + insertionSort | 5,786 | 160,511,224,929 | 740,454,792 |

### KnightsDataTest

| Board | Variant | Bytes | CPU | Memory |
|---|---|---:|---:|---:|
| 4×4 | Baseline | 2,116 | 44,087,137,731 | 138,777,221 |
| 4×4 | Shared + sort/quicksort | 2,107 | 34,374,651,385 | 107,774,699 |
| 4×4 | Shared + insertionSort | 1,945 | 34,246,470,503 | 107,148,594 |
| 6×6 | Baseline | 3,532 | 105,396,171,993 | 246,404,128 |
| 6×6 | Shared + sort/quicksort | 3,524 | 80,303,398,619 | 187,956,137 |
| 6×6 | Shared + insertionSort | 3,362 | 80,182,714,496 | 187,353,930 |
| 8×8 | Baseline | 3,942 | 212,745,799,112 | 438,507,966 |
| 8×8 | Shared + sort/quicksort | 3,934 | 142,638,730,848 | 297,887,493 |
| 8×8 | Shared + insertionSort | 3,772 | 142,435,620,629 | 296,892,547 |

### Scala 3.8.4 baselines

The retained shared + insertion-sort variant was also measured on Scala 3.8.4 to update the existing compiler-specific budget assertions. Native Knights produced the same budgets and sizes as Scala 3.3.8. KnightsData produced:

| Board | Bytes | CPU | Memory |
|---|---:|---:|---:|
| 4×4 | 1,885 | 29,166,961,445 | 91,769,154 |
| 6×6 | 3,293 | 75,701,311,378 | 174,290,190 |
| 8×8 | 3,703 | 137,519,680,014 | 282,560,647 |

### Recommendation

Keep explicit sharing and use `insertionSort`. Sharing provides most of the CPU reduction. Insertion sort then saves 162 bytes in both implementations at all three board sizes, while also lowering CPU and memory. Its quadratic worst case is bounded here by eight elements.

For 8×8 Knights, sharing reduces CPU by 26.38%; insertion sort saves another 1.16% relative to sharing with merge sort. The combined CPU reduction is 27.23%. For KnightsData the corresponding reductions are 32.95%, 0.14%, and 33.05%.

## HTLC CAPE

“Before” uses the CSE and inliner from `c8a247859` (master before these branch changes); “after” uses the current `V3Optimizer` at `27ffbfdfb`. Both optimize the same lowered HTLC term, with CCE disabled, and evaluate the same CAPE inputs. This isolates the optimizer changes from compiler/library changes. No HTLC source changes were made.

Fees use the repository’s `CardanoInfo.mainnet` parameter snapshot: 15 lovelace/reference-script byte, 0.0577 lovelace/memory unit, and 0.0000721 lovelace/CPU unit. These are not a live network-parameter lookup.

```text
execution fee = ceil(memory × 0.0577 + CPU × 0.0000721)
reference fee = script bytes × 15
combined fee  = execution fee + reference fee
```

Assumes this script is the only charged reference script, within the first 25,600-byte tier. Execution fees use the measured budget without a safety margin. These totals exclude the base transaction fee, transaction byte fee, and reference-script publication/storage costs.

| Path | Variant | Bytes | CPU | Memory | Reference fee | Execution fee | Combined fee |
|---|---|---:|---:|---:|---:|---:|---:|
| Claim | before | 548 | 18,046,963 | 43,917 | 8,220 | 3,836 | 12,056 |
| Claim | after | 569 | 18,406,297 | 44,945 | 8,535 | 3,921 | 12,456 |
| Refund | before | 548 | 16,660,141 | 40,984 | 8,220 | 3,566 | 11,786 |
| Refund | after | 569 | 16,987,475 | 41,812 | 8,535 | 3,638 | 12,173 |

All fees are lovelace. Claim increases by 400 lovelace (+3.32%); refund by 387 (+3.28%). The 21 additional bytes add 315 lovelace to either path; execution adds 85 and 72 lovelace respectively. The current HTLC result is worse on both size and execution cost.

## Validation and artifacts

Each of the four Knights variants ran the 4×4, 6×6 and 8×8 tests, plus the existing native 4×4 duplicate test. HTLC compared successful claim and refund cases. Diagnostic budget assertions were bypassed during measurement; exact solution/success assertions remained enabled. The retained implementation restores budget assertions with measured baselines.

Final `sbtn quick` passed with budget assertions restored, including 3,802 core tests and 599 example tests.

Commands used:

```sh
sbtn 'scalusExamplesJVM/testOnly *KnightsTest *KnightsDataTest -- -z 100_'
```

Raw JSON measurements, serialized benchmark programs, and logs are retained locally under `/tmp/scalus-knights-sort/`. The HTLC comparison used temporary copies of the baseline optimizer passes; those copies are not production changes.
