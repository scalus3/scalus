# SortedMap lookup cost findings (Eq-scan vs Ord early-exit)

Date: 2026-08-19. Status: **experiment reverted, findings kept**.

Context: after landing `Value.hasOnly` (commit `bec24be38`), which beats
`tokens(cs) === SortedMap.singleton(...)` by ~35% in fee using a plain
`equalsByteString` scan, we tested the hypothesis: "change `SortedMap.get`
from `Ord`-based lookup to an `Eq`-based scan and everything gets cheaper."
The hypothesis is **mostly false for generic code**. This doc records why,
so we don't re-learn it.

## Benchmark 1: lookup strategies on concrete `ByteString` keys

Setup: exact single-token check on a `Value` with n policies, V3 pipeline,
optimized UPLC, PV11 mainnet prices. Fee in lovelace.

- A: `tokens(cs) === SortedMap.singleton(tn, 1)` - `SortedMap.get` with
  `Ord[ByteString]` (2 builtins per visited entry, early exit on `Less`),
  then structural `===` of the inner map.
- B: `hasOnly(cs, tn, 1)` - plain `equalsByteString` scan (1 builtin per
  entry, no early exit), then one `equalsData`.
- C: best-case early-exit scan - `lessThanByteString`-first (1 builtin per
  skipped entry, exit once key sorts after target), then one `equalsData`.

| n  | first (A/B/C)       | middle              | last                 | absent-early        | absent-late          |
|----|---------------------|---------------------|----------------------|---------------------|----------------------|
| 3  | 1076 / 786 / 701    | 1435 / 1006 / 984   | 1794 / 1227 / 1267   | 899 / 1157 / 542    | 1716 / 1157 / 1175   |
| 10 | 1076 / 786 / 701    | 2512 / 1667 / 1832  | 4307 / 2769 / 3245   | 899 / 2699 / 542    | 4229 / 2699 / 3154   |
| 50 | 1076 / 786 / 701    | 9691 / 6074 / 7485  | 18665 / 11582 / 14552| 899 / 11513 / 542   | 18587 / 11513 / 14460|

Per-visited-entry fee: A ~354, C ~283, B ~220.

Conclusions:

1. **For a present key, sortedness can never help.** Both strategies visit
   the same entries up to the match; the ordered comparison only costs more
   per entry. Even C (optimal early exit) loses to B beyond ~3 entries.
2. **Early exit pays only on misses** that sort early: A wins over B when
   the miss exits within roughly the first ~60% of the map. For a validator
   check where `false` means "transaction fails", the miss path cost is
   irrelevant - hence `hasOnly` uses the plain scan.
3. B's memory is 2-4x lower than A's (no `Option`/`Order`/tuple boxing),
   and memory dominates the mainnet fee formula.

## Benchmark 2: switching `SortedMap.get` to `Eq` (reverted)

The change: move `get`/`getOrFail`/`at`/`contains` to an `[A: Eq]`
extension with `if key === k then Some(v) else go(tail)`; keep
`insert`/`delete` on `Ord`; `List.groupMap`/`groupMapReduce` kept a private
`Ord`-based lookup (`getByOrd`), since their accumulator lookups are mostly
misses.

**The killer detail: generic `===` does not lower to the cheap builtin.**
Inside generic code the keys are packed `Data` (type-var repr), so the
structural-`Eq` lowering emits `equalsData` on the raw `Data` keys - not
`equalsInteger`/`equalsByteString`. `equalsData` has a large per-call CPU
intercept and low memory use. Measured on a singleton `BigInt`-keyed map
(hit), whole-program cpu/mem:

| implementation                                 | cpu       | mem  |
|------------------------------------------------|-----------|------|
| old `Ord` get (unIData + 2x lessThan + Order)  | 1,309,043 | 5994 |
| new `Eq` get (equalsData on packed keys)       | 1,761,779 | 3997 |
| ideal: concrete-typed clone (equalsInteger)    |   832,313 | 4229 |

The ideal row is only reachable when key types are concrete at the
comparison site (like `hasOnly`), not from generic `get`.

5-entry `ByteString`-keyed map, new-Eq get vs old-Ord get:

| position     | fee new | fee old | cpu delta |
|--------------|---------|---------|-----------|
| hit first    | 443     | 459     | +53%      |
| hit last     | 1319    | 1632    | +62%      |
| absent-early | 1302    | 343     | ~8x       |
| absent-late  | 1302    | 1615    | +63%      |

Real validators (scalus-examples budget pins): memory -4..6%,
cpu +1.5..3.5%, net fee down a few percent.

Why it was reverted despite the net fee win:

- The win is memory-traffic savings, not cheaper comparisons; CPU rises
  everywhere (+50-60% on lookup-heavy micro paths).
- `contains`/`get` on an **absent** key - the common "check absence"
  pattern - regresses 4-8x in fee (full `equalsData` scan, no early exit).
- Cost of landing: 3 MiMa filters (`get`/`at`/`contains` evidence
  `Ord` -> `Eq`), ~3 core + ~20 example budget re-pins, some with dual
  compiler-generation baselines.

## Follow-ups worth doing instead

1. **Compiler lowering improvement (the real prize):** when generic `===`
   compares values whose runtime repr is `Data` but whose `Eq` instance is
   the canonical one for a builtin-representable type (`BigInt`,
   `ByteString`), lower to `unIData`/`unBData` + `equalsInteger`/
   `equalsByteString` instead of `equalsData`. That would make an Eq-based
   `get` strictly better on hits and re-open this experiment with better
   numbers.
2. For hot per-policy checks on `Value`, use `hasOnly` (landed) or
   hand-written concrete-key scans; they are 20-35% cheaper in fee than
   `get`-based code either way.

Reproduction: the benchmark sources are archived in
`docs/internal/benchmarks/sortedmap-lookup/`. Drop them into
`scalus-core/jvm/src/test/scala/scalus/uplc/eval/` and run with
`sbtn "scalusJVM/testOnly scalus.uplc.eval.<Name>"`. Each compiles the
variants with `toUplcOptimized()` and reports budgets via `evaluateDebug`
with `CardanoInfo.mainnet` prices. Note: `GetLoweringDiffExplorationTest`
benchmarks the *Eq-based* `get`, so its "public get" rows only reproduce
the table above on a tree with the (reverted) Eq change applied.
