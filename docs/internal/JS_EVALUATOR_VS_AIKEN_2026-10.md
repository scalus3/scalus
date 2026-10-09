# scalus.js evaluator vs Aiken after lucid #749 (2026-10-06)

## Result

On lucid-evolution main (bd751a68), the Aiken default is faster than Scalus 1.3.0 on Midgard's test
suites: 600 s against 816 s wall, all 34 tests passing on both. In September Scalus was ahead (2.1x).
Scalus did not slow down; Aiken became much faster.

## Final result after the fixes (2026-10-09)

Master 00d0f53ba, `TxEvaluator` with one shared `ScriptCache(64)`, against lucid main bd751a68 Aiken
(speed WASM). Full Midgard recordings, one engine per Node process, median of 3 runs per request,
`bench/js/compare-evaluators.mjs --engine aiken=... ` then `--engine txeval-cache=<scalus.js>`.
ExUnits equal Aiken's recorded result on all 55,001 requests.

| Suite | Requests | Aiken | Scalus | Scalus / Aiken |
|---|---:|---:|---:|---:|
| deep deposit | 2,010 | 27.95 s | 23.54 s | 0.84x |
| mint authorization | 13,116 | 54.28 s | 59.08 s | 1.09x |
| value conservation | 39,875 | 55.60 s | 51.87 s | 0.93x |
| total | 55,001 | 137.83 s | 134.49 s | 0.98x |

Scalus 1.3.0 on the 300/1,500/3,000-request samples was 1.73x / 3.01x / 2.21x. On the same samples
the cached `TxEvaluator` is 0.92x / 1.40x / 1.16x; the full recordings reuse the cache more. Aiken's
mint run had load average 7 (others 2–3.4), so the mint gap may be a little larger. Largest known
remaining cost on mint: BLAKE2b in JS (16% on 2026-10-07).

## What made Aiken faster (lucid-evolution)

| Commit | Change | Effect |
|---|---|---|
| f44c0cf3 | Build `TxInfo` **and its Data encoding** once per transaction; sorted inputs resolved once; decoded scripts kept in a bounded cache (64 entries) **across calls** | Aiken per evaluation on deep deposit: 445 ms (09-26) to 14.8 ms |
| 87be6a02 | uplc 1.1.24 (aiken#1437): no argument-size measurement for builtins whose cost is constant in CPU and memory | removes a full ScriptContext walk per `unConstrData`, `chooseData`, `sndPair`, ... |
| c3e264fe, 1e8f1e7b | wasm built with `-O3` instead of `-Oz`, shipped as the Node default | 25-30% |
| #749 (lucid) | evaluate the draft once; reuse an identical request within a completion | both engines: equal call counts once the Scalus default has the same memo |

## Cross-check: does Scalus do the same?

| Aiken has | Scalus 1.3.0 | Where |
|---|---|---|
| TxInfo built once per tx | yes: `lazy val txInfoV1/V2/V3` | `PlutusScriptEvaluator.scala:530` |
| **TxInfo Data encoding built once per tx** | **no**: `ScriptContext(txInfo, ...).toData` per redeemer re-encodes the whole TxInfo | `PlutusScriptEvaluator.scala` `evalPlutusV{1,2,3}Script` |
| **decoded-script cache across calls** | **no**: `_cachedDeBruijned` is per `PlutusScript` instance, and every call decodes the transaction anew | `PlutusScript` |
| **skip size measurement for constant-cost builtins** | **no**: `DefaultCostingFun.calculateCost` maps `MemoryUsage.memoryUsage` over every argument; `memoryUsageData` walks the whole tree and calls `toScalaList` at every node | `CostModel.scala:689`, `MemoryUsage.scala:63` |
| debug logging cost | already free: JS logger checks the level before forcing the by-name message | `LoggerPlatform.scala` (js) |

## Measurements

Recorded Midgard requests (the arguments of `eval_phase_two_raw`, dumped on 09-23), replayed
in-process. Aiken: lucid main's `uplc` speed build. Scalus: published 1.3.0. ExUnits identical on every
request.

| Workload (sample) | Aiken | Scalus 1.3.0 | Scalus slower by |
|---|---:|---:|---:|
| Deep deposit (300) | 2.84 s | 4.49 s | 1.58x |
| Mint authorization (1,500) | 3.03 s | 8.92 s | 2.94x |
| Value conservation (3,000) | 2.88 s | 5.78 s | 2.00x |

Call-path overhead, same requests, same Scalus build:

| Workload | A `evalPlutusScripts` (CBOR map) | B `evaluateTx` (CBOR pairs) | C Lucid adapter (`Utxo` handles) |
|---|---:|---:|---:|
| Deep deposit | 4.98 s | +5% | +9% |
| Mint authorization | 7.80 s | -2% | +10% |
| Value conservation | 5.70 s | +1% | +21% |

## Profile (Scalus engine, all three samples, 36.9 s sampled)

Readable Scala.js linker output (`scalus-cardano-ledger-opt/main.js`, 1.3.0), `node --cpu-prof`.
Shares are of total sampled time; inclusive groups overlap (CEK contains the builtins).

| Hot spot | Share | Cause |
|---|---:|---|
| CEK machine | 58.5% incl. | script execution |
| `CekMachine.applyEvaluate` array copying | 11.9% | `Compute(ctx, env :+ (name, arg), term)`: the environment is an `ArraySeq[(String, CekValue)]`, copied on every lambda application |
| `DataApi.readBoundedBytesIndef` | 10.3% | chunked (>64 byte) bytestrings in Data CBOR accumulate through a generic `ArrayBuffer`, boxing every byte |
| BLAKE2b (`@noble/hashes`) | 9.6% | scripts' `blake2b_*` builtins via `NodeJsPlatformSpecific`; pure JS where Aiken is native |
| `PlutusV3Params` / `MachineParams` | ~10% incl. | the 350-entry cost model parsed and the machine parameters rebuilt on every call |
| flat / DeBruijn script decode | ~8% incl. | every script decoded on every call |
| `Constant.seqLiftValue` | 6.2% incl. | lifting Data lists into constant lists |
| garbage collector | 7.1% | |
| `DefaultCostingFun` (argument sizes) | 2.9% incl. | constant-cost builtins still measure arguments |

## Plan, ranked by measured share and effort

| # | Change | Share | Effort |
|---|---|---:|---|
| 1 | Cache `MachineParams` per cost-model content | ~10% | small |
| 2 | `readBoundedBytesIndef`: grow an `Array[Byte]` directly, no boxing | ~10% | small |
| 3 | Bounded decoded-script cache across calls, keyed by script hash (Aiken parity) | ~8% | small |
| 4 | Skip `memoryUsage` when both CPU and memory cost are constant (aiken#1437 parity) | ~3% | small |
| 5 | CEK environment: `Vector` instead of `ArraySeq`, on JS only (measured 09-24: JS -9% heavy, -4% light; JVM CEK +2-4%, hence JS-only). A cons list was measured and rejected (JVM +18-31%); a random-access list, as Plutus uses, is untried | ~12% | small (Vector, JS-only) |
| 6 | Faster BLAKE2b on Node (wasm, or a tuned implementation) | ~10% | medium |
| 7 | Encode TxInfo to Data once per transaction (Aiken parity) | not isolated: inlined | small-medium |
| 8 | Investigate `seqLiftValue`; cheaper `Utxo` handle construction in the adapter (+9-21%) | ~6% + adapter | medium |

1-4 are about 30% of engine time for small changes. With 5 and 6 the total is about half, which would
close the deep-deposit and value-conservation gaps (1.58x, 2.0x) but not mint authorization (2.94x),
which needs its own profile.

## Also found

`Utxo.toObject().scriptRef` returns the on-chain ScriptRef encoding (tag 24 wrapping
`[language, bytes]`), but `withScriptRef({type, script})` expects the script itself. Feeding one to the
other silently yields a different script hash ("Script not found").

## Reproducing

- Dumps: `.claude/worktrees/js-cek-perf/bench/js/fixtures/{midgard-deep-deposit-2026-09-23, ab-2026-09-23/*.dump}`
- Engine comparison: `bench/js/compare-evaluators.mjs --engine aiken=<lucid>/packages/uplc/dist/speed/node/uplc_tx.js --engine scalus=<scalus.js>`
- Lucid-path re-measurement: `fixtures/lucid-main-aiken-bd751a68-2026-10-06/`, `fixtures/lucid-scalus-77748e53-2026-10-06/`

## Deep deposit profile after 411184c0a (2026-10-06)

Readable linker output of `perf/js-evaluator`, `evalPlutusScripts`, 300 requests, 9.7 s sampled.
The fixes so far did not move deep deposit (4.47 s against Aiken's 2.84 s): its time is the CEK loop.

| Hot spot | Share |
|---|---:|
| CEK machine, inclusive | 71% |
| `computeCek` / `returnCek` / `applyEvaluate` self | 13% / 7.5% / 4% |
| environment copy (`ArraySeq.appended`, `arraycopyGeneric`, `$objectGetClass`) | ~11% |
| garbage collector | 10% |
| builtin application (`evalBuiltinApp`), inclusive | 15% |
| script decode (flat, De Bruijn) | 4% |
| BLAKE2b | 3.5% |
| cost-model parse (`MachineParams`) | 0.3% |

September's env measurement (`fixtures/env-*`): `Vector` made heavy-deep 46.0 s to 42.0 s (-9%) on JS;
a cons list gave nothing on JS. Reachable without new machinery: Vector env (-9%), script cache
(-4%), BLAKE2b (at most -3.5%): about 1.58x to 1.35x. Closing the rest needs a faster interpreter
core on JS, for example compiling UPLC terms to JS closures.
