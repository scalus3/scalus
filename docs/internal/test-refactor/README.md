# Test refactoring – working directory

Spec: `docs/superpowers/specs/2026-10-20-test-refactoring-design.md`.
All findings, with evidence and status: [`findings.md`](findings.md).

## State (2026-10-22)

P2 is done: 42 cluster reports in `reports/`, 810 findings, checked by `scripts/check_report.py`.
Main-code defects found on the way: F-33 (fixed), F-34, F-35 to F-82. Next: P3 (style guide, helper
catalog, backlog).

## State (2026-10-21)

CI baseline: ci-jvm 3.3.8 job 15.0 min on ubuntu-26.04 (F-27); CI-JS passes with RAM and swap full (F-26).

P0 (measure), P0b (profile) and P1 (CI-time wins, local part) are done. Next: measure P1 on CI,
then the P2 pilot.

P1 result, local, master 19008f4ed + P1: `jvm/test` 148 s instead of 299 s, 8450 tests green
(the 4 report-only CCE tests moved to `nightly.yml`).

| P1 change | Finding | Local effect |
|---|---|---|
| `testForkedParallel` for forked test JVMs | F-4 | `jvm/test` 299 s → 220–230 s |
| lazy generator branches in `EtaReduceCekConformanceTest` | F-16 | property 3.3 s → 0.37 s, same terms |
| one off-chain object per suite in `LinkedListTest`, `AmmTest` | F-19 | 66 s → 6.2 s, 18.5 s → 4.6 s |
| `nightly.yml` runs the `Benchmark` tests | `[P1-3]` | 80 tests that CI never ran |

Open, need the owner: F-2 (needs a CI debug run, so a pushed branch), F-3 (move the JS MiMa
checks to CI-JS), F-19 option 2 and F-21 (main-code memos), F-14, F-15, F-20 (main code).

## Biggest levers found

| Lever | Finding | Cause class | Size |
|---|---|---|---|
| `++<version>` is silent and slow on CI | F-2, F-27 | CI | 2m20s per job (ubuntu-26.04) |
| `testForkedParallel` for forked modules | F-4, F-18 | build | scalusJVM 49 s → 15 s locally; examples 2m47s serial |
| Off-chain objects recompile their applied script per test | F-19 | test + main | ~60 % of examples CPU |
| CSE re-collects after each extraction | F-20 | main | top self-time item in examples |
| `freeVars` per node in `Inliner.tryPartialEval` | F-21 | main | 22.6 % of examples CPU |
| Eager support bindings per compile | F-14 | main | 19.3 % of core CPU |
| `jvm/doc` compiles the JS projects | F-3 | build | ~1.5 min per CI run |

## Artifacts

| File | What | Produced by |
|---|---|---|
| `timings.csv` | seconds per suite and test | `scripts/timings.py` |
| `shared-helpers.csv`, `repeated-defs.csv` | helper inventory | `scripts/helpers.py` |
| `gen-stats.md` | generator statistics | `GenStatsTest` (tag `Benchmark`) |
| `baseline/` | coverage baseline, flaky list | `scripts/coverage-run.sh`, `scripts/coverage.py` |
| `coverage.md` | how to run coverage and the gate | – |
| `profiles/<module>.txt` | `jfr view` extracts | `scripts/profile.sh` |
| `profiles/<module>-analysis.md` | CPU by suite and Scalus method, allocation | `scripts/profile_analyze.py` |

`profile_analyze.py <file.jfr> --focus <method>` shows which suites and callers pay for one
method. The `.jfr` files stay in `/tmp/trp-profiles/` and are not committed.
