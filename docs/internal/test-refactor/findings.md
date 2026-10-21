# Test refactoring – findings log

Every finding of the test refactoring program goes here (spec `[FL-1]`), with its evidence and
status. Spec: `docs/superpowers/specs/2026-10-20-test-refactoring-design.md`.

Status values: `open` (not acted on), `backlog` (a backlog unit owns it), `fixed` (commit),
`rejected` (with the reason).

## P0 – Measure (2026-10-20, master 7822c9a39)

### F-1 Tests are only 8.3 of 23.7 min of the ci-jvm 3.3.8 job – open

Update 2026-10-21: on ubuntu-26.04 the job takes 15.0 min, of it `jvm/test` 286 s (F-27).

Evidence: CI run 39473481085, job 3.3.8, step "Build and test in Nix devshell" (18:18:41–18:42:21).

| Phase | Time |
|---|---|
| Checkout, caches, nix devshell | ~4.5 min |
| `++3.3.8`, silent | ~4.2 min (see F-2) |
| `scalafmtCheckAll` | 0.8 min |
| `jvm/Test/compile` | 3.6 min |
| `jvm/test` (scalusJVM 8m16s, all modules concurrent) | 8.3 min |
| `mima` + `jvm/doc` + `docs/unidoc` | ~2.5 min |

Conclusion: test refactoring alone can cut at most part of 8.3 min. F-2 and F-3 need no test change.

### F-2 `++<version>` costs ~3–4 min on every CI job, <1 s locally – fixed (P1, cause corrected)

Update 2026-10-21: still 2m20s per job on ubuntu-26.04 (F-27).

Cause, re-validated 2026-10-21 (the first explanation, "parsing `;`-joined commands is slow",
was incomplete):

1. Not `++`: a debug run on CI (branch `ci/f2-debug`, run 39621552853) shows `++3.3.8` alone
   takes 1 s.
2. Thread dumps of sbt during the silent phase (`/tmp/trp0/f2dumps`): the main thread is in
   sbt's command parser, 4 levels deep in `sbt.BasicCommands$.aliasBody` (BasicCommands.scala:549),
   each level in `sbt.internal.Act.taskKeyExtra` → `KeyIndex.keys`/`tasks`. In sbt 1.13.0,
   `aliasBody` runs `Parser(removeAlias(state, name).combinedParser)(value)`: an alias's parser
   parses the alias's own body with the full command parser, which contains every other alias.
3. The cost comes from the alias definitions, not from the `;`: same command line, A/B of
   build.sbt only:

   | build.sbt | `sbt "++3.3.8;clean"` | `sbt "++3.3.8" "clean"` |
   |---|---|---|
   | multi-step aliases (3a1653c0d) | 122 s | 23 s |
   | `commandSequence` (dd0f1d102) | 8 s | 4 s |

4. Bisection of the 18 aliases (old build.sbt, `sbt "++3.3.8;clean"`): all 120 s; without `ci`
   5 s; without `precommit`, `cleanpile`, `quick`, `it` 6 s; without `ci-jvm` 14 s; without the
   three `mima*` 24 s; only the four `++` aliases 5 s; only the five ci aliases 5 s. So it needs
   the combination, not one alias. A common feature: alias names that are prefixes of other names
   (`ci` of `ci-jvm`, `ci-js`, `ci-native`, `ci-jvm-3_3_8`; task `clean` of alias `cleanpile`).
5. Not reproduced in minimal builds (`/tmp/sbt-alias-repro*`): 30 empty projects with the exact
   alias block, also with 88 000 settings, also with the real plugins.sbt: 3-7 s. So it also needs
   something of the real build (candidates: the Scala.js/Native cross projects and their many
   keys per project).
6. Not a regression: sbt 1.10.11, 1.11.7, 1.12.0, 1.13.0 all take 128-136 s on the old build.sbt.

Resolution: `commandSequence` in build.sbt replaces every multi-command alias (ci-*, ci, mima*,
quick, precommit, it; the unused `cleanpile` was removed). A command defined with `Command.command`
has a fixed parser (its name), so no alias body is parsed at all. `sbt ci-jvm-3_3_8` now starts its
first step at once. Upstream: filed as https://github.com/sbt/sbt/issues/9945 (2026-10-21;
reproducer master 19008f4ed, 56 s).

Evidence: CI log, 3.3.8 job: sbt banner at 18:21:09, `Setting Scala version to 3.3.8` at 18:25:19,
no output between. Jobs 3.9.0 and 3.3.7 show the same gap (switch done at 18:24:20–18:24:26).
Locally, `sbt "++3.3.8"` takes <1 s (`/tmp/trp0/exp1.log`, 22:56:35).

Conclusion: the cause is CI-specific (candidates: dependency download with a cold coursier cache,
or the `++` reload resolving all 30 projects). Needs a CI run with timestamps or `-debug`. P1 item.

### F-3 The JVM job compiles the JS projects for MiMa – fixed (P1, cause corrected)

Evidence: CI log 18:39:00 `compiling 305 Scala sources to .../scalus-core/js/...`, `done compiling`
at 18:40:13 (73 s), inside `jvm/doc` (88 s total). Same for `scalus-cardano-ledger/js` (14 s).

Conclusion: some JVM project's `doc` depends on a JS project. ~1.5 min per run. P1 item.

Correction 2026-10-21: the cause is not `jvm/doc`. The `mima` alias (`build.sbt`, "ABI
compatibility gate") runs `scalusJS/mimaReportBinaryIssues` and
`scalusCardanoLedgerJS/mimaReportBinaryIssues` on purpose: the JS artifacts are part of the stable
surface. The `[success]` lines after `jvm/test` in the CI log are the 5 MiMa commands (14 s, 88 s,
7 s, 20 s, 1 s), then `jvm/doc`. Fix idea: run the two JS MiMa checks in the CI-JS job, which
compiles JS anyway and finishes earlier (9m57s versus 15.0 min).

Resolution (P1): aliases `mima-jvm` and `mima-js`; `mima` runs both. `ci-jvm-3_3_8` runs
`mima-jvm`, `ci-js` runs `mima-js` after `js/doc`. Both halves pass locally.

### F-4 scalusJVM runs its suites serially in one forked JVM – fixed (P1)

Evidence: `build.sbt` sets `Test / fork := true` for scalusJVM and no `testForkedParallel`.
Locally, `scalusJVM/test` alone: 49 s serial, **15 s** with
`set LocalProject("scalusJVM") / Test / testForkedParallel := true`; 4070 tests green both times
(`/tmp/trp0/exp1.log`, `/tmp/trp0/exp2.log`).

Conclusion: P1 candidate. Must be measured on the 4-CPU CI runner and repeated several times to
catch shared-state flakiness (system properties, global caches, the JIT staging compiler).
Other forked modules (`Test / fork := true` at build.sbt lines 590, 628, 798, 824, 841, 865) need
the same check.

Resolution (P1, 2026-10-21): `ThisBuild / Test / testForkedParallel := true`, with `false` for
`scalusCardanoLedgerIt`. Measured on rebased master 19008f4ed, same sbt session, compiled:

| Project | Serial | Parallel |
|---|---|---|
| scalusJVM (4979 tests) | 108 s | 59 s |
| scalusExamplesJVM | 219 s | 172 s |
| scalusUplcJitCompiler | 19 s | 18 s |
| scalusUtxoCellJVM | 14 s | 11 s |

Full `jvm/test`, 3 runs with parallel suites: 225, 230, 220 s wall, all green, 0 failures;
serial baseline 299 s. Not yet measured on CI. The examples module stays the long pole: the
`LinkedListTest` and `AmmTest` suites are serial chains of recompiles (F-19).

### F-5 Full `jvm/test` locally: 6 min, all modules compete for CPU – info

Evidence: `jvm/test` in a fresh worktree: real 366 s, user 875 s, all green
(`/tmp/trp0/jvm-test.log`). Inside it, scalusJVM took 4m22s versus 49 s alone (F-4).
Suite totals per module from JUnit XML (`timings.csv`): examples 166 s, core 67 s,
ledger 37 s, verification 22 s, jit 17 s.

Caveat: JUnit suite times in a forked JVM seem to exclude suite construction. A Scalus suite
often compiles its scripts in the class body, so the XML undercounts. Profiling (P0b) must use
wall time, not XML sums.

### F-6 Slowest tests – open

From `timings.csv`:

| Seconds | Suite – test |
|---|---|
| 23.5 | `PipelineDeterminismCrossJvmTest` – every example validator compiles to the same bytes in a JVM with different identity hashes |
| 13.4 | `LinkedListTest` – property: duplicate key is always rejected |
| 13.3 | `LinkedListTest` – property: deinit succeeds iff list is empty |
| 8.3 | `AmmTest` – property: k-invariant holds for all valid swaps |
| 7.3 | `ChangeOutputDiffHandlerTest` – fees okay property |
| 6.6 ×4 | `LinkedListTest` – 4 more properties (66 s for the suite) |
| 5.5 | `JitShapeTest` – the largest corpus script compiles and spends the CEK budget |
| 4.8 | `AmmTest` – deposit-then-redeem |
| 4.5 | `SameCborTest` – create two JVMs and compile the same script in each |
| 4.5 | `EtaReduceCekConformanceTest` – random terms |

The old reports (360 s `BilinearAccumulatorBenchmarkTest`) are stale: it is already tagged
`Benchmark`.

### F-7 Generator gaps (`gen-stats.md`) – open

| Generator | Finding |
|---|---|
| `Coin` | never produces 0 (0 of 10 000) |
| `v2.TxOut`, `v1.Value` | 74 % / 69 % of samples hold a negative token amount; a real TxOut never does |
| `Interval` | 67 % of bounds are integers above 64 bit; realistic POSIX times are rare |
| `TxInfo` | median 2–4k nodes, depth up to 129, 15 s per 10 000 samples – a CI-time lever |
| `Term`, `Program` | median ~512 nodes, max 24k; 9 s per 10 000 samples |
| `Constant` | one sample with 44 142 nodes |
| `ExUnits` | zero budget in 0.2 % of samples |
| `TransactionBody`, `GovAction`, `Redeemers`, `Certificate`, `TransactionOutput`, `Transaction` | discard 1.5–4.8 % (`suchThat` filters) |

Tool limit: `GenStatsTest` stops at `KeepRaw`, so `Transaction` shows only its wrapper.

### F-8 Helper structure – open

Evidence: `shared-helpers.csv`, `repeated-defs.csv`.

- 6 traits named `ArbitraryInstances` in 2 chains:
  `uplc.test` → `cardano.address` → `cardano.ledger`, and `uplc.test` → `v1` → `v2` → `v3`.
  `uplc.test.ArbitraryInstances` has no companion object, so callers must import a subclass.
- The v1/v2/v3 files live in `ledger/api/v*` but declare `scalus.cardano.onchain.plutus.v*`.
- The tag `scalus.testing.Benchmark` is defined privately in 3 scalus-core files. (Correction:
  `scalus-testkit` has a shared `scalus.testing.Benchmark`; examples use it. scalus-core tests
  cannot, because scalus-core does not depend on scalus-testkit.)
- Top repeated local defs: `run` (22 files), `check` 13, `createProvider` 13, `lower` 11,
  `spend` 11, `validate` 9, `runValidator` 7, `createEmulator` 5, `mkContext` 5, `runWithBudget` 4.
- 18 non-suite test files have no detected users (candidates for dead code; verify by hand).

### F-9 scoverage instrumentation breaks `@Compile` code – fixed (workaround)

Evidence: `sbt coverage scalusJVM/Test/compile` fails with 224 error lines:
`Literals of type Int can't be used in Scalus scripts` at
`G1Accumulator.scala:35`, tree `Invoker.invoked(8737, ".../scoverage-data")`
(`/tmp/trp0/cov1.log`).

Cause: Scala 3 runs `instrumentCoverage` before `firstTransform`; the `Scalus` phase runs after
`firstTransform` (`scalus-plugin/.../Plugin.scala:169`). The probes are inside the trees that the
plugin turns into SIR.

`coverageExcludedFiles` with the 107 `@Compile` main files had no effect on 3.3.8
(`/tmp/trp0/cov2.log`): same error. That attempt used patterns with the `.scala` suffix, so it does
not prove that 3.3.8 lacks file exclusion. Retried with suffix-free patterns: 3.3.8 compiles
scalusJVM, ledger and examples tests with 0 errors (`/tmp/trp0/cov338.log`).

Options:
1. Exclude the files. **Works on 3.3.8 and 3.9.0**; the first 3.3.8 attempt failed only because
   of the `.scala` suffix.
2. Make the Scalus plugin drop `scala.runtime.coverage.Invoker.invoked` calls. A main-code
   change; it would also give coverage of the on-chain code's JVM execution.

Resolution: coverage runs on the default 3.3.8 with 146 files excluded: the `@Compile` files plus the
files that call plugin intrinsics (`compiledModules("…")` in `IntrinsicResolver.scala` needs a
literal argument; the probes wrapped it). The patterns MUST omit the `.scala` suffix: scoverage
matches the path without it. Procedure: `coverage.md`. Option 2 stays open as a later
improvement.

### F-10 Coverage baseline, 2026-10-21 – info

Evidence: `baseline/summary.csv`; `coverage-run.sh` = clean build plus all of `jvm/test` on 3.3.8,
all tests green, 11 min per run. (A first baseline on `++3.9.0` with 4 projects was replaced.)

| Module (code measured) | Statements | Covered | Branches | Covered |
|---|---|---|---|---|
| scalus-core | 47 628 | 74.1 % | 9 075 | 67.7 % |
| scalus-cardano-ledger | 12 242 | 70.3 % | 1 648 | 60.9 % |
| scalus-plugin | 10 147 | 36.2 % | 2 446 | 24.2 % |
| scalus-verification | 3 865 | 70.5 % | 759 | 58.9 % |
| scalus-testkit | 3 356 | 29.3 % | 336 | 33.3 % |
| scalus-examples | 2 020 | 70.0 % | 154 | 52.6 % |
| bloxbean-cardano-client-lib | 2 009 | 19.3 % | 297 | 10.1 % |
| scalus-ts-exporter | 1 542 | 93.2 % | 280 | 86.8 % |
| scalus-utxo-cell | 1 424 | 81.5 % | 251 | 70.5 % |
| scalus-uplc-jit-compiler | 836 | 94.4 % | 224 | 91.5 % |

Low spots worth a P2 look: scalus-plugin 24 % of branches (compile-time code, see F-12),
scalus-testkit 29 %, bloxbean 19 %. Excluded from the numbers: the 146 `@Compile`/intrinsic files.

### F-11 Coverage differs between identical runs – fixed (gate tolerates it)

Evidence: two coverage runs of the same commit: 27 151 vs 27 149 uncovered statements; 6 differ.
Covered only in one run: `DefaultFun.scala:1613 encode`, `Address.scala:149 toString`,
`Certificate.scala:116 scriptHashOption`, `MemoryUsage.scala:128 memoryUsage`. Without handling,
the gate reported 2 "lost" statements with no code change.

Cause: unseeded property tests (random values per run).

Resolution: `baseline/flaky.tsv` (6 statements); the gate ignores them. A fixed ScalaCheck seed
would not cover tests that use `scala.util.Random` directly.

Open question for P2: the same randomness means a property test covers some branches only
sometimes. Those branches (the flaky list) are candidates for explicit example tests or better
generators.

### F-12 Plugin and macro coverage depends on the incremental build – fixed (clean runs)

Evidence: two full `jvm/test` coverage runs on 3.3.8, the second one incremental: 1926 statements
differ. 994 are in `scalus-plugin` (`PatternMatchingCompiler.scala` 360, `SIRType.scala` 170,
`DefaultFun.scala` 138), 804 in `scalus-core` macros (`PlutusDataSchema.scala` 227,
`ToDataMacros.scala` 221, `FromDataMacros.scala` 149, `EqMacros.scala` 130), 128 in
`UtxoQueryMacros.scala`.

Cause: the plugin and the macros run inside scalac. Their statements are covered while the test
sources compile, not while the tests run. An incremental run compiles nothing and covers none.

Resolution: `coverage-run.sh` always starts with `clean`. Two clean runs differ in 39 statements
only (`DefaultFun.scala` 12, `PatternMatchingCompiler.scala` 11, `FlatInstances.scala` 7); they are
`baseline/flaky.tsv`. This keeps plugin coverage in the gate,
which matters: deleting a test source can remove the only compile that reaches a plugin path.

## P0b – Profile (2026-10-21, a026e6cef)

Method: `scripts/profile.sh` (JFR `profile` settings, `stackdepth=1024`), analysis by
`scripts/profile_analyze.py`; reports in `profiles/<module>.txt` and `profiles/<module>-analysis.md`.
Shares are of the CPU samples of one module's test run, run alone. JFR adds overhead
(scalusJVM: 114 s profiled vs 51 s plain), so read shares, not seconds.

The default JFR stack depth (64) hid the test frame in ~45 % of samples (deep lowering
recursion); with 1024 it is 2.3 %.

### F-13 Core tests spend 46 % of CPU compiling SIR to UPLC – info

Evidence: `profiles/core-analysis.md`: `UplcPipeline.run` 46.4 % inclusive,
`SirToUplcV3Lowering.lower` 23.8 %, `V3Optimizer.apply` 22.4 %.

Conclusion: the lever for core test time is compilation, in main code (F-14, F-15) and in tests
that compile the same code again (to check in P2: suites that call `compile`/`toUplc` per test
instead of once per suite).

### F-14 `ScalusRuntime.initSupportBindings` lowers every support definition on every compile – open

Cause class: `main-code`. Decision: `later`.

Evidence: 19.3 % of core test CPU (about 42 % of the whole compile), via
`SirToUplcV3Lowering.newLoweringContext` → `ScalusRuntime.initContext` →
`initSupportBindings` (`ScalusRuntime.scala:36`). Paid by `ListTest`, `ValueTest`,
`SortedMapTest`, `OptionTest` and every other suite that compiles.

Cause: the function lowers all definitions of all support modules into each new
`LoweringContext`, eagerly, including the ones the script never references. Its comment says why:
every `SIR.ExternalVar` reference must resolve to the same `VariableLoweredValue`.

Fix idea: lower a support definition on first lookup and memoize it in the context. That keeps one
`VariableLoweredValue` per definition. Needs care: the `inUplcConstrListScope` flag must be set the
same way at the first lookup. Benefit: every compile, also for users, not only tests.

### F-15 Optimizer passes format log messages that nobody reads – open

Cause class: `main-code`. Decision: `later` (own `perf:` commit; `Logger` is in the MiMa-checked
package `scalus.uplc.eval`).

Evidence: ~3.8 % of core test CPU in `Term.pretty`/`TermPrinter` called from `Inliner.go`,
`Inliner.tryPartialEval` and `EtaReduce.etaReduce`.

Cause: `V3Optimizer` and `V1V2Optimizer` (`OptimizerPipelines.scala`) create a collecting `Log()`
and pass it to every pass. The passes build messages such as
`s"Inlining $name with ${inlinedArg.show}"` (`Inliner.scala:272`) on every rewrite.

Fix idea: a by-name message (`log(msg: => String)`) or a `NoLogger` default with an opt-in
collecting logger for debugging.

### F-16 `EtaReduceCekConformanceTest` takes 22 % of core test CPU – fixed (P1)

Cause class: `test-code`. Decision: `fix-now` candidate for P1.

Evidence: 22.4 % of core samples; inside it, `genConst` 13.7 % self and `genTerm` 4.8 %. The
suite runs `forAll(genClosedTerm, minSuccessful(3000))` (line 383). `genConst` and the other
generators are `def`s (line 157), so each recursive call builds new `Gen` objects; allocation
shows `Gen$$anon$7` 5 % and `Gen$RetryUntilException` 1.6 %.

Fix idea: make the leaf generators `val`s; check where the retries come from. Do not lower
`minSuccessful`: the owner ruled out fewer iterations.

Resolution (P1): the real cause was different. `genTerm(depth)` built all ~15 recursive branches
eagerly, each calling `genTerm(depth - 1)`: about 15^depth `Gen` objects per sample, rebuilt for
each of the 3000 samples (`Gen.sized`). Each recursive branch is now wrapped in `Gen.lzy` (helper
`rec`). With seed `-S 424242`, the property's summary (3000 checked, 831 reduced, 83 quirks,
3 budget increases, same examples) is identical before and after, so the generated terms are the
same; the property went from 3.3 s to 0.37 s.

### F-17 Test-side term printing – open

Evidence: `ValueIntrinsicsLoweringTest.hasBuiltin` prints whole terms to search them
(13 samples, ~1 %); `LetChainRegroupCekConformanceTest` and `CommonSubexpressionEliminationTest`
print terms inside `forAll` bodies.

Cause class: `test-code`. Fix idea: search the term structurally; build clue strings only on
failure. Small, but a pattern for the style guide.

### F-18 Examples tests: 92 % of CPU compiles UPLC; the module runs 2m47s alone, serially – open

Evidence: `profiles/examples-analysis.md`: `UplcPipeline.run` 91.9 % inclusive,
`V3Optimizer.apply` 89.7 %, `CommonSubexpressionElimination.apply` 59.7 %, `Inliner.apply`
27.1 %. `profiles/examples.txt`: "Run completed in 2 minutes, 47 seconds", 722 tests.
`scalusExamplesJVM` forks (`build.sbt:798`) without `testForkedParallel`, like scalusJVM (F-4).

Conclusion: the largest module, and serial. Two levers: F-19 (compile less often) and
`testForkedParallel` (P1, measure on CI). F-20 and F-21 make each compile cheaper.

### F-19 Off-chain objects recompile their applied script per test case – fixed in tests (P1); main-code option open

Cause class: `test-code` and `main-code`. Decision: owner to choose (see fix ideas).

Evidence: `LinkedListTest` = 45.4 % of examples CPU, all under `LinkedListOffchain.apply`;
`AmmTest` = 14.6 %, ~72 % of it under `AmmOffchain.apply`. `LinkedListOffchain.scala:44`:
`mintingContract.apply(cfg.toData)`, then `.script`. `PlutusV3.apply` (`Compiled.scala:477`)
applies the argument at SIR level, so every new instance lowers and optimizes the whole program
again. `LinkedListTest` builds a new instance per property case (`setup`, line 1034) with the same
parameters each time.

Fix ideas:
1. Test side, `fix-now`: build the off-chain object (or the applied script) once per suite when
   the parameters do not change. Must be repeated in each suite with this pattern.
2. Main code, one place: memoize the applied `PlutusV3` per argument constant in `apply` (bounded
   cache). Helps users who apply parameters in a loop too. Needs a decision on the memory bound.

Example of the size: removing the repeated compiles from these two suites would cut roughly
60 % of the examples module's CPU.

Resolution, test side (P1): `LinkedListTest` and `AmmTest` build their off-chain object once per
suite and evaluator mode (two lazy vals) instead of in every `createSetup()`; each test still gets
a fresh `Emulator`. `PlutusScriptEvaluator`, `LinkedListOffchain` and `AmmOffchain` hold no
mutable state (only method-local `var`s). Results, alone: `LinkedListTest` 66 s → 6.2 s (23 green),
`AmmTest` 18.5 s → 4.6 s (16 green). No other examples suite has this `createSetup` pattern.
Fix idea 2 (memoize in `PlutusV3.apply`) stays open for the owner: it would help users too.

### F-20 CSE re-collects the whole term after each extraction – open

Cause class: `main-code`. Decision: `later` (algorithm change in a correctness-sensitive pass).

Evidence: in examples, `CommonSubexpressionElimination$.isAncestorOrSelf` is the top self-time
item (27.9 %), plus `longestCommonPrefix` 3.6 %, `TermKey.equals` 5.2 %, `Term.~=~` 9.9 %.

Cause: `apply` (`CommonSubexpressionElimination.scala`, the `while candidate.nonEmpty` loop)
extracts one candidate, then calls `collect` on the whole rewritten term again: O(k·n) for k
extractions. Inside `collect`, `add` scans all groups of a key linearly and compares `Vector[Int]`
paths with `startsWith` (`isAncestorOrSelf`, line 401). The comments explain the re-collect: old
paths and savings go stale after a rewrite.

Fix ideas: index the groups of a key by region (walk the region's ancestors, O(depth) lookups,
instead of a scan); re-collect only the rewritten subtree.

### F-21 `Inliner.tryPartialEval` computes free variables at every node – fixed (perf)

Cause class: `main-code`. Decision: `later` (small change; could be `fix-now` in its own
`perf:` commit if the owner wants).

Evidence: `TermAnalysis.freeVars` 22.6 % self in examples; 43.7 % of those samples come directly
from `Inliner.tryPartialEval`, most of the rest from `List.flatMap` inside `freeVars` itself.

Cause: `tryPartialEval` (`Inliner.scala:193`) calls `term.freeVars` for each `Apply`/`Force` node
that `Inliner.go` visits; `freeVars` walks the whole subtree. Over one pass that is O(n²) for deep
terms, and the pipeline runs the inliner several times.

Fix idea: memoize free variables per node identity for one pass (an `IdentityHashMap`, as CSE
already does for hashes), or compute them bottom-up in `go`.

Resolution (branch perf/compile-hot-paths): `Inliner.cachedFreeVars`, an identity-keyed cache
reset per `apply`. The 57 programs of `PipelineDeterminismChild` (5 optimizer configurations)
compile to identical bytes (md5 equal). Warm corpus compile: 9 s → 7-8 s (sbt reports whole
seconds). scalus.uplc.transform.* and eval.*: 2776 tests green.

### F-22 Smaller modules – info

| Module | Test run alone | Top suite | Note |
|---|---|---|---|
| scalusCardanoLedgerJVM | 8 s | `ChangeOutputDiffHandlerTest` 33 % | `sha2_512` 7 % (HD wallet derivation), `blake2b_256` 5 % |
| scalusUplcJitCompiler | 15 s | – | 87 % in the staging compiler thread (`StagingCompilerThread.compile` 38.6 %), by design |
| scalusVerification | 4.5 s | `UplcBlasterTest` 44 % | lowering of the proved scripts |

Below the 30 s threshold of `[PR-1]`; recorded for completeness.

The profile script forces `fork := true`. `scalusCardanoLedgerJVM` does not fork by default, and
4 tests of `EvalPlutusScriptsTest` then failed with `NoSuchFileException`: they read
`scalus-examples/js/src/main/ts/*.cbor` relative to the working directory. `ROOT_CWD=1` fixes the
profile run. Hidden coupling worth a look in P2: tests that depend on the working directory.

### F-23 Suite-level profiles `[PR-2]` – info

The top suites of `timings.csv` are all in scalusJVM or scalusExamplesJVM, so the module profiles
attribute them (CPU by suite, `--focus`). Separate suite recordings were not needed:
`PipelineDeterminismCrossJvmTest` spends 100 % in `PipelineDeterminismChild.render` (it starts
child JVMs by design); `BooleanOptimizerImpactTest` 90 % in `UplcPipeline.run`.

## Notes from other work (2026-10-21)

### F-24 CI-JS runners die in ~20 % of runs – open

Source: session scalus-e6 (CI-JS fix on branch `ci-js-memory`, edits only
`.github/workflows/ci-js.yml`).

Evidence (as reported): "The runner has received a shutdown signal" in 21 of 104 CI-JS attempts on
ubuntu-24.04 since 2026-10-01, always during `js/test`; likely memory or CPU starvation on the
16 GB runner. The fix under test limits `concurrentRestrictions` to 2 for `Tags.Test` and
`ScalaJSTags.Link`.

Consequence for P1: `nightly.yml` must not run JS tests with more parallelism than ci-js uses;
reuse the ci-js limits once they land.

### F-25 Baseline predates new conformance tests – info

Local master `49d57852f` (not on this branch) adds 906 `.flat` conformance cases to
`PlutusConformanceTest`, and one `build.sbt` line in the scalusCardanoLedger `.jsSettings` block.
After this branch is rebased, `timings.csv` and the coverage baseline must be regenerated before
the first gate run: new tests can only add coverage, but the timing of `PlutusConformanceTest`
changes.

### F-26 CI-JS passes after the fix, but RAM and swap are full at the end of js/test – open

Evidence: CI-JS run on master `ef8b011b8` (2026-10-21), memory sampler of commit `5d2c855c4`
(limit 2 for `Tags.Test` and `ScalaJSTags.Link`). Runner: 15 983 MB RAM, 3 071 MB swap.

| Time | Phase | Used RAM | Swap |
|---|---|---|---|
| 10:50:19 | before the JS test links | 5 545 MB | 0 |
| 10:51:19 | js/test | 14 501 MB | 99 MB |
| 10:53:19 | end of js/test | 15 471 MB | 3 070 MB (full) |

The 10 parallel runs on branch `ci-js-memory` all passed; their peak was 15 578 MB.
Job time on master: 9m57s, down from 14m48s on 2026-10-20 (less swapping).
The one CI-JS failure of the day (`feat/verification-strings`, 09:36) had another cause: the nix
binary cache hit GitHub's API rate limit (`HTTP error 418 … rate limit exceeded`).

Conclusion: the fix works but leaves no margin. Candidates: a smaller sbt heap for this job
(`.jvmopts` sets `-Xmx8g`; the linker needs less), `Tags.limit(Tags.Test, 1)` only for the
heaviest module, or a lower Node `--max-old-space-size`. Measure each with the sampler.

### F-27 ubuntu-26.04 runners cut the ci-jvm 3.3.8 job from 24.7 to 15.0 min – info

Evidence: master `ef8b011b8` (2026-10-21) moved CI-JVM back to `ubuntu-latest`, now image
ubuntu-26.04 (version 20261012.163.1). CI-JVM run 39586819675, job 3.3.8:

| Phase | 2026-10-20 (ubuntu-24.04) | 2026-10-21 (ubuntu-26.04) |
|---|---|---|
| `++3.3.8`, silent | 4m10s | 2m20s (10:47:19 → 10:49:39) |
| `scalafmtCheckAll` | 50 s | 34 s |
| `jvm/Test/compile` | 216 s | 140 s |
| `jvm/test` | 498 s | 286 s |
| job total | 24.7 min | 15.0 min |

Other jobs of the same run: 3.8.4 14.9 min, 3.9.0 17.7 min, 3.3.7 9.7 min; CI-JS 9m57s.

Conclusion: the P1 baseline is now 15.0 min, not 24.7. F-2 remains: the silent `++` still costs
2m20s per job, about 15 % of the job. `jvm/test` (286 s) is now about a third of the job.

## Term generators (2026-10-21)

### F-28 The let-chain generator's if-then-else shape always fails – fixed

Cause class: `test-code`. Decision: fix with the generator unification (F-29) or alone.

Evidence: `LetChainRegroupCekConformanceTest.scala:148` builds
`Force(IfThenElse $ s $ Delay(t) $ Delay(f))`. `TermDSL`'s `Conversion[DefaultFun, Term]` yields a
bare `Builtin(IfThenElse)`, without the type force a polymorphic builtin needs. A probe evaluated
the shape with `s = true`: `UnexpectedBuiltinTermArgumentMachineError`; the forced form
`Force(Force(Builtin(IfThenElse)) $ ...)` returns `1`. So this branch never exercises
if-then-else; it only checks that the optimized term fails the same way.

Resolution: commit "test: force ifThenElse in the let-chain generator" (branch
test-refactor/term-gens); the suite passes.

### F-29 Nine `Term` generators, four families, copied pieces – fixed (owner-approved scope)

Evidence: survey of all `Gen[Term]` sources (2026-10-21):

| Family | Generators | Purpose |
|---|---|---|
| shared random term | `uplc/test/ArbitraryInstances.scala` `arbitraryTerm` (+ `Program`, `Constant`, `DefaultUni`) | syntax round-trips: Flat, parser, de Bruijn, DSL; ~10 suites |
| env-scoped conformance terms | `EtaReduceCekConformanceTest` (A), `CseCekConformanceTest` (B), `LetChainRegroupCekConformanceTest` (C) | optimizer pass vs CEK |
| arithmetic trees | `CommonSubexpressionEliminationTest` (D), `CommonContextExtractionTest` (E) | closed, typed, terminating |
| enumerated corpus | `TermAnalysisCekConformanceTest` | deterministic, not ScalaCheck |

Duplication: D and E `genInt`/`genBinOp` are verbatim copies, and E's `genBinOp` is unused.
A and C `genConst` is verbatim. A, B and C share the env-scoped skeleton (Var from env, LamAbs,
let, Delay, `Force(Delay)`, Constr, Case, Error) and the same `run`/`Outcome`/budget harness.

Eager recursion (the F-16 pattern): exponential in `ArbitraryInstances.scala` lines 290, 293,
300, 310 (4 eager calls per level, about 4^7 `Gen` objects per sample at size 100); linear or
shallow in B line 371, C lines 159-167, D line 504.

Coverage notes: `arbitraryTerm` never generates String, BLS or BuiltinValue constants (its
`DefaultUni` has no such leaves), and its users run ScalaTest's default 10 cases except
`CommonSubexpressionEliminationTest` (100).

Resolution (branch test-refactor/term-gens, owner said "go generators"):
1. `arbitraryTerm` branches wrapped in `Gen.lzy`: for seeds 0-1999 the terms and programs are
   identical (MD5), sampling 2.4-2.9x faster. Kept self-contained: `ArbitraryInstances.scala` is
   copied into the published scalus-testkit artifact (build.sbt), so it must not depend on
   test-only helpers.
2. `scalus.uplc.test.TermGen` (shared test sources): `branch` (lazy weighted branch),
   `smallConst`, `smallInt`, `arithOp`, `arithTree`. Users: EtaReduce, LetChain, CSE and CCE
   tests. The CCE copy of the arithmetic tree was unused and is gone. EtaReduce's seeded
   fingerprint is unchanged.
3. Not unified, on purpose: the three conformance generators (each targets one pass's shapes)
   and their `run`/`agree` harnesses (each compares differently; a shared one would need three
   options to save about ten lines per suite).
Open: the String/BLS/BuiltinValue gap of `arbitraryTerm`, and the default 10 cases of its users –
P2 items for their clusters.

### F-30 Lazy support bindings (F-14) help core tests, not real contracts – info

Evidence: `initSupportBindings` is 19.3 % of core test CPU but 1.1 % of examples CPU, where all of
lowering is 3.3 % and the optimizer ~90 % (CSE alone 59.7 %). Core tests compile many tiny
snippets, so the fixed per-compile cost dominates there. A lazy version needs a registry outside
`LocalScope` (lowering restores older scopes in ~10 places) and must keep the output bytes.
Decision: not built; F-20 (CSE) is the lever for users. Reconsider if core test time matters.

### F-31 P1 on CI: the longest job drops from 17.7 to 12.1 min – info

Evidence: branch `test-refactor/p1` at `dd0f1d102`, runs 39625799663 (CI-JVM), 39625799813
(CI-JS), 39625799501 (CI-Native), 2026-10-21, all green, ubuntu-26.04. Baseline: master
`ef8b011b8`, same day, same image.

| Job | master | P1 |
|---|---|---|
| CI-JVM 3.3.8 | 15.0 min | 10.0 min |
| CI-JVM 3.9.0 | 17.7 min | 8.1 min |
| CI-JVM 3.8.4 | 14.9 min | 7.9 min |
| CI-JVM 3.3.7 | 9.7 min | 5.0 min |
| CI-JS | 9.9 min | 12.1 min |
| CI-Native | 11.0 min | 10.4 min |

Inside the 3.3.8 job: the silent phase before the first step is gone (sbt ready 15:30:14, `++`
at 15:30:15; it was 2m20s on master and 9 min on the first P1 run before `commandSequence`);
`jvm/test` 157 s instead of 286 s.

CI-JS: the 72 s silent phase after `set` is gone too, and `mima-js` adds ~25 s. The job was still
2.2 min slower than on master; P1 changes nothing in the Scala.js tests (they do not fork), so
this is likely runner variance – check over more runs. Its memory peak was 14.4 GB RAM and
1.6 GB swap (master: 15.5 GB and full swap).

Wall time of CI (longest job): 17.7 min → 12.1 min.

### F-32 Catch-all failure handling in conformance tests – partly fixed

Evidence: `EtaReduceCekConformanceTest.run` mapped every `NonFatal` exception to
`Outcome.Failed`, so a host crash (a bug in the pass or the machine) on both sides counted as
agreement. `LetChainRegroupCekConformanceTest` already caught only `MachineError`, with a comment
saying why. Fixed in EtaReduce (commit "test: count only UPLC failures …"); the 3000 seeded cases
still pass with the same terms, so none had hidden a host crash.

Open: `CommonContextExtractionTest` compares `Try(term.evaluate).toOption` before and after, so a
host exception on both sides also passes. A P2 `strengthen-assertion` item; search for the same
`Try(...).toOption` pattern in the other optimizer suites.
