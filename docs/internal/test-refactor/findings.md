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

### F-33 `Term.~=~` ignores the number of `Constr` fields and `Case` branches – fixed

Evidence: `Term.scala:363-364` compares the fields and branches with `zip`, which stops at the
shorter list. So `Constr(0, List(a)) ~=~ Constr(0, List(a, b))` is true, and
`Constr(0, Nil) ~=~` any `Constr(0, …)` is true. `α_==` (`Term.scala:336-345`) checks the sizes.
Found by the P2 pilot C9 agent; confirmed by reading.

Effect on tests: 243 `~=~` uses in 21 test files. An assertion on a `Constr` or `Case` term
still passes if a pass drops a field or a branch.

Effect on main code: CSE's `TermKey.equals` (`CommonSubexpressionElimination.scala:364`) is
`~=~`, and `AlphaTermKey.equals` uses `TermKey` equality to resolve collisions of the weak
`alphaHash`. Two terms that differ only in a trailing field or branch are equal keys when their
hashes collide. The hash folds each field into the result (`structuralHash`, line 391), so a
collision needs a 32-bit coincidence or a crafted constant: CSE then shares two different terms.

Fix: add `a1.size == a2.size` and `c1.size == c2.size` to the two cases, plus a regression test.
Run the scalus.uplc tests and the 57-program byte oracle: a test that starts failing was hiding
a wrong term.

Resolution: the two size checks and `TermEqualityTest`, whose 3 new tests fail without the fix.
`scalusJVM/test` passes (4988 tests): no existing assertion was hiding a wrong term. The
57-program byte oracle is identical before and after (md5 7faed22b…), so CSE output does not
change.

### F-34 `ForcedBuiltinsExtractor` can capture a user variable – open

Evidence: `ForcedBuiltinsExtractor.scala:62-67`: `env` starts empty and `LamAbs` only removes
names from it, so `freshName` never renames and the extracted builtin is always bound as
`__<Builtin>`, e.g. `__HeadList`. The binders wrap the whole term (lines 105-110). A user
binder named `__HeadList` that encloses a use of `headList` captures that use: one use inside
a lambda counts 2 (lines 33, 41), so a single use triggers the extraction. A free `__HeadList`
in an open input is captured too. Later steps do not rename it: `TermNaming.scala:72-73` only
strips the `__cse_`/`__cce_` prefixes. P2 pilot T-2; confirmed by the verify agent, by reading.

Reach: not from the default V3 lowering, which appends `'N` to names
(`LoweringContext.scala:188-195`). Likely from the Scott/SoP backends, which keep SIR names
(`BaseSimpleLowering.scala:595,623`), and always from hand-written UPLC.

Fix: track the names in scope (or all names in the term) and make `freshName` avoid them. Add a
regression test with a user binder `__HeadList` and one with a free `__HeadList`.

## Main-code defects found in P2 (2026-10-22)
Logged from the cluster reports `[P2-13]`. Each entry repeats the report's claim and evidence;
the report has the full action. "By reading" means no test or run confirmed it yet.

### F-35 `Address.fromBytes` and `Cip1852.parsePath` accept input the ledger or the spec rejects – open
Cluster `address-wallet-blueprint`, report finding C35-1, verdict CONFIRMED.

By reading, Address.fromBytes claims to be 'the ledger's decodeAddr' (1218-1219) but accepts bytes
that decodeAddr rejects. cardano-ledger conway-1.22.1.0, Cardano/Ledger/Address.hs: a Byron address
needs header exactly 0x82 (headerByron, 540-544) and is decoded in full with decodeCrcProtected
(decodeBootstrapAddress, 688-697; Byron Address.hs:174); a Shelley header must have header .&. 0x8E
== 0 (headerNonShelleyBits, 547-548, check at 653-659), so the network nibble is 0 or 1 and types
8-15 are not output addresses. Scalus: (a) any header 0x80-0x8F becomes a ByronAddress with no CBOR
or CRC check (1262, 1462-1467), so Address.fromBytes(Array(0x80.toByte)) succeeds and fails only
later in the lazy `parsed`; (b) a Shelley header with network nibble 2-15 parses to Network.Other
(1317 and the other parseTypeN); (c) TransactionOutput reads its address with Address.read
(TransactionOutput.scala:273, 293), so a stake address (header 0xe_/0xf_) is accepted as an output
address. A grep of scalus.cardano.ledger.rules finds no rule that references StakeAddress, so such
an output passes validation in Scalus and fails deserialization on a node. The tests pin the lax
behavior: Arbitrary[TransactionOutput] = autoDerived (ledger ArbitraryInstances:335) draws Address
from address.ArbitraryInstances, which yields StakeAddress in 34 % and Network.Other in ~20 % of
samples (gen-stats.md), and CborSerializationTest:113 round-trips them.

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/address/Address.scala:1218-1229`, `scalus-core/shared/src/main/scala/scalus/cardano/address/Address.scala:1244-1267`, `scalus-core/shared/src/main/scala/scalus/cardano/address/Address.scala:1462-1467`, `scalus-core/shared/src/main/scala/scalus/cardano/ledger/TransactionOutput.scala:273`, `scalus-core/shared/src/main/scala/scalus/cardano/ledger/TransactionOutput.scala:293`, `scalus-core/shared/src/test/scala/scalus/cardano/ledger/ArbitraryInstances.scala:335`, `scalus-core/shared/src/test/scala/scalus/cardano/ledger/CborSerializationTest.scala:113`

Verification: The ledger source (../cardano-ledger at conway-1.22.1.0, libs/cardano-ledger-
core/src/Cardano/Ledger/Address.hs) matches the claim. headerByron = 0b10000010, and isByronAddress
is equality. Byron bytes go through decodeBootstrapAddress -> decodeFull' (lines 688-697). A Shelley
header must satisfy header .&. (0x82 .|. 0x0C) == 0 (lines 547-548 and 653-659), so the network is
only bit 0 and every header with bit 7 set (types 8-15, stake 0xE_/0xF_) is rejected. Scalus
Address.scala:1244-1267 dispatches on the high nibble only. parseType8 (1462-1467) wraps any 0x8_
header in ByronAddress with no CBOR or CRC check; parsed is lazy (928). Network.fromNetworkId
(154-157) maps 2-15 to Other. TransactionOutput.scala:273/293 use Address.read
(Address.scala:1197-1201), which calls fromBytes, so a StakeAddress is accepted as an output
address. In rules/, only WrongNetworkValidator checks output addresses, through
getNetwork.contains(expected). A StakeAddress on the right network passes it, so (c) is reachable.
Note: WrongNetworkValidator does reject Network.Other at validation, so (b) is a parse-level
leniency only, not a validation hole. The generator pins hold: ledger ArbitraryInstances:335 is
autoDerived, address ArbitraryInstances:9-17 gives Other at weight 1/5, and
CborSerializationTest:112-113 round-trips TransactionOutput.

Action: Main-code note: in fromBytes require header == 0x82 for Byron and decode it (reuse
ParsedByronAddress.parse + isValid), and reject a Shelley/stake header whose network nibble is not 0
or 1; in TransactionOutput read the address with a variant that rejects StakeAddress
(fromBech32("stake1...") must keep returning StakeAddress, RewardAccount.scala:60 relies on it).
Tests: add the three negative cases to CardanoAddressTest and a TransactionOutput decode test; then
apply C35-9 to the generator.

### F-36 JSON codecs that cannot read their own output (Timelock Blockfrost, `DataReadWriter`, blueprint `Purpose`) – open
Cluster `address-wallet-blueprint`, report finding C35-2, verdict CONFIRMED.

By reading (lines 485-501 never executed in the baseline): the Purpose JSON codec is incomplete
(marked todo in the source). (1) Purpose.OneOf encodes as an empty JSON array `[]`, while CIP-57
writes `{"oneOf": [...]}`; (2) decodeValue calls readString, so it cannot read back the `[]` it
wrote, nor the CIP-57 oneOf object that Aiken emits; a blueprint whose TypeDescription has purpose =
Some(OneOf(...)) does not round-trip; (3) an unknown purpose string (e.g. "vote" or "propose") falls
through the match and throws scala.MatchError instead of in.decodeError. The string "oneOf" decodes
to OneOf(Seq.empty).

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/Blueprint.scala:476-501`, `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/Blueprint.scala:410-416`

Verification: Blueprint.scala:482-490: decodeValue calls in.readString and then does a non-exhaustive String
match, so an unknown string throws scala.MatchError, not decodeError. "oneOf" maps to
OneOf(Seq.empty) with a // todo. Lines 492-501: OneOf encodes as an empty array via
writeArrayStart/writeArrayEnd. readString cannot read an array or an object back, so
TypeDescription(purpose = Some(OneOf(...))) (411-416, JsonCodecMaker.make) does not round-trip.
Compare DataType's codec (459-464), which uses in.decodeError correctly.

Action: Main-code note: encode OneOf as {"oneOf": [..]} and decode both forms, with decodeError on unknown
input. Test: in BlueprintTest, round-trip a TypeDescription for each Purpose case including
OneOf(Seq(Spend, Mint)), parse a CIP-57 oneOf object, and assert a decode error
(JsonReaderException) for an unknown purpose string.

### F-37 BIP-39 seed: passphrase not NFKD-normalized; a non-ASCII passphrase gives a different wallet than other tools – open
Cluster `address-wallet-blueprint`, report finding C35-3, verdict NARROWED.

By reading: BIP-39 derives the seed with PBKDF2 over the mnemonic 'in UTF-8 NFKD' and salt
'mnemonic' + passphrase 'again in UTF-8 NFKD'. mnemonicToSeed uses passphrase.getBytes("UTF-8") with
no NFKD, so a non-ASCII passphrase in composed form (e.g. "caf\u00e9") gives a seed different from
other BIP-39 implementations (which decompose to "cafe\u0301"). normalizeWords also uses the
default-locale toLowerCase, so on a JVM with a Turkish default locale an upper-case word with I
(e.g. "LIBRARY") lowers to a dotless i and the mnemonic is rejected. Cardano keys
(Bip32Ed25519.masterKeyFromEntropy) do not use mnemonicToSeed, so the impact is limited to callers
of the BIP-39 seed.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/wallet/hd/Bip39.scala:108-113`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/wallet/hd/Bip39.scala:201-205`

Verification: Both defects are real. Bip39.scala:108-113 builds the salt as ("mnemonic" +
passphrase).getBytes("UTF-8") with no NFKD. Bip39.scala:201-202 normalizeWords uses the default-
locale toLowerCase, so a JVM in a tr locale lowercases "LIBRARY" to a dotless i and the word lookup
fails. Scala.js toLowerCase is locale-independent, so this is JVM-only. The impact statement is
wrong for the locale part. normalizeWords is also used by mnemonicToEntropy (Bip39.scala:47-48) and
isValidMnemonic (23-24). mnemonicToEntropy feeds Bip32Ed25519.masterKeyFromMnemonic (181-183), which
HdAccount/HdKeyPair.fromMnemonic use. Corrected claim: the NFKD gap affects only mnemonicToSeed
callers, but the locale bug also rejects upper-case mnemonics in the Cardano key path
(masterKeyFromMnemonic) on a Turkish-locale JVM. Fix both with Locale.ROOT, and add an upper-case
mnemonic test.

Action: Main-code note only: NFKD-normalize mnemonic and passphrase (java.text.Normalizer is not available
on Scala.js; it needs a platform shim or a documented ASCII-only restriction) and use
toLowerCase(Locale.ROOT). Test: add the trezor vectors with a non-ASCII passphrase (the Japanese
vector file) on JVM.

### F-38 `Address.fromBytes` and `Cip1852.parsePath` accept input the ledger or the spec rejects – open
Cluster `address-wallet-blueprint`, report finding C35-4, verdict CONFIRMED.

By reading: Cip1852.parsePath accepts invalid paths and disagrees with Bip32Ed25519.parsePath.
"m/1852'/1815'/0'/0/-1" returns Some((0, 0, -1)) and "m/1852'/1815'/-5'/0/0" returns account -5,
because role/index use "-1".toInt and parseHardenedIndex does not check the sign, while Cip1852.path
rejects negatives (146-148). parseHardenedIndex accepts ' and H but not h, which
Bip32Ed25519.parsePath accepts (229-230); a path without the m/ prefix is accepted (161), while
Bip32Ed25519 requires m/ or M/ (218-221) and Cip1852 rejects M/. Low impact: the result is only used
to describe a path.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/wallet/hd/Cip1852.scala:159-178`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/wallet/hd/Cip1852.scala:187-212`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/wallet/hd/Bip32Ed25519.scala:216-246`

Verification: Cip1852.scala:159-178: role and index use plain toInt, so "-1" and "+1" parse. parseHardenedIndex
(204-209) checks only the suffix (' or H) and then toInt, so "-5'" gives account -5. The m/ prefix
is optional (161), and "M/..." splits into 6 parts and returns None. Bip32Ed25519.parsePath
(216-246) requires m/ or M/, accepts h, and rejects negatives. Cip1852.path (145-149) rejects
negatives, so the two disagree. grep finds no use of Cip1852.parsePath or parseAccountPath in main
code, so the impact is low (public API only).

Action: Main-code note: reject negative numbers and accept the same suffixes and prefixes as
Bip32Ed25519.parsePath. Test: extend Cip1852Test 'parsePath - invalid paths' (71-80) with "-1"
role/index/account, a non-hardened purpose "m/1852/1815'/0'/0/0" (reaches the NonFatal catch at
175-177) and the h suffix.

### F-39 Bloxbean interop: `Data.Map` goes through a Scala `HashMap`, so entry order changes and the datum hash differs – open
Cluster `benchmarks-bloxbean-sbt`, report finding C15-5, verdict NARROWED.

By reading (not executed): Interop.toPlutusData(Data.Map(items)) builds the CCL map with
items...toMap.asJava. A Scala immutable Map with more than 4 entries is a HashMap, so the
MapPlutusData iterates in hash order, not in the Data.Map entry order; duplicate keys, which Plutus
maps allow, are silently dropped. A Data.Map with keys I(5), I(4), I(3), I(2), I(1) gives a
MapPlutusData whose getMap.keySet is not in that order; Data.Map([(I 1, I 0), (I 1, I 1)]) gives 1
entry. Any non-canonical datum, or a datum with a repeated key, converted to CCL gets a different
hash. The InteropMapOrderingTest round trip cannot see it: CCL's serializeToBytes encodes
canonically (CborSerializationUtil.serialize defaults to canonical = true, cardano-client-common
0.7.3), which re-sorts the keys, and both tests feed already sorted keys.

Evidence: `bloxbean-cardano-client-lib/src/main/scala/scalus/bloxbean/Interop.scala:190-201`, `bloxbean-cardano-client-lib/src/test/scala/scalus/bloxbean/InteropMapOrderingTest.scala:31-47`, `bloxbean-cardano-client-lib/src/test/scala/scalus/bloxbean/InteropMapOrderingTest.scala:70-91`

Verification: The defect is real. Interop.scala:190-201 passes `items...toMap.asJava` into the Lombok
`MapPlutusData.builder().map(...)`. That builder stores the given map as-is (MapPlutusData.java in
cardano-client-plutus 0.7.3: `@Builder.Default map = new LinkedHashMap<>()`, which the builder
overrides). A Scala Map with more than 4 entries is a HashMap, so `getMap` iterates in hash order.
Duplicate keys are dropped by `.toMap`. Any user can reach this through the public
`Interop.toPlutusData`; no main code calls it (grep finds only Interop.scala).  The hash consequence
is overstated. CCL hashes and serializes canonically by default: `PlutusData.getDatumHashAsBytes`
and `serializeToBytes` call `CborSerializationUtil.serialize(DataItem)`, which sets canonical=true
(CborSerializationUtil.java:124-126). `CustomMapEncoder.java:45,79` then re-sorts keys in a TreeMap
and also merges equal keys. `Transaction.serialize` is canonical too (Transaction.java:117). So the
order loss does not change CCL's datum hash. A non-canonical datum gets a different CCL hash even
after the fix. Duplicate keys are lost by any java.util.Map, and the canonical TreeMap would merge
them anyway.  Corrected claim: toPlutusData loses Data.Map entry order in `MapPlutusData.getMap` for
maps with more than 4 entries. Only code that iterates getMap, or that serializes with `serialize(x,
false)`, can see this. Fix it with a LinkedHashMap filled in entry order. Document that duplicate
keys cannot be kept, because a java.util.Map cannot hold them and CCL canonical CBOR merges them
anyway.

Action: Main-code note only: fill a java.util.LinkedHashMap in entry order instead of .toMap.asJava, and
document (or reject) duplicate keys, which a java.util.Map cannot hold. Test: see C15-6.

### F-40 DataLowering.lowerDataSelect (used by the Scott and SumOfProducts backends) maps field 'args' to Data.Constr a – open
Cluster `compiler.lowering.intrinsics`, report finding C10-1, verdict CONFIRMED.

DataLowering.lowerDataSelect (used by the Scott and SumOfProducts backends) maps field 'args' to
Data.Constr at line 287, but the Data.Constr branch only accepts 'constr' and 'dataArgs' (317, 322).
The real field name is 'args' (SIRType.scala:89, Data.scala:142), so selecting `.args` on a
Data.Constr throws 'Unknown field args for Data.Constr'; the 'dataArgs' arm is unreachable. Found by
reading; lines 313-328 are uncovered in the baseline.

Evidence: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/simple/DataLowering.scala:287,315-328`, `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRType.scala:89`, `scalus-core/shared/src/main/scala/scalus/uplc/builtin/Data.scala:142`

Verification: DataLowering.scala:287 maps field "constr" | "args" to SIRType.Data.Constr.name. The Constr branch
at :316-328 matches only "constr" (:317) and "dataArgs" (:322), and every other name throws 'Unknown
field ... for Data.Constr' (:325-327). The real field name is "args": SIRType.scala:89 has
TypeBinding("args", ...) and Data.scala:142 has `case class Constr(constr: BigInt, args:
PList[Data])`. No code outside DataLowering uses 'dataArgs', so that arm can never match. Reachable
path: lowerDataSelect is called from ScottEncodingLowering.scala:119 and
SumOfProductsLowering.scala:138 when isDataVariantType(scrutinee.tp) holds. So `c.args` on a
Data.Constr-typed value fails at lowering whenever the user picks the Scott or SumOfProducts
backend. The default SirToUplcV3Lowering backend does not take this path.

Action: Log in findings.md. Main-code note: rename the 'dataArgs' arm to 'args'. Test side: see C10-3 (a
Data select test on all four simple backends reproduces it).

### F-41 The only test in ZeroArgMethodLoweringTest is `ignore`d since 2026-03 and documents an open compiler defect – open
Cluster `compiler.lowering.intrinsics`, report finding C10-2, verdict CONFIRMED.

The only test in ZeroArgMethodLoweringTest is `ignore`d since 2026-03 and documents an open compiler
defect: calling a zero-arg method `foo()` in a @Compile object yields `foo((), ())` in SIR and
LoweringException 'Expected function type, but have: ByteString'. It is not in findings.md, and
nobody re-checks whether it is still present. Not re-run here (no sbt).

Evidence: `scalus-core/jvm/src/test/scala/scalus/compiler/sir/lowering/ZeroArgMethodLoweringTest.scala:13-44`

Verification: ZeroArgMethodLoweringTest.scala:36 is `ignore(...)`. The comment at :33-35 calls it a known bug:
`foo()` lowers as `foo((), ())`, which gives 'Expected function type, but have: ByteString'. The
file was added in 60319a822 (2026-03-07). Later commits only reformatted it or cleared warnings. No
later commit in git log fixes zero-arg methods; 46d10169e is older and covers a different case.
findings.md does not mention the test, and the grep for ZeroArg/zero-arg finds only reports and
timings.csv. The plugin zero-arg tests at CompilerPluginToSIRTest.scala:752-800 use local defs
inside compile{} and SortedMap methods, not a `def f(): T` called from a sibling def in a @Compile
object, so they neither show a fix nor cover this case. The action (re-enable it in one sbt run,
then log or keep it) is sound.

Action: Coordinator: change `ignore` to `test` in one sbt run. If it passes, keep it as a test (bug fixed).
If it fails, log the defect in findings.md and keep the ignore with the F-number in its comment.

### F-42 SIR.~=~ (SIRUnify.unifySIRExpr, Var case) maps names one way and not injectively – open
Cluster `compiler.plugin`, report finding C08-1, verdict CONFIRMED.

SIR.~=~ (SIRUnify.unifySIRExpr, Var case) maps names one way and not injectively: a left name not
yet in varNames is bound to whatever right name it meets, with no check that the right name is
already the image of another left name. So a compiled term that refers to a wrong or free variable
still unifies. Concrete pair: left LamAbs(l, Apply(headList, Var("m"))) vs right LamAbs(l,
Apply(headList, Var("l"))) returns true today (l->l, then m is unseen and is mapped to l). The ~=~
assertions in CompilerPluginToSIRTest/CompilerPluginBuiltinsToSIRTest (132 uses) cannot catch a
wrong variable reference. Same class of defect as F-33 (Term.~=~), but a different function.

Evidence: `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRUnify.scala:118-135`, `scalus-core/jvm/src/test/scala/scalus/compiler/CompilerPluginToSIRTest.scala:164-227`, `scalus-core/jvm/src/test/scala/scalus/compiler/CompilerPluginBuiltinsToSIRTest.scala:1139-1213`

Verification: SIRUnify.scala:118-125: on Var vs Var, if name1 is not in env.varNames it is bound to name2 with no
reverse check (no right->left map anywhere in Env, :10-21). varNames is threaded through LamAbs
(:166-172) and Apply (:190-193), so for LamAbs(l, Apply(f, Var m)) vs LamAbs(l, Apply(f, Var l)):
param binds l->l, then m is unseen and binds m->l, success (types equal). unifyBinding (:1063-1068)
has the same one-way pattern. SIR.~=~ (SIR.scala:186-190) calls unifySIR with Env.empty, so the
CompilerPluginToSIRTest/CompilerPluginBuiltinsToSIRTest asserts (e.g.
CompilerPluginToSIRTest.scala:224) use this check. Reachable in normal test use: a plugin bug that
emits a wrong or free variable name with matching type passes these asserts. Test-oracle defect, not
a runtime defect.

Action: Main-code note: keep a reverse map (right name -> left name) in Env and fail when a name is bound to
two different partners, as alpha-equivalence does. Test side: add SIRUnifyTest (shared) cases that
assert the pair above and Var("x") ~=~ Var("y") after LamAbs(x) vs LamAbs(x) return false; then run
scalusJVM/test to see which existing expected SIR was hiding a wrong name.

### F-43 SIR.~=~ on two Let nodes with different flags succeeds – open
Cluster `compiler.plugin`, report finding C08-2, verdict CONFIRMED.

SIR.~=~ on two Let nodes with different flags succeeds: SIRUnify.scala:159-164 only println's
'warning: let flags are different' and unifies with v1.flags & v2.flags. CompilerPluginToSIRTest
'compile def' expects LetFlags.Recursivity on the binding of recursive def c (line 216); a plugin
that stops marking recursion still passes this test, and the warning goes to stdout.

Evidence: `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRUnify.scala:147-170`, `scalus-core/jvm/src/test/scala/scalus/compiler/CompilerPluginToSIRTest.scala:164-227`

Verification: SIRUnify.scala:159-167: when v1.flags != v2.flags the Let case only println's a warning and returns
UnificationSuccess with flags v1.flags & v2.flags. No failure path exists.
CompilerPluginToSIRTest.scala:216 expects LetFlags.Recursivity on the Let for recursive def c, and
:224 asserts compiled ~=~ expected; a plugin that emits LetFlags.None there would still pass, with
only a stdout warning. Proposed SIRUnifyTest case (Recursivity vs None is false) is correct;
tightening may turn some existing expectations red.

Action: Main-code note: return UnificationFailure when v1.flags != v2.flags (or make the relaxation an
explicit Env option used only by callers that need it). Test side: add a SIRUnifyTest case: Let(...,
Recursivity) ~=~ Let(..., None) is false; then run scalusJVM/test and fix the expected SIR of any
test that turns red.

### F-44 Error-trace abbreviation misses errors under `Cast` and `Select` – open
Cluster `compiler.sir`, report finding C13-23, verdict CONFIRMED.

By reading (not executed): AbbreviateErrorTraces.transformAnnotatedSIR has no Cast or Select case
and falls into `case _ => sir` (147). An Error under a Cast (the plugin emits Cast around typed
expressions) or inside a Select scrutinee keeps its full message and is missing from the
abbreviation map. RemoveTraces handles both nodes (RemoveTraces.scala:98-102). Low impact today: no
main code calls AbbreviateErrorTraces.

Evidence: `scalus-core/shared/src/main/scala/scalus/compiler/sir/AbbreviateErrorTraces.scala:115-148`, `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIR.scala:398`, `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIR.scala:501`

Verification: AbbreviateErrorTraces.scala:115-148 matches Error, Let, LamAbs, Apply, And, Or, Not, IfThenElse,
Constr and Match. Every other node falls through to `case _ => sir` (147), including Select
(SIR.scala:398) and Cast (SIR.scala:501). So an Error inside a Cast term or a Select scrutinee keeps
its full message. RemoveTraces handles both nodes (RemoveTraces.scala:98-102). The plugin emits Cast
in pattern-match narrowing (PatternMatchingCompiler.scala:2132,2583; SIRCompiler.scala:1568). The
bug is not reachable from normal use today: the only reference outside docs is
AbbreviateErrorTracesSpec, and no main code or plugin calls the class. The finding states this
correctly.

Action: Add AbbreviateErrorTracesSpec cases: Cast(Error("Failed check"), Integer) and Select(Error(..),
field) are abbreviated; also cover the uncovered LamAbs, Apply, And/Or/Not, Constr, Match and
transformModule branches with one tree each. Main-code note: add Cast and Select cases, or delete
the class if it has no user.

### F-45 `SirDSL.lam` types each parameter with the body's type – open
Cluster `compiler.sir`, report finding C13-28, verdict CONFIRMED.

By reading (not executed): SirDSL.lam(names*)(term) (and its alias λ) types each parameter Var as
`s.tp`, the type of the body built so far, not the parameter's type: lam("x")(Const(1)) gives
LamAbs(Var("x", Integer), ...) for any x, and the outer params get the inner lambda's Fun type.
Every SIR lambda built with it is mistyped. No main or test code calls it today (the λ calls in
tests are the UPLC TermDSL).

Evidence: `scalus-core/shared/src/main/scala/scalus/compiler/sir/SirDSL.scala:28-37`

Verification: SirDSL.scala:28-37: lam folds right over names and builds SIR.Var(e, s.tp, ...), where s is the body
accumulated so far. So lam("x")(Const(1)) types x as Integer, the body type. With 2 names, the outer
param gets the inner LamAbs's Fun type. There are no callers: grep over the whole repo finds SirDSL
used by PrettyPrinter (only lamAbsToList/applyToList) and by 5 test files. None of those test files
calls lam( or λ(. Note: SirDSL is public in scalus.compiler.sir, a MIXED MiMa package, so deleting
lam/λ needs per-symbol MiMa filters, or a deprecation.

Action: Main-code note only: delete SirDSL.lam/λ, or take typed params (lam(params: SIR.Var*)(body)). Test
side: the shared SirTestDsl of C13-18 must not delegate to it.

### F-46 In all three validators, findSignatureOrFail(sigs) ignores its parameter – fixed
Cluster `examples.1`, report finding C25-5, verdict NARROWED.

In all three validators, findSignatureOrFail(sigs) ignores its parameter: it tests
signatories.isEmpty and signatories.head (the closure value) and recurses with signatories.tail, so
every call sees the same list. With signatories [other, deadbeef] the first call recurses into an
identical call forever: on-chain the script runs until the budget is exhausted, and the JVM call
PubKeyValidator.validator(...) throws StackOverflowError. 'Signature not found' is unreachable
unless the list is empty. Both tests use a single signatory, so the loop never runs. The bloxbean
test tree holds a copy of the same file with the same defect.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/PubKeyValidator.scala:17-23`, `scalus-examples/jvm/src/main/scala/scalus/examples/PubKeyValidator.scala:31-36`, `scalus-examples/jvm/src/main/scala/scalus/examples/PubKeyValidator.scala:46-51`, `bloxbean-cardano-client-lib/src/test/scala/scalus/examples/PubKeyValidator.scala:18-22,32-37,47-52`, `scalus-examples/jvm/src/test/scala/scalus/examples/PubKeyValidatorTest.scala:38`, `bloxbean-cardano-client-lib/src/test/scala/scalus/bloxbean/PlutusScriptEvaluatorTest.scala:61,116`

Verification: The defect is real in all three defs and in both copies. scalus-
examples/jvm/src/main/scala/scalus/examples/PubKeyValidator.scala:17-21, 31-36 and 46-51, and
bloxbean-cardano-client-lib/src/test/scala/scalus/examples/PubKeyValidator.scala:18-22, 32-37 and
47-52, test and recurse on the closure `signatories`, never on `sigs`. Each call passes
`signatories.tail`, but the next call reads `signatories` again. So with a non-empty list whose head
is not the key, the recursion never ends. On-chain the script runs until the budget is exhausted.
'Signature not found' is reachable only for an empty list. Current tests use a single matching
signatory, so they cannot catch it: PubKeyValidatorTest.scala:38 and bloxbean
PlutusScriptEvaluatorTest.scala:32 and :87 use V2/V3. One correction to the JVM symptom: in
`validator` (V1) the recursive call is in tail position in a local def, and scalac's tailcalls phase
turns it into a loop without @tailrec. So the JVM call hangs forever instead of throwing
StackOverflowError. In V2/V3 the call is inside the `trace("nope")(...)` argument, which is not a
tail position, so those throw StackOverflowError. Corrected action: use `sigs` in all six bodies.
Add the [other, deadbeef] success case and the [other] failure case. Re-pin `flatBytesLen == 120`
(PubKeyValidatorTest.scala:53), because the fix changes the V1 script size. Run the JVM-side
negative case with a timeout or use the on-chain evaluator, because the unfixed V1 JVM call hangs.

Action: Main code: use sigs in the body of all three findSignatureOrFail (sigs.isEmpty, sigs.head,
findSignatureOrFail(sigs.tail)), in both copies. Tests: add to PubKeyValidatorTest a case with
signatories [other, deadbeef] (must succeed) and one with [other] (must fail with 'Signature not
found'). Optionally drop the unused validator def from the bloxbean copy (only validatorV2/V3 are
used there).

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): PubKeyValidator walks the signatories list it is given".

### F-47 SnarkVerificationKey.nPublic is never read, and derive returns as soon as vk_ic is Nil, ignoring any remaining – fixed
Cluster `examples.1`, report finding C25-6, verdict CONFIRMED.

SnarkVerificationKey.nPublic is never read, and derive returns as soon as vk_ic is Nil, ignoring any
remaining public inputs. So grothVerify(factorialVK, <the valid 3! proof>, List(561, 3, 999))
returns true: derive([ic1, ic2], [561, 3, 999], ic0) -> derive([ic2], [3, 999], ...) -> derive([],
[999], ...) = result. A contract that reads public inputs from a redeemer can be given trailing
values that the proof does not bind. Standard Groth16 verifiers reject a public-input list whose
length is not nPublic. Parity with the ak-381 source could not be checked offline.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/Groth16.scala:49-57`, `scalus-examples/jvm/src/main/scala/scalus/examples/Groth16.scala:108-118`, `scalus-examples/jvm/src/main/scala/scalus/examples/Groth16.scala:138-158`, `scalus-examples/jvm/src/main/scala/scalus/examples/Groth16.scala:176-178`, `scalus-examples/jvm/src/test/scala/scalus/examples/Groth16Test.scala:57-69`

Verification: `nPublic` is declared at Groth16.scala:50, and no code reads it. The only references to it are the
test fixtures Groth16Test.scala:20 and :115. `grothVerify` drops it with the `_` pattern at
Groth16.scala:144. `derive` (Groth16.scala:108-118) returns `result` when `vk_ic` is Nil, whatever
`public` still holds, and it fails ('Invalid input') only when `public` runs out first. So for
factorialVK, which has 3 IC points (2 after the head), List(561, 3, 999) reduces to the same G1
point as List(561, 3), and the valid 3! proof (Groth16Test.scala:57-68) is accepted. Any contract
that passes redeemer-supplied public inputs to grothVerify and then reads input index >= nPublic
gets a value the proof does not bind. I could not check parity with ak-381 offline. Even if ak-381
behaves the same way, the length check is a missing guard in the Scalus example. The proposed tests
are valid additions.

Action: Main code: in grothVerify require public.length == vk.nPublic and vkIC.length == nPublic + 1 (or
make derive fail when vk_ic is Nil and public is not). Tests: add to Groth16Test a case with one
extra public input (must fail both on- and off-chain) and one with a short list ('Invalid input').

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): reject Groth16 public inputs the verification key does not bind".

### F-48 Betting: the mint branch does not check that the script output holds the minted token – fixed
Cluster `examples.2`, report finding C11-8, verdict CONFIRMED.

By reading (not executed): the minting branch (quantity +1) checks the config of the unique script
output but never checks that this output holds the minted token. A user can mint the bet token to
their own wallet next to a dummy script output, and later lock the token at the script address with
a forged Config (e.g. oracle == player1, which the mint forbids). Join accepts it: it only requires
value.tokens(scriptHash).nonEmpty (98-101). This contradicts the comment at 244-248 ('The token can
only ever sit in a bet UTxO at the script address'), so a joiner can be paired with a self-appointed
oracle.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/betting/BettingValidator.scala:243-277`, `scalus-examples/jvm/src/main/scala/scalus/examples/betting/BettingValidator.scala:244-248`, `scalus-examples/jvm/src/main/scala/scalus/examples/betting/BettingValidator.scala:98-101`, `scalus-examples/jvm/src/main/scala/scalus/examples/betting/BettingValidator.scala:103-106`

Verification: The mint branch (BettingValidator.scala:250-277) only finds the unique output at
Address.fromScriptHash(policyId) (253-257). It then checks that output's Config: player1 signs,
player2 empty, oracle !== player1, expiration. Nothing checks that this output's value holds
tokens(policyId). So a player1-signed tx can mint +1 of any token name to the player's own wallet
next to a token-less dummy script output with a valid Config. Later, a plain payment (no script runs
on outputs) locks the token at the script address with a forged inline Config, e.g. oracle ==
player1 and player2 empty. Join (82-138) checks only value.tokens(scriptHash).nonEmpty (98-101),
player2 empty, and a continuing-output config that keeps player1/oracle unchanged. It never checks
player1 !== oracle, so the forged bet is joinable. The comment at 244-248 ('The token can only ever
sit in a bet UTxO at the script address') is false. Anyone who builds their own tx (not via
BettingTransactions.init) can reach this. No existing test covers it: BettingValidatorTest has only
one mint test (44), and its token sits correctly. The proposed test and the main-code fix (require
scriptOutput.value.tokens(policyId) to hold the minted token with quantity 1) are correct.

Action: Add a test: mint +1 with the token paid to player1's address and a token-less script output carrying
a valid Config; expect the minting policy to fail. Main-code note only: require
scriptOutput.value.tokens(policyId) to contain the minted token (quantity 1).

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): require the betting mint to place the bet token in the bet output".

### F-49 LotteryValidator is open to double satisfaction in Lose and Timeout – fixed
Cluster `examples.5`, report finding C18-1, verdict CONFIRMED.

LotteryValidator is open to double satisfaction in Lose and Timeout. Each script run checks only
that some output pays the winner/revealer at least its own input's lovelace
(tx.outputs.at(winnerOutputIdx) at 193 and 246; paysAtLeast at 278-282). Nothing ties that output to
one lottery input or forbids a second lottery input. Example: Alice revealed first in two games (10
ADA pots each) and both opponents did not respond. After the deadline anyone (no signature is
checked in Timeout) spends both pots with Timeout redeemers (Alice's preimages are public), pays
Alice 10 ADA once and keeps the other 10 ADA. Lose has the same hole for a loser of two games
against the same winner (one payout, same winnerOutputIdx). The README promises the Timeout pot 'can
only go to the rightful claimant'; the June fix bc4b45020 closed only the single-input theft.
LotteryScenarioTest's games 0 and 1 already share player one (Alice), so the precondition is
realistic.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/lottery/LotteryValidator.scala:186-200`, `scalus-examples/jvm/src/main/scala/scalus/examples/lottery/LotteryValidator.scala:240-253`, `scalus-examples/jvm/src/main/scala/scalus/examples/lottery/LotteryValidator.scala:202-218`, `scalus-examples/jvm/src/main/scala/scalus/examples/lottery/LotteryValidator.scala:255-272`, `scalus-examples/jvm/src/main/scala/scalus/examples/lottery/LotteryValidator.scala:277-282`

Verification: The defect is real and reachable. Timeout (LotteryValidator.scala:202-218, 255-272) checks only the
preimage hash, isEntirelyAfter(deadline), and paysAtLeast(tx, revealerPkh, amount). paysAtLeast
(277-282) is tx.outputs.exists(credential === pkh && lovelace >= amount), and amount is this input's
lovelace only (line 76). There is no signer check, no count of own-script inputs and no sum over
lottery inputs, so one 10 ADA output to Alice satisfies both script runs. The preimage is public
because it appeared in the earlier RevealPlayerOne/Two redeemer. Lose (186-200, 240-253) reads
tx.outputs.at(winnerOutputIdx) from the redeemer. Two lottery inputs can name the same index, so a
player who lost two games to the same winner pays once. The precondition is realistic:
LotteryScenarioTest.scala:132-133 has games 0 and 1 both with Alice as player one. README.md:41-44
promises that the Timeout pot goes 'to the rightful claimant'. bc4b45020 added only the single-input
paysAtLeast pin. No test in scalus-examples/jvm/src/test/scala/scalus/examples/lottery covers multi-
input spends. The proposed negative test is valid, and the main-code fix belongs to the owner.

Action: Owner decision (main code). Test side: add a negative test in LotteryValidatorTest that creates two
games with the same revealer, reveals both, and submits one Timeout tx spending both pots with a
single payout to the revealer; it must fail. Main side fix options: require that exactly one input
sits at the own script address (as DaBondPoolValidator.scala:100 does with findUniqueOrFail), or
require the payout amount to cover the sum of all lottery inputs.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): spend one lottery input and keep the whole pot in a reveal".

### F-50 OptimizedPaymentSplitterValidator.reward counts OUTPUTS, not distinct payees – fixed
Cluster `examples.6`, report finding C31-1, verdict CONFIRMED.

OptimizedPaymentSplitterValidator.reward counts OUTPUTS, not distinct payees: actualNPayed is +1 per
payee output (lines 180, 187) and line 192 only requires payees.length === actualNPayed. With payees
A, B and 100 locked, a submitter A can pay A 50 (+change) and A 50 again: n = 2, every output is to
a payee, the redeemer (A, 100, 50, 2) passes all checks, and B gets nothing. Same with 3 payees and
a duplicated output to one payee. The naive validator aggregates per payee (AssocMap, lines 79-107)
and is not affected. By reading, not executed. No example has two outputs to one payee, and the
proofs fix the output shape to both(toKey(a,..), toKey(b,..)).

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/paymentsplitter/OptimizedPaymentSplitterValidator.scala:170-192`, `scalus-examples/jvm/src/test/scala/scalus/examples/paymentsplitter/PaymentSplitterTestCases.scala:163-182`, `scalus-examples/jvm/src/test/scala/scalus/examples/paymentsplitter/PaymentSplitterVerificationTest.scala:256-260`

Verification: The defect is real. In OptimizedPaymentSplitterValidator.scala:170-189 the output fold adds n+1 for
every PubKeyCredential output to a payee (lines 180 and 187), with no per-credential aggregation.
Line 191 only requires actualNPayed === nPayed and line 192 only requires payees.length ===
actualNPayed. sumPayeeOutputs is bound (line 170) but never used. Scenario: payees (A, B), 100
locked, A submits with its own input. Outputs are A 50+change and A 50. Both outputs go to the
change branch, so n=2 and changeSum is at least 50. The redeemer (A, 100, 50, 2) gives remainder 0
(lines 199-204). All checks pass and B gets nothing. The spend endpoint is reachable in normal use:
it only calls StakeValidator.spendMinimal (line 102), so the reward check is the only guard. The
3-payee variant also works: two exact-split outputs to B and none to C give n=3. The naive validator
in PaymentSplitterValidator.scala:79-109 sums per payee in an AssocMap, then counts distinct payees,
so the same transaction gives nPayed=1 and fails 'Not all payees were paid'. No row in
PaymentSplitterTestCases.scala:74-203 has two outputs to one payee.

Action: Add a test row to PaymentSplitterTestCases (or a direct test in
OptimizedPaymentSplitterValidatorTest): payees (A, B), contract 100, outputs (A gets 50+change, A
gets 50), honest redeemer; assert both validators reject. Main-code note: count distinct paid payees
(aggregate per credential as the naive one does, or require each non-change payee credential to
appear once).

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): count each payee once in the optimized payment splitter".

### F-51 OracleValidator.spend checks the non-zero rate and the denominator on the INPUT state (88-89), for every redee – fixed
Cluster `examples.6`, report finding C31-20, verdict CONFIRMED.

OracleValidator.spend checks the non-zero rate and the denominator on the INPUT state (88-89), for
every redeemer, and never on the continuation state of Update (107-119). An authorized Update to
rate 0 (or denominator 0) is accepted, and the new oracle UTxO can then never be spent again, not
even by Burn (which runs the same input check): the beacon is locked forever, and
PricebetValidator.Win compares against the unchecked rate (135). By reading, not executed. Only the
authorized signer can trigger it.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/pricebet/OracleValidator.scala:86-89`, `scalus-examples/jvm/src/main/scala/scalus/examples/pricebet/OracleValidator.scala:97-119`, `scalus-examples/jvm/src/main/scala/scalus/examples/pricebet/PricebetValidator.scala:121-137`

Verification: In OracleValidator.scala:87-89, spend runs checkDenominator and the !isZero check on the INPUT datum
(state), and does so for every redeemer. The Update branch (98-119) checks only the continuation
address and newState.timestamp. It never checks newState.exchangeRate. Mint (41-70) does not check
the datum either. PricebetTransactions.updateOracle passes newExchangeRate through with no check.
After an authorized Update to Rational(0,1), every later spend of that UTxO, Update or Burn, fails
at line 89 ('Zero rate is not allowed'). After an Update to denominator 0, it fails at line 88
(Prelude.scala:219-220). So the oracle UTxO and its beacon are locked forever.
PricebetValidator.scala:121-137 reads the oracle as a reference input, so line 88 does not run
there. Win then compares against the unchecked rate with cross-multiplication
(Prelude.scala:208-209). With 1/0 against 3/2: 1*2 > 3*0, so the player wins. Only
config.authorizedSigner can trigger this (lines 92-95). That is normal use, for example a mistake by
the oracle operator. The action in the finding is correct.

Action: Add a test: Update to Rational(0,1) is accepted today, then Update and Burn of the new UTxO fail.
Main-code note: move checkDenominator/isZero to newState in the Update branch (and drop it from
Burn).

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): keep the oracle beacon in a valid oracle UTxO from mint to burn".

### F-52 VaultValidator.initiateWithdrawal checks owner, recoveryKey, status=Pending and finalizationDeadline of the ne – fixed
Cluster `examples.8`, report finding C34-1, verdict CONFIRMED.

VaultValidator.initiateWithdrawal checks owner, recoveryKey, status=Pending and finalizationDeadline
of the new datum, but not newDatum.waitTime nor newDatum.amount. deposit (line 94) and cancel (line
177) compare waitTime with the incoming datum, so a waitTime written during initiation is permanent.
Threat model from the file's own comment (157-159): a stolen owner key. Attack: (1) attacker
initiates with output waitTime = 0 (this deadline still uses the old waitTime); (2) recovery
cancels, and cancel must keep waitTime = 0; (3) attacker initiates again, deadline = validTo + 0
(130-131); (4) finalize passes in the next slot, so the recovery key has no wait window. The
unpinned amount also lets the recovery signer take inputLovelace minus datum.amount in cancel
(171-175), and an amount that differs from the value makes finalize fail.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:104-138`, `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:94`, `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:156-178`, `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:130-131`

Verification: VaultValidator.scala:104-138: initiateWithdrawal reads newDatum and checks only status.isPending
(133) and finalizationDeadline (134-137). requireSameOwner (196-200) checks only owner and
recoveryKey. Nothing compares newDatum.waitTime or newDatum.amount with datum, and the value check
is only out lovelace >= input lovelace (121-124). requireEntireVaultIsSpent is not called here.
cancel (177) requires vaultDatum.waitTime === datum.waitTime and deposit (94) does the same, so
after a Pending datum carries waitTime = 0 no path can restore it. The builder cancel
(VaultTransactions.scala:101) copies currentDatum, so it keeps waitTime = 0. Attack path, with a
stolen owner key: initiate with waitTime 0 -> recovery cancels -> re-initiate, deadline = validTo +
0 (130-131) -> finalize (142) needs only validRange entirely after the deadline, so it succeeds in
the next slot. The amount part also holds: cancel pins out lovelace to the new datum.amount
(171-175), not to the input lovelace. finalize needs datum.amount === input lovelace (144, 182-186).
The fix and test actions are sound.

Action: Main code: in initiateWithdrawal require newDatum.waitTime === datum.waitTime and newDatum.amount
=== datum.amount (and out lovelace === datum.amount). Test: add two negative cases to the Vault
fill-gap suite (C34-4).

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): spend one vault input and keep the wait time in a withdrawal request". The request keeps the wait time and may only add lovelace, which the datum amount must record (the first version required the exact lovelace, which blocked a top-up for the larger Pending datum).

### F-53 deposit has no signature check and no one-vault-input guard – fixed
Cluster `examples.8`, report finding C34-2, verdict CONFIRMED.

deposit has no signature check and no one-vault-input guard. Each vault input checks only its own
datum against the unique output at the vault address (findContinuingOutputOrFail): out lovelace >
own input, datum.amount = own input lovelace, new amount = out lovelace, and
owner/recoveryKey/status/waitTime/finalizationDeadline equal to its own datum. Two vaults that one
owner locks with one waitTime have equal datums except amount (lock sets Idle and deadline 0,
VaultTransactions 31-38) and share the unparameterized script address. Anyone can spend both with
Deposit and pay one output of max(A, B) + 1 lovelace; both runs pass and the spender keeps the rest.
EXAMPLES_REVIEW.md:262 lists only the finalize variant as deferred; this deposit variant needs no
Pending state and no key.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:81-102`, `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultValidator.scala:188-191`, `scalus-examples/jvm/src/main/scala/scalus/examples/vault/VaultTransactions.scala:31-38`, `docs/internal/EXAMPLES_REVIEW.md:262`

Verification: deposit (VaultValidator.scala:81-102) has no signature check and no own-input uniqueness guard.
getVaultOutput uses findContinuingOutputOrFail (190-191). That function returns the unique output
whose whole address equals the own input address (Contexts.scala:1037-1041). Two vault inputs at the
same unparameterized script address resolve to the same single output. The per-input checks are: out
lovelace > own input (90); datum.amount === own input lovelace (91, 182-186); newDatum.amount ===
out lovelace (93); waitTime, deadline and status equal to own datum (94-101); owner and recoveryKey
equal (85, 196-200). lock (VaultTransactions.scala:31-38) creates Idle with deadline 0, so two locks
with the same owner, recovery and waitTime have datums that differ only in amount. One output of
max(A,B)+1 with that datum satisfies both runs. Any party can submit this, and it keeps about
min(A,B)-1 minus the fee. EXAMPLES_REVIEW.md:262 defers only the finalize variant.
VestingValidator.scala:79-82 shows the findUniqueOrFail guard to copy.

Action: Main code: add a single-own-input guard (txInfo.inputs.findUniqueOrFail on the own credential, as
VestingValidator 79-82 does) to deposit, and to finalize/cancel for the known deferred variant.
Test: two-vault deposit case in C34-4.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(examples): spend one vault input and keep the wait time in a withdrawal request".

### F-54 VestingTransactions.withdraw pays the continuing output Value.lovelace(contractAmount - amount), which drops n – open
Cluster `examples.8`, report finding C34-3, verdict CONFIRMED.

VestingTransactions.withdraw pays the continuing output Value.lovelace(contractAmount - amount),
which drops native tokens, while the validator requires contractOutput.value === ownInput.value -
lovelace(requested). lock accepts any Value, so a vesting UTxO with tokens cannot be partially
withdrawn through the builder. Low severity: it fails closed.

Evidence: `scalus-examples/jvm/src/main/scala/scalus/examples/vesting/VestingTransactions.scala:73-75`, `scalus-examples/jvm/src/main/scala/scalus/examples/vesting/VestingTransactions.scala:37-45`, `scalus-examples/jvm/src/main/scala/scalus/examples/vesting/VestingValidator.scala:126-132`

Verification: VestingTransactions.scala:73-75 pays Value.lovelace(contractAmount - amount) back to the script on a
partial withdrawal. lock (28-48) accepts any Value and pays it as-is (45), so tokens can be locked.
The validator requires contractOutput.value === ownInput.value - Value.lovelace(requestedAmount)
(VestingValidator.scala:126-129). With tokens in the input, the builder output lacks them, the
tokens go to change, and the script fails with ContinuingValueMismatch. It fails closed. A full
withdrawal (amount == contractAmount, line 74) emits no continuing output, so it is not affected.
The fix vestingUtxo.output.value - Value.lovelace(amount) matches the validator check.

Action: Main code: pay vestingUtxo.output.value - Value.lovelace(amount) back to the script. Test: a partial
emulator withdrawal of a UTxO locked with a token in VestingTransactionTest.

### F-55 JS `Utxo` constructor truncates a non-integer `outputIndex` – open
Cluster `js-facade`, report finding C28-8, verdict NARROWED.

By reading (not executed): `new Utxo(txHash, outputIndex, address, value)` converts outputIndex with
`outputIndex0.toInt`. In Scala.js, 1.5 becomes 1, NaN becomes 0 and 1e10 saturates to Int.MaxValue,
so the handle silently names a different output than the caller passed. Every other numeric argument
of the facade goes through intOf/safeInteger and throws TypeError (JEvaluatorTest:424-430 pins
exactly this for the protocol version: 'would otherwise saturate through .toInt'). Only a negative
index fails, through TransactionInput's require.

Evidence: `scalus-core/js/src/main/scala/scalus/cardano/ledger/JsUtxo.scala:83-84`, `scalus-core/js/src/main/scala/scalus/utils/scalajs/internal/helpers.scala:74-78`, `scalus-cardano-ledger/js/src/test/scala/scalus/uplc/eval/JEvaluatorTest.scala:424-430`

Verification: The defect is real and reachable. JsUtxo.scala:83 does
`TransactionInput(TransactionHash.fromHex(txHash0), outputIndex0.toInt)`, and the class is
@JSExportTopLevel("Utxo"). From JavaScript, `new Utxo(hash, 1.5, addr, value)` silently names output
1. NaN gives 0, and 2^31 saturates to Int.MaxValue. The other facade numbers go through
intOf/safeInteger (helpers.scala:74-85), which throw TypeError, as JEvaluatorTest.scala:424-430 pins
for the protocol version. The proposed test is wrong for -1. intOf(-1, ...) passes, because -1 is a
safe integer and a valid Int (helpers.scala:75-77). So -1 never becomes a TypeError naming
outputIndex. It fails later at TransactionInput's `require(index >= 0)` (TransactionInput.scala:22)
with an IllegalArgumentException. Corrected action: assert a TypeError naming outputIndex for 1.5,
NaN and 2^31. Assert an IllegalArgumentException for -1, or add a non-negative check in the
constructor if a TypeError is wanted there too. Main-code fix: use `intOf(outputIndex0,
"outputIndex")`.

Action: Add a JsUtxoTest case: for outputIndex 1.5, NaN, -1 and 2^31, the constructor throws (a TypeError
naming outputIndex). Main-code note only: use intOf(outputIndex0, "outputIndex") in the constructor.

### F-56 Stdlib `Ord[IntervalBound]` orders inclusive above exclusive at the same time, which is wrong for lower bounds – fixed
Cluster `ledger.api`, report finding C22-1, verdict CONFIRMED.

By reading (the file is @Compile, so excluded from coverage, F-9): Ord[IntervalBound] breaks a time
tie with Ord[Boolean] (true > false), so an inclusive bound sorts ABOVE an exclusive bound at the
same time. That is right for upper bounds and wrong for lower bounds, and min/max/isNever use the
one Ord for both. Counterexamples against the code's own doc comments: (a) intersection([5,10],
(5,10]) = Interval(max(incl 5, excl 5) = incl 5, ...) = [5,10], which contains 5 although (5,10]
does not ('largest interval contained in both'); (b) hull([5,10], (5,10]) = (5,10], which does not
contain 5 although [5,10] does ('smallest interval that contains both'); (c) isNever((5,5]): from
<=> to is Less (excl < incl), not an open interval, so it returns false, while contains(t) is false
for every t ('contains no values'). No test has two bounds at the same time with mixed inclusivity:
the hull/intersection examples use inclusive bounds at distinct times, and isNever has two examples
(Greater, never).

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Contexts.scala:90-91`, `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/Ord.scala:43-46`, `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Contexts.scala:114-128`, `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Contexts.scala:208-221`, `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Contexts.scala:281-298`, `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/IntervalTest.scala:161-252`

Verification: Traced by hand. Ord[IntervalBound] (Contexts.scala:90-91) compares time first and then isInclusive
with Ord[Boolean] (Ord.scala:43-46, true > false). So at equal time an inclusive bound is Greater
than an exclusive one. min/max (114-128) and hull/intersection (208-221) use this one Ord for lower
bounds too. (a) intersection([5,10],(5,10]): max(incl 5, excl 5) gives Greater, so the result is lhs
= incl 5. The result [5,10] contains 5, but (5,10] does not. (b) hull([5,10],(5,10]): min gives excl
5, so the result is (5,10] and 5 is lost. (c) isNever(Interval(excl 5, incl 5)): from <=> to is
Less. isOpenInterval is false, so the method returns false (281-296). But contains (231-249) needs
t>5 and t<=5, so no t is contained. The Equal branch is not reached. The defect is reachable through
the public API, e.g. Interval.intersection(validRange, Interval.entirelyBetween(...)) when the bound
times match. No test has mixed inclusivity at one time (IntervalTest.scala:161-252 uses only
between/before/after/never). One caveat for the proposed main fix: Equal-time infinite bounds also
give a wrong answer (see C22-2).

Action: Test side: add the contains-oracle property of C22-2 with the small-time generator of C22-3, plus
three pinned examples (a)-(c) asserting the documented results: intersection([5,10],(5,10]) ===
Interval(finiteExclusive(5), finiteInclusive(10)); hull([5,10],(5,10]) === between(5,10);
isNever(Interval(finiteExclusive(5), finiteInclusive(5))). They fail until the main code is fixed.
Main-code note only: give lower bounds the opposite tie-break (inclusive lower < exclusive lower),
e.g. separate minLower/maxLower helpers used by hull/intersection, and in isNever treat Equal-time
bounds as never unless both are inclusive.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(ledger-api): order lower bounds correctly in Interval hull, intersection and isNever".

### F-57 Timelock.blockfrostReadWriter cannot read its own output for TimeStart/TimeExpire – open
Cluster `ledger.domain`, report finding C05-1, verdict CONFIRMED.

Timelock.blockfrostReadWriter cannot read its own output for TimeStart/TimeExpire. The writer builds
ujson.Obj("slot" -> slot) with slot: Long (SlotNo = Long), and ujson 4.4.3 converts a bare Long
through JsonableLong to a Str (ujson/Value.scala:151; the same trap ProtocolParamsTest:20-28
documents for ProtocolParams). The reader does json("slot").num.toLong, which throws on a Str. So
write(TimeStart(10)) gives {"type":"after","slot":"10"} and read of that fails. Real Blockfrost
sends a number, so BlockfrostProvider (the only reader, BlockfrostProvider.scala:1250) works; the
public writer is wrong. Also .num.toLong loses precision above 2^53. By reading, not executed; the
reader branches all/any/atLeast/after/before (257-270) are uncovered in the baseline.

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/ledger/Timelock.scala:229-249`, `scalus-core/shared/src/main/scala/scalus/cardano/ledger/Timelock.scala:266-269`, `scalus-core/jvm/src/test/scala/scalus/cardano/ledger/ProtocolParamsTest.scala:20-28`

Verification: Timelock.scala:247-250: the writer builds ujson.Obj("type"->"after", "slot"->slot) with slot: SlotNo
= Long (Timelock.scala:12,37-38). "slot"->slot is a varargs item typed (String, Value), so the Long
goes through ujson JsonableLong. In ujson 4.4.3 that conversion returns Str(i.toString)
(ujson/Value.scala:151, read from the sources jar). The reader at Timelock.scala:266-269 calls
json("slot").num, and Value.num throws InvalidData on a non-Num (Value.scala:64-67). So
read(write(TimeStart(10))) throws. The only production reader is BlockfrostProvider.scala:1250,
which gets numeric slots from the API, so that path works. The broken path is the public given used
as a writer, for example upickle write(t)(using Timelock.blockfrostReadWriter). No test uses
blockfrostReadWriter. The precision loss from .num.toLong above 2^53 is also real.

Action: Add a property test in TimelockTest: for any generated Timelock, read(write(t))(using
Timelock.blockfrostReadWriter) == t, plus one example that reads a Blockfrost-shaped JSON with
numeric slots. Main-code note only: write the slot with ujson.Num(slot.toDouble) or read both Num
and Str (as the ProtocolParams reader does).

### F-58 StakeDistribution claims to implement the SNAP rule, but fromCertState passes only the pool delegations of the – open
Cluster `ledger.state`, report finding C41-1, verdict CONFIRMED.

StakeDistribution claims to implement the SNAP rule, but fromCertState passes only the pool
delegations of the accounts, so computeSnapshot never adds the account reward balances. cardano-
ledger's resolveActiveInstantStakeCredentials (libs/cardano-ledger-
core/src/Cardano/Ledger/State/Stake.hs:225-244) adds balanceAccountStateL to the UTxO stake of each
registered, pool-delegated credential, and counts a delegated account with no UTxO stake but a non-
zero balance. Scalus gives that account 0 stake and lowers activeStake and poolStakes by the sum of
all balances. Also, stakeCredentials keeps unregistered credentials, which the ledger drops
(Map.dropMissing). Impact today is low: no main code calls fromCertState or computeSnapshot
(EmulatorBase.scala:211 uses only aggregateUtxoStake).

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/StakeDistribution.scala:113-122`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/StakeDistribution.scala:30-48`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/ConwayAccountState.scala:17-22`

Verification: StakeDistribution.scala:113-122 builds only the delegation maps from certState.dstate.accounts.
ConwayAccountState.balance (ConwayAccountState.scala:17-22) is never read. computeSnapshot (30-48)
sums only the UTxO stake from aggregateUtxoStake (56-61). The ledger does it differently. cardano-
ledger libs/cardano-ledger-core/src/Cardano/Ledger/State/Stake.hs:225-244 uses
resolveActiveInstantStakeCredentials: Map.dropMissing drops unregistered UTxO credentials,
mapMaybeMissing counts a non-zero balance of a delegated account with no UTxO, and
zipWithMaybeAMatched adds stake <> balance. So Scalus under-counts poolStakes and activeStake by the
balances of registered, pool-delegated accounts. stakeCredentials keeps unregistered credentials.
poolStakes is not affected by those credentials, because delegations come only from registered
accounts. The only main-code use of StakeDistribution is aggregateUtxoStake at
EmulatorBase.scala:211. Nothing calls fromCertState or computeSnapshot, so the impact is low, as the
finding says. The proposed test expectation (10+5+3 = 18 ADA for poolStakes(P) and activeStake) is
correct, and it fails today with 10 ADA.

Action: Add a test: CertState with one account (balance 5 ADA, delegated to pool P) whose credential has 10
ADA of UTxO stake, and one account (balance 3 ADA, delegated to P) with no UTxO; assert
poolStakes(P) == 18 ADA and activeStake == 18 ADA. Main-code note only: add account balances of
pool-delegated registered accounts in fromCertState, as the ledger does.

### F-59 multiplyByDouble computes Coin((coin.value.toDouble * factor).toLong) – open
Cluster `ledger.state`, report finding C41-2, verdict CONFIRMED.

multiplyByDouble computes Coin((coin.value.toDouble * factor).toLong). Above 2^53 lovelace (mainnet
reserves are near that) and for factors like 0.003 that have no exact binary form, the result
differs from the ledger's floor of the exact rational: computeTotalRewards(Coin(10000000000000666),
Coin(0), rho = 0.003) gives a monetary expansion of 30000000000002 where floor(10000000000000666 * 3
/ 1000) = 30000000000001 (checked with IEEE doubles; about 0.7 % of reserves values in 1e15..4.5e16
differ). RewardsConfig takes Double by API, so the fix is a rational config. No main code calls
RewardsCalculation; only RewardsCalculationTest does. Second defect in the same object:
splitPoolRewards does not cap the leader share. When poolReward < cost (e.g. reward 300 ADA, cost
340 ADA), leaderReward = cost + 0 = 340 ADA, more than the pool earned, so leader + members exceeds
totalRewards and lovelace is created; the ledger's leaderRew gives the whole pool reward to the
leader when it is at most the cost.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/RewardsCalculation.scala:157-158`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/RewardsCalculation.scala:47-61`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/RewardsCalculation.scala:120-128`

Verification: RewardsCalculation.scala:157-158 computes Coin((coin.value.toDouble * factor).toLong). I checked
with IEEE doubles: 10000000000000666 is exactly representable, and int(10000000000000666.0*0.003) =
30000000000002, while 10000000000000666*3//1000 = 30000000000001. computeTotalRewards (47-61) uses
this helper for rho and tau. Second defect: splitPoolRewards (120-128) sets remainingAfterCost = 0
when poolReward <= cost, then leaderReward = cost + 0 = cost. With reward 300 ADA and cost 340 ADA,
the leader gets 340 ADA, which is more than the pool earned. Haskell
eras/shelley/impl/src/Cardano/Ledger/Shelley/Rewards.hs:122-123 calcStakePoolOperatorReward has `f
<= cost = f`. memberRewardsPool (131-132) becomes 0, so the members get 0 each and the total is 340
> 300. A repo-wide grep finds no main-code caller of RewardsCalculation outside its own file. Both
proposed tests fail today, as the finding says.

Action: Add tests that fail today: reserves 10000000000000666 with rho 3/1000 gives the exact floor
30000000000001; splitPoolRewards(300 ADA, cost 340 ADA, ...) gives leaderReward == 300 ADA and empty
or zero member rewards. Main-code note only: take rho, tau and margin as rationals and compute
floor(BigInt * num / den); cap the leader reward at poolReward.

### F-60 SubmitError.fromException documents (598-604) that a missing collateral input or reference input is 'not that  – open
Cluster `node.emulator`, report finding C17-1, verdict CONFIRMED.

SubmitError.fromException documents (598-604) that a missing collateral input or reference input is
'not that condition' and keeps its own rule name (BadCollateralInputsUTxO / BadReferenceInputsUTxO),
and SubmitErrorTest:168-185 and 341-353 pin that on hand-built exceptions. But DefaultValidators is
a SortedSet ordered by name, so AllInputsMustBeInUtxoValidator always runs first, and
Validator.validate stops at the first Left. That validator checks spend, collateral and reference
inputs together and throws BadAllInputsUTxOException, which fromException maps to UtxoNotAvailable
(the 'race a caller retries on') with the collateral/reference inputs in unavailableInputs;
SubmitErrorTest:146-160 pins exactly that for a collateral-only miss. So with the default rules the
documented own-name branches are unreachable from the Emulator, and the FeesOkValidator 'order
independent' fallback (139-142) never fires. The observable behavior matches the Haskell ledger
(Babbage BadInputsUTxO covers all inputs); the doc, the two tests and the 'not a retry' promise do
not. Found by reading, not executed.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockchainProvider.scala:598-604,616-622`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/rules/AllInputsMustBeInUtxoValidator.scala:15-28`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/rules/DefaultValidators.scala:10-11`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/rules/FeesOkValidator.scala:139-142`, `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/node/SubmitErrorTest.scala:146-160,168-185,341-353`

Verification: The finding is real. DefaultValidators.all is a SortedSet ordered by name
(DefaultValidators.scala:10,43). Validator.name is the class simple name (Entities.scala:86), so
AllInputsMustBeInUtxoValidator sorts first. Validator.validate folds with flatMap and stops at the
first Left (Entities.scala:136-137). Validators run before mutators (Entities.scala:220). Both JVM
and JS Emulator default to DefaultValidators.all (jvm Emulator.scala:69, js Emulator.scala:57).
AllInputsMustBeInUtxoValidator checks spend, collateral and reference inputs together and returns
BadAllInputsUTxOException (AllInputsMustBeInUtxoValidator.scala:15-26). fromException maps that
exception to UtxoNotAvailable, with the collateral and reference inputs added to unavailableInputs
(BlockchainProvider.scala:617-620). So under the default rules nothing can raise
BadCollateralInputsUTxOException or BadReferenceInputsUTxOException. That includes
FeesOkValidator.extractCollateralUtxos (FeesOkValidator.scala:139-142) and the phase-2 collateral
path in PlutusScriptsTransactionMutator (PlutusScriptsTransactionMutator.scala:84-91). The doc at
BlockchainProvider.scala:597-604 is therefore wrong for the default emulator. So is the comment at
582-584, which says a provider naming BadCollateralInputsUTxO reports 'the same rule the emulator
does'. SubmitErrorTest:146-160 pins the actual behavior; 168-185 and 341-353 pin the documented
behavior on hand-built exceptions. One reach note: with validators = Set.empty, a missing input
makes AllResolvedScripts throw inside the mutator, and NonFatal wraps that as
IllegalArgumentException (PlutusScriptsTransactionMutator.scala:105-110). The own-name rules stay
unreachable in that config too. The proposed EmulatorParityTest cases need hand-built transactions.

Action: Add an EmulatorParityTest case: submit through the default rules (1) a tx whose only missing input
is a collateral input and (2) one whose only missing input is a reference input; assert the rule
each gets and the unavailableInputs set. Main-code note only: the owner decides whether the doc at
BlockchainProvider.scala:598-604 changes to say all missing inputs are UtxoNotAvailable through the
default rules, or fromException splits BadAllInputsUTxOException by which set is non-empty; then
align SubmitErrorTest:146-185 with the decision.

### F-61 buildQuery translates a strict comparison into a non-strict filter – open
Cluster `node.query-stream`, report finding C37-1, verdict CONFIRMED.

buildQuery translates a strict comparison into a non-strict filter: `u.output.value.coin > amount`
(and `amount < u.output.value.coin`) match the guards at UtxoQueryMacros.scala:144 and :152 and
become UtxoFilter.MinLovelace(amount), which evalFilter evaluates as `coin >= amount`
(UtxoQuery.scala:237). Coin has a real `>` (Types.scala:53), so the lambda compiles and the query
silently also returns UTxOs holding exactly `amount`. No test uses `>` or `<`.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/UtxoQueryMacros.scala:137-154`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/UtxoQuery.scala:84-85,236-237`, `scalus-core/shared/src/main/scala/scalus/cardano/ledger/Types.scala:53`

Verification: UtxoQueryMacros.scala:137-146 guard is `(op == ">=" || op == ">")` and :148-154 is `(op == "<=" ||
op == "<")`; both emit `UtxoFilter.MinLovelace($amountExpr)` unchanged. UtxoQuery.scala:85 documents
MinLovelace as 'at least', and evalFilter at :236-237 is `output.value.coin >= amount`. Coin is
`final case class Coin(value: Long)` with real `infix def >` and `<` (Types.scala:37,53,55), so
`u.output.value.coin > Coin.ada(5)` type-checks. BlockchainProvider.scala:113-114 `queryUtxos`
passes the lambda straight to buildQuery, and the original predicate is not re-applied. Normal use
therefore reaches the bug: a UTxO holding exactly `amount` is wrongly returned. grep finds no test
or main code that uses the strict `>`/`<` forms. The proposed tests are sound and add coverage only.

Action: Add to UtxoQueryMacrosSpec (or the DSL examples): an Emulator with UTxOs of exactly 5 ADA and 6 ADA
at one address; `queryUtxos(u => u.output.address == a && u.output.value.coin > Coin.ada(5))` must
return only the 6 ADA UTxO; same for the reversed `Coin.ada(5) < u.output.value.coin`. Main-code
note only: translate `>`/`<` to MinLovelace(Coin(amount.value + 1)), or reject them at compile time.

### F-62 Latent (no provider emits BlockCoverage.Sources today – open
Cluster `node.query-stream`, report finding C37-2, verdict CONFIRMED.

Latent (no provider emits BlockCoverage.Sources today; BlockfrostChainFollower is used only by its
test): SubscriptionSupport classifies TransactionQuery.MintsPolicy as Indexed when the provider
pushes down Asset (line 188), but SubscriptionHub.coversTxQuery treats MintsPolicy as never covered
under Sources coverage (lines 555-557). On a metered provider such a subscription is accepted and
then never delivered anything, not 'some latency' as the comment at 529-535 says: every block it
would see carries Sources coverage, so its watermark never advances and it gets neither events nor
Idle.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/stream/SubscriptionSupport.scala:188`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/stream/internal/SubscriptionHub.scala:529-558`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/stream/internal/BlockfrostChainFollower.scala:247-253`

Verification: SubscriptionSupport.scala:188 maps `MintsPolicy` to `pushdown.contains(PushdownKind.Asset)`, so it
is Indexed under Metered caps with Asset pushdown. SubscriptionHub.scala:555-557 coversTxQuery
returns false for MintsPolicy under `BlockCoverage.Sources`. releaseLocked
(SubscriptionHub.scala:411-427,449) keeps only blocks where `covers` is true, so for Sources-only
blocks `due` stays empty. The watermark never advances, and the subscriber gets no Included or Idle
event. This contradicts the 'costs some latency' comment at :529-535. The defect is latent. The only
Sources emitter is BlockfrostChainFollower.scala:252, and grep finds no main-code reference to it
outside its own file. It also probes only FromAddress (:230). No main code declares
PushdownKind.Asset or ScanSupport.Metered outside SubscriptionSupport. Record the bug in findings.md
as the report proposes.

Action: Record in findings.md. Test once a Sources-coverage provider is wired: a hub with Metered caps and
pushdown Asset, a MintsPolicy transaction subscription, a block with Sources coverage that minted
under the policy; assert either SubscriptionSupport.of returns Unindexed/Unsupported for it or the
subscriber receives the Included event. Main-code note only: make indexedTxQuery and coversTxQuery
agree on MintsPolicy.

### F-63 Payment splitters (example and pattern) count outputs, not distinct payees: one payee can take another's share – fixed
Cluster `patterns`, report finding C32-1, verdict CONFIRMED.

By reading (not executed): the reward endpoint adds 1 to actualNPayed per OUTPUT, and checks only
that each output goes to some payee (payees.contains) and that each non-change output equals
splitPerPayee. 'Not all payees were paid' compares payees.length with the output count, not with the
number of distinct payees. With payees [A, B, C], A the change payee, contract input 30, fee payer A
input 10, fee 2: outputs A->18, B->10, B->10 give actualNPayed = 3 = nPayed = payees.length,
remainder 0, change 18 >= 10, so the tx validates and C gets nothing (two outputs to A work the same
way). The naive PaymentSplitterValidator groups outputs per payee in an AssocMap first, so it is not
affected; the OptimizedPaymentSplitterValidator in scalus-examples has the same per-output count
(other cluster).

Evidence: `scalus-design-patterns/src/main/scala/scalus/examples/StakeValidatorPaymentSplitterExample.scala:137-157`, `scalus-design-patterns/src/test/scala/scalus/patterns/StakeValidatorPaymentSplitterTest.scala:105-116`, `scalus-examples/jvm/src/main/scala/scalus/examples/paymentsplitter/PaymentSplitterValidator.scala:80-109`, `scalus-examples/jvm/src/main/scala/scalus/examples/paymentsplitter/OptimizedPaymentSplitterValidator.scala:170-192`

Verification: Traced StakeValidatorPaymentSplitterExample.scala:137-157. The output fold adds n+1 per output (146,
153). It checks only payees.contains (144) and value === splitPerPayee for non-change outputs
(149-152). Line 157 compares payees.length with the output count, not with distinct payees. Example:
payees [A,B,C], change payee A, contract input 30, fee payer A input 10, fee 2, outputs A->18,
B->10, B->10, redeemer nPayed=3, split=10, sum=30. Input fold: A is the fee payer (121-123); 30
comes from the script. Outputs: n=3 and changeSum=18. 156 and 157 pass (3 == 3). remainder 30-30=0
is in [0,3) (166). 18 >= 10 (170). So the tx validates and C gets nothing. Ledger value conservation
holds (40 = 38 + 2). sumPayeeOutputs is never used. The bug is reachable by any fee payer who builds
the payout tx. OptimizedPaymentSplitterValidator.scala:170-192 has the same per-output count. Test
note: the TestCase helper defaults nPayed to the distinct output count
(StakeValidatorPaymentSplitterTest.scala:201), so the new test must set overrideNPayed = Some(3L).
That test fails, as intended, until the main code is fixed.

Action: Add a failure test to StakeValidatorPaymentSplitterTest: payees (A, B, C), outputs A->18, B->10,
B->10, nPayed 3; expect failure. Main-code note only: count distinct payee credentials (fold outputs
into an AssocMap per payee as PaymentSplitterValidator does) in both
StakeValidatorPaymentSplitterExample and OptimizedPaymentSplitterValidator.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(design-patterns): count each payee once and reject native tokens in the stake splitter".

### F-64 Commit 3c5cba3c8 'fix(examples) – fixed
Cluster `patterns`, report finding C32-2, verdict CONFIRMED.

Commit 3c5cba3c8 'fix(examples): paymentsplitter reject native-token contract inputs' added
require(input.resolved.value.withoutLovelace.isZero, "Contract input must contain only ADA") to both
scalus-examples splitters, because outputs reconcile lovelace only and the fee payer could pocket
native tokens held by a contract UTxO. The design-patterns copy,
StakeValidatorPaymentSplitterValidator, still sums only getLovelace of contract inputs and has no
such check.

Evidence: `scalus-design-patterns/src/main/scala/scalus/examples/StakeValidatorPaymentSplitterExample.scala:125-126`, `scalus-examples/jvm/src/main/scala/scalus/examples/paymentsplitter/OptimizedPaymentSplitterValidator.scala:152-158`, `scalus-examples/jvm/src/main/scala/scalus/examples/paymentsplitter/PaymentSplitterValidator.scala:64-70`

Verification: StakeValidatorPaymentSplitterExample.scala:125-126 adds only input.resolved.value.getLovelace for
contract inputs. grep finds no 'withoutLovelace' and no 'only ADA' anywhere in scalus-design-
patterns/src/main. Both scalus-examples splitters have the require:
PaymentSplitterValidator.scala:68-69 and OptimizedPaymentSplitterValidator.scala:156-157, added by
commit 3c5cba3c8. The bug is reachable. Outputs must go to payees (143-144), and only lovelace is
reconciled. So a native token held by a contract UTxO can go to the change payee, who is the fee
payer, in any output. Test note: the TestCase helper builds only Value.lovelace inputs
(StakeValidatorPaymentSplitterTest.scala:219). The new test needs the helper extended with a token-
bearing contract input. The expected message exists only after the main code gets the require.

Action: Add a failure test: one contract input with 30 lovelace plus a native token, otherwise the 'two
payees split equally' case; expect failure 'Contract input must contain only ADA'. Main-code note
only: port the require from OptimizedPaymentSplitterValidator.scala:153-158.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(design-patterns): count each payee once and reject native tokens in the stake splitter".

### F-65 Math.log(self, base) guards only base <= 0 – fixed
Cluster `prelude`, report finding C04-1, verdict CONFIRMED.

Math.log(self, base) guards only base <= 0. For base = 1 and self >= 1, go(ret, n) never reaches n <
1 because n / 1 == n, so the recursion never ends: on the JVM it loops forever (tailrec), on-chain
it runs until the budget is exhausted. The scaladoc promises 'greatest k such that base^k <= self',
which does not exist for base 1. MathTest tests bases 0, 2, 3, 4 and 42, never 1.

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/Math.scala:301-306`, `scalus-core/shared/src/test/scala/scalus/cardano/onchain/plutus/prelude/MathTest.scala:344-386`

Verification: Math.scala:301-306: `if base <= 0 then 0 else go(0, self)` with `go(ret, n) = if n < base then ret
else go(1 + ret, n / base)`. For base = 1 and self >= 1, `n < 1` is always false and `n / 1 == n`,
so the recursion never ends. It is @tailrec, so the JVM loops forever; on-chain it runs until the
budget is exhausted. self <= 0 with base 1 returns 0, so only self >= 1 hangs. Any caller of the
public `Math.log(x, base = 1)` reaches it. MathTest.scala:344-386 tests bases 2, 3, 0, 4 and 42,
never 1. The advice to add the test only after the fix is correct, because EvalTestKit runs the JVM
code first and would hang.

Action: Main-code note: decide the contract for base 1 (return 0 like base <= 0, or fail with a message) and
guard it before go. Test: only after the fix, add log(BigInt(5), base = BigInt(1)) to the 'log'
test. Do NOT add it before the fix with assertEval*/assertEvalFails: every EvalTestKit assertion
runs the JVM code first and would hang CI. If a pre-fix test is wanted, compile the term and
evaluate only the UPLC with a small explicit budget, expecting budget exhaustion.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(prelude): fail Math.log for base 1 instead of looping forever". Base 1 fails with a message; Aiken's stdlib `log` (v4.0.0) has the same loop and fails on budget, and Plinth has no integer log.

### F-66 `List.show` prints a trailing `, ` – open
Cluster `prelude.list`, report finding C23-1, verdict CONFIRMED.

By reading (not executed): showList appends ", " after every element, including the last, so List(1,
2).show is "[1, 2, ]" and List(1).show is "[1, ]". Show[Data] for a Data list (Show.scala:45-55)
puts the separator only between elements and ShowTest pins "[0, \"0011\"]". No test in the
repository calls Show[List[T]].

Evidence: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/List.scala:309-316`, `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/Show.scala:45-55`, `scalus-core/jvm/src/test/scala/scalus/cardano/onchain/plutus/prelude/ShowTest.scala:43-46`

Verification: List.scala:309-316: `go` adds `head.show` and then ", " for every Cons, with no special case for the
last element. The final result is "[" + go + "]", so List(1,2).show gives "[1, 2, ]" and
List(1).show gives "[1, ]". Nil gives "[]". Show[Data] in Show.scala:44-55 (showBuiltinList) puts ",
" only between elements, and ShowTest.scala:43-46 pins "[0, \"0011\"]". The bug is reachable:
`showList` is a public given in the List companion, so any `xs.show` on a List[T: Show] uses it, on-
chain (log/trace messages) and off-chain (the JVM evaluates the same Scala code). A repo-wide grep
finds no other definition of Show[List] and no test that calls it. Adding the three assertEvalEq
cases is pure new coverage. They fail on the current main code until the separator is fixed.

Action: Add a ListTest case: assertEvalEq(List.empty[BigInt].show, "[]"),
assertEvalEq(List.singleton(BigInt(1)).show, "[1]"), assertEvalEq(Cons(BigInt(1), Cons(BigInt(2),
Nil)).show, "[1, 2]"). Main-code note only: emit the separator before every element except the
first, as Show[Data] does.

### F-67 Blockfrost `FromTransaction`/`FromInputs` returns spent outputs – open
Cluster `testing.integration`, report finding C38-1, verdict CONFIRMED.

By reading (not executed): BlockfrostProvider answers UtxoSource.FromTransaction and FromInputs from
GET /txs/{hash}/utxos, which lists every output of the transaction, spent or not;
fetchUtxosFromTransaction maps all of them (986-990) and never checks whether an output was
consumed. The Emulator answers the same sources from its live UTxO set (EmulatorBase 688-691). So on
Blockfrost/Yaci, findUtxo(input) and findUtxos(inputs) return an already spent output as found, and
a TxBuilder that resolves inputs this way builds a transaction the node rejects with BadInputsUTxO.
No test spends an output and queries it again: UtxoQueryIntegrationTest:118-159 and
BlockfrostEndpointsTest:280-292 check only that results come from the queried tx. The shared
interface states the intended meaning: BlockchainProvider.checkTransaction's doc (249-252) says a
transaction whose outputs are all spent reads as NotFound 'since it produces no current UTxOs'.
Caveat: Yaci Store is only a Blockfrost-compatible proxy; if its /txs/{hash}/utxos filters spent
outputs, the Yaci test passes while real Blockfrost still misbehaves.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockfrostProvider.scala:972-1001`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockfrostProvider.scala:1017-1028`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/EmulatorBase.scala:688-691`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockchainProvider.scala:34-40`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockchainProvider.scala:249-252`

Verification: BlockfrostProvider.scala:991-1001 maps every element of json("outputs") from GET /txs/{h}/utxos to a
UTxO. It indexes them with zipWithIndex and never reads a consumed_by_tx (or collateral) field: `git
grep consumed_by` finds no match anywhere in the repo. FromTransaction (730-731) returns that map
directly. FromInputs (719-728) goes through fetchUtxoFromInput (1036-1043), which looks the input up
in the same map. So a spent output reads as found. EmulatorBase.scala:688-691 filters the live UTxO
set instead. The behavior is reachable from the public API:
BlockchainProvider.findUtxo/findUtxos(inputs) (BlockchainProvider.scala:34-48) build FromInputs, and
TxBuilder resolves deferred queries through reader.findUtxos (TxBuilder.scala:2090).
UtxoQueryIntegrationTest:118-159 checks only nonEmpty and the txId. No test spends an output and
then queries it. The Yaci caveat stands, so the proposed BackendStub unit test is the deterministic
check. I could not confirm, without the network, that real Blockfrost sends spent outputs in this
list, but the claim matches Blockfrost's documented consumed_by_tx field.

Action: Add a test to UtxoQueryIntegrationTest: submit tx1 (alice pays 10 ADA to bob), then tx2 that spends
output (tx1, 0); assert that FromTransaction(tx1) does not contain (tx1, 0), and that
ctx.provider.findUtxo(TransactionInput(tx1.id, 0)) is Left(NotFound). For a deterministic check that
does not depend on Yaci, also add a shared unit test with an sttp4 BackendStub (as in C38-2): the
stubbed /txs/{h}/utxos returns two outputs, one with consumed_by_tx set; assert FromTransaction
returns only the unspent one. Main-code note only: if it fails, drop outputs whose consumed_by_tx is
set (or intersect with the address UTxOs).

### F-68 Blockfrost `FromAsset` silently returns partial results when one holder request fails – open
Cluster `testing.integration`, report finding C38-2, verdict CONFIRMED.

By reading (not executed): UtxoSource.FromAsset fetches /addresses/{holder}/utxos/{asset} once per
holder and returns Left only when every holder fails (`combined.isEmpty && errors.nonEmpty`, 712).
If one holder request fails (429 rate limit, timeout: fetchUtxosFromBech32Address maps any exception
to NetworkError) and another succeeds, the query returns Right with the failed holder's UTxOs
silently missing. This is the same defect BlockfrostOrCombineTest documents as fixed for Or: an
incomplete answer reported as success. Lines 699-718 are uncovered in the baseline; the only test,
BlockfrostEndpointsTest:598-622, needs a Blockfrost key and never runs on CI.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockfrostProvider.scala:699-718`, `scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/node/BlockfrostOrCombineTest.scala:8-15`

Verification: BlockfrostProvider.scala:700-718: each holder is fetched with fetchUtxosFromBech32Address (952-968).
That call goes through fetchAllPages to fetchPage (109-128), which throws on any non-2xx, non-404
status such as 429. The throw is recovered into Left(NetworkError) at 965-967. Line 712 then returns
Left only when `combined.isEmpty && errors.nonEmpty`. So one failed holder plus one successful
holder gives a Right that silently lacks the failed holder's UTxOs. BlockfrostProvider takes
`backend: Backend[Future]` as a using parameter (35-43), so the stub test is feasible. No
BackendStub is used in the repo yet (`git grep BackendStub` is empty), so the test will be the first
one. Note: a 404 on a holder still maps to an empty result. That is correct, and the test must not
use 404 as the failure.

Action: Add a shared unit test with an sttp4 BackendStub: /assets/{a}/addresses returns two holders, the
first holder's utxos request returns HTTP 429 and the second returns one UTxO; assert
findUtxos(UtxoQuery(FromAsset(p, n))) is Left(NetworkError). Main-code note only: make it fail when
any holder fails, as combineOr does.

### F-69 Emulator and testkit: wrong submit-error rule name, `checkTransaction` always `Confirmed`, `getDatum`… – open
Cluster `testing`, report finding C12-1, verdict CONFIRMED.

By reading (not executed): Scenario.provider.checkTransaction maps
`s.emulator.findUtxos(UtxoQuery(FromTransaction(txHash)))` with `case Right(_) => Confirmed`, but
ImmutableEmulator.findUtxos always returns Right (line 118: `Right(EmulatorBase.evalQuery(...))`).
So every hash, including one never submitted, is reported Confirmed; pollForConfirmation (371-376)
inherits it. The mutable Emulator answers with hasTx (EmulatorBase:317-320), ImmutableEmulator has
hasTx too (line 79), and the streaming conformance suite pins exactly this contract
(StreamProviderConformance:240). Lines 357-376 are uncovered in the baseline.

Evidence: `scalus-testkit/shared/src/main/scala/scalus/testing/Scenario.scala:357-369`, `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala:117-118`, `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala:79`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/EmulatorBase.scala:317-320`, `scalus-testkit/jvm/src/main/scala/scalus/testing/stream/StreamProviderConformance.scala:240`

Verification: Scenario.scala:356-369: checkTransaction maps
s.emulator.findUtxos(UtxoQuery(FromTransaction(txHash))) with Right(_) => Confirmed.
ImmutableEmulator.scala:117-118 always returns Right(EmulatorBase.evalQuery(...)), and evalQuery
FromTransaction (EmulatorBase.scala:690-691) only filters, so it yields Right(empty) for an unknown
hash. Result: every hash is Confirmed. pollForConfirmation (371-376) delegates to it.
ImmutableEmulator.hasTx exists (line 79), and the mutable emulator overrides checkTransaction with
hasTx (EmulatorBase.scala:317-320); its doc comment names this exact pitfall. Reachable through the
public Scenario.provider (line 326), e.g.
provider.flatMap(_.checkTransaction(TransactionHash.fromHex("ab"*32))) returns Confirmed.

Action: Add a ScenarioTest case: through Scenario.provider, checkTransaction(TransactionHash.fromHex("ab" *
32)) must be NotFound; after Scenario.submit(tx) it must be Confirmed for tx.id, also when every
output of tx has been spent. Main-code note only: use `if s.emulator.hasTx(txHash) then Confirmed
else NotFound`.

### F-70 Scenario.provider.getDatum always returns pure(None), while the ImmutableEmulator it wraps retains datums – open
Cluster `testing`, report finding C12-2, verdict CONFIRMED.

Scenario.provider.getDatum always returns pure(None), while the ImmutableEmulator it wraps retains
datums: ImmutableEmulator.getDatum (90) and asReader.getDatum (135-136) resolve them, and
ImmutableEmulatorTest:179-182 asserts the reader resolves a datum ('was always None before'). So
Scenario.snapshotReader and Scenario.provider disagree on the same state: a datum-hash lookup from
inside a Scenario step via the provider never succeeds.

Evidence: `scalus-testkit/shared/src/main/scala/scalus/testing/Scenario.scala:352-355`, `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala:90`, `scalus-testkit/shared/src/main/scala/scalus/testing/ImmutableEmulator.scala:135-136`, `scalus-testkit/jvm/src/test/scala/scalus/testing/ImmutableEmulatorTest.scala:179-182`

Verification: Scenario.scala:351-354: provider.getDatum is scenarioLogicMonad.pure(None) for every hash.
ImmutableEmulator.submit adds the tx datums (ImmutableEmulator.scala:70, binaryDatums ++
binaryDatumsOf(tx), which includes inline datums, EmulatorBase.scala:795-800). getDatum (line 90)
and asReader.getDatum (135-136) resolve them. ImmutableEmulatorTest.scala:176-181 pins the reader
behavior. snapshotReader (Scenario.scala:389-390) uses asReader. So in one Scenario state,
provider.getDatum(h) returns None and snapshotReader.getDatum(h) returns Some(d). Any public use of
Scenario.provider.getDatum reaches this.

Action: Add a ScenarioTest case: submit a tx paying to Bob with an inline datum (payTo(..., (_: Transaction)
=> Data.unit)), then via Scenario.provider assert getDatum(hash) == Some(Data.unit), and that it
equals snapshotReader.getDatum. Main-code note only: `leaf(s => Done(s,
s.emulator.getDatum(datumHash)))`.

### F-71 Preconfiguration.resolveUtxos assigns auto indices from one counter starting at 0, ignoring explicit `idx` val – open
Cluster `testing`, report finding C12-3, verdict CONFIRMED.

Preconfiguration.resolveUtxos assigns auto indices from one counter starting at 0, ignoring explicit
`idx` values that share the genesis hash, then builds the result with `.toMap`, so a collision
silently drops a UTxO. Input {"utxo":{"alice":[{"ada":1,"idx":0}],"bob":[{"ada":2}]}} gives Alice
Input(genesis,0) and Bob Input(genesis,0): one UTxO instead of two, whichever map order upickle
yields. The same happens for two explicit entries with the same tx_id and idx. PreconfigurationTest
only checks distinct auto indices (63-78) and an explicit tx_id/idx alone (80-87).

Evidence: `scalus-testkit/shared/src/main/scala/scalus/testing/Preconfiguration.scala:71-89`, `scalus-testkit/jvm/src/test/scala/scalus/testing/PreconfigurationTest.scala:63-87`

Verification: Preconfiguration.scala:71-89: one autoIdx counter starts at 0 and ignores explicit idx values. The
genesis hash is the default when tx_id is absent. The result is keyed by Input (Map flatMap plus
.toMap), so equal keys collapse. {"utxo":{"alice":[{"ada":1,"idx":0}],"bob":[{"ada":2}]}} gives
Input(genesis,0) twice, so only 1 UTxO is left, silently. Two entries with the same tx_id and idx
collapse the same way. PreconfigurationTest.scala:63-78 checks only auto indices, and 80-87 checks
one explicit entry, so no test catches this. Reachable from user JSON through fromJson +
resolveUtxos.

Action: Add a PreconfigurationTest case with the JSON above asserting resolveUtxos returns 2 UTxOs (or, if
the owner prefers, that it throws IllegalArgumentException on a duplicate input). Main-code note
only: skip auto indices already taken by explicit idx on the same tx hash, and require no duplicate
inputs before `.toMap`.

### F-72 programToPlutusFileContent writes a PlutusV4 script under the envelope type "PlutusScriptV3" (line 48, uncover – open
Cluster `testing`, report finding C12-4, verdict CONFIRMED.

programToPlutusFileContent writes a PlutusV4 script under the envelope type "PlutusScriptV3" (line
48, uncovered), so a V4 (Dijkstra) script file claims to be V3 and a tool that reads the envelope
evaluates it under the wrong language; the reader accepts only V1-V3 envelopes. The owner may
already track this as the stale V4 envelope type; whether a PlutusScriptV4 envelope name exists
upstream yet is not verified.

Evidence: `scalus-core/shared/src/main/scala/scalus/utils/Utils.scala:44-49`, `scalus-core/shared/src/main/scala/scalus/utils/Utils.scala:51-52`

Verification: Utils.scala:44-48 maps PlutusV4 to "PlutusScriptV3". supportedEnvelopeTypes (51-52) holds V1-V3
only, so a V4 envelope is not readable, and a V4 script is written with a V3 label. Reachable
through the public Program.writePlutusFile(path, Language.PlutusV4) (Program.scala:126-127, 353-354)
and Utils.programToPlutusFileContent. No test passes PlutusV4 (UtilsTest uses only V1, V2 and V3).
The owner already tracks this as the 'stale V4 envelope type'. The upstream V4 envelope name is not
verified, so the action needs an owner decision, as the finding says.

Action: Decide the V4 envelope name (or make programToPlutusFileContent reject PlutusV4) and pin it with a
UtilsTest case: programToPlutusFileContent(p, Language.PlutusV4) yields the decided type, and
readPlutusFileContent accepts it.

### F-73 `TxBuilder` cannot spend a datum-less PlutusV3 script UTxO – open
Cluster `txbuilder.core`, report finding C20-1, verdict CONFIRMED.

By reading (not executed): TxBuilder cannot spend a script UTxO that has no datum, even when the
script is PlutusV3. buildDatumWitness maps datumOption None to Datum.DatumInlined
(TxBuilder.scala:1970), and useDatum returns Left(DatumIsMissing) for every UTxO with datumOption
None (TransactionStepsProcessor.scala:287-288), without looking at the script language. The ledger
rule in this repository accepts a missing datum for PlutusV3
(MissingRequiredDatumsValidator.scala:71, CIP-69, Conway). Coverage marks line 288 unexecuted, so no
test spends a datum-less script UTxO; the test comment 'TxBuilder requires a datum for script
outputs' (TxBuilderCompleteTest.scala:1332) records the limitation as if it were intended.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionStepsProcessor.scala:284-288`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala:1968-1971`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/rules/MissingRequiredDatumsValidator.scala:71`, `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TxBuilderCompleteTest.scala:1332`

Verification: The bug is real and normal use reaches it. All four TxBuilder.spend(utxo, redeemer, script)
overloads (TxBuilder.scala:288, 316, 344, 374) call buildDatumWitness. For a UTxO with datumOption
None, that function returns Datum.DatumInlined (TxBuilder.scala:1970). It then builds a
ThreeArgumentPlutusScriptWitness. TransactionStepsProcessor.scala:273 always calls useDatum for that
witness. useDatum returns Left(DatumIsMissing) for datumOption None (lines 286-288) and never checks
the script language. The ledger rule accepts a missing datum for PlutusV3: `case None if
script.language == Language.PlutusV3 =>` (MissingRequiredDatumsValidator.scala:71). Example:
spend(Utxo(in, Output(v3ScriptAddr, Value.ada(10))), redeemer, alwaysOkV3) fails with
BuildStepException, although the ledger accepts the transaction. Grep finds DatumIsMissing only at
its definition (StepError.scala:73) and its use (TransactionStepsProcessor.scala:288), so no test
reaches this path. The comment at TxBuilderCompleteTest.scala:1332 records the limitation. The
proposed V3 success test and V2 DatumIsMissing test are sound.

Action: Add a test in TxBuilderCompleteTest: a PlutusV3 alwaysOk script UTxO with datumOption = None;
spend(utxo, redeemer, alwaysOkScript).payTo(Bob, 5 ADA).complete(emulator, Alice); assert it
succeeds and emulator.submit(signed tx) isRight. Keep a PlutusV2 variant that expects
DatumIsMissing. Main-code note only: in useDatum, accept None when the resolved script is PlutusV3
(the language is known from the attached script or the reference UTxO).

### F-74 `editTransactionSafe` drops the whole witness set, not only the redeemers – open
Cluster `txbuilder`, report finding C09-1, verdict CONFIRMED.

By reading (not executed): toEditableTransactionSafe sets `updatedTx =
tx.withWitness(TransactionWitnessSet.empty)` (line 222), so editTransactionSafe returns a
transaction with no vkey witnesses, native/Plutus scripts, datums or bootstrap witnesses. Its doc
says it only removes the redeemers, and the unsafe twin toEditableTransaction keeps the rest of the
witness set. TransactionEditorTest cannot see it: every fixture has a witness set that holds only
redeemers. No main code calls editTransactionSafe (only the test), so the impact is the public API.

Evidence: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionEditor.scala:205-224`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionEditor.scala:179-203`, `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionEditor.scala:320-336`, `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TransactionEditorTest.scala:80-106`

Verification: TransactionEditor.scala:222 sets `updatedTx = tx.withWitness(TransactionWitnessSet.empty)`. The doc
at 206-208 says it only removes redeemers. The unsafe twin at 196-200 keeps the witness set and
clears only `redeemers`. editTransactionSafe (320-336) calls toEditableTransactionSafe on the input
and on the processed tx, then fromEditableTransaction (247-263), which copies the now-empty witness
set. So vkey witnesses, scripts, datums and bootstrap witnesses are dropped. You reach it through
the public API: TransactionEditor.editTransactionSafe(identity)(tx) on any tx whose witness set
holds more than redeemers. The only callers are in TransactionEditorTest.scala (lines 102, 163,
224). Those fixtures build anyNetworkTx and set only redeemers, so the test passes. The suggested
fix compiles: Transaction.scala:81 has the overload `withWitness(f: TransactionWitnessSet =>
TransactionWitnessSet)`.

Action: Add a test: a tx with one input, a Spend redeemer, a PlutusV1 script (script1) and one VKeyWitness
in the witness set; assert editTransactionSafe(identity)(tx) == Right(tx) and that
editTransaction(identity)(tx) keeps the script and the witness. Main-code note only: build updatedTx
with `tx.withWitness(_.copy(redeemers = None))`.

### F-75 JVM `Builtins.indexArray` truncates the index to 32 bits; the CEK does not – open
Cluster `uplc.builtin`, report finding C36-1, verdict CONFIRMED.

By reading, not executed: the JVM BuiltinArray.apply indexes with n.intValue, which keeps only the
low 32 bits. Builtins.indexArray(arr, BigInt(2).pow(32)) on a non-empty array returns arr(0), and
2^32 + 1 returns arr(1), where the CEK IndexArray checks the BigInt against [0, length) and fails.
Off-chain evaluation of @Compile code (compiled.code, EvalTestKit's JVM side) and on-chain
evaluation then disagree. Builtins.scala:469-479 never executes in the baseline: BuiltinArrayTest
only evaluates UPLC, so its out-of-bounds tests (5 and -1) never reach the JVM path. The exception
class differs too (IndexOutOfBoundsException, documented at Builtins.scala:476-478); that is by
design and not part of this finding.

Evidence: `scalus-core/shared/src/main/scala/scalus/uplc/builtin/BuiltinArray.scala:48`, `scalus-core/shared/src/main/scala/scalus/uplc/Builtin.scala:1293-1298`, `scalus-core/shared/src/main/scala/scalus/uplc/builtin/Builtins.scala:479`, `scalus-core/shared/src/test/scala/scalus/uplc/builtin/BuiltinArrayTest.scala:125-134,168-177`

Verification: The finding holds. One correction: BuiltinArray.scala:48 is in shared/, so the defect is on every
platform, not only the JVM. `def apply(n: BigInt): A = array(n.intValue)` keeps the low 32 bits.
BigInt(2^32).intValue == 0, (2^32+1).intValue == 1 and (-(2^32)).intValue == 0, so each call returns
an element instead of throwing. Builtins.indexArray (Builtins.scala:479) delegates to it with no
bounds check. The CEK IndexArray (Builtin.scala:1293-1298) checks `idx < 0 || idx >= arr.length` on
the BigInt and throws BuiltinException. The JIT (scalus-uplc-jit-compiler
BuiltinImpls.scala:343-349) does the same check. Reachable when @Compile code runs directly as Scala
off-chain with a large index: on-chain it fails, off-chain it returns arr(0) or arr(1).
BuiltinArrayTest.scala:125-134 and 168-177 only call term.evaluateDebug (the CEK path), so they
never reach BuiltinArray.apply. The proposed test and fix are correct.

Action: Add a JVM-side test: Builtins.indexArray(BuiltinArray(BigInt(1), BigInt(2)), n) must throw for n =
2^32, 2^32 + 1 and -(2^32), as the CEK does for the same term. Main-code note: in BuiltinArray.apply
check `n < 0 || n >= array.length` before `n.toInt` and throw (BuiltinException or
IndexOutOfBoundsException, as the doc says).

### F-76 JSON codecs that cannot read their own output (Timelock Blockfrost, `DataReadWriter`, blueprint `Purpose`) – open
Cluster `uplc.builtin`, report finding C36-2, verdict CONFIRMED.

By reading the upickle 4.4.3 and ujson sources, not executed: DataReadWriter (still a public given
in Data's implicit scope, kept for source compatibility since 0107bb88a) writes the Constr index
with writeJs(constr: BigInt). upickle's BigIntWriter is a SimpleMapKeyWriter, so the index becomes a
JSON string (ujson.Str("0")). The reader takes json.obj("constructor").num, and ujson.Value.num
throws InvalidData on a Str. So upickle.default.read[Data](upickle.default.write(Data.Constr(0,
Nil))) throws for every Constr. Integers fail the same way: writeJs(value.toLong) goes through
visitInt64, which emits a string above 2^53, and values beyond Long go through BigIntWriter; the
reader's .num.toLong then throws (and would lose precision through Double if it did not). No test
anywhere uses DataReadWriter (DataApi.scala:66-108 unexecuted).

Evidence: `scalus-core/shared/src/main/scala/scalus/uplc/builtin/DataApi.scala:61-110`, `scalus-core/shared/src/main/scala/scalus/uplc/builtin/DataApi.scala:68,79,86,105`, `scalus-core/shared/src/main/scala/scalus/uplc/builtin/Data.scala:142`

Verification: Verified against the upickle 4.4.3 sources. DataReadWriter (DataApi.scala:61) is a public given in
`object Data extends DataApi` (Data.scala:83). It writes `"constructor" -> writeJs(constr)`.
BigIntWriter is a SimpleMapKeyWriter (upickle-implicits Writers.scala:71, 138-141) whose write0
calls out.visitString, so the index becomes ujson.Str. The reader calls
`json.obj("constructor").num`, and ujson Value.scala:64-67 throws InvalidData for anything but
ujson.Num. So every Constr round-trip throws. For Data.I, writeJs(Long) goes through visitInt64
(JsVisitor.scala:31-34), which emits visitString when |i| > 2^53. Values beyond Long use
BigIntWriter, which also emits a string. `.num` on the read side then throws. A grep of scalus-*
sources finds no use of DataReadWriter outside its definition. Small ints in I(...) and B round-trip
fine, so the defect is in Constr (every index) and in large I only.

Action: Add a test in DataJsonTest: for Data.Constr(0, [I(1)]), I(2^53 + 1) and I(2^64), assert
upickle.default.read[Data](upickle.default.write(d)) == d. Main-code note: make DataReadWriter
delegate to dataJsonCodec (bimap via Data.toJson / Data.fromJson on the rendered ujson value), or
deprecate it.

### F-77 `satMul(-1, Long.MinValue)` does not saturate – open
Cluster `uplc.eval.budget`, report finding C21-1, verdict NARROWED.

By reading (not executed): satMul(-1, Long.MinValue) returns Long.MinValue instead of saturating to
Long.MaxValue. r = -1 * MinValue wraps to MinValue, and the overflow check r / a == b evaluates
MinValue / -1, which the JVM (JLS 15.17.2) also wraps to MinValue, so it equals b and the wrapped r
is returned. The mirrored order satMul(Long.MinValue, -1) is caught (r / a = 1). No test uses -1
with MinValue in that order. Practical reach is low: it needs a negative coefficient times an
already negatively saturated value.

Evidence: `scalus-core/shared/src/main/scala/scalus/uplc/eval/CostModel.scala:59-66`, `scalus-core/shared/src/test/scala/scalus/uplc/eval/CostingIntegerTest.scala:66-81`

Verification: The defect is real. CostModel.scala:59-66 computes r = -1 * MinValue, which wraps to MinValue. The
check r / a = MinValue / -1 also wraps to MinValue (JLS 15.17.2; Scala.js and Native emulate the
same) and equals b, so the code returns MinValue, not MaxValue. Reversing the order is safe:
satMul(MinValue, -1) gives r/a = 1 != -1, so it saturates. The defect cannot be reached from cost
evaluation. The buggy case needs the right operand b == Long.MinValue. Every `*` site puts a non-
negative memory size or a non-negative product on the right (CostModel.scala:92, 109, 121 c11*(x*y),
130, 161, 184, 192, 275, 290). At line 102 the right operand (arg1-arg2).max(minimum) has non-
negative args, so it is never below -MaxValue. Corrected claim: satMul(-1, Long.MinValue) returns
MinValue (latent, not reachable from any current costing function). Fix it as hygiene
(Math.multiplyHigh check or a special case), not as a costing bug.

Action: Main-code note: detect the case explicitly, e.g. use Math.multiplyHigh(a, b) != (r >> 63) as the
overflow test, or special-case a == -1 && b == Long.MinValue. Test side: the property in C21-2
covers it with edge values in the generator; until the fix lands, add satMul(-1L, Long.MinValue) ==
Long.MaxValue as an expected-failure example.

### F-78 The Scalus MkCons accepts a non-constant head when the list is a `(list value)` constant and returns a VList – fixed
Cluster `uplc.eval`, report finding C24-1, verdict CONFIRMED.

The Scalus MkCons accepts a non-constant head when the list is a `(list value)` constant and returns
a VList; Plutus 1.63 MkCons (PlutusCore/Default/Builtins.hs:1669-1683) takes both arguments as
SomeConstant, so the reference machine fails with an unlifting error. A script that conses a native
Constr onto a (list value) succeeds in Scalus (emulator, budget estimation) but fails on chain. The
compiler emits (list value) element types for native-Constr representations
(LoweredValueRepresentation.scala:534,924), so this is reachable from @UplcRepr(UplcConstr) code;
whether a released contract hits it is unverified. Second defect in the same case: Builtin.scala:520
builds VList(elem :: Nil) and drops every element of a non-empty (list value) constant. The VList
discharge path (Cek.scala:1744-1757) has no test at all.

Evidence: `scalus-core/shared/src/main/scala/scalus/uplc/Builtin.scala:505-521`, `scalus-core/shared/src/main/scala/scalus/uplc/eval/Cek.scala:505-509`, `scalus-core/shared/src/main/scala/scalus/uplc/eval/Cek.scala:1744-1757`, `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValueRepresentation.scala:534,924`

Verification: Builtin.scala:505-525: the first MkCons case needs both args to be VCon. A non-VCon head (VConstr)
goes to case 2 or 3. Case 3 (line 520-521) matches `VCon(Constant.List(DefaultUni.BuiltinValue,
_))`, ignores the tail and returns `VList(elem :: Nil)`. So consing onto a non-empty (list value)
loses every old element. DefaultUni.BuiltinValue is the CIP-153 `value` type (DefaultUni.scala:39,
UplcParser.scala:158), so `(con (list value) [...])` with elements is a legal, parseable constant.
Plutus Builtins.hs:1669-1683 (local clone ../iohk/plutus) types MkCons as `SomeConstant uni a ->
SomeConstant uni [a]`. A constr head therefore fails unlifting on chain, but Scalus returns a
success. Reachability: ProdUplcConstr/SumUplcConstr.defaultUni returns BuiltinValue
(LoweredValueRepresentation.scala:534,924). SumBuiltinListEmitter.genNil
(typegens/SumBuiltinListEmitter.scala:51-58) and ScalusRuntime.scala:868 emit
`Constant.List(elementRepr.defaultUni(..), Nil)`. annotations.scala:85 documents
`@UplcRepr(SumBuiltinList(UplcConstr))` as a user-facing option. No test under scalus-
core/*/src/test mentions VList or `list value`, so the discharge path (Cek.scala:1744-1757) has no
test. The proposed action adds tests only.

Action: Test-only first: add a CEK test that evaluates [(force mkCons) (constr 0 []) (con (list value) [])]
and asserts an evaluation failure (Plutus behavior), and one that conses a constr onto a non-empty
(list value) and asserts the old elements are not lost. Owner decision: gate the VList extension off
for ledger evaluation or remove it; if it stays, add a discharge test for an all-constant VList and
for the IllegalStateException path.

Resolution: confirmed by runs. On the old code, `[(force mkCons) (constr 0 []) (con (list value) [])]`
succeeded on the CEK machine and on the JIT (`ListValue(ConstrValue(0))`). `MkCons` in the CEK
machine (`Builtin.scala`) and in the JIT (`BuiltinImpls.mkCons`) now fails for a non-constant
head, as Plutus does. `jvm/test` passes with no other change, so no test relied on the old
cases; the byte oracle is identical and MiMa is clean. `VList` and `ListValue` are now never
created (only `MkCons` created them). `CekValue.VList` is deprecated (removing it would break
MiMa for 1.4.1) and its dead branches are gone; the JIT, which has no MiMa check, dropped
`ListValue`. The compiler side is F-83.

### F-79 TermSanitizer's doc requires ASCII (first char ASCII letter or '_', then ASCII letter/digit/'_'/'\''), but isV – open
Cluster `uplc`, report finding C07-6, verdict CONFIRMED.

TermSanitizer's doc requires ASCII (first char ASCII letter or '_', then ASCII
letter/digit/'_'/'\''), but isValidFirstChar uses Char.isLetter, isValidSubsequentChar uses
isLetterOrDigit and line 94 uses isDigit, all Unicode. So sanitizeName keeps 'é', 'π' or an Arabic
digit, and Term.show / Program.show (Pretty[Term] sanitizes, Term.scala:630-632) print names the
reference parser rejects. Checked with uplc 1.70.0.0: `(program 1.0.0 (lam é é))` fails with
"unexpected 'é'" and `(program 1.0.0 (lam x٣ x٣))` with "unexpected '٣'", while `(lam e e)` parses.
Scala identifiers may be Unicode, so the names reach UPLC through SIR. Found by reading (not run
through Scala).

Evidence: `scalus-core/shared/src/main/scala/scalus/uplc/TermSanitizer.scala:5-12`, `scalus-core/shared/src/main/scala/scalus/uplc/TermSanitizer.scala:94`, `scalus-core/shared/src/main/scala/scalus/uplc/TermSanitizer.scala:117-118`, `scalus-core/shared/src/main/scala/scalus/uplc/TermSanitizer.scala:127-128`, `scalus-core/shared/src/main/scala/scalus/uplc/Term.scala:630-632`

Verification: The doc comment (TermSanitizer.scala:10-11) requires ASCII. The code uses Unicode predicates:
isValidFirstChar is c.isLetter || '_' (117-118), isValidSubsequentChar is c.isLetterOrDigit || '_'
|| '\'' (127-128), and line 94 uses firstChar.isDigit. So sanitizeName keeps 'é' and '٣'. Term.show
is pretty.render (Term.scala:378). Pretty[Term] calls TermSanitizer.sanitizeNames
(Term.scala:630-632), so show and pretty emit these names unchanged. I reproduced the parser
rejection with the local reference binary (/nix/store/...plutus-executables-exe-
uplc-1.70.0.0/bin/uplc evaluate -i). `(program 1.0.0 (lam é é))` fails with "unexpected 'é'" at
1:21. `(program 1.0.0 (lam x٣ x٣))` fails with "unexpected '٣'". `(lam e e)` evaluates to `(lam e-0
e-0)`. Reachability: Scala identifiers may contain Unicode letters, and lambda and let names flow
through SIR into UPLC LamAbs names, so a validator with such a binder prints unparseable UPLC. The
proposed rows (é -> _, xé -> x_, x٣ -> x_) are correct after an ASCII-only fix, and they fail today.

Action: Test side: add rows "é" -> "_", "xé" -> "x_", "x٣" -> "x_" to the sanitizer table, plus the property
in C07-12; both fail today. Main-code note only: restrict the three predicates to ASCII ('a'-'z',
'A'-'Z', '0'-'9').

### F-80 UtxoCells: no check that the continuing output keeps the beacon token – fixed
Cluster `utxocells`, report finding C33-1, verdict CONFIRMED.

By reading (not executed): no on-chain check makes the continuing output keep the beacon token, or
any value. verifyContinuingOutput (UtxoCell.scala:60-79) checks only the address and the inline
datum; transitionSpend and verifyFlowSpend never read tx.mint on a non-terminal spend;
setContinuingValue (OnChainCellOps.scala:54-73) floors the value only when the transition calls it.
The test 'CounterCell UPLC evaluates correctly with valid ScriptContext' passes with no beacon
anywhere in the transaction (the input value at CounterCellSpec:294 and the output at 299 are plain
lovelace). Only the off-chain builder adds the beacon (UtxoCellDef.scala:132-136). Consequence:
anyone who may spend (CounterCell Increment needs no signature) can move the beacon and the locked
ADA to their own address and leave a beacon-less continuing output; they can then pay the beacon
back to the script address with a forged datum, which no validator checks, and findUtxo
(UtxoCellDef.scala:51-60) selects the forged state. A variant: a non-terminal spend plus a -1 mint
passes the mint handler's burn branch (FlowCellValidator.scala:55-56), so the cell becomes
unfindable.

Evidence: `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/UtxoCell.scala:55-80`, `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/FlowCellValidator.scala:47-58`, `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/UtxoCellDef.scala:132-136`, `scalus-utxo-cell/shared/src/test/scala/scalus/utxocells/CounterCellSpec.scala:174-188`, `scalus-utxo-cell/shared/src/test/scala/scalus/utxocells/CounterCellSpec.scala:284-316`

Verification: Confirmed by reading; not executed. UtxoCell.scala:55-80 (verifyContinuingOutput) checks only three
things: one output at the script credential, the full address, and the inline datum. It never checks
value or the beacon. transitionSpend (UtxoCell.scala:221-241) and verifyFlowSpend (184-198) check
the mint only when nextState is None. FlowCellValidator.mintCell (47-58) and transitionMint
(243-256) accept qty == -1 through verifyBurnBeacon, which checks only mint.hasOnly(policy, name,
-1) at UtxoCell.scala:168-177. So a non-terminal spend plus a burn passes both handlers.
setContinuingValue (OnChainCellOps.scala:54-73) floors the value only when the transition calls it,
and CounterCell/TestFlowValidator never call it. The test fixture makeSpendingContext
(CounterCellSpec.scala:284-316) uses plain lovelace on the input (294) and the output (299), and the
'valid ScriptContext' test (174-188) succeeds with it. That proves no beacon is needed on-chain.
Only the off-chain builder adds the beacon (UtxoCellDef.scala:132-137, UtxoFlowDef.scala:110-112).
findUtxo (UtxoCellDef.scala:51-60) picks any UTxO that holds the beacon, so an attacker can pay the
beacon to the script address with a forged datum. Payments to a script address run no validator.
Anyone who may spend can reach this: CounterCell Increment and the CounterCellV2 transition need no
signer.

Action: Log in findings.md. Disproving test for the verify agent: in CounterCellSpec, build a spending
context whose own input holds 10 ADA + 1 beacon, with the continuing output at the script address
with the correct next datum but no beacon, and the beacon in a pubkey output; assert the spend fails
(today it succeeds). Main-code note only: require the continuing output to carry exactly one beacon
(value.hasNft(policyId, beaconName)) in verifyContinuingOutput, and on non-terminal spends require
that tx.mint has no entry for the own policy.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(utxo-cell): keep the beacon in the continuing output of a cell spend". The review added a single-cell-input check for terminal spends (two terminal spends with one burn let a second beacon leave).

### F-81 UtxoFlow macros: free variables of chunk 0 become datum fields, so the transaction builder chooses them… – fixed
Cluster `utxocells`, report finding C33-2, verdict CONFIRMED.

By reading (not executed): every free variable that the body of chunk 0 references becomes a datum
field (UtxoFlowMacros.scala:765-778) and is decoded from the datum, replacing the original symbol
(1100-1103). The test at UtxoFlowSpec:624-627 shows it: minBid, referenced in chunk 0, must be
supplied in datum0. The mint handler accepts any initial datum (datum == mint redeemer,
FlowCellValidator.scala:106-118). So in ParameterizedFlowValidator (UtxoFlowSpec:812-822) the check
requireSignedBy(p.seller) in chunk 0 uses p taken from the datum that the initializer chose, not the
script parameter, although the trait docs (FlowCellValidator.scala:85-87) promise parameterization.
A variable first used in a later function-entry chunk is packed from the real closure value
(UtxoFlowSpec:519-593 works with datum0 = Constr(0, [])), so the hole is only for chunk-0
references.

Evidence: `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/UtxoFlowMacros.scala:765-778`, `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/UtxoFlowMacros.scala:1100-1103`, `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/FlowCellValidator.scala:85-87`, `scalus-utxo-cell/shared/src/main/scala/scalus/utxocells/FlowCellValidator.scala:106-118`, `scalus-utxo-cell/shared/src/test/scala/scalus/utxocells/UtxoFlowSpec.scala:624-627`, `scalus-utxo-cell/shared/src/test/scala/scalus/utxocells/UtxoFlowSpec.scala:812-822`

Verification: Confirmed by reading; not executed. For chunk 0, boundSoFar starts as excludedSymbols = {cpsCtx,
cellCtx} (UtxoFlowMacros.scala:743-756). freeVars (353-375) keeps every local Ident whose owner is
not a package-level owner (isLocalSymbol, 342-348). So `p`, a val in flowDispatch(param) or in the
caller after inlining, becomes a datum field (765-778). Dispatch decodes it from the datum and
rebinds the symbol (1077-1103). Nothing special-cases chunk 0: grep finds no 'index == 0' or outer-
scope handling. UtxoFlowSpec.scala:624-627 shows the same mechanism: minBid must come from datum0.
The mint handler accepts any datum equal to the redeemer (FlowCellValidator.scala:106-118 ->
verifyMintResult, UtxoCell.scala:139-162). So in ParameterizedFlowValidator
(UtxoFlowSpec.scala:811-822), requireSignedBy(p.seller) uses the seller that the initializer chose.
The trait's own Scaladoc example (FlowCellValidator.scala:74-80) has the same flaw, because it uses
p.minBid and p.seller in chunk 0. The only on-chain test of ParameterizedFlowValidator is compile-
only (UtxoFlowSpec.scala:854-859), so no test catches this.

Action: Log in findings.md. Disproving test: compile ParameterizedFlowValidator, apply the parameter
FlowParams(seller = Alice, ..) to the program, init with datum Constr(0, [FlowParams(seller = Bob,
..).toData]), advance chunk 0 signed only by Bob in the Emulator; if it validates, the parameter is
dead. Main-code note only: exclude symbols bound outside the define body from chunk-0 datum fields
(reference them directly), or document and check them at mint.

Resolution (2026-10-23): reproduced by a failing test first, then fixed in "fix(utxo-cell): read flow parameters from the closure, not from the datum". The review added: the flow mint requires the empty initial datum `Constr(0, [])`, and a flow that does not start with an await, or whose first chunk has datum fields, is a compile error.

### F-82 `Data.Map` equality ignores entry order and duplicates, so `equalsData` differs from Plutus – fixed

Evidence: `Data.scala:162-167` defines `Map.equals` and `hashCode` over `values.toScalaList.toSet`
(since 7f149ace5, 2023-06-07, "Data.Map.equals"). `Builtins.equalsData` is `d1 == d2`
(`Builtins.scala:654`), and the CEK `EqualsData` calls it (`Builtin.scala:723-731`). Plutus
compares `Data` maps structurally, as ordered lists of pairs. So
`equalsData (Map [(1,a),(2,b)]) (Map [(2,b),(1,a)])` is `True` in Scalus and `False` on the
node, and `Map [(1,a),(1,a)]` equals `Map [(1,a)]`. A script that branches on this result
validates in the Scalus evaluator and Emulator and fails on chain, or the reverse.

Found by the verify agent of `compiler.plugin` (C08-11) and confirmed by reading; not executed.
The set equality looks deliberate (Scala-side convenience); the fix must keep the CEK
order-sensitive, for example a separate `equalsData` that compares the pair lists, plus a
conformance test with a reordered map and a map with a duplicate key.

Resolution: confirmed by a run: the new `CekBuiltinsTest` "EqualsData compares map entries in
order" got `True` for a reordered map. Plutus 1.63 derives `Eq` for `Data`
(`PlutusCore/Data.hs:48`) and `EqualsData` is `(==)` (`Default/Builtins.hs:1835-1837`). Fix: `Data.Map`
uses the case-class equality, so `==`, `equalsData` and nested maps are all order-sensitive.
`jvm/test` passes with no other change, so no code relied on the set equality; the byte oracle is
identical and MiMa is clean.

### F-83 `@UplcRepr(SumBuiltinList(UplcConstr))` compiles to code that fails on chain – fixed

Evidence: `SumUplcConstr.defaultUni` and `ProdUplcConstr.defaultUni` return `BuiltinValue`
(`LoweredValueRepresentation.scala:534,924`), so `SumBuiltinListEmitter.genNil`
(`typegens/SumBuiltinListEmitter.scala:51-58`) and `ScalusRuntime.scala:868` emit
`(con (list value) [])` for a builtin list of native `constr` values, and the list grows by
`mkCons` of `constr` values. A builtin list holds constants only, so Plutus fails at the first
cons; only the old Scalus CEK and JIT accepted it (F-78). `annotations.scala:75-90` shows this
annotation on `ChessSet.visited` as an example. After F-78, such code fails in Scalus too.

Reach: only with an explicit `@UplcRepr(SumBuiltinList(UplcConstr))`, or a `BuiltinList[T]`
whose element type `T` has no `Data` form. A default `List` of `UplcConstr` elements uses
`SumCaseUplcConstrEmitter` (native constr lists) and is not affected. No test compiles and runs
such code (`FieldAnnotationTestDefs.WithNativeList` is only checked at the SIR level).

Fix options: reject the representation at lowering with a clear error, or lower it to the native
constr list. Remove the example from the scaladoc of `UplcRepr`.

Resolution (agreed with Ruslan, the feature owner, 2026-10-22): the plugin rejects
`@UplcRepr(SumBuiltinList(UplcConstr))` with a compile error (`UplcReprChecks`, called from both
annotation encoders); `SumBuiltinList` takes a `Data`, primitive or pair element. Nothing is
deprecated and the public API is unchanged, so 1.4.1 stays MiMa-compatible. The `UplcRepr`
scaladoc example now uses `@UplcRepr(UplcConstr)` on the list, and `WithNativeList` uses
`SumBuiltinList(Constant)`. Test: `SumBuiltinListReprTest`. Left open: a `BuiltinList[T]` whose
`T` has no `Data` form still lowers to a `(list value)`; `VList` is deprecated, see F-78.

### F-84 Lottery first reveal can drain the pot – fixed

Found by the security review of the S2 fixes, 2026-10-23; present before them. The reveal branches
(Empty to RevealPlayerOne/RevealPlayerTwo) checked only the datum of the continuing output, not its
value, so the revealing player could send min-ADA to the continuing output and keep the rest of
the pot; one reveal could also merge two pots. Resolution: both reveal branches require a single
lottery input and a continuing output with the input's tokens and at least its lovelace
(`hasSameTokensAndAtLeastAda`; the first version required the exact value, which blocked a reveal
that must add ADA for the larger datum) ("fix(examples): spend one lottery
input and keep the whole pot in a reveal").

### F-85 Vault example: native tokens in a vault UTxO can be stripped – open

Found by the F-53 fix agent's security review, by reading. Deposit requires an ADA-only output but
does not require an ADA-only input, and needs no signature, so anyone can lock a vault UTxO with
tokens and any depositor can then take the tokens. Cancel compares only lovelace on the output, so
tokens can be added (min-ADA griefing). Fix: require ADA-only inputs and outputs, or compare whole
values.

### F-86 Oracle example: a negative rate passes the Update check – open

Found by the F-51 fix agent, by reading. `Update` now rejects a zero numerator or denominator, but
a negative one passes, and `Ord[Rational]` cross-multiplication flips for a negative denominator:
rate 8/-5 compares greater than a bet of 3/2, so `PricebetValidator.Win` lets the player win. The
mint does not validate the initial oracle datum either. Fix: require numerator > 0 and denominator
> 0 in Update and in mint.

### F-87 Groth16 example: public inputs are not reduced modulo the scalar field order – open

Found by the F-47 fix agent, by reading. Public scalars are not range-checked against the
BLS12-381 scalar field order r, so x and x + r derive the same point and both verify (input
malleability). snarkjs and other verifiers reject inputs >= r. Fix: require 0 <= x < r for each
public input. Not checked against ak-381.

### F-88 Oracle beacon lifecycle not enforced – fixed

Found by the owner while reviewing the S2 fixes, 2026-10-23; the skills missed it (see the skills
commit "check the token lifecycle and honest transactions in reviews"). The oracle mint did not
place the beacon, and `Update` did not keep it in the continuation, so the beacon could leave the
oracle and come back with any datum, while Pricebet trusts any beacon holder at the oracle script.
Fixed in "fix(examples): keep the oracle beacon in a valid oracle UTxO from mint to burn", which
also drops the spend `datum.isDefined` check and reduces the policy Burn branch to
`onlyBurnsUnder`, both implied by the invariant. Also closes the mint half of F-86 (a zero rate);
a negative rate stays open there.

### F-89 Example defects found by the blind skill-verification reviews – open

Two security reviews of OracleValidator, PricebetValidator, LotteryValidator and VaultValidator
(old and new skill, 2026-10-23), by reading; not yet reproduced:

- Pricebet `Join` has no single-own-input guard: one continuing output satisfies two equal bets,
  and the joiner stakes nothing (DS-1). `Join` also checks lovelace only (tokens of the bet leave).
- Pricebet `Win` accepts an oracle reading of any age, and one older than the join, so a player
  can join when the result is known; `!isEntirelyAfter(deadline)` lets `Win` run after the deadline.
- Pricebet hard-codes the beacon name `ORACLE`, while the oracle mints `config.beaconName`.
- Lottery: the first reveal has no deadline (a late revealer times out at once); player keys are
  not committed, so a mempool observer can copy a preimage and take a reveal or the winning
  payout; payouts match the payment credential only, and lovelace only.
- Cross-script double satisfaction: one output to a player can satisfy a lottery `Timeout`/`Lose`
  and a vault `Finalize` at once (DS-2); a tagged payout (`hasPaidTagged`) closes it.
- Vault: `Cancel` does not check `requireEntireVaultIsSpent`; `Finalize` matches the owner by
  payment credential only; a lost owner key leaves no exit (design). Token stripping is F-85.
