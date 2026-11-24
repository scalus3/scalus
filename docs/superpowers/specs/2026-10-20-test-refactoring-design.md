# Test Suite Review and Refactoring – Design

Status: approved design, 2026-10-20. This spec is also the execution guide; there is no
separate implementation plan.

## Goals

In priority order. When two goals conflict, the higher goal wins.

1. Reduce CI wall time. The critical path is the `ci-jvm` Scala 3.3.8 job (24.7 min on
   2026-10-20; 3.9.0: 18.3, 3.8.4: 17.2, ci-js: 14.8, ci-native: 15.3).
2. Improve coverage: exhaustive edge cases, and property tests where a law exists.
3. Make test code uniform: one set of shared helpers, tools and approaches.
4. Remove code duplication in tests.
5. Remove test duplication: tests that check the same logic again, and example groups
   that one property test can replace.
6. Verify the quality of the ScalaCheck generators: constructor coverage, value
   distribution, edge values.

**Why:** the owner chose CI time first. A property test that is slower than the examples it
replaces needs a reason, or it gets the `Slow` tag.

## Scope

- In scope: all test sources in the repository, about 600 files and 143k lines. 528 suites
  extend `AnyFunSuite`.
- In scope: test build settings in `build.sbt`, and CI workflows under `.github/workflows/`.
- Out of scope: main code, except where a backlog item names it.
- Out of scope: lowering the number of ScalaCheck samples on CI. The owner rejected it.

## Definitions

| Term | Meaning |
|---|---|
| cluster | A package, or a group of small packages, with at most about 3k test lines, plus the main code it tests. The unit of analysis in P2. |
| finding | One item in a cluster report: a claim, evidence, an action, and a scope. |
| scope | One kind of refactoring, from the fixed scope list below. |
| unit | One scope applied to one module. One unit is one branch in one worktree. |
| coordinator | The main Claude session. It dispatches agents, runs sbt, and gates each unit. |
| edit agent | A subagent that edits test files of one unit. It never runs sbt. |
| coverage baseline | The scoverage result per package, measured on master in P0. |
| coverage gate | The check in [CG-1] to [CG-4]. |
| backlog | The ordered list of units that P3 produces. |

### Scope list

| Scope | Meaning |
|---|---|
| `ci-time` | Make a test faster, tag it `Slow`, split a suite, or move a benchmark. |
| `adopt-helper` | Replace local code with an existing shared helper. |
| `extract-helper` | Create a shared helper from repeated local code. |
| `merge-duplicate-tests` | Merge or delete tests that assert the same behavior. |
| `to-property` | Replace an example group with a property test. |
| `fill-gap` | Add a test for an uncovered behavior that matters. |
| `strengthen-assertion` | Fix an assertion that cannot fail, or one that checks too little. |
| `generator` | Fix or extend an `Arbitrary`/`Gen` instance. |
| `style` | Align a test with the style guide. |
| `simplify` | Make test code shorter or clearer, with the same assertions. |
| `budget-pin` | Add or fix a budget pin, per `[BP-1]` to `[BP-9]`. |
| `main-bug` | A defect in main code, found while reading tests. It goes to `findings.md`, not to the backlog. |

## Phases

| Phase | Actor | Output | Depends on |
|---|---|---|---|
| P0 Measure | scripts and sbt, no LLM | timings, coverage baseline, helper inventory, generator statistics | – |
| P0b Profile | coordinator, JFR | CPU and allocation analysis of the slow modules and suites | P0 timings |
| P1 CI-time wins | coordinator, one worktree | `Slow` tag, nightly job, moved benchmarks, split suites | P0 timings |
| P2 Analyze | report agents and verify agents | one JSON report per cluster | P0 |
| P3 Spec | Fable reviewer | style guide, helper catalog, backlog | P2 |
| P4..N Implement | coordinator and edit agents | one branch per unit, fast-forward merged | P3 |

All P0 to P3 artifacts go to `docs/internal/test-refactor/`.

## Findings log

- The coordinator MUST record every finding of every phase in
  `docs/internal/test-refactor/findings.md`. `[FL-1]`
- Each finding MUST have a stable ID `F-<n>`, its evidence, a conclusion and a status. `[FL-2]`
- The status MUST be one of `open`, `backlog`, `fixed`, `rejected`. `[FL-3]`
- A `rejected` finding MUST state the reason. `[FL-4]`
- The coordinator MUST NOT renumber or delete a finding. `[FL-5]`

**Why:** the program runs for weeks. A finding that lives only in a chat transcript is lost, and
the next session repeats the measurement. A rejected finding with its reason stops the same idea
from coming back.

## P0 – Measure

**Purpose:** produce objective numbers so that agents do not guess.

### Timings

- The coordinator MUST measure timings with a fresh `jvm/test` run on master. `[P0-1]`
- A script MUST convert `target/test-reports/*.xml` to `timings.csv`. Each row holds module,
  suite, test, and seconds. `[P0-2]`
- The coordinator MUST read the step times from the last green CI logs. `[P0-3]`

**Why:** the JUnit reports in the tree are stale. For example, `BilinearAccumulatorBenchmarkTest`
(360 s in an old report) is already tagged `scalus.testing.Benchmark` and excluded. `[P0-3]`
separates compile time from test time. If compile time dominates a job, test refactoring
cannot fix that job.

### Coverage

- The coordinator MUST run a scoverage spike on one module before the baseline. `[P0-4]`
- The spike MUST show if instrumentation changes the SIR that the Scalus plugin generates
  for `@Compile` code. `[P0-5]`
- If instrumentation changes the SIR, the build MUST exclude `@Compile` code from
  instrumentation. `[P0-6]`
- The spike MUST record which packages run only through the CEK machine. The coverage
  gate MUST ignore those packages. `[P0-7]`

  *Implementation status (2026-10-21):* there is no package list. `scripts/coverage-exclude.sh`
  excludes 146 files (`@Compile` code and plugin intrinsics) from instrumentation, so their
  statements never reach the reports or the gate. See finding F-9.
- The coverage baseline MUST be stored per package in `docs/internal/test-refactor/baseline/`. `[P0-8]`
- Coverage MUST be opt-in: `sbt coverage test coverageReport`. Push CI MUST NOT run it. `[P0-9]`

**Why:** tests evaluate on-chain code as UPLC, not as JVM code. scoverage reports that code as
uncovered even when tests exercise it. Without `[P0-7]`, every on-chain gap reads as real.

### Helper inventory

- A script MUST list every `object`, `trait` and `def` in shared test sources, with its use
  count. `[P0-10]`
- The script MUST flag local helpers whose names repeat across files. `[P0-11]`

*Example, illustrative:* `mkEmulator` defined locally in 9 files is one flagged row.

### Generator statistics

- A suite `GenStatsTest` MUST sample 10 000 values per `Arbitrary` instance. `[P0-12]`

  *Implementation status (2026-10-21):* 34 of the 111 instances in the 6 `ArbitraryInstances`
  files are measured (`gen-stats.md`). A P2 cluster that depends on an unmeasured instance adds
  it to `GenStatsTest` first.
- `GenStatsTest` MUST carry the tag `scalus.testing.Benchmark`, so CI does not run it. `[P0-13]`
- For each instance, `GenStatsTest` MUST report the share of each constructor. `[P0-14]`
- For each instance, `GenStatsTest` MUST report histograms of size and depth. `[P0-15]`
- For each instance, `GenStatsTest` MUST report the hit rate of these edge values: `0`, `-1`,
  minimum, maximum, empty collection, and byte strings of length 32 and 64. `[P0-16]`

*Example, illustrative:* `Data: Constr 12%, Map 3%, List 8%, I 61%, B 16%; empty List 0.4%`.

## P0b – Profile

**Purpose:** find where the slow tests spend CPU and memory, in the test code and in the main
code they call, so that the inefficiencies get fixed now or later.

### Targets

- The coordinator MUST profile each module whose test run takes more than 30 s. `[PR-1]`
- The coordinator MUST profile each suite in the top 15 of `timings.csv`. `[PR-2]`
- The coordinator MUST profile a module on its own, not inside a full `jvm/test`. `[PR-3]`

**Why:** `[PR-3]`: inside `jvm/test`, all modules compete for CPU. A profile taken there shows the
contention, not the code (F-5 in the findings log).

### Method

- The coordinator MUST record with Java Flight Recorder in the forked test JVM, with the
  `profile` settings. `[PR-4]`
- The coordinator MUST NOT change `build.sbt` to enable profiling. It MUST use a `set` command
  or an opt-in system property. `[PR-5]`
- For each recording, the coordinator MUST extract: hot methods, allocation by class, allocation
  by site, GC pause time, and wall time. `[PR-6]`
- The coordinator MUST store each extract as text in `docs/internal/test-refactor/profiles/`. `[PR-7]`
- The coordinator MUST NOT commit the binary `.jfr` files. `[PR-8]`

*Example, illustrative, for `[PR-4]` and `[PR-5]`:*

```
sbt 'set LocalProject("scalusExamplesJVM") / Test / javaOptions +=
       "-XX:StartFlightRecording=filename=/tmp/prof/examples.jfr,settings=profile"' \
    "scalusExamplesJVM/testOnly scalus.examples.linkedlist.LinkedListTest"
jfr view hot-methods /tmp/prof/examples.jfr
jfr view allocation-by-class /tmp/prof/examples.jfr
```

### Analysis

- Each profile finding MUST name one cause class: `test-code`, `main-code`, `generator`,
  `harness` or `jvm`. `[PR-9]`
- Each profile finding MUST state the fix and the decision `fix-now` or `later`. `[PR-10]`
- A `fix-now` decision MUST be limited to a change that touches one place and keeps the
  behavior. `[PR-11]`
- A fix of main code MUST go in its own `perf:` or `fix:` commit, never in a test refactor
  commit. `[PR-12]`
- The coordinator MUST record every profile finding in the findings log, `fix-now` or
  not. `[PR-13]`

*Example, illustrative:* `LinkedListTest` spends 60 % of CPU in `PlutusV3.compile` because every
property case compiles the validator again. Cause `test-code`. Fix: compile once per suite.
Decision `fix-now`. If instead the cost sits in the CEK machine's environment lookup, the cause is
`main-code` and the decision is `later`, with the hot frames recorded.

**Why:** `[PR-11]`: a large optimization in the middle of a measurement phase makes the next
measurement incomparable. `[PR-12]`: a reviewer must see a behavior-relevant main-code change
alone.

## P1 – CI-time wins

**Purpose:** shorten the `ci-jvm` 3.3.8 job without loss of coverage. P1 does not wait for P2.

- The build MUST define the tag `scalus.testing.Slow`. `[P1-1]`

  *Implementation status (2026-10-21):* not added. No test meets `[P1-4]`: the slowest tests are
  slow because they recompile per case (F-19), and `PipelineDeterminismCrossJvmTest` guards every
  compiler change, so it must stay on push CI. The tag comes with its first user.
- The build MUST exclude `Slow` from `test` and `testQuick` with the existing
  `excludeFromTest`. `[P1-2]`
- A new workflow `nightly.yml` MUST run the tests tagged `Slow` and `Benchmark` on a
  schedule. `[P1-3]`

  *Implementation status (2026-10-21):* `nightly.yml` runs the `Benchmark` tests (80 tests, 485 s
  locally). Before it, CI never ran them.
- A test SHOULD get the `Slow` tag when it takes more than 10 s and its code under test
  changes rarely. `[P1-4]`
- A test that measures speed and asserts nothing MUST move to `bench/` as a JMH
  benchmark. `[P1-5]`
- A test that pins a budget MUST stay in the test suite. `[P1-6]`
- A suite whose run time exceeds 30 s SHOULD be split into suites that sbt can run in
  parallel. `[P1-7]`
- The coordinator MUST NOT change `Test / parallelExecution := false` of `scalusNative`
  without a measurement on a 16 GB runner. `[P1-8]`
- P1 is done when the CI log shows a shorter `ci-jvm` 3.3.8 job. `[P1-9]`

**Why:** sbt runs suites in parallel, but the tests of one suite run one after another. A 71 s
`ListTest` is one serial chain. `[P1-6]`: a budget pin is a regression test. `[P1-8]`: the
native tests ran out of RAM in parallel on CI.

*Example, illustrative, for `[P1-5]` and `[P1-6]`:* `SortBudgetTest` moves to `bench/` only if
it asserts nothing. If it asserts exact ExUnits, it stays.

## P2 – Analyze

**Purpose:** for each cluster, report what it tests, how well, at what cost, and what should
change.

### Clusters

- The coordinator MUST split the test sources into about 35 clusters before P2. `[P2-1]`
- Each cluster MUST include the main code it tests. `[P2-2]`
- The coordinator MUST run one pilot cluster first: `scalus.uplc.transform`. `[P2-3]`
- The owner MUST approve the pilot report before the other clusters run. `[P2-4]`
- The coordinator MUST ask the owner before it starts a `Workflow` run with more than 10
  agents. `[P2-5]`

**Why:** `[P2-2]`: coverage gaps exist only in relation to the code under test. `[P2-3]`: that
cluster has visible duplication (three `*CekConformanceTest` files) and existing property
tests, so it tests the schema well.

### Report agent input

- The test files and the main code of the cluster.
- The P0 data of the cluster: uncovered branches, timings, helper rows, generator statistics,
  and the profile findings of its suites.
- The catalog of existing shared helpers, for example `EvalTestKit`, `ArbitraryInstances`,
  `ArbitraryDerivation`, `ScenarioTest`.

### Checks

A report agent MUST perform each check below. `[P2-6]`

| ID | Check | Method |
|---|---|---|
| C1 | Behavior map | List the public behaviors of the main code. Map each to its tests. |
| C2 | Coverage gaps | Read the uncovered branches. Keep the ones that matter: error paths, empty input, boundaries, budget limits. |
| C3 | Test duplication | Find groups of tests that assert one behavior. |
| C4 | Property candidates | Find example groups that are instances of one law: roundtrip, oracle, idempotence, invariant. |
| C5 | Code duplication | Find copied setup, and local helpers that repeat a shared helper. |
| C6 | Weak assertions | For each assertion, name the code mutation that breaks it. Report the ones with no such mutation. |
| C7 | Cost | For each slow test, name the cause and a fix. |
| C8 | Generator fit | Report the cases that the cluster's generators never produce, and the code branches on. |
| C9 | Simplification | Find dead test code, values built by hand that a shared builder or `scalus.uplc.DSL` makes, and tests that are much longer than what they assert. |

### Report format

- A report agent MUST write one JSON file per cluster to
  `docs/internal/test-refactor/reports/<cluster>.json`, valid against
  `docs/internal/test-refactor/report.schema.json`. `[P2-7]`
- The coordinator MUST run `scripts/check_report.py` on each report and fix or discard what it
  rejects. It checks the schema, and that every `file:line` is a repository path to a line that
  exists. `[P2-8]`
- Each finding MUST name exactly one scope from the scope list. `[P2-9]`
- A report agent MUST NOT edit any file except its report. `[P2-10]`
- A verify agent MUST try to disprove each finding of scope `merge-duplicate-tests` or
  `to-property`. `[P2-11]`
- The coordinator MUST drop a finding that its verify agent disproves. `[P2-12]`
- The coordinator MUST log each `main-bug` finding in `findings.md` and set its `findingsLog`.
  `[P2-13]`

Fields of a finding, and what uses them:

| Field | Use |
|---|---|
| `id`, `check` | references in chat, commits and the backlog; which checks give value |
| `scope`, `module` | P3 groups findings into units: one scope in one module |
| `evidence` | `[P2-8]`; the verify agent starts from it |
| `claim`, `action` | the verify agent tests the claim; the P4 edit agent gets the action |
| `coverageRisk` | goal 2: what a deletion or merge can lose |
| `ciSavingSec`, `linesRemoved` | backlog order: goal 1 (CI time), then goals 4–5 (less code) |
| `confidence`, `verdict`, `verification` | trust; `verification` overrides `claim` and `action` when the verdict is `NARROWED` |

*Example, from the pilot report:*

```json
{
  "id": "T-16",
  "check": "C3",
  "scope": "merge-duplicate-tests",
  "module": "scalus-core",
  "evidence": ["scalus-core/shared/src/test/scala/scalus/uplc/transform/CseComputationSharingTest.scala:43-46",
               "scalus-core/shared/src/test/scala/scalus/uplc/transform/CommonSubexpressionEliminationTest.scala:73-77"],
  "claim": "'still rejects duplicated small values' asserts the same behavior as 'should not extract work-free terms'",
  "action": "Move tests 15-41 into CommonSubexpressionEliminationTest; delete 43-46 and the empty file",
  "coverageRisk": "none: both pin 'a small duplicated constant is not shared'",
  "ciSavingSec": 0,
  "linesRemoved": 25,
  "confidence": "medium",
  "verdict": "CONFIRMED"
}
```

**Why:** `[P2-11]`: report agents are confident and sometimes wrong. A wrong deletion loses
coverage silently. `[P2-13]`: main code is out of scope, but a defect found in passing must not
get lost; the pilot found two (F-33, F-34).

## P3 – Spec

**Purpose:** turn the reports into rules and an ordered backlog.

- The Fable reviewer MUST write a test style guide to `docs/internal/test-refactor/style-guide.md`. `[P3-1]`
- The Fable reviewer MUST write a catalog of approved shared helpers to
  `docs/internal/test-refactor/helpers.md`. `[P3-2]`
- The Fable reviewer MUST merge `extract-helper` and `adopt-helper` findings across clusters. `[P3-3]`
- The Fable reviewer MUST write the backlog to `docs/internal/test-refactor/backlog.md`. `[P3-4]`
- The backlog MUST be grouped by scope first, then by module. `[P3-5]`
- *Withdrawn 2026-10-27:* `[P3-6]` (order scopes by CI time saved). P1 took the large CI wins;
  the 761 remaining findings save 61 s in total, so that order carries no information.
- The owner MUST approve the backlog before P4 starts. `[P3-7]`
- The backlog MUST order the scopes as follows. `[P3-8]`

  | Step | Scope | Reason for the position |
  |---|---|---|
  | 1 | `extract-helper` | Later steps use the new helpers. This step also implements the budget-pin API and its update tool. |
  | 2 | `adopt-helper` | Needs step 1. |
  | 3 | `simplify` | Largest shrink (about 15k lines). |
  | 4 | `merge-duplicate-tests` | About 7k lines. `[AG-8]` protects the assertions. |
  | 5 | `budget-pin` | Pins go on the tests that survived steps 3 and 4. |
  | 6 | `generator` | Property tests need good generators. |
  | 7 | `to-property` | Needs step 6. |
  | 8 | `strengthen-assertion` | Only on the tests that survived. |
  | 9 | `fill-gap` | New tests come last, in the final style. |

- The Fable reviewer MUST fold each `ci-time` and `style` finding into the earliest unit, in
  `[P3-8]` order, that touches the same file. A finding with no such unit MUST go to the unit of its module in step 3. `[P3-9]`
- `helpers.md` MUST show, for each new or changed helper: its signature, one before/after
  example from a real test, and the number of call sites it replaces. `[P3-10]`
- The owner MUST approve each helper in `helpers.md` before any unit applies it. `[P3-11]`
- The owner approves `helpers.md` in steps D1 to D8, one decision at a time. Each step has a
  decision note, then a pilot on one or two suites, then the rollout as backlog units. `[P3-11a]`

  | Step | Topic | Pilot |
  |---|---|---|
  | D1 | Pin core: `Pin`, the per-suite budget table, the update command, the failure message | `MathTest` |
  | D2 | Pins in prelude and stdlib assertions, labels | part of `ListTest` |
  | D3 | Pins of validators and transactions, tx fee, CAPE | `AmmTest`, `BettingValidatorTest` |
  | D4 | One assertion family, one `Benchmark` tag, Emulator setup helpers | 2 to 3 suites |
  | D5 | Generators (`ArbitraryInstances`) | `ValueTest` |
  | D6 | Ledger rules kits | 2 cert suites |
  | D7 | Compiler and SIR test DSLs | 1 lowering suite |
  | D8 | Module-local kits | per kit |

  *Decisions (2026-10-29), D1:* a generated table per suite (`<Suite>Budgets.scala`), `Pin` as an
  enum `Eval | Tx | Size`, numbers in groups of three (`1_101`), the compiler generation read by a
  macro at the table's compile site.

  *Decisions (2026-10-29), D2:* the label of a pin is the source text of the evaluated code,
  whitespace collapsed, and an explicit label overrides it; failures are never pinned (`[BP-1c]`);
  `EvalTestKit` mixes in `BudgetPinsSuite`; the old `assertEvalWithBudget*` methods keep working
  until the `budget-pin` units migrate their callers, and the last unit deletes them.

  *Decisions (2026-10-30), D2 revision:* `assertEvalEq` takes the expected value first:
  `assertEvalEq(expected)(code)`, or `assertEvalEq(expected, arg = a)(code)` for a function of a
  Data argument; the legacy order `assertEvalEq(code, expected)` stays for the published testkit.
  An explicit label is a scope, `pinLabel("sum") { ... }`, not a parameter: a `label: String`
  parameter made `assertEvalEq(code, expected)` with a `String` result resolve to the wrong
  overload. The label macro lives in scalus-core main, `scalus.testing.internal` (F-91).

  *Decisions (2026-10-30), D2 split:* two forms. `assertEvalEq(expected)(code)` checks correctness
  of a closed expression and never pins (`[BP-1d]`). `assertEvalEq(expected)(f).at(args)` applies a
  function of 1 to 3 arguments to runtime Data arguments, checks the result and pins the budget;
  the arguments come after the code, so the code types them (`(min).at(1, 2)` needs no `BigInt`).
  A test that never calls `.at` on such a check fails. `MathTest` drops `optimizeUplc = false`.
  `test(...) { }` uses braces; a `:` block is for a long code argument.

  *Decisions (2026-10-30), D3:* `ScalusTest` gets the budget pins: `checkResult(expected, result,
  program)` pins a successful validator run as `Pin.Eval` (the program's size, the combined fee);
  `emulator.submitPinned(tx)` pins the transaction under test as `Pin.Tx`; `emulator.submitOk(tx)` submits a
  setup transaction without a pin (`[BP-4a]`); `pinSize(bytes)` pins a script size. Failing runs are
  never pinned. A test name that embeds a measured value becomes a fixed name plus `pinSize`.

  *Decisions (2026-11-01), D3 revision:* one family of names. `assertSuccess(result, program)` pins
  a successful run, `assertSuccess(result)` does not; `assertFailure(result, message)` checks the
  log message; `assertOutcome(result, expectedError)` serves tests that take the outcome as a
  parameter. `emulator.submitOk(tx)` asserts acceptance and pins the transaction under test,
  `emulator.submitSetup(tx)` asserts acceptance without a pin, `emulator.submitFails(tx, message)`
  asserts a rejection. `buildFails(message)(tx)` replaces `assertScriptFail`. `checkResult`,
  `success`, `failure`, `assertScriptFail` and `compareBudgetWithReferenceValue` are deprecated
  since 1.5.0; the reference budgets of the benchmarks stay as comments. `EmulatorBase.initialUtxosOf`
  is `private[node]`; `Emulator.withOutputs(outputs: TransactionOutput*)` is the public form.

  *Decisions (2026-10-30), D4:* `EvalTestKit` and `ScalusTest` are the only assertion family. The
  entry points of `EvalTestDsl`, `ResultAssertions`, `Evaluator` and `BaseValidatorTest(+Jvm)` are
  `@deprecated` since 1.5.0 and go in the next release. One `scalus.testing.Benchmark` tag lives in
  core test sources and is copied into the testkit. `Emulator.withOutputs(Output(address, value), ...)`
  (ledger main, JVM and JS) builds an Emulator with one UTxO per output; `withAddresses` stays for
  equal values. The local copies of `genesisHash`, `given Options` and `given PlutusVM` go to the
  `adopt-helper` rollout units, not to D4.

  *Decisions (2026-11-21), D5:* the owner approved every recommendation of
  `docs/internal/test-refactor/d5-generators.md`. One `given Arbitrary` per type generates
  well-formed values; every other purpose is a named `Gen` (`gen<Role>`, `genAny`, `genInvalid`,
  `genMalformed…Bytes`). `Arbitrary[v1.Value]` is non-negative; `genMintValue` and `genAnyValue`
  are signed. Scalars mix 50 % realistic core, 25 % listed edges, 25 % wide part by bit length.
  Pooled keys only for the `Value` policy ids and asset names. Transaction-level collections: 1 to
  4 in 90 % of samples, 5 to 16 in 10 %. Run count stays 10; an off-chain law first runs
  `forAll(Table(edges))`, an on-chain law takes its edges through the `Table` form of `checkEval`.
  The quality gate is a seeded CI test (`ArbitraryInstancesQualityTest`); GenStats stays the report.

  *Decisions (2026-11-24), D6:* the owner approved every recommendation of
  `docs/internal/test-refactor/d6-rules-kits.md`. The certificate suites share a `CertsTestKit`
  without generators, with one context builder by slot. `ValidatorRulesTestKit` bounds the units
  of generated redeemers to `maxTxExecutionUnits`. Example tests use fixed values and `Party`
  keys; a law test is a `forAll`. H-50 stays in D6; H-38 moves to D8. `genValidTx` moves to the
  `to-property` step. The `ArbitraryDerivation.split` fix waits until D6 has seeded the rule
  suites. The fee code keeps exact `BigInt` inside the rules and the throw at `Coin`.
- The Fable reviewer MUST choose the helper API from what the code base already has, plus
  what the findings ask for. It MUST name the existing helper that each new helper replaces or
  extends. `[P3-12]`
- `helpers.md` MUST define the budget-pin API that `[BP-1]` to `[BP-8]` require. `[P3-13]`

*Example, illustrative, for `[P3-3]`:* five clusters each build an Emulator with a funded
party. P3 creates one `extract-helper` unit for it, not five.

### Method

1. A script writes `docs/internal/test-refactor/p3/findings-by-scope/<scope>.jsonl`: every P2
   finding except `main-bug`, with its cluster, module and verdict. The 42 reports (1.8 MB) are
   too large for one agent.
   - The script also writes `helper-mentions.jsonl`: findings of any scope whose `action` names a
     helper or asks for one.
   - The script joins findings across scopes and clusters by evidence path and by the def names
     of `repeated-defs.csv`, into `cross-cluster.jsonl`.
   - A second script writes `budget-inventory.csv`: every on-chain evaluation site (file, line,
     suite, test) with its pin state: `none`, `upper-bound`, `exact`, `exact+fee`, `size`,
     `tx-fee`, `forced-fee` or `property`.
2. Agent H (Fable) reads the `extract-helper`, `adopt-helper` and `simplify` digests, the helper
   inventory (`shared-helpers.csv`, `repeated-defs.csv`), `helper-mentions.jsonl`,
   `cross-cluster.jsonl`, `budget-inventory.csv` and the existing helper sources. It writes
   `helpers.md`, including the budget-pin API and where it lives (core test sources, or the
   published `scalus-testkit` main, which MiMa and the interop rules govern).
3. The owner reviews `helpers.md` `[P3-11]`.
4. Agent S (Fable) reads the `style` digest, `helpers.md`, the `lanthorn:writing-tests` and
   `scalus:contract-test` skills, and samples of the best and worst tests the reports name. It
   writes `style-guide.md`.
5. A script groups the remaining findings into draft units by scope and module, with the size
   limit of `[P4-3]`.
6. Agent B (Fable) turns the draft units into `backlog.md`: it merges duplicate findings, folds
   `ci-time` and `style` per `[P3-9]`, builds the `budget-pin` units from `budget-inventory.csv`,
   and orders per `[P3-8]`.
7. The owner approves the backlog `[P3-7]`.

**Why:** `[P3-8]`: shrinking first avoids work on tests that are deleted anyway, and new tests are
written once, in the final style. The cost is one gap in the safety net: the coverage gate sees a
lost statement, not a weaker assertion. `[AG-8]` closes that gap. `[P3-11]`: a helper used by
hundreds of call sites is expensive to change after the units apply it.

## Budget pins

**Purpose:** catch budget drift of on-chain code, and record each improvement.

Definitions for this section:

| Term | Meaning |
|---|---|
| on-chain test | A test that evaluates UPLC compiled from Scalus code: a validator, or a prelude or stdlib function. |
| budget pin | An exact expected value of memory, CPU steps and fee for one evaluation. |
| execution fee | `ExUnits.fee(CardanoInfo.mainnet.protocolParams.executionUnitPrices)` (ceiling). The prices come from the checked-in snapshot `blockfrost-params-epoch-645.json`. A snapshot change updates every fee pin in one commit, with the tool. |
| combined fee | The execution fee plus the Conway tiered reference-script fee of the script's size (`RefScriptFee.fee`, mainnet `minFeeRefScriptCostPerByte`). Model: one evaluation per transaction, with the script as a reference script. |

- Each on-chain operation that validators call MUST have at least one pinned evaluation with
  runtime inputs: `assertEvalEq(expected)(f).at(args)`. `[BP-1]`
- A closed expression, `assertEvalEq(expected)(code)`, MUST NOT be pinned. `[BP-1d]`
- An evaluation whose expected result is a failure MUST NOT be pinned. `[BP-1c]`
- An evaluation with ScalaCheck inputs is exempt from `[BP-1]`. A property test SHOULD keep one
  fixed-input case with a pin. `[BP-1a]`
- A differential test that asserts one budget equals another (for example JIT = CEK) is exempt
  from `[BP-1]`. `[BP-1b]`
- A budget pin MUST compare exact values. It MUST NOT be an upper bound. `[BP-2]`
- A budget pin of an evaluation MUST include memory, CPU steps, the script size in bytes, and the
  combined fee. `[BP-3]`
- The failure message of an evaluation pin MUST split the combined fee into its execution and
  reference-script parts. `[BP-3a]`
- A test that builds and balances a transaction MUST pin the transaction under test: the sum of
  its redeemer budgets, its size as the fee rule counts it (`toCborForFeeCalculation`), and
  `tx.body.value.fee`. A fee that the test forces is not a pin. `[BP-4]`
- A setup transaction MUST NOT be pinned. `[BP-4a]`
- The failure message of a transaction pin MUST split the fee into its size part, its execution
  part and the rest (reference scripts and balancer margin). `[BP-4b]`
- A test of a validator SHOULD also pin the script size in bytes. `[BP-5]`
- When the pinned values differ between compiler generations, the pin MUST hold one value per
  generation. `[BP-6]`
- The compiler generation MUST be readable on every platform and project that runs a pinned
  test. `Generation.current` reads it at compile time, so it needs no build setting. `[BP-6a]`
- A pin that checks several `Options` MUST hold one value per `Options`. `[BP-6b]`
- On a mismatch, the failure message MUST show the expected and actual values and the
  difference in percent, for each pinned quantity. It MUST show the pin key, and the percent
  with one decimal and a sign, relative to the expected value. `[BP-7]`
- A tool MUST be able to update every pin from a test run, without manual edits. `[BP-8]`
- Each pin MUST have a key that is unique in its suite: the test name, plus a label when a test
  pins more than one evaluation. The tool MUST update by key, never by value. `[BP-8a]`
- The tool MUST run on the JVM. JS and Native runs MUST only check the pins. `[BP-8b]`
- A pin whose key no test uses MUST fail the suite. A test that evaluates on-chain code and finds
  no pin for its key MUST fail. `[BP-8c]`
- A commit that changes a pin MUST state the old and new values, or the total change, in its
  message. `[BP-9]`

*Example, illustrative, for `[BP-2]` and `[BP-7]`:* a pin of `memory = 25 694`, `steps = 4 809 547`
fails when an optimization lowers memory to `25 100`, with the message
`memory 25 694 -> 25 100 (-2.3 %)`. The developer updates the pin, and the commit message records
the improvement.

**Why:** `[BP-1]`, `[BP-1d]`: the Inliner's partial evaluation runs the CEK machine on a closed
expression at compile time and folds it to a constant. The result check still holds, but the budget
is the cost of a constant (memory 200, steps 16_100; 60 of 71 closed pins in `ListTest`). Whether
an expression folds depends on the optimizer, so a pin on it would turn into noise. Runtime
arguments arrive as Data and cannot fold.

`[BP-3]`: optimizer decisions trade script size against execution (inlining, unrolling, constant
pooling); size is most of a script's fee. A pin on the execution fee alone would report an unrolling
as an improvement while the transaction gets more expensive.

`[BP-2]`: an upper bound catches a regression but hides an improvement; the owner wants
both visible. `[BP-3]`: the owner compares on fee and memory, not on CPU. `[BP-5]`: script size is
most of the script fee. `[BP-8]`: `scripts/update-budgets.py` rewrites `ExUnits` literals by
value, so two tests with the same literal overwrite each other's pin: `ValueTest` has
`ExUnits(memory = 500, steps = 64100)` at 6 sites. `[BP-8c]` makes a lost pin (`[AG-9]`) visible
without review.

*Implementation status (2026-10-27):* `EvalTestKit` has `assertEvalWithBudgets` and
`assertEvalWithBudgetAndFee`. There are 809 `ExUnits(...)` literals in 79 test files; some are
inputs, not pins (`PlutusScriptsTransactionMutatorTest`, `ExUnitsTest`). Fee pins exist in about
18 places. `[BP-8]` to `[BP-8c]` are not met. Fable's review (2026-10-27) recommends a generated,
committed Scala table per suite (`<Suite>Budgets.scala`, a map from key to pin, one column per
generation), rewritten by a JVM run with `-Dscalus.budgets.update=true`. `helpers.md` decides.

## P4..N – Implement

**Purpose:** apply the backlog one unit at a time, with proof of no coverage loss.

### Units and concurrency

- Each unit MUST cover one scope in one module. `[P4-1]`
- Each unit MUST live on its own branch in its own git worktree. `[P4-2]`
- A unit SHOULD change at most about 800 lines. A larger unit MUST split by package. `[P4-3]`
- A `budget-pin` unit MAY split by file. Then `[P4-4]` means one commit per file. `[P4-3a]`
- Two active units MUST NOT change the same suite or its budget table. `[P4-5a]`
- Each unit MUST have one commit per cluster. `[P4-4]`
- At most 2 units MAY be active at one time. `[P4-5]`
- The coordinator MUST run `sbtn shutdown` in the worktree when its unit ends. `[P4-6]`

**Why:** `[P4-5]`: the machine has 36 GB of RAM. Each sbt server takes 8 GB, and the owner keeps
one in the primary checkout. `[P4-3]` and `[P4-4]`: the owner reviews each unit in one sitting,
commit by commit.

### Loop per unit

1. Edit agents edit the files of the unit in parallel. Each agent owns a disjoint file set.
2. The coordinator runs `Test/compile` once per batch. It sends each error to the agent that
   owns the file.
3. The coordinator runs `testOnly` on the changed suites.
4. The coordinator runs the coverage gate.
5. If the unit changed `shared/` tests, the coordinator runs `scalusJS/Test/compile` and
   `scalusNative/Test/compile`.
6. The coordinator runs `scalafmtAll`, commits, and runs `/code-review`.
7. The owner reviews the branch.
8. The coordinator rebases the branch on master, fast-forward merges it, and removes the worktree.

### Coverage gate

- The coordinator MUST run scoverage on the touched packages of each unit. `[CG-1]`
- The coordinator MUST compare the result with the coverage baseline. `[CG-2]`
- A unit with a lost statement or a lost branch MUST NOT merge. `[CG-3]`
- The gate MUST ignore the packages that `[P0-7]` lists. `[CG-4]`

  *Implementation status (2026-10-21):* met by the file exclusion of `[P0-7]`. The gate also
  ignores `baseline/flaky.tsv` (F-11, F-12).

### Rules for edit agents

- An edit agent MUST change test sources only, unless the backlog item names main code. `[AG-1]`
- An edit agent MUST NOT run sbt. `[AG-2]`
- An edit agent MUST follow the style guide and the helper catalog. `[AG-3]`
- An edit agent MUST NOT add a shared helper that no backlog item names. `[AG-4]`
- If a test passed before the refactor and fails after it, the edit agent MUST stop and
  report. `[AG-5]`
- A bug found that way MUST become a separate `fix:` commit, outside the refactor
  commits. `[AG-6]`
- Each commit that deletes a test MUST name, in its message, the test that now covers the
  behavior. `[AG-7]`

- When an edit agent merges tests, the merged test MUST keep every assertion of each source
  test, except one that a kept assertion implies. Exact equality implies any predicate on the
  same value. `[AG-8]`
- When an edit agent merges or deletes tests, it MUST keep every budget pin of the source
  tests. A `to-property` unit MUST keep one fixed-input case with a pin per source pin. `[AG-9]`
- An edit agent MUST treat the `action` of an `UNVERIFIED` finding as a hypothesis. It MUST
  confirm the `claim` on the `evidence` lines before it edits. If the claim does not hold, it
  MUST skip the finding and report it. `[AG-10]`

**Why:** `[AG-5]`: a new property test can find a real bug. A silent "fix" of the test would
hide it. `[AG-7]`: the owner can check each deletion against the coverage diff. `[AG-10]`: 593 of
the 810 P2 findings were not verified. `[AG-8]`: the
coverage gate does not see a weaker assertion. `[AG-9]`: a lost pin hides future budget drift.

## Execution order

1. P0: `[P0-1]` to `[P0-3]` (timings).
2. P0: scoverage spike `[P0-4]` to `[P0-7]`.
3. P1, in one worktree. It runs in parallel with the rest of P0.
4. P0: coverage baseline, helper inventory, generator statistics.
4b. P0b: profiling `[PR-1]` to `[PR-13]`. Its `fix-now` items go to P1 or to their own commits.
5. P2 pilot, then owner review `[P2-4]`.
6. P2 for the remaining clusters, after owner approval of the agent count `[P2-5]`.
7. P3, then owner approval `[P3-7]`.
8. P4 units in backlog order.
