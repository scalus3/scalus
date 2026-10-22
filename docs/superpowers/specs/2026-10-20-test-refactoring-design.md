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
- The backlog MUST order scopes by estimated CI time saved, largest first. `[P3-6]`
- The owner MUST approve the backlog before P4 starts. `[P3-7]`

*Example, illustrative, for `[P3-3]`:* five clusters each build an Emulator with a funded
party. P3 creates one `extract-helper` unit for it, not five.

## P4..N – Implement

**Purpose:** apply the backlog one unit at a time, with proof of no coverage loss.

### Units and concurrency

- Each unit MUST cover one scope in one module. `[P4-1]`
- Each unit MUST live on its own branch in its own git worktree. `[P4-2]`
- A unit SHOULD change at most about 800 lines. A larger unit MUST split by package. `[P4-3]`
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

**Why:** `[AG-5]`: a new property test can find a real bug. A silent "fix" of the test would
hide it. `[AG-7]`: the owner can check each deletion against the coverage diff.

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
