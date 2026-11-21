# CI speed and cache – design

area: ci

Date: 2026-11-21. Status: draft, not built.

## 1. Overview

*Informative.* Each push to the repository starts five push jobs: CI-JVM for three Scala
versions, CI-JS and CI-Native. Each job installs Nix, restores its caches, enters the `ci` dev
shell, and runs one sbt alias. Each alias starts with `clean`, so sbt compiles and tests all
from scratch, and the slowest job takes 15 minutes.

After this work, a push job restores the sbt task cache of the last master run. sbt then takes
each compile and each passing test suite whose inputs did not change from that cache. A push
that changes only tests or docs runs only the affected suites. A nightly job still runs `clean`
and `testFull` for each configuration, so a wrong cache hit cannot hide a failure for more than
a day.

The caches also get smaller. Each cache kind keeps one entry per job, and the Nix store keeps
only the paths of the dev shell. So the cache of the repository stays under the GitHub limit,
and no entry evicts another one that a job needs.

## 2. Goals and non-goals

*Informative.*

Goals:

- A CI-JVM or CI-Native job of a push that changes no main source finishes in a few minutes,
  not 15.
- CI-JS gets the same gain in sbt, but it still runs its npm and browser tests on each push.
  `runNpmTests` and `checkDtsUpToDate` are uncached tasks, and the browser tests run outside
  sbt. Making them cacheable is out of scope. *Decision of the developer, 2026-11-21.*
- The GitHub Actions cache of the repository stays under its 10 GB limit.
- Each workflow uses the same cache setup, so one change applies to all of them.
- The Nix part of a job gets smaller and faster.
- A measured answer on nix-direnv for jobs with more than one step.

Non-goals:

- A remote build cache (Bazel gRPC). The owner chose the GitHub Actions cache on 2026-11-21.
- A self-hosted runner.
- Quality gates (`-Werror`, scalafix, a commit message check, dependency updates). They get
  their own design after this one (section 10).
- A change to `release.yml` beyond the cache actions. The release builds from a clean tree.
- Fewer or faster tests. The test refactoring program owns that.

## 3. Phases

Each phase lands on its own, in this order.

| Phase | Content | Gain |
|---|---|---|
| 1 | Cache reduction: Nix store with GC, one dependency cache, cleanup of old entries | Cache under the limit; no gain in time |
| 2 | Task cache and test reuse: no `clean` on push, `test` instead of `testFull`, nightly full run | Most of the gain |
| 3 | Job shape: format check, MiMa and docs out of the 3.3.9 job | 1.5 to 2 minutes of wall time |
| 4 | Nix shell: smaller closure, nix-direnv measurement | Up to 1 minute per job |

Phase 1 comes first, because phase 2 adds large entries to a cache that is full today.

## 4. Measured state

Measured on 2026-11-21 on master, run 44907842256 (CI-JVM) and its sibling runs.

| Job | Duration |
|---|---|
| CI-JVM 3.3.9 | 15.1 min |
| CI-JVM 3.9.0 | 13.7 min |
| CI-JVM 3.10.1 | 13.4 min |
| CI-JS | 14.1 min |
| CI-Native | 10.5 min |

The CI-JVM 3.3.9 job:

| Phase | Time |
|---|---|
| Checkout and cache restore | 25 s |
| `nix develop .#ci`: flake evaluation and 727 store paths copied | 60 s |
| sbt start and build load | 40 s |
| `clean`, format check, compile | 3.5 min |
| `jvm/testFull` | 8 min |
| MiMa, `jvm/doc`, `docs/unidoc` | 1.5 min |

The GitHub Actions cache of the repository: 13.4 GB in 1206 entries, against a limit of 10 GB.

| Owner | Size | Entries | Cause |
|---|---|---|---|
| `coursier/cache-action`, coursier part | 6.4 GB | 17 | One key per job and per matrix entry, 285 to 677 MB each; two generations kept |
| `coursier/cache-action`, `sbt-ivy2` part | 3.3 GB | 19 | `~/.ivy2/cache` and `~/.sbt`, per job |
| `magic-nix-cache-action` | 2.2 GB | 1166 | One entry per store path; nothing removes a path that no shell uses |
| Lean-Proofs workspace | 0.7 GB | 1 | – |

Other facts:

- Of the last 60 commits on master, 34 change only tests or docs, 8 change a leaf module, 14
  change `scalus-core` or `scalus-plugin` main sources, and 4 change the build.
- A developer machine with sbt 2.1: `~/.sbt/boot` holds `scala-3.8.4` and `scala-3.9.0`, the
  boot directories of the sbt 2 launcher. `~/.sbt/1.0` holds the state of sbt 1.
- The flake evaluation downloads haskell.nix and hackage.nix sources. They come from the
  `plutus` input, which gives the `uplc` binary and the `plutus-conformance` corpus.
- `scalus.utils.Macros` and `CekBuiltinsJsTest` run the `uplc` binary.
- The CI-JVM 3.9.0 job built `nodejs` on the runner: the Nix cache did not hold it.

Measured on 2026-11-21, branch run 44931794907 (step 1 of 6.5), on an empty Nix store:

| Fact | Result |
|---|---|
| Closure of the `ci` shell (`nix path-info -S`) | 5.1 GB |
| Flake inputs, sum of `nix flake archive` paths (measured locally) | 4.3 GB; the largest three are 1.2, 0.7 and 0.6 GB |
| `/nix/store` with the shell and the inputs | 13 GB |
| `nix develop .#ci` from the public binary caches only | 1.3 min |
| GC roots for the flake inputs (`nix-store --realise`) | 3.3 min more |

## 5. Approaches

### Decision: Keep the sbt task cache in the GitHub Actions cache

Status: Accepted
Area: ci
Context: sbt 2 stores each task result under a hash of its inputs. CI must carry that store
from one run to the next.
Options:
- `actions/cache` of `~/.cache/sbt`: no account, no secret. Each entry counts against the 10 GB
  limit, and jobs do not share entries.
- A Bazel gRPC remote cache (BuildBuddy): jobs share entries, no quota. It needs an account, a
  secret, and a third party that stores build outputs.
- A self-hosted bazel-remote: full control. Someone must run and pay for a server.
Chosen: `actions/cache`, by the owner on 2026-11-21, together with the cache reduction of
phase 1.
Consequences: Adds the rules of section 7. Phase 1 must free the space first.

### Decision: Replace magic-nix-cache with a GC-rooted store cache

Status: Accepted
Area: ci
Context: `magic-nix-cache-action` saves each store path as its own entry and never removes one.
Options:
- Keep it and delete old entries by a script: the action gives no list of the paths in use.
- `nix-community/cache-nix-action`: one entry per flake state. It collects garbage before the
  save, so only rooted paths stay.
- A Cachix or Attic binary cache: no quota, but an account and a secret.
Chosen: `cache-nix-action`, because it needs no account and its size follows the dev shell.
Consequences: Adds the rules of section 6.1.

### Decision: Reuse cached test passes on push, run all tests nightly

Status: Accepted
Area: ci
Context: tests are 8 of 15 minutes. sbt 2 `test` skips a suite whose inputs did not change
since a passing run; `testFull` and `testOnly` run every suite.
Options:
- `test` on push, `clean` and `testFull` nightly: most of the gain. A wrong cache hit hides a
  failure until the next night.
- `testFull` on push, cache for compile only: safe. The job stays near 11 minutes.
Chosen: `test` on push, by the owner on 2026-11-21.
Consequences: Adds the rules of section 7.2 and 7.3.

## 6. Phase 1 – cache reduction

### 6.1 Nix store

- Each workflow that enters a dev shell MUST use `nix-community/cache-nix-action` and MUST NOT
  use `magic-nix-cache-action`. `[ci.nix.one-action]`
- The primary key of the Nix store cache MUST hold the runner OS, the name of the dev shell and
  a hash of `flake.nix` and `flake.lock`. `[ci.nix.key]`
- The restore step MUST fall back to the newest entry of the same OS and dev shell.
  `[ci.nix.restore-prefix]`
- Exactly one job per dev shell and runner architecture MUST save the Nix store cache.
  `[ci.nix.one-saver]`
- Each other job MUST use the restore-only form of the action. `[ci.nix.restore-only]`
- The saving job MUST add a GC root for the dev shell and for each flake input before the save.
  `[ci.nix.gc-roots]`
- The saving job MUST collect every path without a GC root before the save. `[ci.nix.gc-before-save]`
- A job MUST save the Nix store cache only on a push to master. `[ci.nix.save-on-master]`
- The saving job MUST delete the older Nix store entries of its dev shell with the purge
  inputs of `cache-nix-action`, not with a separate step. `[ci.nix.purge]`

**Why:** `[ci.nix.gc-roots]` and `[ci.nix.gc-before-save]`: after a change of `flake.lock` the
job restores the old store by prefix. Without the collection, the old paths go into the new
entry, and the entry grows with each update of the flake. `[ci.nix.one-saver]`: two jobs that
save one key race. `[ci.nix.key]`: the `ci-lean` shell holds a Z3 that the `ci` shell does not,
so the two shells need their own entries. `[ci.nix.save-on-master]`: a branch can read an entry
of master, but master cannot read an entry of a branch. `[ci.nix.purge]`: the action has
`purge`, `purge-prefixes`, `purge-primary-key` and `purge-created`, so it applies
`[ci.prune.after-save]` to the Nix store itself. Purge needs `actions: write` (`[ci.prune.permission]`).

*Example, illustrative, for `[ci.nix.purge]`.*

```yaml
- uses: nix-community/cache-nix-action@v7
  with:
    primary-key: nix-${{ runner.os }}-ci-${{ hashFiles('flake.nix', 'flake.lock') }}
    restore-prefixes-first-match: nix-${{ runner.os }}-ci-
    gc-max-store-size-linux: 0
    purge: true
    purge-prefixes: nix-${{ runner.os }}-ci-
    purge-created: 0
    purge-primary-key: never
```

*Example, illustrative.* The saving job for the `ci` shell is CI-JVM 3.3.9. CI-JS, CI-Native,
Nightly and Integration tests restore its entry. Lean-Proofs saves its own entry for
`ci-lean`.

### 6.2 Dependency cache

- Each workflow that runs sbt MUST use one `actions/cache` step for the sbt dependencies, and
  MUST NOT use `coursier/cache-action`. `[ci.deps.one-step]`
- The dependency cache MUST hold `~/.cache/coursier` and `~/.sbt/boot`. `[ci.deps.paths]`
- The dependency cache MUST NOT hold `~/.ivy2/cache` or `~/.sbt/1.0`. `[ci.deps.no-sbt1]`
- The key of the dependency cache MUST hold the runner OS, the month, and a hash of
  `build.sbt`, `project/*.sbt` and `project/build.properties`. `[ci.deps.key]`
- The key of the dependency cache MUST also hold the name of the job. `[ci.deps.key-job]`
- The restore step MUST fall back to the newest entry of the same OS, job and month.
  `[ci.deps.restore-prefix]`
- Each push job MUST save its dependency cache only on a push to master. `[ci.deps.save-on-master]`

**Why:** `[ci.deps.one-step]`: `coursier/cache-action` saves two caches per job, and keeps
two generations of each. `[ci.deps.key-job]`: each job resolves other dependencies: other
compilers, the Scala.js linker, the Scala Native toolchain. One shared entry from one job
leaves the other jobs to download theirs on each run. With `[ci.prune.after-save]`, five
entries of about 300 to 700 MB stay, against 9.7 GB today. *Decision of the developer,
2026-11-21.* `[ci.deps.no-sbt1]`: sbt 2 resolves
through coursier; the two paths hold sbt 1 state (section 4). `[ci.deps.key]`: a restore by
prefix adds new files to old ones, so the entry grows with each change of the build. The month
in the key starts a new entry each month, without the old files.

*Example, illustrative.* A push on 2026-12-03 that changes `build.sbt` restores the newest
`deps-Linux-ci-js-2026-12-` entry, downloads the new dependencies, and saves a new entry under the
new hash. On 2027-01-02 the first master run finds no `deps-Linux-ci-js-2027-01-` entry. It
downloads every dependency once.

*Spike result needed.* The first run checks that `~/.ivy2/cache` stays empty, and that sbt
does not download its launcher again when `~/.sbt/boot` is restored. When one of the two fails,
the developer changes `[ci.deps.paths]` and records it here.

### 6.3 Scope and triggers

- `ci-jvm.yml`, `ci-js.yml`, `ci-native.yml`, `it.yml`, `nightly.yml`, `release.yml` and
  `lean-proofs.yml` MUST follow the rules of 6.1. `[ci.scope.all-nix]`
- `crypto-jni.yml` and `secp256k1-jni-release.yml` MUST NOT use a Nix store cache action.
  `[ci.scope.rare-no-nix-cache]`
- The rules of 6.2 MUST apply to each workflow that runs sbt in a dev shell. `[ci.scope.sbt]`
- `deploy-site.yml` MUST keep its present setup. `[ci.scope.deploy-site]`
- `ci-jvm.yml`, `ci-js.yml` and `ci-native.yml` MUST have a `workflow_dispatch` trigger.
  `[ci.scope.dispatch]`

**Why:** `[ci.scope.all-nix]`: these workflows enter the `ci` or the `ci-lean` shell.
`[ci.scope.rare-no-nix-cache]`: the two workflows run only on a change of the crypto libraries,
on up to 5 platforms, each with its own shell. Their entries would expire after 7 days without
use, and until then they take space from the entries of the push jobs. Without a cache they copy
the store from the binary caches of `nixConfig`, which `magic-nix-cache-action` also falls back
to today. `[ci.scope.deploy-site]`: it does not use Nix.
`[ci.scope.dispatch]`: the three push workflows ignore `.github/workflows/**`, so a commit that
changes only a workflow starts no run. A manual run tests that commit. *Decisions of the
developer, 2026-11-21.*

### 6.4 Removal of old entries

- After a job saves an entry, the job MUST delete each older entry of the same cache kind and the
  same job. `[ci.prune.after-save]`
- For the dependency cache and the task cache, a step after the save MUST do the deletion with
  `gh cache delete`. `[ci.prune.gh-delete]`
- The workflow MUST grant `actions: write` only to the job that deletes entries.
  `[ci.prune.permission]`
- The developer MUST delete all `coursier-*`, `sbt-ivy2-*` and magic-nix-cache entries once,
  after phase 1 lands on master. `[ci.prune.one-time]`
- The developer MUST ask the owner before the deletion of `[ci.prune.one-time]`.
  `[ci.prune.ask-first]`

**Why:** GitHub removes an entry only after 7 days without use, or when the cache passes its
limit. Then it removes the least recently used entries first, which can be the entry that the
next job needs. `[ci.prune.after-save]` keeps one entry per cache kind and job, so the cache
never reaches the limit.

*Example, illustrative.* A master push saves `sbt-task-Linux-ci-js-2026-12-<sha2>`. The same
job then deletes `sbt-task-Linux-ci-js-2026-12-<sha1>`. It keeps the entries of the other
jobs.

### 6.5 Phase 1 steps

Each step is one commit on the branch of this work, unless it says otherwise.

*Test runs.* A branch run cannot save under `[ci.nix.save-on-master]`, and `ci-js.yml` and
`ci-native.yml` get `workflow_dispatch` only when it reaches master. So, until step 8, the save
condition also accepts the work branch, and a branch push that also changes a file outside
`.github/workflows/` starts the runs. Step 8 removes the branch from the save condition. The
branch push needs a yes from the owner.

1. **Measure the Nix store.** On a branch run of CI-JVM 3.3.9, print the size of the store
   closure of `.#ci` and of the flake inputs (`nix path-info -rS`), and the compressed size of
   one `cache-nix-action` entry. Record both in section 4.
   Check: when the entry of `ci` and the entry of `ci-lean` together pass 4 GB, stop and ask.
2. **Add `scripts/ci/prune-caches.sh <prefix> <keep-key>`.** It lists the entries whose key
   starts with the prefix (`gh cache list --key`), and deletes each one but `<keep-key>`. It
   needs only `bash` and `gh`.
   Check: a dry run (`--dry-run`) on master's entries prints the keys it would delete and
   deletes none.
3. **Change `ci-jvm.yml`.** Add `workflow_dispatch` where it is missing. Replace
   `magic-nix-cache-action` and `coursier/cache-action` by the steps of 6.1 and 6.2. The
   3.3.9 entry saves the `ci` store; each matrix entry saves its own dependency cache and runs
   the prune script. Give `actions: write` to these jobs only.
   Check: two runs on the branch. The second run restores both caches by exact key, the
   first run's older entries are gone, and its "Build and test" step is not slower than on
   master.
4. **Change `ci-js.yml` and `ci-native.yml` the same way, restore-only for the Nix store.**
   Check: one run each, green, with the Nix store restored from the 3.3.9 entry.
5. **Change `it.yml`, `nightly.yml`, `release.yml`.** Restore-only for the Nix store and the
   dependency cache; no save.
   Check: one dispatch run of `nightly.yml` and of `it.yml`, green.
6. **Change `lean-proofs.yml`.** It saves the `ci-lean` store on master, with its own prefix.
   Check: one dispatch run, green.
7. **Change `crypto-jni.yml` and `secp256k1-jni-release.yml`.** Remove `magic-nix-cache-action`.
   Check: one dispatch run of `crypto-jni.yml`, green.
8. **Land on master.** Remove the work branch from the save conditions in one commit. Then
   fast-forward master, push, and check every push workflow of that commit.
9. **Purge the old entries.** Ask the owner first (`[ci.prune.ask-first]`). Then delete each
   entry whose key starts with `coursier-`, `sbt-ivy2-`, `determinatesystem-` or matches the
   magic-nix-cache form `<hash>.nar…` or `<hash>.narinfo`.
   Check: `gh api repos/{owner}/{repo}/actions/cache/usage` reports less than 5 GB, and the
   next master run of each push workflow is green and restores its caches.

Phase 1 is done when step 9 passes. Record the numbers of steps 1, 3 and 9 in section 4.

## 7. Phase 2 – task cache and test reuse

### 7.1 Task cache

- Each push job MUST restore `~/.cache/sbt` with `actions/cache`. `[ci.task.restore]`
- The key of the task cache MUST hold the runner OS, the name of the job, the month and the
  commit. `[ci.task.key]`
- The restore step MUST fall back to the newest entry of the same OS, job and month.
  `[ci.task.restore-prefix]`
- Each push job MUST save its task cache only on a push to master. `[ci.task.save-on-master]`
- A job MUST NOT restore or save a `target/` directory. `[ci.task.no-target]`
- The sum of all cache entries MUST stay under 8 GB. `[ci.task.budget]`

**Why:** `[ci.task.key]`: the jobs build different Scala versions and platforms, so each job
has its own task results. The month bounds the growth: each run adds its new results to the
restored ones. `[ci.task.no-target]`: sbt then builds each project from a cache hit or from
scratch, never from incremental state of another run. Incremental state is the known cause of
stale SIR in the example validator tests. `[ci.task.budget]` leaves 2 GB of the limit for the
Lean workspace and for a branch run.

*Spike setup.* A branch run can restore an entry that an earlier run of the same branch saved.
So the spike runs on a branch `ci/task-cache-spike` whose workflows also save on that branch.
The first run fills the cache, the next runs make the changes of section 7.2. The branch is
deleted after the spike, and master gets the rules as written. *Decision of the developer,
2026-11-21.*

*Spike result needed.* The spike measures the size of `~/.cache/sbt` after a full run of each
job. When the sum of the five entries, the Nix store entries and the dependency entry passes
8 GB, the developer stops and asks. A fallback is a prune step that deletes each file of
`~/.cache/sbt` that the run did not read, by its access time.

### 7.2 Push aliases

- The sbt alias of each push job MUST NOT run `clean`. `[ci.alias.no-clean]`
- The sbt alias of each push job MUST run `test`, not `testFull`. `[ci.alias.test]`
- The sbt build MUST add a digest of `flake.lock` and of the value of `SCALUS_TEST_ENV` to the
  test inputs of each project, through `Test / extraTestDigests`. `[ci.alias.extra-digests]`
- The release workflow MUST run `clean` and MUST NOT restore the task cache.
  `[ci.alias.release-clean]`

**Why:** `[ci.alias.no-clean]`: after `clean`, sbt ignores the task cache for the next command
(AGENTS.md, sbt 2 rules). `[ci.alias.extra-digests]`: sbt does not see the
`plutus-conformance` corpus or `SCALUS_TEST_ENV` as test inputs. The corpus, the JDK and the
`uplc` binary all come from `flake.lock`, so its hash covers them.
`[ci.alias.release-clean]`: a published artifact must come from a clean build.

*Spike result needed.* The spike checks each of these cases on CI. Each must rerun the
affected suites:

| Change | Expected |
|---|---|
| none | 0 suites run |
| one test file of `scalus-examples` | the suites of that file only |
| one line of `scalus-plugin` main sources | every suite of each project that the plugin compiles |
| `flake.lock` | every suite |
| `SCALUS_TEST_ENV` | every suite |
| a suite that cancels its tests (no Lean workspace) | runs again on the next push |

When one case does not give the expected result, the developer stops and asks.

### 7.3 Nightly full run

- A nightly workflow MUST run each push alias with `clean` and `testFull`, for each push job.
  `[ci.nightly.full]`
- The nightly full run MUST NOT restore the task cache. `[ci.nightly.no-task-cache]`
- The nightly full run MUST report each result to Discord, as the push jobs do.
  `[ci.nightly.notify]`

- The nightly full run MUST report the count of compiler warnings of each job.
  `[ci.nightly.warnings]`
- A push job MUST report the count of compiler warnings of the files that it compiled.
  `[ci.alias.warnings-push]`

**Why:** `[ci.nightly.full]` catches a pass that the cache took from a run whose inputs were
incomplete. `[ci.nightly.no-task-cache]`: a restored cache would let `clean` be the only guard.
`[ci.nightly.warnings]`: on a cache hit sbt compiles nothing, so a push job sees only the
warnings of changed files. A release needs 0 warnings in the whole build, and only a clean
compile shows them all. *Decision of the developer, 2026-11-21.*

*Example, illustrative.* A test reads a file that no digest covers. A change of that file makes
the test fail, but push CI reports the cached pass. The nightly run fails the same night.

## 8. Phase 3 – job shape

- The CI-JVM workflow MUST run the format check in its own job. `[ci.jobs.format]`
- The format job MUST NOT compile. `[ci.jobs.format-no-compile]`
- The CI-JVM workflow MUST run MiMa, `jvm/doc` and `docs/unidoc` in their own job, on the
  default Scala version. `[ci.jobs.docs-mima]`
- That job MUST run in parallel with the CI-JVM 3.3.9 job. `[ci.jobs.docs-mima-parallel]`
- That job MUST restore the task cache of the CI-JVM 3.3.9 job. `[ci.jobs.docs-mima-cache]`
- That job MUST NOT save a task cache. `[ci.jobs.docs-mima-no-save]`

**Why:** the 3.3.9 job sets the wall time of CI-JVM, and it carries 1.5 minutes of checks that
the other versions skip. `[ci.jobs.docs-mima-cache]`: the job restores the entry of the last
master run, so it compiles only the projects that the push changed.
`[ci.jobs.docs-mima-no-save]`: a save under the 3.3.9 key races with that job, and a sixth
entry costs space for few hits. *Decision of the developer, 2026-11-21.*

## 9. Phase 4 – Nix shell

This phase is a spike. Its results decide the rules, which a later edit of this document adds.

The spike measures:

1. The time of `nix develop .#ci` with a warm store, split into evaluation and copy.
2. The share of the evaluation that the `plutus` input causes.
3. Each test that runs the `uplc` binary, and whether a `flake = false` input of the Plutus
   repository can give the corpus without haskell.nix.
4. Why the CI-JVM 3.9.0 job builds `nodejs` on the runner.
5. A job with three steps in the dev shell: `nix develop` in each step against one
   `nix print-dev-env` or `direnv export gha` in the first step.

- The developer MUST NOT remove the `uplc` binary from the `ci` shell while a test runs it.
  `[ci.shell.keep-uplc]`
- The flake MUST define the shells `ci` (CI-JVM, CI-JS, it, nightly, release), `ci-native` and
  `ci-lean`, each with only the packages that its jobs run. `[ci.shell.per-job]`
- The CI shells MUST NOT hold chromium. `[ci.shell.no-chromium]`
- On x86_64 Linux, the CI shells MUST take `uplc` from the static binary of the plutus release
  that the `plutus` input pins. `[ci.shell.uplc-release]`
- The saving job MUST root only its dev shell, not the flake inputs. `[ci.nix.root-shell-only]`
- The key of a Nix store entry MUST end the shell name with `_`, so that no prefix of one shell
  matches another shell. `[ci.nix.key-terminator]`

*Implementation status of the rules above: built on the work branch, 2026-11-21, not yet run
on CI.* `[ci.nix.root-shell-only]` replaces the flake-input part of `[ci.nix.gc-roots]`.

### Decision: Run the browser test with the runner's Google Chrome

Status: Accepted
Area: ci
Context: chromium is 1.5 GB of the 5.1 GB `ci` shell, and only `test:browser` of CI-JS uses it.
Options:
- The runner's Chrome: 0 MB. Its version follows the weekly runner image, not `flake.lock`.
- A `ci-js` shell with the pinned chromium: 2.9 GB, its own cache entry.
Chosen: the runner's Chrome, by the owner on 2026-11-21.
Consequences: Adds `[ci.shell.no-chromium]`. A local run already uses the installed Chrome, or
`CHROME_BIN`.

### Decision: One Nix store entry per CI shell, rooting the shell only

Status: Accepted
Area: ci
Context: rooting the flake inputs fetched haskell.nix, hackage and CHaP: 4.3 GB and 3.3 min.
The `ci` shell evaluates without them when it takes the release `uplc`.
Options:
- One entry that roots all CI shells: one saver, but `ci-native` clang and `ci-lean` Z3 ride in
  every restore.
- One entry per shell, each saved by the one job that uses it: each restore holds only its shell.
Chosen: one entry per shell, by the developer on 2026-11-21. CI-JVM 3.3.9 saves `ci`, CI-Native
saves `ci-native`, Lean-Proofs saves `ci-lean`.
Consequences: Adds `[ci.shell.per-job]`, `[ci.shell.uplc-release]`, `[ci.nix.root-shell-only]`
and `[ci.nix.key-terminator]`.

*Spike results, 2026-11-21.* Closures are nar sizes of the Linux outPaths, read from
cache.nixos.org and cache.iog.io with `nix path-info --store <cache> -rS`, so no Linux builder is
needed. The `ci` shell is 5331 MB (1712 MB compressed).

| Idea | Saving | What it costs |
|---|---|---|
| No chromium in `ci` | 1529 MB | `test:browser` falls back to the runner's `google-chrome`; the browser is then not pinned by `flake.lock`. A `ci-js` shell with chromium is 2863 MB. |
| `plutus = { flake = false; }` | about 4.1 GB of flake inputs (52 lock nodes to 8) | `uplc` needs another source: the 9.3 MB `uplc-x86_64-linux-ghc96` release asset by `fetchurl`; no macOS asset |
| clang and llvm only in a `ci-native` shell | about 1.1 GB | none known; llvm may go from `ci-native` too |
| No `glibcLocales` | 233 MB | none: `LC_ALL=C` is built into glibc |
| `mkShellNoCC` for the JVM and JS shells | 332 MB | none known |
| Headless JDK | 0 MB with chromium, 270 MB without | none: `sbt.override { jre }` adds no second JDK |
| sbt with only the Linux x86_64 `sbtn` | about 136 MB | a local `overrideAttrs` build |
| elan and z3 out of `ci` | 345 MB | none: the Lean and Z3 tests are canceled without them, as today |
| `nodejs_slim` | 26 MB | drops npm, which `runNpmTests` needs: rejected |

Other facts:

- Flake inputs are fetched lazily. A shell that does not use `plutus.packages` fetches the plutus
  source but not haskell.nix, hackage or CHaP. `nix flake archive` fetches and roots all 52
  lock nodes.
- The tests that run `uplc` are `ProgramFlatTest`, `FromDataDerivationTest`, `CekBudgetJVMTest`,
  `BaseValidatorTestJvm` (through `UplcCli`) and `CekBuiltinsJsTest`. `scalus.utils.Macros`
  reads the corpus at compile time on every platform and does not run `uplc`.
- `uplc` has a runtime closure of 89 MB.
- The x64 Ubuntu runner images (`ubuntu-latest`, `ubuntu-24.04`, `ubuntu-26.04`) have Google
  Chrome on `PATH`; the arm64 images and `ubuntu-slim` have no Chrome (actions/runner-images
  readmes, 2026-11-21). The image, and so the Chrome version, changes about each week.

**Why:** the job has one Nix step today, so nix-direnv saves time only after phase 3 splits a
job into more steps. `.envrc` loads the `default` shell, not `ci`, so CI cannot reuse it as it
is.

## 10. Later: quality gates

*Informative.* A later design adds quality gates to CI, for example compiler warnings as
errors, scalafix with `OrganizeImports`, a commit message check, a check that each path cited in
`AGENTS.md` exists, a coverage report in the nightly run, and Renovate for dependency updates.
It reuses the nightly workflow of section 7.3.

## 11. Assumptions

- GitHub Actions gives each repository 10 GB of cache and removes the least recently used
  entries first above that limit. GitHub sets this limit. `[ci.assume.cache-limit]`
- sbt 2.1 keys a task result by its declared inputs, and a test pass by the test classpath and
  `Test / extraTestDigests`. sbt sets this. `[ci.assume.sbt-keys]`
- On Linux, sbt 2.1 keeps its task cache in `~/.cache/sbt`. sbt sets this.
  `[ci.assume.cache-dir]`
- A branch run can restore an entry that a master run saved. GitHub sets this.
  `[ci.assume.branch-reads-master]`

## 12. Definitions

- **push job** – a job of `ci-jvm.yml`, `ci-js.yml` or `ci-native.yml`. It runs on each push.
- **nightly full run** – the jobs of `[ci.nightly.full]`.
- **task cache** – the sbt 2 disk cache in `~/.cache/sbt`.
- **dependency cache** – the coursier cache and the sbt boot directory.
- **Nix store cache** – the entry that `cache-nix-action` saves.
- **cache kind** – one of: task cache, dependency cache, Nix store cache.
- **spike** – a time-boxed test whose result is a decision, recorded in this document.

## 13. Open questions

*Informative.*

- Whether `~/.sbt/boot` is enough for the sbt 2 launcher on CI: open, the spike of section 6.2
  decides it.
- The size of the task cache per job: open, the spike of section 7.1 decides it.
- Whether sbt 2.1 has `Test / extraTestDigests` under that name: open, the spike of section 7.2
  checks it before the build uses it.
- Whether the `plutus` input can shrink: open, phase 4 decides it.
