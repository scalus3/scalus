# Migrating the Scalus build to sbt 2

Status: on sbt 2.1.0 (released 2026-11-16) on branch `worktree-sbt2-migration`. All CI gates and `it` pass locally (2026-11-20). Not merged.

## Decision

Migrate straight to sbt 2.1.0. The 2.0.12 spike (2026-10-30) recommended "port on 2.0, merge on
2.1"; 2.1.0 is now final, so 2.0 is skipped.

Why 2.1 and not 2.0:

- Forked-test parallelism is back in 2.1, and on by default: `testForkedParallel` defaults to
  true, `testTopology` splits each forked subproject into 2 workers, and each worker runs 2
  threads. `jvm/testFull` on 2.1.0, warm server, tests compiled: 613 suites, 0 failed, 94-98 s.
  With `testForkedParallel := false` it took 166 s. (A first run of 195 s included compiling.)
- 2.1 adds the opt-outs this build may need: `Test / test / cacheStores := Nil` (test cache),
  `CacheLevelTag.InputOnly`, and `clean` as a cache invalidator.
- RC3 fixed "test reporting cached results after a resource changed", one of the hidden-input
  risks found in the 2.0 spike.

What sbt 2 gives this build:

- Incremental `test`: a second `scalusJVM/test` with no change took 1 s instead of 129 s.
- One sbt 2 host builds, tests and documents both axes of `scalusSbtPlugin` (sbt 1 and sbt 2).
- Task cache: `jvm/Test/compile` after the rebase hit 35% from the disk cache.

## sbt 2.1 facts that affect us

- Metabuild is Scala 3.9.0. A plugin built with sbt 2.1 does not load in sbt 2.0. A plugin built
  with sbt 2.0 loads in sbt 2.1. So `scalusSbtPlugin` keeps its sbt 2 axis on Scala 3.8.4 (new
  val `sbtPluginScala3Version`) and `pluginCrossBuild / sbtVersion` 2.0.0. One artifact serves
  sbt 2.0 and 2.1 users.
- Ivy is removed. Publishing is new ("Ivyless") code. Dry-run the release before the first tag.
- `clean` invalidates the cache for the *next* command only. The `ci-*` and `precommit` aliases
  run `clean` several commands before the tests, so they now use `testFull`.
- The disk cache lives in `~/Library/Caches/sbt/v2` (macOS) or `~/.cache/sbt` (Linux). It was
  12 GB on the dev machine after a few runs. CI does not restore it: `coursier/cache-action`
  saves only `~/.sbt` and `~/.ivy2/cache`.

## Plugins

All on Maven Central as `<name>_sbt2_3`, checked 2026-10-30. No version changes are needed.

| Plugin | We pin | sbt 2 versions |
|--------|--------|----------------|
| sbt-scalajs | 1.22.0 | 1.22.0 |
| sbt-scalajs-crossproject | 1.4.0 | 1.4.0 |
| sbt-scala-native | 0.5.12 | 0.5.11, 0.5.12 |
| sbt-scala-native-crossproject | 1.4.0 | 1.4.0 |
| sbt-scalafmt | 2.6.2 | 2.5.6 .. 2.6.2 |
| sbt-ci-release | 1.12.1 | 1.11.2 .. 1.12.1 |
| sbt-welcome | 0.6.0 | 0.6.0 |
| sbt-jmh | 0.4.8 | 0.4.8 |
| sbt-unidoc | 0.6.1 | 0.6.0, 0.6.1 |
| sbt-mima-plugin | 1.2.1 | 1.1.5 .. 1.2.1 |
| sbt-buildinfo | 0.13.2 | 0.13.1, 0.13.2 |
| sbt2-compat | 0.2.0 | 0.1.0, 0.2.0 |
| sbt-scoverage | 2.4.4 | 2.4.1 .. 2.4.4 |

## Spike results (sbt 2.0.12, JDK 25.0.3)

- `project/` metabuild, including `scalus-sbt-plugin` sources via `unmanagedSourceDirectories`,
  compiles as Scala 3 with no edit.
- `sbt projects` lists all 31 projects after the edits below.
- `scalusJVM/compile`: success, 34 s, compiler plugin injected.
- `scalusSbtPlugin`: both axes compile and pass 10/10 tests from the sbt 2 host:
  Scala 2.12.21 against sbt 1.9.0, and Scala 3.8.4 against sbt 2.0.0. The sbt 1 launcher that
  the build.sbt comment requires is no longer necessary.
- `jvm/Test/compile`: success, 84 s.
- `scalusJVM/test` inside `nix develop .#ci` (sbt runner 1.12.11, JDK 21 launches 2.0.12):
  5054 passed, 2 failed, 129 s. Both failures had one cause (edit 7 below). After the fix, both
  suites pass. Outside nix, more tests fail because the Plutus `uplc` binary is missing; that is
  expected and not an sbt 2 issue.
- A second `scalusJVM/test` with no change ran 0 tests in 1 s: under sbt 2, `test` is
  `testQuick`.
- Not run in the spike: JS and Native tests, `ci-*` aliases, `mima`, `docs/unidoc`, publishing.
  `mima-js` matters most: `mimaPreviousArtifacts` of the JS projects is where the platform-aware
  `%%` must resolve to `_sjs1_3`.
- Observed once: in the first `scalusJVM/test` run (outside nix, JDK 25.0.3) the forked test JVM
  lost its socket ("3 failing attempts to read event from server socket") and the sbt server
  exited. No crash log. Not reproduced inside nix (JDK 21). Cause unknown; watch for it.
- The standalone `sbtn` (Coursier) connects to a 2.0.12 server, so the `sbtn` commands in
  `AGENTS.md` keep working. `testFull` exists in 2.0.12 (`Keys.scala:390`).

### Edits the spike needed (all in build.sbt)

1. `%%%` -> `%%` (23 sites). In sbt 2, `%%` reads the `platform` setting.
2. `PathFinder.get` -> `.get()` (3 sites).
3. Classpath entries and `packageBin` are `xsbti.HashedVirtualFileRef`, not `File`. Resolve them
   with `fileConverter.value.toPath(...)`. New helper `classpathFiles(cp, converter)` (6 sites).
4. Task caching. sbt 2 caches every task whose inputs and output are hashable. Tasks with side
   effects or with non-hashable types must be `Def.uncached(...)`:
   - side effects: `copySharedFiles`, `installNpmTestDeps`, `prepareNpmPackage`, `runNpmTests`,
     `generateDts`, `checkDtsUpToDate`, `prepareVerificationExamples`, `generateLlmsApi`;
   - `incOptions` overrides written with `:=` (no JsonFormat for `IncOptions`). The review
     replaced them with `~=`, which needs no wrapper. `x := x.dependsOn(y).value` is passed
     through uncached by sbt 2, so it needs no wrapper either.
   A side-effect task that compiles without `Def.uncached` is the dangerous case: sbt can skip it
   silently on a cache hit. Every custom task must be reviewed, not only the ones that failed.
5. `licenses := List("Apache 2" -> url)` -> `List(License.Apache2)`; `url(...)` -> `uri(...)`.
6. Root project renamed `scalus` -> `scalus-root`. sbt 2 puts outputs in
   `target/out/<platform>/<scala>/<name>/`, so root and `scalusJVM` collided.
7. Tests that start a child JVM with `-cp $java.class.path` (`SameCborTest`,
   `CseDeterminismCrossJvmTest`) got `ClassNotFoundException`: sbt 2 starts forked tests through a
   worker, and `java.class.path` lacks the test classes. They now read `scalus.test.classpath`,
   which build.sbt already passes. Search other modules for `java.class.path` too.

## Results on sbt 2.1.0 (2026-11-19, `nix develop .#ci`, JDK 21.0.11)

The branch was rebased onto master (154 commits). Then:

- `sbt projects`: loads with no error and no warning.
- `jvm/Test/compile` + `scalusCardanoLedgerIt/Test/compile`: success, 95 s.
- `jvm/testFull`: 613 suites, 0 failed, 195 s (after the 2 fixes below; first run had 4 failures).
- `+scalusSbtPlugin/test` and `+scalusSbtPlugin/doc`: success on both axes.
- `generateLlmsApi`: runs the generator once (`runMain` from a task still works) and writes
  `scalus-site/public/llms-api.txt`.
- `ci-jvm` (2026-11-20): passes in full, 263 s: format checks, compile, `jvm/testFull` (613
  suites, 0 failed), `mima-jvm`, `jvm/doc`, `docs/unidoc`. MiMa really runs although it shows
  "cache 100%": making `NonPolymorphicInstantiationMachineError` final made it fail against
  `scalus_3:1.5.0`.
- `ci-js`: passes step by step: `js/testFull` (317 suites), `js/doc`, `mima-js` (so `%%` resolves
  to `_sjs1_3`), `scalusTsExporter/testFull` (7 suites), `checkDtsUpToDate`, `runNpmTests`
  (27 files, 185 tests). One `npm ci` hung for 22 h on dead HTTPS connections after the network
  dropped; a re-run installed in 13 s. Not an sbt issue.
- `ci-native`: passes, 385 s, 198 suites (after fix 11).
- CI command lines: sbt 2 joins separate arguments into one command, so
  `sbt "set ..." ci-js` fails before `ci-js` starts (tested). Fixed with one `;`-joined argument in
  `ci-js.yml`, `lean-proofs.yml` and `scalus-site/scripts/build-version.sh` (tested the `;` form
  with and without `--batch`; `build-version.sh` itself not run on a tag).
- Publish dry run (`publish`, `+scalusPlugin/publish`, `+scalusSbtPlugin/publish` to a local file
  repo): the same artifact IDs as 1.5.0, plus `scalus-plugin_3.10.1` (new on master). Each POM
  checked (`scalus_3`, `_sjs1_3`, `_native0.5_3`, ledger, testkit, both sbt plugins) has the same
  dependencies, versions and scopes as 1.5.0, after fix 12. The license entry is now SPDX
  `Apache-2.0` with an `https` URL. Files per artifact: jar, sources, javadoc, POM, as in 1.5.0.
  Not tested: signing and the Sonatype upload of `ci-release` (`publishSigned` and `sonaUpload`
  exist on 2.1).
- `it` (2026-11-20, Docker, CI data from the `it-data` release and the pinned Dugite corpus):
  passes, 186 s. All 9 suites ran: 83 tests passed, 0 failed. The 51 canceled tests are
  `BlockfrostEndpointsTest`, which needs a Blockfrost key. `CaptureNodeErrorsTest` rewrites its 2
  `yaci-error-*.json` fixtures with new transaction IDs on every run, as on sbt 1.

New fixes on 2.1.0:

8. `exportJars := false` (bare, so every subproject). sbt 2 defaults it to `true`; tests that turn
   a resource URL into a directory `Path` (`BlocksLedgerRulesValidator`, `CardanoLedgerVectorsTest`)
   failed with `FileSystemNotFoundException`. This keeps the sbt 1 classpath shape. Moving those
   tests to stream resources and dropping the override is a later, separate change.
9. `PipelineDeterminismCrossJvmTest` (new on master) had the edit-7 problem. `scalusExamplesJVM` now
   also gets `-Dscalus.test.classpath`.
10. `leanBuild` (new on master) is a side-effect task: `Def.uncached`.
11. `libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % VersionScheme.Always`.
    Scala Native publishes `test-interface` as "strict"; sbt 2.1 failed `scalusNative / update` on
    0.5.12 over scalatest's 0.5.8/0.5.10, which sbt 1 resolved the same way without an error. With
    `%%` the scheme is stored as plain `test-interface` and does not match, so the full name is used.
12. `scalusSbtPlugin`: `autoScalaLibrary` only on the 2.12 axis, and `scala3-library_3` as
    `provided` on the Scala 3 axis. An sbt 2.1 host otherwise publishes it with compile scope;
    1.5.0 and other sbt 2 plugins (sbt-buildinfo, sbt-ci-release) have `provided`.
13. Rebase onto master `eb1e2b79f` (2026-11-21) brought `jvmJsoniterForMacros` (`d4f05d8a6`),
    which puts the JVM `jsoniter-scala-core_3` first on `scalusNative`'s classpath for the
    CardanoInfo macro. Two sbt 2 changes: `Classpaths.managedJars` takes a `FileConverter`
    (`fileConverter.value`), and the dependency needs `.platform(Platform.jvm)`. Without it, the
    platform-aware `%%` resolves the Native jar and the build stops with "the JVM
    jsoniter-scala-core for the macros is not resolved" (tested). The rebase also took master's
    literal `"3.8.4"` for the sbt 2 axis of `scalusSbtPlugin` instead of a separate val.

## Behavior changes: status

1. Bare settings. DONE: `generateLlmsApi` and the bare `Compile / doc / scalacOptions` are
   `LocalRootProject /`-scoped, as they were in effect on sbt 1. `autoCompilerPlugins`,
   `Test / publishArtifact := false`, `commands +=`, `logo`, `usefulTasks` now reach every project;
   all are harmless.
2. `run` in the client. DONE: `generateLlmsApi` works unchanged on 2.1.0.
3. Output paths. DONE: `scripts/js-bundle-size.sh` and `scripts/cse-corpus.py` point at
   `target/out/...` (paths read from sbt with `show ...LinkerOutputDirectory` / `resourceManaged`).
   `FlatSIRClausifyTest` keeps its stale `scala-3.3.6` path: it was already dead (`pending` when the
   file is missing) before this change.
4. `testForkedParallel`. DONE: it is the 2.1 default, so the build no longer sets it. Other knobs
   (`workerMaxInstances` 4, `testForkedParallelism` 6, `subprojectSplit(4)`) measured 89-101 s,
   within noise of the default.
5. Cached `test`. DONE for the aliases: every `ci-*` alias and `precommit` uses `testFull`; `quick`
   keeps `jvm/testQuick`. CI has no cross-run test cache (cache-action does not save sbt's disk
   cache). OPEN: a local `testQuick` does not see the `plutus-conformance` corpus or
   `SCALUS_TEST_ENV` as inputs; run `testFull` after changing either.
6. `.jvmopts` flags. OPEN (cosmetic).
7. `exportJars`. DONE (fix 8).
8. Forked `run` in the build root. CHECKED for `llmApiGen`; `bench`, `scalusVerification`,
   `scalusTsExporter` still to check.
9. CI single server per job. No change needed.
10. Global base `~/.config/sbt/2`. OPEN: check docs that name `~/.sbt`.

Source: <https://www.scala-sbt.org/2.x/docs/en/changes/migrating-from-sbt-1.x.html>.

## Review fixes (2026-11-20)

A review of the first commit found these; all applied:

1. `test` and `testQuick` do not call `executeTests` in sbt 2, so `js/test` skipped
   `installNpmTestDeps`. The hook is now on `Test / loadedTestFrameworks`, which every test task
   loads. Checked: with the install task set to fail, plain `scalusUtxoCellJS/test` fails.
2. `incOptions` sites use `~=`. The old `Compile / incOptions := { incOptions.value ... }` read the
   unscoped key and dropped the config's `TransactionalManagerType`; it is kept now. The default
   `withApiDebug(false)` and `withRelationsDebug(false)` are gone. `withUseOptimizedSealed(true)`
   and `withRecompileOnMacroDef(false)` stay: they still change Zinc behavior, and Zinc notes the
   first can undercompile code whose macros list sealed children.
3. Deleted settings that are sbt 2 defaults: `testForkedParallel`, `parallelExecution`,
   `onChangedBuildSource`, `autoCompilerPlugins`, `Test / publishArtifact`.
4. `Compile / compile` and the 2 npm hooks drop `Def.uncached`. The 9 custom side-effect tasks keep
   it: a cache hit would skip npm, `lake`, file writes into the source tree, or git checks.
5. `build-version.sh` runs `sbt --server --batch`, so an sbt 2 tag leaves no 8 GB server behind.
6. AGENTS.md says `sbt` (the thin client in sbt 2), adds the sbt 2 rules, and fixes the server
   `pgrep` pattern (`sbt-launch`, not `xsbt.boot.Boot`).

Open, optional: `-Jdummy` from the plugin jar's content hash instead of `lastModified`;
`Test / extraTestDigests` for `SCALUS_TEST_ENV` and the corpus; sweep `sbtn ... test` in README,
CONTRIBUTING, docs and skills.

## Out of scope

- `scalus-crypto-jni` and `scalus-secp256k1-jni` are separate builds that run sbt on Java 11 in
  `crypto-jni.yml`. sbt 2 needs JDK 17+. Leave them on sbt 1.
- g8 templates and the eject flow: they have their own sbt version.

## Tasks

1. DONE: spike edits 1-7, 2.1.0 fixes 8-13, behavior changes 1-5 and 7.
2. DONE: `ci-jvm`, `ci-js`, `ci-native`, `it`, publish dry run, CI command lines, AGENTS.md.
3. Left: check forked `run` of `bench`, `scalusVerification`, `scalusTsExporter`;
   `.jvmopts` flags; move the resource-directory tests off `Paths.get(url.toURI)` and drop
   `exportJars := false` (separate change).
4. First release on sbt 2.1: watch `ci-release` (signing, Sonatype upload) and the site deploy
   (`build-version.sh` on the new tag).
