# Profile Manifest + Schema Version Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the Scalus profile output a versioned, discoverable contract: `profile.json` carries a `schemaVersion` field, and the evaluator writes a `profile-manifest.json` that is the single entry point describing every rendered profile run (script identity, redeemer, budget, files).

**Architecture:** Two producer-side changes. (1) `ProfileFormatter.toJson` emits `"schemaVersion": 1` as its first field, with the version exposed as a constant. (2) `PlutusScriptEvaluator`'s `renderProfile` collects which files it actually wrote per script run and rewrites `profile-manifest.json` in the report output directory after each run; entries are keyed by `(scriptHash, redeemerTag, redeemerIndex)` so re-evaluations (fee balancing) overwrite rather than accumulate, mirroring the stable overwriting profile file names. Consumers (e.g. the Scalus VS Code extension) read the manifest instead of glob-guessing.

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest (AnyFunSuite). All touched code is cross-platform `shared/` code; file I/O goes through `scalus.uplc.builtin.platform` (already used in the file). On JS `ProfileReporting.render` returns `None`, so no files and no manifest get written there — the new code must not change that.

## Global Constraints

- Commit directly on `master` (repo convention; no branches/worktrees). Run `sbtn scalafmtAll` before every commit.
- Conventional commit style (`feat:`, `docs:` ...). NEVER add `Co-Authored-By: Claude` or any Claude/Anthropic trailer (user's global rule, overrides harness defaults).
- No em dashes in docs/commit text; use en dashes (–).
- Public API stays backward compatible (`sbtn mima` gate exists). Adding `val JsonSchemaVersion` to `object ProfileFormatter` is additive = OK. All `PlutusScriptEvaluator` changes are to `private` members = OK.
- No fully qualified names inline – import `scala.collection.concurrent.TrieMap` etc. (repo style rule).
- Scala 3 style per CLAUDE.md: braces for multi-line defs, indentation syntax for small `if`/`match`, `then` in `if`.
- sbt test invocations: run in foreground (background sbtn runs hang silently – known issue).

## File Map

- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/eval/ProfileFormatter.scala` (toJson, new constant)
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/eval/ProfileFormatterTest.scala`
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/PlutusScriptEvaluator.scala` (renderProfile + manifest writer, new private case class + TrieMap field)
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/ledger/EvaluatorReportConfig.scala` (scaladoc only)
- Test: `scalus-cardano-ledger/jvm/src/test/scala/eval/EvalPlutusScriptsTest.scala`
- Modify: `scalus-site/content/testing/profiling.mdx` (new docs section)

---

### Task 1: `schemaVersion` in `profile.json`

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/eval/ProfileFormatter.scala:380-395`
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/eval/ProfileFormatterTest.scala`

**Interfaces:**
- Produces: `ProfileFormatter.JsonSchemaVersion: Int` (= 1), used by Task 2's manifest test only conceptually (manifest has its own literal `1`). `toJson` output gains a first field `"schemaVersion": 1`.

- [x] **Step 1: Write the failing test**

In `ProfileFormatterTest.scala`, directly after the existing `test("toJson includes all sections")` block (around line 120), add:

```scala
    test("toJson carries the profile.json schema version") {
        assert(ProfileFormatter.JsonSchemaVersion == 1)
        val json = ProfileFormatter.toJson(data)
        assert(json.contains(s""""schemaVersion": ${ProfileFormatter.JsonSchemaVersion}"""))
        // schemaVersion must be the first field so humans spot it immediately
        assert(json.linesIterator.drop(1).next().contains("schemaVersion"))
    }
```

(`data` is the fixture already used by the neighboring toJson tests in this file.)

- [x] **Step 2: Run test to verify it fails**

Run: `sbtn "scalusJVM/testOnly scalus.uplc.eval.ProfileFormatterTest"`
Expected: compile error `value JsonSchemaVersion is not a member of object ...ProfileFormatter` (a compile failure is this step's "failing test").

- [x] **Step 3: Implement**

In `ProfileFormatter.scala`, immediately above `def toJson` (line ~380), add the constant:

```scala
    /** Version of the `profile.json` document produced by [[toJson]] (emitted as its
      * `"schemaVersion"` field). Bump on any incompatible change to the JSON shape so consumers
      * (e.g. the Scalus VS Code extension) can detect and reject profiles they don't understand.
      */
    val JsonSchemaVersion: Int = 1
```

Then inside `toJson`, right after `sb.append("{\n")` (line ~388), emit the field first:

```scala
        sb.append(s"""  "schemaVersion": $JsonSchemaVersion,\n""")
```

Also extend the `toJson` scaladoc first sentence to: `Renders the full profiling data as JSON (machine-readable, schema version [[JsonSchemaVersion]]).`

- [x] **Step 4: Run test to verify it passes**

Run: `sbtn "scalusJVM/testOnly scalus.uplc.eval.ProfileFormatterTest"`
Expected: all tests PASS (including the pre-existing toJson tests – they assert field presence, not absence of others).

- [x] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/uplc/eval/ProfileFormatter.scala \
        scalus-core/shared/src/test/scala/scalus/uplc/eval/ProfileFormatterTest.scala
git commit -m "feat(uplc): add schemaVersion field to profile.json output

profile.json is consumed by external tools (VS Code extension); a version
field lets them detect schema drift instead of failing silently."
```

---

### Task 2: `profile-manifest.json` written by the evaluator

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/PlutusScriptEvaluator.scala` (renderProfile at :371-394, call site at :719-724, new members near :356)
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/ledger/EvaluatorReportConfig.scala:79-84` (scaladoc)
- Test: `scalus-cardano-ledger/jvm/src/test/scala/eval/EvalPlutusScriptsTest.scala`

**Interfaces:**
- Consumes: nothing from Task 1 (the manifest has its own independent `"schemaVersion": 1` literal).
- Produces: file `<outputDir>/profile-manifest.json` with shape:

```json
{
  "schemaVersion": 1,
  "runs": [
    { "scriptHash": "<hex>", "language": "PlutusV3",
      "redeemer": { "tag": "Spend", "index": 0 },
      "budget": { "mem": 123, "cpu": 456 },
      "files": [ { "format": "json", "file": "<hex>-Spend-0.profile.json" } ] }
  ]
}
```

`files[].file` is the path as written: relative to the manifest's directory for `ProfileDestination.File`, absolute for `ProfileDestination.AbsoluteFile`. Console outputs produce no `files` entry; a run whose outputs are all console-only (or JS, where render returns None) produces no manifest entry, and if no run produced files the manifest is not written at all.

- [x] **Step 1: Write the failing test**

In `EvalPlutusScriptsTest.scala`, after the existing `test("profile = Full writes per-script HTML + CSV reports")` block, add:

```scala
    test("profile = Full writes schemaVersion'd profile.json and a profile-manifest.json") {
        val dir = Files.createTempDirectory("scalus-profile-manifest-test")
        try {
            val report = EvaluatorReportConfig(
              enabled = true,
              outputDir = dir.toString,
              artifacts = Set.empty, // profile only, no .flat dump
              profile = ProfileLevel.Full
            )
            // Two evaluations of the same tx must overwrite manifest runs, not duplicate them.
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)
            evalPlutusScripts(tx7430, utxo7430, SlotConfig.mainnet, report)

            val files = Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .map(_.getName)

            val jsonName = files.find(_.endsWith(".profile.json")).get
            val profileJson = new String(Files.readAllBytes(dir.resolve(jsonName)), "UTF-8")
            assert(profileJson.contains("\"schemaVersion\": 1"))

            val manifest = new String(
              Files.readAllBytes(dir.resolve("profile-manifest.json")),
              "UTF-8"
            )
            assert(manifest.contains("\"schemaVersion\": 1"))
            // The tx runs 2 scripts; re-evaluation must not duplicate the runs.
            assert(
              "\"scriptHash\"".r.findAllIn(manifest).size == 2,
              s"expected 2 runs in manifest:\n$manifest"
            )
            assert(manifest.contains("\"language\""))
            assert(manifest.contains("\"redeemer\""))
            assert(manifest.contains("\"budget\""))
            // Every file the manifest lists must exist on disk.
            val listed = "\"file\": \"([^\"]+)\"".r.findAllMatchIn(manifest).map(_.group(1)).toSeq
            assert(listed.nonEmpty)
            listed.foreach { f =>
                assert(Files.exists(dir.resolve(f)), s"manifest lists missing file $f")
            }
        } finally
            Option(dir.toFile.listFiles())
                .getOrElse(Array.empty[java.io.File])
                .foreach(_.delete())
            Files.deleteIfExists(dir)
    }
```

- [x] **Step 2: Run test to verify it fails**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly eval.EvalPlutusScriptsTest"`
Expected: the new test FAILS with `NoSuchFileException: .../profile-manifest.json` (older tests still pass).

- [x] **Step 3: Implement in PlutusScriptEvaluator.scala**

3a. Add to the imports at the top of the file: `import scala.collection.concurrent.TrieMap` (merge into the existing `scala.collection` import group if one exists).

3b. Inside `DefaultImpl`, next to `budgetLogPath` (line ~356), add the run registry:

```scala
        /** One rendered profile run: what was profiled and which report files were written.
          * `files` holds (format label, path) pairs – paths relative to the manifest's directory
          * for [[ProfileDestination.File]] outputs, absolute for
          * [[ProfileDestination.AbsoluteFile]].
          */
        private final case class ProfileRun(
            scriptHash: String,
            language: Language,
            tag: RedeemerTag,
            index: Int,
            mem: Long,
            cpu: Long,
            files: Seq[(String, String)]
        )

        /** Rendered profile runs keyed by (scriptHash, tag, index) – the same stable key the
          * overwriting file names use, so fee-balancing re-evaluations replace their entry
          * instead of accumulating. Mirrors the profile files this evaluator wrote to disk.
          */
        private val profileRuns = TrieMap.empty[(String, String, Int), ProfileRun]
```

3c. Replace `renderProfile` (lines ~371-394) with a version that takes the language, records written files, and refreshes the manifest. Keep the existing scaladoc, extend its first paragraph with: `File outputs are also recorded in profile-manifest.json (see [[writeProfileManifest]]).`

```scala
        private def renderProfile(
            result: Result,
            scriptHash: ScriptHash,
            redeemer: Redeemer,
            language: Language
        ): Unit = result.profile.foreach { data =>
            val key = s"${scriptHash.toHex}-${redeemer.tag}-${redeemer.index}"
            val written = Seq.newBuilder[(String, String)]
            report.effectiveProfileOutputs.foreach { out =>
                ProfileReporting.render(data, out.format, report.profile, report.maxRows).foreach {
                    content =>
                        out.destination match
                            case ProfileDestination.Console =>
                                log.info(s"Profile $key:\n$content")
                            case ProfileDestination.File(name) =>
                                val file = s"$key.$name"
                                platform.writeFile(reportPath(file), content.getBytes("UTF-8"))
                                written += formatLabel(out.format) -> file
                            case ProfileDestination.AbsoluteFile(path) =>
                                val sep = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))
                                if sep > 0 then platform.createDirectories(path.substring(0, sep))
                                platform.writeFile(path, content.getBytes("UTF-8"))
                                written += formatLabel(out.format) -> path
                }
            }
            val files = written.result()
            if files.nonEmpty then
                profileRuns((scriptHash.toHex, redeemer.tag.toString, redeemer.index)) = ProfileRun(
                  scriptHash.toHex,
                  language,
                  redeemer.tag,
                  redeemer.index,
                  data.totalBudget.memory,
                  data.totalBudget.steps,
                  files
                )
                writeProfileManifest()
        }

        /** Lower-case manifest label for a profile format: "text", "csv", "html", "json". */
        private def formatLabel(format: ProfileFormat): String = format.toString.toLowerCase

        /** Write `profile-manifest.json`: the machine-readable entry point (schema version 1)
          * listing every profile run this evaluator has rendered to files, so tools (e.g. the
          * Scalus VS Code extension) can discover profiles without guessing file names.
          */
        private def writeProfileManifest(): Unit = {
            val runs = profileRuns.values.toSeq
                .sortBy(r => (r.scriptHash, r.tag.toString, r.index))
                .map { r =>
                    val files = r.files
                        .map { case (fmt, f) => s"""{ "format": "$fmt", "file": "$f" }""" }
                        .mkString(", ")
                    s"""    { "scriptHash": "${r.scriptHash}", "language": "${r.language}", """ +
                        s""""redeemer": { "tag": "${r.tag}", "index": ${r.index} }, """ +
                        s""""budget": { "mem": ${r.mem}, "cpu": ${r.cpu} }, """ +
                        s""""files": [$files] }"""
                }
            val json =
                s"""{
                   |  "schemaVersion": 1,
                   |  "runs": [
                   |${runs.mkString(",\n")}
                   |  ]
                   |}
                   |""".stripMargin
            platform.writeFile(reportPath("profile-manifest.json"), json.getBytes("UTF-8"))
        }
```

Note the `data.totalBudget` fields are `memory` and `steps` (an `ExBudget`), and `redeemer.index` is already an `Int`.

3d. Update the single call site (line ~719-724) to pass the language:

```scala
                if report.enabled && report.profile != ProfileLevel.Off then
                    renderProfile(
                      vm.evaluateScriptProfile(applied),
                      plutusScript.scriptHash,
                      redeemer,
                      vm.language
                    )
```

3e. In `EvaluatorReportConfig.scala`, extend the `effectiveProfileOutputs` scaladoc (lines ~80-83). Replace the last sentence (`The profile.json is the machine-readable rendering ...`) with:

```
      * The `profile.json` is the machine-readable rendering editors/tools consume – e.g. the
      * Scalus VS Code extension annotates source lines with per-line cost from it. File outputs
      * are indexed in `profile-manifest.json` (written next to them), which is the discovery
      * entry point: it maps each script/redeemer run to its rendered files.
```

- [x] **Step 4: Run test to verify it passes**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly eval.EvalPlutusScriptsTest"`
Expected: all tests PASS, including the two pre-existing dump/profile tests.

- [x] **Step 5: Cross-platform compile check**

Run: `sbtn "scalusCardanoLedgerJS/compile; scalusCardanoLedgerJVM/Test/compile"`
Expected: success (the JS build compiles the same shared code against the no-op `ProfileReporting`).

- [x] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/PlutusScriptEvaluator.scala \
        scalus-core/shared/src/main/scala/scalus/cardano/ledger/EvaluatorReportConfig.scala \
        scalus-cardano-ledger/jvm/src/test/scala/eval/EvalPlutusScriptsTest.scala
git commit -m "feat(ledger): write profile-manifest.json indexing rendered profile runs

Each profiled script run (scriptHash, redeemer tag/index) is recorded with
its language, profiling budget and the report files written. The manifest
(schema version 1) is the discovery entry point for tools like the Scalus
VS Code extension, replacing glob-and-mtime guessing. Runs are keyed like
the stable file names, so re-evaluations overwrite instead of accumulate."
```

---

### Task 3: Document the machine-readable output contract

**Files:**
- Modify: `scalus-site/content/testing/profiling.mdx` (insert new section after `## HTML Output`, before `## Toggle Profiling in Tests`)

**Interfaces:**
- Consumes: the file/manifest naming from Task 2 and `schemaVersion` from Task 1 (documentation must match them exactly).

- [x] **Step 1: Add the docs section**

Insert after the `## HTML Output` section (its code block ends around line 66):

````markdown
## Machine-Readable Output (profile.json + manifest)

`PlutusScriptEvaluator` can render profiles to files on every evaluation. The easiest switch is
the environment variable (see [Project Commands](/docs/get-started/project-commands)):

```sh copy
SCALUS_PROFILE=full sbt test
```

For each profiled script run this writes, into the report output directory (`SCALUS_DUMP_DIR`,
default: the working directory):

- `<scriptHash>-<tag>-<index>.profile.html` – the interactive report
- `<scriptHash>-<tag>-<index>.profile.json` – machine-readable data (`"schemaVersion": 1`)
- `<scriptHash>-<tag>-<index>.profile.csv` – flat table for spreadsheets
- `profile-manifest.json` – the discovery entry point listing every run

File names are stable per `(scriptHash, redeemer tag, index)`, so repeated evaluations (e.g.
during fee balancing) overwrite rather than accumulate. The manifest maps each run to its files:

```json
{
  "schemaVersion": 1,
  "runs": [
    { "scriptHash": "ab12…", "language": "PlutusV3",
      "redeemer": { "tag": "Spend", "index": 0 },
      "budget": { "mem": 185927967, "cpu": 49131853260 },
      "files": [ { "format": "html", "file": "ab12…-Spend-0.profile.html" },
                 { "format": "json", "file": "ab12…-Spend-0.profile.json" },
                 { "format": "csv",  "file": "ab12…-Spend-0.profile.csv" } ] }
  ]
}
```

Tools should start from `profile-manifest.json` and check `schemaVersion` (currently `1` for
both the manifest and `profile.json`). The Scalus VS Code extension consumes `profile.json` to
annotate source lines with per-line cost.
````

- [x] **Step 2: Verify the site content builds / renders**

No site build is wired into sbt; verify by eyeballing the diff (` ```json ` fence closed, heading levels consistent) and run `sbtn scalafmtAll` (no-op for mdx but keeps the tree formatted).

- [x] **Step 3: Commit**

```bash
git add scalus-site/content/testing/profiling.mdx
git commit -m "docs: document profile.json schema version and profile-manifest.json"
```

---

### Task 4: Final verification

- [x] **Step 1: Run the fast full check**

Run: `sbtn quick` (format check, compile, jvm/testQuick)
Expected: green. If stale-class weirdness appears, `sbtn clean` first.

- [x] **Step 2: MiMa gate**

Run: `sbtn mima`
Expected: no errors (only additive/private changes were made).

- [x] **Step 3: Rebase-then-push guard**

```bash
git pull --rebase && git log --oneline -4
```

Push only if the user has asked for a push; otherwise stop after local commits (repo default).

## Self-Review Notes

- Spec coverage: schemaVersion field (Task 1), manifest with schema version + run identity + files (Task 2), docs contract (Task 3). The previously discussed relative-source-paths / speedscope / snapshots items are explicitly out of scope of this plan.
- Types cross-checked: `redeemer.index: Int`, `redeemer.tag: RedeemerTag`, `data.totalBudget: ExBudget` with `memory`/`steps: Long`, `Language` enum `toString` gives `PlutusV1/V2/V3`; `flatFileName` already interpolates `tag`/`index` the same way, so manifest keys match on-disk names.
- JS safety: `ProfileReporting.render` returns `None` on JS, so `written` stays empty, no manifest entry and no manifest write happens there; no new code paths execute.
