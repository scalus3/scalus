# Blueprint Resource Generator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `sbt package`/`publish` automatically embed the CIP-57 blueprint JSON in the JAR by turning `blueprint` into an sbt resource generator that writes to `resourceManaged`, with a clean opt-out.

**Architecture:** Extract the generation body into a pure helper, repoint output from the Zinc-owned `classDirectory` to the sbt-owned `Compile / resourceManaged`, and register a skip-gated wrapper in `Compile / resourceGenerators`. The wrapper reads `dependencyClasspath + classDirectory` (never `fullClasspath`) to avoid a task-dependency cycle, and is suppressed by `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var. The explicit `sbt blueprint` command stays available.

**Tech Stack:** sbt AutoPlugin (cross-built Scala 2.12 / sbt 1 and Scala 3 / sbt 2), `sbtcompat.PluginCompat` shim, Java reflection into compiled `scalus.cardano.blueprint.Contract` objects.

## Global Constraints

- Single source file: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala`. The real plugin is `object ScalusSbtPlugin`; `object ScalusBlueprintPlugin` is a deprecated alias delegating `projectSettings` — **do not** duplicate settings into it.
- Must compile on **both** cross axes: sbt 1 / Scala `2.12.21` and sbt 2 / Scala `3.8.4`. Verify with `+scalusSbtPlugin/compile`.
- sbt is provided by the nix flake. Run every sbt command as `nix develop --command sbt <args>`. (`sbt`/`sbtn` are not on the bare `PATH`.)
- **Cycle rule:** any task registered in `resourceGenerators` MUST use `Compile / dependencyClasspath` plus `Compile / classDirectory`, NEVER `Compile / fullClasspath`. `fullClasspath` → `exportedProducts` → `products` → `copyResources` → `resources` → `resourceGenerators`, which closes a cycle once the task is itself a generator.
- **Eager-`.value` rule:** the skip gate MUST use `Def.taskIf` (not `Def.task` with an inner `if`). `.value` lifts an unconditional dependency, so a plain `if` would still run generation when skipped. `Def.taskIf` is available on sbt 1.4+ (baseline 1.5.8) and sbt 2.
- Keep `blueprint` and `blueprint / skip` at project (Zero) config — not `Compile`-scoped. This keeps bare `sbt blueprint` working and makes the natural opt-out `blueprint / skip := true` resolve correctly via Compile→Zero delegation. (A `Compile / blueprint / skip := false` default would shadow a Zero-scoped user override — footgun, avoid.)
- In-JAR path is unchanged: `META-INF/scalus/blueprints/<Contract>.json`. Only the on-disk pre-package location moves (to `resource_managed/main/...`).
- Formatting: `nix develop --command sbt scalusSbtPlugin/scalafmtCheck` must pass before each commit.
- Commit style: conventional commits (`refactor:`, `feat:`, `docs:`). Never add a Claude/Anthropic co-author trailer.

---

### Task 1: Extract the pure `writeBlueprints` helper (no behavior change)

Refactor only: pull the generation body out of `blueprintTask` into a private helper, keeping output in `classDirectory` exactly as today. This isolates the file-writing logic so Task 2/3 only change wiring, not generation.

**Files:**
- Modify: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` (`blueprintTask` at lines 36-52; add helper near the other private helpers ~line 119)

**Interfaces:**
- Produces: `private def writeBlueprints(classesDir: java.io.File, cp: Seq[java.io.File], resourceRoot: java.io.File, log: sbt.util.Logger): Seq[java.io.File]` — writes one JSON per discovered Contract under `resourceRoot/META-INF/scalus/blueprints/`, returns the files written. Consumed by `blueprintTask` in Tasks 1-3.

- [ ] **Step 1: Add the helper** next to `simpleName` (after line 121, before `readManifestClassNames`):

```scala
    /** Generate and write CIP-57 blueprint JSON for every Contract listed in the
      * compiled `blueprint-modules` manifest. Returns the files written.
      *
      * @param classesDir
      *   the Compile classDirectory — holds the manifest and the project's own
      *   compiled classes (loaded via reflection)
      * @param cp
      *   classpath for the reflection classloader (dependencies + classesDir)
      * @param resourceRoot
      *   root under which output is written, at
      *   `resourceRoot/META-INF/scalus/blueprints/<Contract>.json`
      */
    private def writeBlueprints(
        classesDir: java.io.File,
        cp: Seq[java.io.File],
        resourceRoot: java.io.File,
        log: sbt.util.Logger
    ): Seq[java.io.File] = {
        val outDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        IO.createDirectory(outDir)
        loadContracts(classesDir, cp, log).map { case (className, json) =>
            val file = outDir / (simpleName(className) + ".json")
            IO.write(file, json, java.nio.charset.StandardCharsets.UTF_8)
            log.info(s"Wrote ${file.relativeTo(resourceRoot).getOrElse(file)}")
            file
        }
    }
```

- [ ] **Step 2: Replace the `blueprintTask` body** (lines 36-52) to call the helper, still writing to `classDirectory` (resourceRoot = classesDir):

```scala
    lazy val blueprintTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val _ = (Compile / compile).value
        implicit val conv: xsbti.FileConverter = fileConverter.value
        val cp = toFiles((Compile / fullClasspath).value)
        val classesDir = (Compile / classDirectory).value
        val log = streams.value.log
        writeBlueprints(classesDir, cp, classesDir, log)
    }
```

- [ ] **Step 3: Verify formatting**

Run: `nix develop --command sbt scalusSbtPlugin/scalafmtCheck`
Expected: ends with `[success]`, no "must be formatted" errors.

- [ ] **Step 4: Verify both cross axes compile**

Run: `nix develop --command sbt +scalusSbtPlugin/compile`
Expected: `[success]` for the cross build (Scala 2.12 and 3.8.4); no errors.

- [ ] **Step 5: Commit**

```bash
git add scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala
git commit -m "refactor: extract writeBlueprints helper from blueprintTask"
```

---

### Task 2: Repoint output to `resourceManaged` and fix the classpath

Move generated JSON out of the Zinc-owned `classDirectory` into `Compile / resourceManaged`, and switch the reflection classpath from `fullClasspath` to `dependencyClasspath + classDirectory` (required before Task 3 registers this as a generator). Also update the scaladoc.

**Files:**
- Modify: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` (`blueprintTask` body; scaladoc lines 10-22)

**Interfaces:**
- Consumes: `writeBlueprints(...)` from Task 1.
- Produces: `blueprintTask` now writes to `<Compile resourceManaged>/main/META-INF/scalus/blueprints/`. No signature change.

- [ ] **Step 1: Replace the `blueprintTask` body** with the resourceManaged + dependencyClasspath version:

```scala
    lazy val blueprintTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val _ = (Compile / compile).value
        implicit val conv: xsbti.FileConverter = fileConverter.value
        val classesDir = (Compile / classDirectory).value
        // Use dependencyClasspath + classDirectory, NOT fullClasspath: fullClasspath pulls
        // this project's exportedProducts -> products -> copyResources -> resources ->
        // resourceGenerators, which cycles once this task is registered as a resource
        // generator (Task 3). dependencyClasspath excludes the project's own products; we
        // add classDirectory so the project's compiled Contract classes are loadable.
        val cp = toFiles((Compile / dependencyClasspath).value) :+ classesDir
        val resourceRoot = (Compile / resourceManaged).value
        val log = streams.value.log
        writeBlueprints(classesDir, cp, resourceRoot, log)
    }
```

- [ ] **Step 2: Update the scaladoc header** (lines 10-22). Replace the first bullet so it reflects the new behavior:

Old:
```scala
  * - `sbt blueprint` writes each contract's blueprint to
  *   `META-INF/scalus/blueprints/<ContractName>.json` in the classes directory.
```
New:
```scala
  * - `blueprint` generates each contract's CIP-57 JSON into
  *   `resourceManaged/main/META-INF/scalus/blueprints/<ContractName>.json`. It is
  *   registered as a resource generator, so `package`/`publish` embed it in the JAR at
  *   `META-INF/scalus/blueprints/<ContractName>.json`. Opt out with
  *   `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var.
```

- [ ] **Step 3: Verify formatting and both cross axes compile**

Run: `nix develop --command sbt scalusSbtPlugin/scalafmtCheck +scalusSbtPlugin/compile`
Expected: both end in `[success]`, no errors.

- [ ] **Step 4: Verify `blueprint` now writes to `resource_managed`** using the real examples project (publishes the plugin locally into the build, then runs it):

Run:
```bash
nix develop --command sbt scalusExamplesJVM/clean scalusExamplesJVM/blueprint
find scalus-examples/jvm/target -path '*resource_managed/main/META-INF/scalus/blueprints/*.json' | head
```
Expected: at least one path printed, e.g. `.../resource_managed/main/META-INF/scalus/blueprints/HelloCardanoContract.json`. Confirm NO new JSON appeared under `classes/META-INF/scalus/blueprints/` (old location):
```bash
find scalus-examples/jvm/target -path '*classes/META-INF/scalus/blueprints/*.json' | head
```
Expected: empty (or only stale files from before clean — the `clean` above prevents that).

- [ ] **Step 5: Commit**

```bash
git add scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala
git commit -m "refactor: write blueprints to resourceManaged instead of classDirectory"
```

---

### Task 3: Register the skip-gated resource generator

Add the `Def.taskIf` wrapper and wire it into `Compile / resourceGenerators` with a `blueprint / skip := false` default. After this, `package` embeds the JSON unless skipped.

**Files:**
- Modify: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` (add `blueprintGenerator` near `blueprintTask`; extend `projectSettings` at lines 111-117)

**Interfaces:**
- Consumes: `blueprint` task key and `blueprint / skip` setting (project/Zero config).
- Produces: `lazy val blueprintGenerator: Def.Initialize[Task[Seq[java.io.File]]]` — returns `Seq.empty` when skipped, else the result of `blueprint`. Registered in `Compile / resourceGenerators`.

- [ ] **Step 1 (RED): Confirm `package` does NOT yet embed the JSON.** With Task 2 applied but no generator, the JSON lives in `resource_managed` but nothing copies it into the JAR on a clean build:

```bash
nix develop --command sbt scalusExamplesJVM/clean scalusExamplesJVM/packageBin
JAR=$(find scalus-examples/jvm/target -name 'scalus-examples*_3-*.jar' ! -name '*-sources.jar' ! -name '*-javadoc.jar' | head -1)
unzip -l "$JAR" | grep -c 'META-INF/scalus/blueprints/.*\.json'
```
Expected: `0` (resource generators not yet registered, so the clean `packageBin` skips blueprint).

- [ ] **Step 2: Add the generator** immediately after `blueprintTask` (after its closing brace, ~line 52):

```scala
    /** Resource-generator wrapper around `blueprint`. Runs as part of `resources`
      * (so `package`/`publish`/`run`/`test` embed the JSON), unless suppressed by
      * `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var.
      *
      * Uses `Def.taskIf` so the gated branch is genuinely not evaluated: a plain
      * `Def.task { if (...) ... else blueprint.value }` would still run `blueprint`,
      * because `.value` lifts an unconditional task dependency.
      */
    lazy val blueprintGenerator: Def.Initialize[Task[Seq[java.io.File]]] = Def.taskIf {
        if ((blueprint / skip).value || sys.env.contains("SCALUS_SKIP_BLUEPRINT"))
            Seq.empty[java.io.File]
        else
            blueprint.value
    }
```

- [ ] **Step 3: Extend `projectSettings`** (lines 111-117) to add the skip default and the generator registration:

```scala
    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      // Def.uncached opts these out of sbt 2's task cache: `blueprint` returns Seq[File] (not a
      // cacheable output type) and writes files, and `deploy` performs network I/O. No-op on sbt 1
      // (via sbt2-compat).
      blueprint := Def.uncached(blueprintTask.value),
      // Default-on; opt out per project with `blueprint / skip := true`. Defined at project (Zero)
      // config so the generator's Compile-scoped read delegates to it AND a user's Zero-scoped
      // override is honored.
      blueprint / skip := false,
      // Embed blueprints in the JAR via the resources pipeline.
      Compile / resourceGenerators += blueprintGenerator.taskValue,
      deploy := Def.uncached(deployTask.evaluated)
    )
```

- [ ] **Step 4: Verify formatting and both cross axes compile**

Run: `nix develop --command sbt scalusSbtPlugin/scalafmtCheck +scalusSbtPlugin/compile`
Expected: both `[success]`. (If a cycle were introduced, a later step — not compile — would fail with "Cyclic reference"; the compile guards syntax/types.)

- [ ] **Step 5 (GREEN): `package` now embeds the JSON, and no cycle:**

```bash
nix develop --command sbt scalusExamplesJVM/clean scalusExamplesJVM/packageBin
JAR=$(find scalus-examples/jvm/target -name 'scalus-examples*_3-*.jar' ! -name '*-sources.jar' ! -name '*-javadoc.jar' | head -1)
unzip -l "$JAR" | grep 'META-INF/scalus/blueprints/.*\.json'
```
Expected: one or more lines, e.g. `META-INF/scalus/blueprints/HelloCardanoContract.json`. The command must NOT print "Cyclic reference involving".

- [ ] **Step 6: `blueprint / skip := true` opt-out excludes the JSON:**

```bash
nix develop --command sbt 'set scalusExamplesJVM / blueprint / skip := true' scalusExamplesJVM/clean scalusExamplesJVM/packageBin
JAR=$(find scalus-examples/jvm/target -name 'scalus-examples*_3-*.jar' ! -name '*-sources.jar' ! -name '*-javadoc.jar' | head -1)
unzip -l "$JAR" | grep -c 'META-INF/scalus/blueprints/.*\.json'
```
Expected: `0`.

- [ ] **Step 7: `SCALUS_SKIP_BLUEPRINT` env-var opt-out excludes the JSON:**

```bash
SCALUS_SKIP_BLUEPRINT=1 nix develop --command sbt scalusExamplesJVM/clean scalusExamplesJVM/packageBin
JAR=$(find scalus-examples/jvm/target -name 'scalus-examples*_3-*.jar' ! -name '*-sources.jar' ! -name '*-javadoc.jar' | head -1)
unzip -l "$JAR" | grep -c 'META-INF/scalus/blueprints/.*\.json'
```
Expected: `0`.

- [ ] **Step 8: Commit**

```bash
git add scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala
git commit -m "feat: embed blueprints in the JAR via resourceGenerators, with skip opt-out"
```

---

### Task 4: Update docs and the eject-examples template

Reflect the new output location and the auto-embed / opt-out behavior in user-facing docs.

**Files:**
- Modify: `scalus-site/content/dapp-development/sbt-plugin.mdx` (lines 80-86)
- Modify: `eject-examples` (lines 214-221, the `gen_getting_started` "Generate a CIP-57 blueprint" section)

**Interfaces:** none (documentation only).

- [ ] **Step 1: Update `sbt-plugin.mdx`.** Replace lines 80-86 (the `sbt blueprint package` block and the `Wrote` log) so `package` alone is the primary command and the opt-out is documented:

Old:
```mdx
```sh copy
sbt blueprint package
```

```
[info] Wrote META-INF/scalus/blueprints/MyContract.json
```
```
New:
```mdx
```sh copy
sbt package
```

`blueprint` is a resource generator, so `package` (and `publish`) embed the JSON automatically:

```
[info] Wrote META-INF/scalus/blueprints/MyContract.json
```

Run `sbt blueprint` to generate without packaging. To skip generation during `package`,
set `blueprint / skip := true` in `build.sbt`, or pass `SCALUS_SKIP_BLUEPRINT=1`.
```
(The `unzip -p my-project.jar META-INF/scalus/blueprints/MyContract.json` example below stays correct — the in-JAR path is unchanged.)

- [ ] **Step 2: Update `eject-examples`.** Replace lines 214-221 (note the escaped backticks `\`\`\`` are literal heredoc bytes — match them exactly):

Old:
```bash
## Generate a CIP-57 blueprint

\`\`\`sh
sbt blueprint
\`\`\`

Blueprint JSON is written to
\`target/scala-$SCALA_VERSION/classes/META-INF/scalus/blueprints/<Contract>.json\`.
```
New:
```bash
## CIP-57 blueprints

\`sbt package\` embeds a CIP-57 blueprint JSON for every Contract in the JAR at
\`META-INF/scalus/blueprints/<Contract>.json\`. Run \`sbt blueprint\` to generate
without packaging — output goes to
\`target/scala-$SCALA_VERSION/resource_managed/main/META-INF/scalus/blueprints/<Contract>.json\`.
Skip generation during \`package\` with \`blueprint / skip := true\` or \`SCALUS_SKIP_BLUEPRINT=1\`.
```

- [ ] **Step 3: Verify the eject template still renders.** Regenerate one example and confirm the README section is correct and the build still works:

```bash
./eject-examples --only hello /tmp/eject-check
grep -A6 "CIP-57 blueprints" /tmp/eject-check/hello/scalus/Readme.md 2>/dev/null || \
  find /tmp/eject-check -name 'Readme.md' -exec grep -l 'resource_managed' {} +
```
Expected: the rendered README shows the new `resource_managed` path and the skip note. (If the ejected dir layout differs, just confirm a generated `Readme.md` contains `resource_managed`.)

- [ ] **Step 4: Commit**

```bash
git add scalus-site/content/dapp-development/sbt-plugin.mdx eject-examples
git commit -m "docs: blueprints are embedded by sbt package; document skip opt-out"
```

---

### Task 5 (OPTIONAL follow-up): sbt scripted regression test

Not required for correctness — Tasks 2-3 are verified end-to-end against the real `scalusExamples` build (which covers embed, both opt-outs, and the no-cycle property). Add this only if persistent in-repo regression coverage is wanted. It is deferred because it requires `publishLocal` of `scalusJVM` + the cross-versioned `scalusPlugin` compiler plugin and version-templating into the test project — a self-contained mini-subsystem.

**Files (if pursued):**
- Create: `scalus-sbt-plugin/src/sbt-test/blueprint/package-embeds-blueprint/build.sbt`
- Create: `scalus-sbt-plugin/src/sbt-test/blueprint/package-embeds-blueprint/project/plugins.sbt`
- Create: `scalus-sbt-plugin/src/sbt-test/blueprint/package-embeds-blueprint/Main.scala` (a minimal `Contract` mirroring `HelloCardanoContract`)
- Create: `scalus-sbt-plugin/src/sbt-test/blueprint/package-embeds-blueprint/test` (script: `> package`, assert JSON present in jar; `> set blueprint / skip := true`, `> clean`, `> package`, assert absent)
- Modify: `build.sbt` `scalusSbtPlugin` settings — add `scriptedLaunchOpts += "-Dplugin.version=" + version.value`, `scriptedLaunchOpts += "-Dscalus.version=" + version.value`, and `scriptedDependencies := scriptedDependencies.dependsOn(scalus.jvm / publishLocal, scalusPlugin / publishLocal).value`.

**Note for the implementer:** spike this in isolation first — confirm `nix develop --command sbt scalusSbtPlugin/scripted` resolves the locally-published `org.scalus` artifacts at the templated version and that the compiler-plugin cross-version matches the test project's `scalaVersion` (use `3.3.7`, the LTS). If the version coupling proves brittle, leave the `scalusExamples`-based verification (Tasks 2-3) as the regression guard and drop this task.

---

## Self-Review

**Spec coverage** (against the agreed design in the conversation):
- Extract pure helper → Task 1. ✅
- Repoint `classDirectory` → `resourceManaged` → Task 2. ✅
- Fix the `fullClasspath` cycle (`dependencyClasspath + classDirectory`) → Task 2 (Global Constraints + code). ✅
- Register as `resourceGenerators`, scoped correctly, keep `sbt blueprint` working → Task 3 + Global Constraints (Zero-config `blueprint`). ✅
- Skip via `blueprint / skip` (using `Def.taskIf`) and via `SCALUS_SKIP_BLUEPRINT` → Task 3 (code + Steps 6-7). ✅
- `deploy` untouched → not modified in any task. ✅
- Doc/path updates (sbt-plugin.mdx + eject-examples) → Task 4. ✅
- Lands in `ScalusSbtPlugin`, alias untouched → Global Constraints. ✅

**Placeholder scan:** no TBD/TODO; every code and command step is concrete. ✅

**Type consistency:** `writeBlueprints(classesDir, cp, resourceRoot, log): Seq[java.io.File]` is defined in Task 1 and called with matching arg types in Tasks 1-2; `blueprintGenerator: Def.Initialize[Task[Seq[java.io.File]]]` in Task 3 matches `blueprint`'s element type and the `resourceGenerators += ...taskValue` contract. ✅
