# Blueprint Caching, Nested Layout, Aggregate + Explicit Pin Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cached blueprint generation with package-nested per-contract files + an aggregate `plutus.json` in every JAR, plus explicit `blueprintPin`/`blueprintCheck` tasks for committing pinned blueprints.

**Architecture:** Two layers per the spec (`docs/superpowers/specs/2026-06-28-blueprint-caching-and-naming-design.md`): (1) the `blueprint` task generates into `resourceManaged` (fresh in every JAR, git untouched), skipped entirely when a content fingerprint of `classDirectory` is unchanged; (2) a deliberate `blueprintPin` task copies the set to committed locations (`<base>/plutus.json` + `<base>/blueprints/<pkg>/...`). scalus-core gains an optional top-level `scalus.scalaVersion` extension key (valid CIP-57: root allows extra keys, preamble does not) and a `BlueprintTool` the plugin calls reflectively, degrading gracefully on older scalus-core.

**Tech Stack:** Scala 3 (scalus-core, cross JVM/JS/Native), Scala 2.12 + Scala 3 cross-compiled sbt plugin (sbt 1 + sbt 2 via sbt2-compat), jsoniter-scala, ScalaTest, MiMa.

## Global Constraints

- Plugin sources (`scalus-sbt-plugin/`) must compile under BOTH Scala 2.12.21 and Scala 3 (`crossScalaVersions := Seq("2.12.21", scala3NextVersion)`, `build.sbt:875`). No 2.13+/3-only APIs (`sizeIs`, `enum`, top-level defs, `then/do` syntax) in plugin code or plugin tests.
- scalus-core is cross-platform (JVM/JS/Native) – shared sources may use `java.util.*` and `scala.jdk.CollectionConverters` (available on all platforms), nothing JVM-only beyond that.
- Scala 3 code style per project CLAUDE.md: `{}` for top-level definitions, 4-space indent, `then` in `if` expressions.
- Run `sbtn scalafmtAll` before EVERY commit (CI runs `scalafmtCheckAll`; one unformatted file fails the build).
- Commit directly on `master` (no branches). Conventional commit style (`feat:`, `fix:`, `test:`, `docs:`). NEVER add a `Co-Authored-By: Claude` trailer. No em dashes (—) in commit messages or docs; use en dash (–) or hyphen.
- Prefer `sbtn` over `sbt` for all commands.
- MiMa: scalus-core compares against `scalusCompatibleVersion` (= 0.18.0). New exclusion filters go into `mimaBinaryIssueFilters` at `build.sbt:409` with a justifying comment, following the existing pattern there.

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/Blueprint.scala` | Modify | Add `ScalusInfo` case class + optional `scalus` field on `Blueprint` |
| `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/BlueprintTool.scala` | Create | `aggregate` + `stampScalaVersion` JSON-level operations (reflection-friendly signatures) |
| `scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintTest.scala` | Modify | Roundtrip/back-compat tests for the new field |
| `scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintToolTest.scala` | Create | Unit tests for `BlueprintTool` |
| `build.sbt` | Modify | MiMa filters (~line 409); ScalaTest dep for `scalusSbtPlugin` (~line 884) |
| `scalus-sbt-plugin/src/main/scala/scalus/sbt/BlueprintLayout.scala` | Create | Pure, sbt-free helpers: nested path, fingerprint, recursive listing, prune |
| `scalus-sbt-plugin/src/test/scala/scalus/sbt/BlueprintLayoutTest.scala` | Create | Unit tests for `BlueprintLayout` (run under 2.12 and 3) |
| `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` | Modify | Cached generation, nested layout, aggregate, `blueprintPin`/`blueprintCheck`/`blueprintScalaVersion` |

There is no scripted-test infrastructure for the plugin and none is added (an end-to-end scripted test would require publishing scalus-core locally, which pollutes `~/.ivy2/local` – see project memory). Automated coverage = core unit tests + plugin pure-logic unit tests + cross-compilation of both plugin axes. Manual e2e is a documented optional step at the end.

---

### Task 1: `ScalusInfo` field on `Blueprint` (core) + MiMa filters

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/Blueprint.scala:23-26` (case class header) and end of file (~line 359)
- Modify: `scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintTest.scala` (append tests)
- Modify: `build.sbt:409` (MiMa filters)

**Interfaces:**
- Consumes: existing `Blueprint`, `Preamble`, `Validator`, `Language` (all in package `scalus.cardano.blueprint`; `Language` imported from `scalus.cardano.ledger`).
- Produces: `case class ScalusInfo(scalaVersion: Option[String] = None)` and `Blueprint.scalus: Option[ScalusInfo]` (third field, default `None`). Task 2 and the plugin rely on these exact names.

- [ ] **Step 1: Write the failing tests**

Append inside `class BlueprintTest` (before the closing `}` at `BlueprintTest.scala:346`):

```scala
    test("Blueprint JSON roundtrip preserves the top-level scalus toolchain info") {
        val bp = Blueprint(
          Preamble("Stamped", "with scalus info", Language.PlutusV3),
          Seq(Validator(title = "Stamped")),
          scalus = Some(ScalusInfo(scalaVersion = Some("3.3.7")))
        )
        val json = bp.toJson()
        assert(json.contains("\"scalaVersion\": \"3.3.7\""))
        assert(Blueprint.fromJson(json) == bp)
    }

    test("Blueprint without scalus info serializes without the scalus key") {
        val bp = Blueprint(
          Preamble("Plain", "no scalus info", Language.PlutusV3),
          Seq(Validator(title = "Plain"))
        )
        assert(!bp.toJson().contains("\"scalus\""))
    }

    test("Blueprint JSON without a scalus key parses with scalus = None (back-compat)") {
        val json =
            """{ "preamble": { "title": "Old", "plutusVersion": "v3" }, "validators": [] }"""
        val bp = Blueprint.fromJson(json)
        assert(bp.scalus.isEmpty)
        assert(bp.preamble.title == "Old")
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `sbtn "scalusJVM/testOnly scalus.cardano.blueprint.BlueprintTest"`
Expected: COMPILATION FAILURE – `ScalusInfo` not found / no `scalus` parameter on `Blueprint`.

- [ ] **Step 3: Implement**

In `Blueprint.scala`, change the case class header (lines 23-26) from:

```scala
case class Blueprint(
    preamble: Preamble,
    validators: Seq[Validator] = Nil,
) {
```

to:

```scala
case class Blueprint(
    preamble: Preamble,
    validators: Seq[Validator] = Nil,
    scalus: Option[ScalusInfo] = None,
) {
```

Append at the end of the file (after `object Validator`, line ~359):

```scala
/** Scalus-specific toolchain provenance, stored as a top-level extension key of the blueprint
  * document. CIP-57 sets `additionalProperties: false` on `preamble` and `compiler`, but not on
  * the document root, so an extra root key keeps the blueprint valid against the official schema.
  *
  * The Scalus version already lives in `preamble.compiler.version`; this records what the JSON
  * has no other slot for – the Scala toolchain that compiled the contracts, which determines the
  * generated UPLC (different Scala versions can produce different scripts and hashes).
  *
  * @param scalaVersion
  *   the full Scala version the contracts were compiled with (e.g. "3.3.7")
  */
case class ScalusInfo(scalaVersion: Option[String] = None)

object ScalusInfo {
    given JsonValueCodec[ScalusInfo] = JsonCodecMaker.make
}
```

Note: jsoniter's `JsonCodecMaker` omits `None` fields by default, so blueprints without the stamp keep exactly their current JSON – no `"scalus"` key. Unknown keys are skipped on read, so older scalus-core versions still parse stamped files.

- [ ] **Step 4: Run tests to verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.cardano.blueprint.BlueprintTest"`
Expected: PASS (all tests, including the pre-existing ones).

- [ ] **Step 5: Check MiMa and add filters**

Run: `sbtn scalusJVM/mimaReportBinaryIssues`
Expected: FAILURE reporting changed signatures on `scalus.cardano.blueprint.Blueprint` – roughly `copy`, `apply`, and `this` (constructor). Add exclusions for EXACTLY what it reports to `mimaBinaryIssueFilters` in `build.sbt` (after line 422, inside the existing `Seq(...)`), e.g.:

```scala
        // Blueprint gained the optional `scalus` toolchain-provenance field (extra top-level
        // CIP-57 key). Constructor/apply/copy arity changed; source-compatible via the default.
        ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.cardano.blueprint.Blueprint.copy"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.cardano.blueprint.Blueprint.apply"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scalus.cardano.blueprint.Blueprint.this"),
```

Re-run: `sbtn scalusJVM/mimaReportBinaryIssues`
Expected: PASS.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/cardano/blueprint/Blueprint.scala \
        scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintTest.scala build.sbt
git commit -m "feat(blueprint): add optional top-level scalus.scalaVersion provenance to Blueprint

CIP-57 forbids extra preamble/compiler keys but allows extra root keys, so the
Scala toolchain version (which determines generated UPLC) is recorded as a
top-level 'scalus' extension. Absent by default; old JSON still parses."
```

---

### Task 2: `BlueprintTool` (core) – stamp + aggregate

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/BlueprintTool.scala`
- Create: `scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintToolTest.scala`

**Interfaces:**
- Consumes: `Blueprint`, `Preamble`, `CompilerInfo`, `Validator`, `ScalusInfo` (Task 1), `Language`.
- Produces (called reflectively by the plugin in Task 4 – signatures are load-bearing, do not rename):
  - `BlueprintTool.stampScalaVersion(json: String, scalaVersion: String): String`
  - `BlueprintTool.aggregate(jsons: java.util.List[String], title: String, version: String, scalaVersion: String): String`

- [ ] **Step 1: Write the failing tests**

Create `scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintToolTest.scala`:

```scala
package scalus.cardano.blueprint

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Language

import scala.jdk.CollectionConverters.*

class BlueprintToolTest extends AnyFunSuite {

    private def bp(title: String, version: String, compiledCode: String): Blueprint =
        Blueprint(
          Preamble(
            title = title,
            version = Some(version),
            compiler = Some(CompilerInfo("scalus", Some("0.19.0"))),
            plutusVersion = Some(Language.PlutusV3)
          ),
          Seq(Validator(title = title, compiledCode = Some(compiledCode)))
        )

    test("stampScalaVersion adds the top-level scalus key and preserves everything else") {
        val original = bp("A", "1.0.0", "aa")
        val stamped =
            Blueprint.fromJson(BlueprintTool.stampScalaVersion(original.toJson(), "3.3.7"))
        assert(stamped == original.copy(scalus = Some(ScalusInfo(Some("3.3.7")))))
    }

    test("stampScalaVersion overwrites a previous stamp") {
        val original = bp("A", "1.0.0", "aa").copy(scalus = Some(ScalusInfo(Some("3.3.6"))))
        val stamped =
            Blueprint.fromJson(BlueprintTool.stampScalaVersion(original.toJson(), "3.3.7"))
        assert(stamped.scalus == Some(ScalusInfo(Some("3.3.7"))))
    }

    test("aggregate merges validators in input order under a project-level preamble") {
        val a = bp("A", "1.0.0", "aa")
        val b = bp("B", "2.0.0", "bb")
        val json = BlueprintTool.aggregate(
          List(a.toJson(), b.toJson()).asJava,
          "my-project",
          "0.1.0",
          "3.3.7"
        )
        val agg = Blueprint.fromJson(json)
        assert(agg.preamble.title == "my-project")
        assert(agg.preamble.version.contains("0.1.0"))
        assert(agg.preamble.compiler.contains(CompilerInfo("scalus", Some("0.19.0"))))
        assert(agg.preamble.plutusVersion.contains(Language.PlutusV3))
        assert(agg.validators.map(_.title) == Seq("A", "B"))
        assert(agg.validators.flatMap(_.compiledCode) == Seq("aa", "bb"))
        assert(agg.scalus == Some(ScalusInfo(Some("3.3.7"))))
    }

    test("aggregate leaves plutusVersion unset when inputs mix Plutus versions") {
        val a = bp("A", "1.0.0", "aa")
        val v2 = bp("B", "2.0.0", "bb")
        val b = v2.copy(preamble = v2.preamble.copy(plutusVersion = Some(Language.PlutusV2)))
        val json = BlueprintTool.aggregate(
          List(a.toJson(), b.toJson()).asJava,
          "my-project",
          "0.1.0",
          "3.3.7"
        )
        assert(Blueprint.fromJson(json).preamble.plutusVersion.isEmpty)
    }

    test("aggregate of an empty list yields an empty validators array") {
        val json =
            BlueprintTool.aggregate(List.empty[String].asJava, "my-project", "0.1.0", "3.3.7")
        val agg = Blueprint.fromJson(json)
        assert(agg.validators.isEmpty)
        assert(agg.preamble.title == "my-project")
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `sbtn "scalusJVM/testOnly scalus.cardano.blueprint.BlueprintToolTest"`
Expected: COMPILATION FAILURE – `BlueprintTool` not found.

- [ ] **Step 3: Implement**

Create `scalus-core/shared/src/main/scala/scalus/cardano/blueprint/BlueprintTool.scala`:

```scala
package scalus.cardano.blueprint

import scala.jdk.CollectionConverters.*

/** JSON-level blueprint operations used by the Scalus sbt plugin.
  *
  * The plugin (Scala 2.12/3, running inside sbt) calls these reflectively across a
  * `URLClassLoader` boundary, so the signatures use only `java.*` and `String` types.
  * Do not rename or change signatures without updating `ScalusSbtPlugin`.
  */
object BlueprintTool {

    /** Returns `json` with the top-level `scalus.scalaVersion` extension key stamped in,
      * overwriting any previous stamp. See [[ScalusInfo]] for why this lives at the root.
      */
    def stampScalaVersion(json: String, scalaVersion: String): String =
        Blueprint
            .fromJson(json)
            .copy(scalus = Some(ScalusInfo(Some(scalaVersion))))
            .toJson()

    /** Merges per-contract blueprint documents into one CIP-57 document (Aiken-style
      * `plutus.json`): all validators concatenated in input order under a project-level
      * preamble.
      *
      * Compiler info is carried over from the first input that has it (all inputs of one build
      * share the same Scalus version). `plutusVersion` is set only when all inputs agree.
      *
      * @param jsons
      *   per-contract blueprint JSON documents, in the desired validator order
      * @param title
      *   project-level title (typically the sbt project name)
      * @param version
      *   project-level version (typically the sbt project version)
      * @param scalaVersion
      *   the Scala toolchain that compiled the contracts, recorded as `scalus.scalaVersion`
      */
    def aggregate(
        jsons: java.util.List[String],
        title: String,
        version: String,
        scalaVersion: String
    ): String = {
        val bps = jsons.asScala.toSeq.map(Blueprint.fromJson)
        val plutusVersions = bps.flatMap(_.preamble.plutusVersion).distinct
        val preamble = Preamble(
          title = title,
          version = Some(version),
          compiler = bps.flatMap(_.preamble.compiler).headOption,
          plutusVersion = if plutusVersions.length == 1 then plutusVersions.headOption else None
        )
        Blueprint(
          preamble,
          bps.flatMap(_.validators),
          Some(ScalusInfo(Some(scalaVersion)))
        ).toJson()
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.cardano.blueprint.BlueprintToolTest"`
Expected: PASS (5 tests).

- [ ] **Step 5: Verify cross-platform compilation and MiMa**

Run: `sbtn scalusJS/compile scalusNative/compile scalusJVM/mimaReportBinaryIssues`
Expected: compile PASS on both platforms (`java.util.List` + `CollectionConverters` exist on JS/Native javalib); MiMa PASS (`BlueprintTool`/`ScalusInfo` are new classes – additive).

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/cardano/blueprint/BlueprintTool.scala \
        scalus-core/shared/src/test/scala/scalus/cardano/blueprint/BlueprintToolTest.scala
git commit -m "feat(blueprint): BlueprintTool.aggregate and stampScalaVersion for the sbt plugin

Reflection-friendly (java-typed) helpers: merge per-contract blueprints into one
Aiken-style plutus.json document and stamp the scalus.scalaVersion extension."
```

---

### Task 3: `BlueprintLayout` pure helpers (plugin) + test wiring

**Files:**
- Modify: `build.sbt:884` (add ScalaTest to `scalusSbtPlugin`)
- Create: `scalus-sbt-plugin/src/main/scala/scalus/sbt/BlueprintLayout.scala`
- Create: `scalus-sbt-plugin/src/test/scala/scalus/sbt/BlueprintLayoutTest.scala`

**Interfaces:**
- Consumes: nothing project-internal (pure `java.io`/`java.nio`/`java.security`).
- Produces (used by Tasks 4-5):
  - `BlueprintLayout.Scheme: Int` – layout-format constant, bump to invalidate caches
  - `BlueprintLayout.contractRelativePath(className: String): String` – `"scalus.examples.auction.AuctionContract$"` → `"scalus/examples/auction/AuctionContract.json"`
  - `BlueprintLayout.listFilesRecursively(dir: java.io.File): Seq[java.io.File]` – regular files, sorted by path; empty for a non-directory
  - `BlueprintLayout.fingerprint(header: String, root: java.io.File, files: Seq[java.io.File]): String` – SHA-256 hex over header + each file's root-relative path + content
  - `BlueprintLayout.pruneStale(root: java.io.File, keep: Set[java.io.File]): Seq[java.io.File]` – deletes `.json` files under `root` not in `keep`, removes emptied directories, returns deleted files; no-op for a non-existent root

**IMPORTANT:** this module and its test compile under Scala 2.12 AND Scala 3. Stick to the common syntax subset (no `enum`, no `sizeIs`, no significant-indentation syntax, braces everywhere).

- [ ] **Step 1: Add ScalaTest to the plugin project**

In `build.sbt`, inside the `scalusSbtPlugin` settings block (after `scalacOptions ++= Seq("-deprecation", "-feature"),` at line 884), add:

```scala
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % Test,
```

- [ ] **Step 2: Write the failing tests**

Create `scalus-sbt-plugin/src/test/scala/scalus/sbt/BlueprintLayoutTest.scala`:

```scala
package scalus.sbt

import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Files

class BlueprintLayoutTest extends AnyFunSuite {

    private def tempDir(): File = {
        val dir = Files.createTempDirectory("blueprint-layout-test").toFile
        dir.deleteOnExit()
        dir
    }

    private def write(dir: File, relPath: String, content: String): File = {
        val f = new File(dir, relPath)
        f.getParentFile.mkdirs()
        Files.write(f.toPath, content.getBytes("UTF-8"))
        f
    }

    test("contractRelativePath nests by package and strips the module suffix") {
        assert(
          BlueprintLayout.contractRelativePath("scalus.examples.auction.AuctionContract$") ==
              "scalus/examples/auction/AuctionContract.json"
        )
        assert(
          BlueprintLayout.contractRelativePath("scalus.examples.HelloCardanoContract$") ==
              "scalus/examples/HelloCardanoContract.json"
        )
    }

    test("contractRelativePath handles classes without a package") {
        assert(BlueprintLayout.contractRelativePath("RootContract$") == "RootContract.json")
        assert(BlueprintLayout.contractRelativePath("RootContract") == "RootContract.json")
    }

    test("listFilesRecursively returns regular files sorted by path, empty for non-dirs") {
        val dir = tempDir()
        write(dir, "b/two.class", "2")
        write(dir, "a/one.class", "1")
        write(dir, "top.class", "0")
        val listed = BlueprintLayout.listFilesRecursively(dir).map(_.getName)
        // sorted by absolute path: <dir>/a/one.class < <dir>/b/two.class < <dir>/top.class
        assert(listed == Seq("one.class", "two.class", "top.class"))
        assert(BlueprintLayout.listFilesRecursively(new File(dir, "missing")).isEmpty)
    }

    test("fingerprint is stable for identical content and changes with content/header/path") {
        val dir1 = tempDir()
        write(dir1, "a/X.class", "same")
        val dir2 = tempDir()
        write(dir2, "a/X.class", "same")
        def fp(dir: File, header: String): String =
            BlueprintLayout.fingerprint(header, dir, BlueprintLayout.listFilesRecursively(dir))
        assert(fp(dir1, "h") == fp(dir2, "h"))
        assert(fp(dir1, "h") != fp(dir1, "other-header"))
        write(dir2, "a/X.class", "changed")
        assert(fp(dir1, "h") != fp(dir2, "h"))
        val dir3 = tempDir()
        write(dir3, "a/Y.class", "same") // same content, different path
        assert(fp(dir1, "h") != fp(dir3, "h"))
    }

    test("pruneStale deletes unkept json files and emptied dirs, keeps everything else") {
        val root = tempDir()
        val keep = write(root, "scalus/examples/Keep.json", "{}")
        write(root, "scalus/old/Stale.json", "{}")
        val notJson = write(root, "scalus/old/notes.txt", "keep me")
        val deleted = BlueprintLayout.pruneStale(root, Set(keep))
        assert(deleted.map(_.getName) == Seq("Stale.json"))
        assert(keep.isFile)
        assert(notJson.isFile) // non-json files are never deleted
        assert(new File(root, "scalus/examples").isDirectory)
    }

    test("pruneStale removes directories left empty and is a no-op on a missing root") {
        val root = tempDir()
        write(root, "a/b/OnlyOne.json", "{}")
        BlueprintLayout.pruneStale(root, Set.empty[File])
        assert(!new File(root, "a").exists()) // a/b/OnlyOne.json deleted, a/b and a emptied
        assert(root.isDirectory) // the root itself survives
        assert(BlueprintLayout.pruneStale(new File(root, "missing"), Set.empty[File]).isEmpty)
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `sbtn "scalusSbtPlugin/+test"`
Expected: COMPILATION FAILURE – `BlueprintLayout` not found (on both Scala axes).

- [ ] **Step 4: Implement**

Create `scalus-sbt-plugin/src/main/scala/scalus/sbt/BlueprintLayout.scala`:

```scala
package scalus.sbt

import java.io.File
import java.nio.file.Files
import java.security.MessageDigest

/** Pure helpers for the blueprint layout, cache fingerprint and pruning.
  *
  * Deliberately sbt-free (plain `java.io`/`java.nio`) so it unit-tests without an sbt
  * harness and compiles unchanged under Scala 2.12 (sbt 1) and Scala 3 (sbt 2).
  */
object BlueprintLayout {

    /** Layout/format scheme; part of the cache fingerprint header. Bump whenever the
      * generated file layout or JSON shape changes, so stale caches regenerate.
      */
    val Scheme: Int = 1

    /** Derives the package-nested output path for a Contract's blueprint:
      * `scalus.examples.auction.AuctionContract$` -> `scalus/examples/auction/AuctionContract.json`.
      * Nesting by package prevents same-simple-name validators from overwriting each other.
      */
    def contractRelativePath(className: String): String = {
        val parts = className.stripSuffix("$").split('.')
        (parts.init :+ (parts.last + ".json")).mkString("/")
    }

    /** All regular files under `dir` (recursively), sorted by absolute path for determinism.
      * Empty when `dir` does not exist or is not a directory.
      */
    def listFilesRecursively(dir: File): Seq[File] = {
        val buf = scala.collection.mutable.ArrayBuffer.empty[File]
        def walk(f: File): Unit = {
            val children = f.listFiles()
            if (children != null) children.foreach { c =>
                if (c.isDirectory) walk(c) else if (c.isFile) buf += c
            }
        }
        if (dir.isDirectory) walk(dir)
        buf.sortBy(_.getAbsolutePath).toList
    }

    /** SHA-256 hex digest over `header` plus every file's root-relative path and content.
      * Content-based (not mtime-based), so a rebuild that rewrites identical class files
      * still counts as unchanged.
      */
    def fingerprint(header: String, root: File, files: Seq[File]): String = {
        val md = MessageDigest.getInstance("SHA-256")
        md.update(header.getBytes("UTF-8"))
        files.foreach { f =>
            val rel = root.toPath.relativize(f.toPath).toString.replace('\\', '/')
            md.update(0.toByte)
            md.update(rel.getBytes("UTF-8"))
            md.update(Files.readAllBytes(f.toPath))
        }
        md.digest().map(b => "%02x".format(b)).mkString
    }

    /** Deletes every `.json` file under `root` that is not in `keep` (compared canonically),
      * then removes directories left empty. Never touches non-json files. Returns the
      * deleted files. No-op when `root` does not exist.
      */
    def pruneStale(root: File, keep: Set[File]): Seq[File] = {
        val keepCanonical = keep.map(_.getCanonicalFile)
        val stale = listFilesRecursively(root).filter { f =>
            f.getName.endsWith(".json") && !keepCanonical.contains(f.getCanonicalFile)
        }
        stale.foreach(f => Files.deleteIfExists(f.toPath))
        // remove emptied directories bottom-up, keeping root itself
        def dirs(f: File): Seq[File] = {
            val children = f.listFiles()
            if (children == null) Seq.empty
            else children.filter(_.isDirectory).toSeq.flatMap(d => dirs(d) :+ d)
        }
        if (root.isDirectory) {
            dirs(root)
                .sortBy(-_.getAbsolutePath.length)
                .foreach { d =>
                    val children = d.listFiles()
                    if (children != null && children.isEmpty) Files.deleteIfExists(d.toPath)
                }
        }
        stale
    }
}
```

- [ ] **Step 5: Run tests to verify they pass on both Scala axes**

Run: `sbtn "scalusSbtPlugin/+test"`
Expected: PASS under Scala 2.12.21 and under Scala 3 (6 tests each).

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add build.sbt scalus-sbt-plugin/src/main/scala/scalus/sbt/BlueprintLayout.scala \
        scalus-sbt-plugin/src/test/scala/scalus/sbt/BlueprintLayoutTest.scala
git commit -m "feat(sbt-plugin): BlueprintLayout pure helpers (nested paths, fingerprint, prune)

sbt-free logic for the upcoming cached blueprint generation: package-nested
output paths, content-based SHA-256 fingerprint of classDirectory, and stale
.json pruning. Unit-tested on both plugin Scala axes (2.12 and 3)."
```

---

### Task 4: Cached generation with nested layout, stamping and aggregate (plugin)

**Files:**
- Modify: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` – replace `blueprintTask` (lines 41-54), replace `writeBlueprints` (156-170) and `loadContracts` (182-218) with the methods below.

**Interfaces:**
- Consumes: `BlueprintLayout.*` (Task 3); `BlueprintTool.stampScalaVersion` / `BlueprintTool.aggregate` from scalus-core (Task 2) via reflection; existing `readManifestClassNames`, `simpleName` (both KEPT – `deploy` uses them).
- Produces: `blueprint` task now returns per-contract files at `resourceManaged/main/META-INF/scalus/blueprints/<pkg>/<Class>.json` plus the aggregate at `resourceManaged/main/plutus.json`. Task 5's pin/check rely on exactly these locations.

- [ ] **Step 1: Replace `blueprintTask`**

Replace lines 41-54 of `ScalusSbtPlugin.scala` with:

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
        val cacheDir = streams.value.cacheDirectory / "scalus-blueprints"
        val log = streams.value.log
        generateBlueprints(
          classesDir,
          cp,
          resourceRoot,
          cacheDir,
          projectName = name.value,
          projectVersion = version.value,
          scalaVer = scalaVersion.value,
          log
        )
    }
```

- [ ] **Step 2: Replace `writeBlueprints` and `loadContracts`**

Delete `writeBlueprints` (lines 156-170) and `loadContracts` (lines 182-218). In their place add (keeping `simpleName` and `readManifestClassNames` untouched):

```scala
    /** Generate per-contract blueprints (package-nested) plus an aggregate `plutus.json`
      * under `resourceRoot`, skipping all work when the content fingerprint of `classesDir`
      * matches the previous run.
      *
      * Layout:
      *   - `resourceRoot/META-INF/scalus/blueprints/<package>/<Contract>.json` per contract
      *   - `resourceRoot/plutus.json` aggregate (JAR root; Aiken-style single document)
      *
      * Stale outputs from renamed/deleted contracts are pruned so the JAR always holds
      * exactly the current set. Returns the produced (or cached) files.
      */
    private def generateBlueprints(
        classesDir: java.io.File,
        cp: Seq[java.io.File],
        resourceRoot: java.io.File,
        cacheDir: java.io.File,
        projectName: String,
        projectVersion: String,
        scalaVer: String,
        log: sbt.util.Logger
    ): Seq[java.io.File] = {
        val outDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        val aggregateFile = resourceRoot / "plutus.json"
        val header =
            s"scheme=${BlueprintLayout.Scheme};scala=$scalaVer;version=$projectVersion;name=$projectName"
        val classFiles = BlueprintLayout.listFilesRecursively(classesDir)
        val fp = BlueprintLayout.fingerprint(header, classesDir, classFiles)
        val fpFile = cacheDir / "fingerprint"

        readCachedOutputs(fpFile, fp, resourceRoot) match {
            case Some(cached) =>
                log.debug("Blueprints are up to date; skipping generation")
                cached
            case None =>
                val classNames = readManifestClassNames(classesDir).sorted
                if (classNames.isEmpty) {
                    log.warn("No Contract implementations found")
                    Seq.empty
                } else {
                    IO.createDirectory(outDir)
                    val urls = cp.map(_.toURI.toURL).toArray
                    val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
                    try {
                        val jsons = loadContractJsons(cl, classNames, log)
                        val tool = BlueprintToolBridge.load(cl, log)
                        val utf8 = java.nio.charset.StandardCharsets.UTF_8
                        val perContract = jsons.map { case (className, json) =>
                            val file = outDir / BlueprintLayout.contractRelativePath(className)
                            IO.write(file, tool.stamp(json, scalaVer), utf8)
                            log.info(s"Wrote ${file.relativeTo(resourceRoot).getOrElse(file)}")
                            file
                        }
                        val aggregated =
                            tool.aggregate(jsons.map(_._2), projectName, projectVersion, scalaVer)
                                .map { json =>
                                    IO.write(aggregateFile, json, utf8)
                                    log.info(
                                      s"Wrote ${aggregateFile.relativeTo(resourceRoot).getOrElse(aggregateFile)}"
                                    )
                                    aggregateFile
                                }
                        val produced = perContract ++ aggregated.toSeq
                        BlueprintLayout
                            .pruneStale(outDir, perContract.toSet)
                            .foreach(f => log.info(s"Pruned stale blueprint $f"))
                        // Cache only complete runs, so contracts that failed to load are
                        // retried on the next build instead of being silently skipped.
                        if (jsons.size == classNames.size)
                            writeCachedOutputs(fpFile, fp, resourceRoot, produced)
                        produced
                    } finally {
                        cl.close()
                    }
                }
        }
    }

    /** Load each Contract's blueprint JSON via reflection. Failures are logged and skipped. */
    private def loadContractJsons(
        cl: ClassLoader,
        classNames: Seq[String],
        log: sbt.util.Logger
    ): Seq[(String, String)] =
        classNames.flatMap { className =>
            try {
                val cls = cl.loadClass(className)
                val instance = cls.getField("MODULE$").get(null)
                val method = cls.getMethod("blueprintJson")
                val json = method.invoke(instance).asInstanceOf[String]
                Some((className, json))
            } catch {
                case e: java.lang.reflect.InvocationTargetException =>
                    log.error(s"Failed to load Contract $className: ${e.getCause.getMessage}")
                    None
                case e: Exception =>
                    log.error(s"Failed to load Contract $className: ${e.getMessage}")
                    None
            }
        }

    /** Cache file format: fingerprint on the first line, then one produced path per line,
      * relative to the managed-resource root. A hit requires an identical fingerprint AND
      * every recorded file still present on disk.
      */
    private def readCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File
    ): Option[Seq[java.io.File]] =
        if (!fpFile.isFile) None
        else
            IO.readLines(fpFile) match {
                case `fp` :: rest =>
                    val files = rest.map(resourceRoot / _)
                    if (files.forall(_.isFile)) Some(files) else None
                case _ => None
            }

    private def writeCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File,
        produced: Seq[java.io.File]
    ): Unit = {
        val rels = produced.flatMap(f => IO.relativize(resourceRoot, f))
        IO.writeLines(fpFile, fp +: rels)
    }
```

- [ ] **Step 3: Add the `BlueprintToolBridge` reflective adapter**

Add after `writeCachedOutputs` (still inside `object ScalusSbtPlugin`):

```scala
    /** Reflective bridge to scalus-core's `BlueprintTool`. Degrades gracefully when the
      * project depends on a scalus version that predates it: per-contract files are written
      * unstamped and no aggregate is produced.
      */
    private trait BlueprintToolBridge {
        def stamp(json: String, scalaVer: String): String
        def aggregate(
            jsons: Seq[String],
            title: String,
            version: String,
            scalaVer: String
        ): Option[String]
    }

    private object BlueprintToolBridge {
        def load(cl: ClassLoader, log: sbt.util.Logger): BlueprintToolBridge =
            try {
                val cls = cl.loadClass("scalus.cardano.blueprint.BlueprintTool$")
                val instance = cls.getField("MODULE$").get(null)
                val stampM = cls.getMethod("stampScalaVersion", classOf[String], classOf[String])
                val aggM = cls.getMethod(
                  "aggregate",
                  classOf[java.util.List[_]],
                  classOf[String],
                  classOf[String],
                  classOf[String]
                )
                new BlueprintToolBridge {
                    def stamp(json: String, scalaVer: String): String =
                        stampM.invoke(instance, json, scalaVer).asInstanceOf[String]
                    def aggregate(
                        jsons: Seq[String],
                        title: String,
                        version: String,
                        scalaVer: String
                    ): Option[String] = {
                        val list = new java.util.ArrayList[String]
                        jsons.foreach(j => list.add(j))
                        Some(aggM.invoke(instance, list, title, version, scalaVer).asInstanceOf[String])
                    }
                }
            } catch {
                case _: ClassNotFoundException | _: NoSuchMethodException =>
                    log.warn(
                      "scalus-core on the classpath predates BlueprintTool: blueprints are " +
                          "written without scalus.scalaVersion and no aggregate plutus.json is " +
                          "produced. Upgrade the scalus dependency for full output."
                    )
                    new BlueprintToolBridge {
                        def stamp(json: String, scalaVer: String): String = json
                        def aggregate(
                            jsons: Seq[String],
                            title: String,
                            version: String,
                            scalaVer: String
                        ): Option[String] = None
                    }
            }
    }
```

- [ ] **Step 4: Compile on both axes and run plugin tests**

Run: `sbtn "scalusSbtPlugin/+compile" "scalusSbtPlugin/+test"`
Expected: PASS on Scala 2.12 (sbt 1.5.8 API) and Scala 3 (sbt 2.0.0 API). If `IO.relativize`, `IO.writeLines` or `file.relativeTo` fail to resolve on the sbt 2 axis, route them through the `sbtcompat.PluginCompat` shim (see the existing `toFiles` usage at line 8/50) – but they are expected to exist in both.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala
git commit -m "feat(sbt-plugin): cached, package-nested blueprint generation with aggregate plutus.json

blueprint now skips entirely (no classloading, no SIR->UPLC) when a SHA-256
fingerprint of classDirectory plus scheme/scala/version header is unchanged.
Output layout: META-INF/scalus/blueprints/<package>/<Contract>.json per
contract (prevents same-name clashes) plus an Aiken-style aggregate at the
resource root /plutus.json; scalus.scalaVersion stamped via scalus-core's
BlueprintTool (graceful fallback for older scalus versions). Stale outputs
of renamed/deleted contracts are pruned."
```

---

### Task 5: `blueprintPin`, `blueprintCheck`, `blueprintScalaVersion` (plugin)

**Files:**
- Modify: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` – extend `autoImport` (lines 28-37), extend `projectSettings` (lines 126-138), add two tasks, rewrite the object scaladoc (lines 10-25).

**Interfaces:**
- Consumes: `blueprint` task outputs at `resourceManaged/META-INF/scalus/blueprints/**` + `resourceManaged/plutus.json` (Task 4); `BlueprintLayout.pruneStale`, `BlueprintLayout.listFilesRecursively` (Task 3).
- Produces: `blueprintPin: TaskKey[Seq[java.io.File]]`, `blueprintCheck: TaskKey[Unit]`, `blueprintScalaVersion: SettingKey[Option[String]]`; pinned files at `<base>/plutus.json` and `<base>/blueprints/<pkg>/<Contract>.json`.

- [ ] **Step 1: Extend `autoImport`**

Replace the `autoImport` object (lines 28-37) with:

```scala
    object autoImport {
        val blueprint =
            taskKey[Seq[java.io.File]](
              "Generate CIP-57 blueprint JSON for all Contract implementations"
            )
        val blueprintPin =
            taskKey[Seq[java.io.File]](
              "Copy generated blueprints to the committed pin locations (plutus.json and blueprints/)"
            )
        val blueprintCheck =
            taskKey[Unit](
              "Fail if the committed blueprint pins differ from freshly generated blueprints"
            )
        val blueprintScalaVersion =
            settingKey[Option[String]](
              "If set, blueprintPin refuses to run (and blueprintCheck skips) under any other Scala version"
            )
        val deploy =
            inputKey[Unit](
              "Deploy a contract as a reference script UTXO"
            )
    }
```

- [ ] **Step 2: Add the pin and check tasks**

Add after `blueprintGenerator` (line 69):

```scala
    /** Copy the freshly generated blueprints to the committed pin locations:
      * `<base>/plutus.json` (aggregate, the path Aiken tooling expects) and
      * `<base>/blueprints/<package>/<Contract>.json`. A deliberate act – run it when a
      * usable version should be pinned, then commit the result; git history is the pin
      * history. Stale pins of renamed/deleted contracts are pruned.
      */
    lazy val blueprintPinTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val generated = blueprint.value
        val log = streams.value.log
        blueprintScalaVersion.value.foreach { required =>
            if (scalaVersion.value != required)
                sys.error(
                  s"blueprintPin is restricted to Scala $required (blueprintScalaVersion), " +
                      s"but the current scalaVersion is ${scalaVersion.value}. " +
                      s"Run `++$required blueprintPin` instead."
                )
        }
        val resourceRoot = (Compile / resourceManaged).value
        val genDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        val base = baseDirectory.value
        val pinDir = base / "blueprints"
        val copied = scala.collection.mutable.ListBuffer.empty[java.io.File]
        val genAgg = resourceRoot / "plutus.json"
        if (genAgg.isFile) {
            IO.copyFile(genAgg, base / "plutus.json")
            copied += base / "plutus.json"
        }
        val perContract = generated.flatMap(f => IO.relativize(genDir, f).map(rel => (f, rel)))
        perContract.foreach { case (f, rel) =>
            val dest = pinDir / rel
            IO.copyFile(f, dest)
            copied += dest
        }
        BlueprintLayout
            .pruneStale(pinDir, perContract.map { case (_, rel) => pinDir / rel }.toSet)
            .foreach(f => log.info(s"Pruned stale pin $f"))
        log.info(
          s"Pinned ${copied.size} blueprint file(s); review and commit plutus.json and blueprints/"
        )
        copied.toList
    }

    /** Fail when the committed pins differ from freshly generated blueprints. Intended for
      * release pipelines (it cannot stay green between pins under git-derived snapshot
      * versions). Skips with a note under a non-primary Scala version in cross-builds, so
      * `+blueprintCheck` does not produce false failures.
      *
      * `Def.taskIf` so the skipped branch genuinely does not evaluate `blueprint`.
      */
    lazy val blueprintCheckTask: Def.Initialize[Task[Unit]] = Def.taskIf {
        if (blueprintScalaVersion.value.exists(_ != scalaVersion.value)) {
            streams.value.log.info(
              s"blueprintCheck skipped: pins are maintained under Scala " +
                  s"${blueprintScalaVersion.value.getOrElse("?")}, current is ${scalaVersion.value}"
            )
        } else {
            val generated = blueprint.value
            val log = streams.value.log
            val resourceRoot = (Compile / resourceManaged).value
            val genDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
            val base = baseDirectory.value
            val pinDir = base / "blueprints"
            def sameBytes(a: java.io.File, b: java.io.File): Boolean =
                a.isFile && b.isFile && java.util.Arrays.equals(IO.readBytes(a), IO.readBytes(b))
            val problems = scala.collection.mutable.ListBuffer.empty[String]
            val genAgg = resourceRoot / "plutus.json"
            if (genAgg.isFile && !sameBytes(genAgg, base / "plutus.json"))
                problems += s"${base / "plutus.json"} is missing or stale"
            val perContract = generated.flatMap(f => IO.relativize(genDir, f).map(rel => (f, rel)))
            perContract.foreach { case (f, rel) =>
                if (!sameBytes(f, pinDir / rel)) problems += s"${pinDir / rel} is missing or stale"
            }
            val expected = perContract.map { case (_, rel) => (pinDir / rel).getCanonicalFile }.toSet
            BlueprintLayout.listFilesRecursively(pinDir).foreach { f =>
                if (f.getName.endsWith(".json") && !expected.contains(f.getCanonicalFile))
                    problems += s"$f is pinned but no longer generated"
            }
            if (problems.nonEmpty)
                sys.error(
                  ("Pinned blueprints are stale; run `blueprintPin` and commit:" +: problems.toList)
                      .mkString("\n  ")
                )
            log.info("Pinned blueprints are up to date")
        }
    }
```

- [ ] **Step 3: Extend `projectSettings`**

Replace the `projectSettings` block (lines 126-138) with:

```scala
    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      // Def.uncached opts these out of sbt 2's task cache: they return Seq[File] (not a
      // cacheable output type) and write files, and `deploy` performs network I/O. No-op on
      // sbt 1 (via sbt2-compat). Work-skipping for `blueprint` comes from its own content
      // fingerprint of classDirectory, not from sbt's task cache.
      blueprint := Def.uncached(blueprintTask.value),
      // Default-on; opt out per project with `blueprint / skip := true`. Defined at project (Zero)
      // config so the generator's Compile-scoped read delegates to it AND a user's Zero-scoped
      // override is honored.
      blueprint / skip := false,
      blueprintScalaVersion := None,
      blueprintPin := Def.uncached(blueprintPinTask.value),
      blueprintCheck := Def.uncached(blueprintCheckTask.value),
      // Embed blueprints in the JAR via the resources pipeline.
      Compile / resourceGenerators += blueprintGenerator.taskValue,
      deploy := Def.uncached(deployTask.evaluated)
    )
```

- [ ] **Step 4: Rewrite the object scaladoc**

Replace the doc comment at lines 10-25 with:

```scala
/** sbt plugin that adds blueprint generation, pinning and deploy tasks for Cardano smart
  * contracts.
  *
  * Blueprint output has two layers:
  *
  *   - `blueprint` (automatic, cached): generates each contract's CIP-57 JSON into
  *     `resourceManaged/main/META-INF/scalus/blueprints/<package>/<Contract>.json` plus an
  *     aggregate document at `resourceManaged/main/plutus.json`. Registered as a resource
  *     generator, so `package`/`publish` embed both in the JAR (aggregate at `/plutus.json`);
  *     every JAR describes exactly its own code. Generation is skipped when a content
  *     fingerprint of `classDirectory` is unchanged. Git is never touched. Opt out with
  *     `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var.
  *   - `blueprintPin` (deliberate): copies the current set to committed locations –
  *     `<base>/plutus.json` (the path Aiken tooling expects) and
  *     `<base>/blueprints/<package>/<Contract>.json`. Run it when a usable version should be
  *     pinned, then commit; git history is the pin history. `blueprintCheck` fails when pins
  *     are stale (for release pipelines). In cross-built projects set
  *     `blueprintScalaVersion := Some("3.3.7")` so only the primary Scala baseline is pinned.
  *
  * Version provenance lives inside the JSON, never in filenames: Scalus version in
  * `preamble.compiler.version`, Scala version in the top-level `scalus.scalaVersion`
  * extension key (valid CIP-57: the schema allows extra root keys).
  *
  * `sbt "deploy <ContractName> --network preview --blockfrost-key <key> --mnemonic '<words>'"`
  * deploys a validator as a reference script UTXO at the sender's own base address.
  *
  * Environment variables can substitute deploy CLI flags:
  *   - `CARDANO_NETWORK` for `--network` (default: "preview")
  *   - `BLOCKFROST_API_KEY` for `--blockfrost-key`
  *   - `CARDANO_MNEMONIC` for `--mnemonic`
  */
```

- [ ] **Step 5: Compile both axes, run tests**

Run: `sbtn "scalusSbtPlugin/+compile" "scalusSbtPlugin/+test"`
Expected: PASS on both Scala axes. Note: if `Def.taskIf` rejects the `blueprintScalaVersion.value.exists(...)` condition on either axis, fall back to evaluating the guard inside a plain `Def.task` and making `blueprint.value` unconditional for `blueprintCheck` (the extra generation is cheap thanks to the fingerprint cache) – document the change in the commit message.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala
git commit -m "feat(sbt-plugin): blueprintPin/blueprintCheck tasks and blueprintScalaVersion guard

Explicit-pin layer: blueprintPin copies generated blueprints to committed
locations (<base>/plutus.json + <base>/blueprints/<package>/), pruning stale
pins; blueprintCheck fails release pipelines on stale pins and skips under
non-primary Scala versions in cross-builds (blueprintScalaVersion guard)."
```

---

### Task 6: Final verification and docs sweep

**Files:**
- Possibly modify: any `scalus-site/content` page referencing the old blueprint output path.

**Interfaces:** none new.

- [ ] **Step 1: Docs sweep**

Run: `grep -rn "META-INF/scalus/blueprints" scalus-site/content docs README.md 2>/dev/null | grep -v superpowers`
For every hit describing the OLD flat layout (`blueprints/<ContractName>.json` directly under `META-INF/scalus/blueprints/`), update the text to the new nested layout `META-INF/scalus/blueprints/<package>/<Contract>.json` + `/plutus.json` aggregate, and mention `blueprintPin` where the doc discusses committing blueprints. If there are no hits outside `docs/superpowers`, this step is a no-op.

- [ ] **Step 2: Full verification**

Run, in order:
- `sbtn scalafmtAll`
- `sbtn "scalusSbtPlugin/+test"` – expected: PASS both axes
- `sbtn scalusJVM/mimaReportBinaryIssues` – expected: PASS
- `sbtn quick` – expected: format, compile and affected tests all PASS (try `sbtn clean` first if stale-class issues appear)

- [ ] **Step 3: Cross-check against the spec**

Re-read `docs/superpowers/specs/2026-06-28-blueprint-caching-and-naming-design.md` section by section and confirm each spec point maps to shipped code:
- Layer 1 layout (nested per-contract + `/plutus.json` in resourceManaged) – Task 4
- Fingerprint caching, complete-run-only cache writes, pruning – Task 4
- Layer 2 pin locations, prune, `blueprintCheck`, `blueprintScalaVersion` guard – Task 5
- `scalus.scalaVersion` top-level key, stamped in per-contract + aggregate – Tasks 1, 2, 4
- Graceful degradation on old scalus-core – Task 4
- `blueprint / skip` + `SCALUS_SKIP_BLUEPRINT` still honored – untouched `blueprintGenerator`

- [ ] **Step 4: Commit any doc updates**

```bash
sbtn scalafmtAll
git add -A scalus-site/content docs README.md
git commit -m "docs: update blueprint output layout references (nested paths, plutus.json, blueprintPin)"
```

(Skip if Step 1 was a no-op.)

- [ ] **Step 5 (optional, requires user confirmation): manual end-to-end check**

True e2e needs a downstream project using the plugin against a published scalus-core, which means `publishLocal` – this pollutes `~/.ivy2/local` (see project memory: it can shadow Sonatype artifacts and cause bogus downstream errors). Do NOT run it unprompted; instead offer the user this script and let them decide:

```bash
# in scalus:
sbtn "set ThisBuild/version := \"0.0.0-e2e-SNAPSHOT\"" publishLocal scalusSbtPlugin/publishLocal
# in a scaffolded g8 project (scalus3/hello.g8) with scalusVersion := "0.0.0-e2e-SNAPSHOT":
sbt package                      # expect META-INF/scalus/blueprints/<pkg>/*.json + /plutus.json in the JAR
sbt package                      # expect NO "Wrote ..." log lines (cache hit)
sbt blueprintPin                 # expect ./plutus.json + ./blueprints/<pkg>/*.json created
sbt blueprintCheck               # expect success
# edit the contract, then:
sbt blueprintCheck               # expect failure listing stale pins
# afterwards remove the e2e artifacts from ~/.ivy2/local/org.scalus
```
