# E2: Reject By-Name Parameters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn audit finding E2 (by-name parameters `=> T` silently compiled strict) from silent on-chain divergence into a clear, positioned compile error.

**Architecture:** The Scalus phase runs before `ElimByName`, and `SIRTyper` erases `ExprType` to its underlying type, so `def f(b: => Boolean)` compiles as if `b` were strict: code that passes JVM tests can behave differently (or always fail) on-chain, with no diagnostic. The fix adds a check in `SIRCompiler.compileDefDef` — the single chokepoint through which both module-level and block-local `def`s pass (via `compileStmt`) — that reports (via `report.error`, not the throwing `error` helper, so every offending param is flagged and the diagnostic survives to the reporter) an error for every parameter whose declared type is an `ExprType`, pointing the user to `inline` methods or an explicit `() => T` parameter. Testing requires new (small) negative-compilation infrastructure. NOTE (corrected during execution): the test lives in `scalusJVM/Test` (`scalus-core/jvm/src/test/scala/scalus/compiler/plugin/ByNameParamErrorTest.scala`), NOT `scalusPlugin` — the `scalus` project already references `scalusPlugin` via `PluginDependency`, so wiring the plugin jar + classpath into `scalus`'s `.jvmSettings` adds no new cross-project edge, whereas referencing `scalus.jvm` from `scalusPlugin`'s settings creates a project-loading cycle (`scalus` → `PluginDependency` → `scalusPlugin` → `scalus`, StackOverflow at load). The test drives `dotty.tools.dotc.Driver` with the packaged plugin jar and the real scalus-core classpath, both handed to the forked test JVM as system properties. Bonus: living under `scalus.compiler.*`, it also runs in the `lts-prev` 3.3.7 CI canary.

**Tech Stack:** Scala 3 compiler plugin (dotty API), sbt (`sbtn`), ScalaTest.

## Global Constraints

- The ONLY production change is the by-name check in `compileDefDef` in `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala`. Do NOT change `SIRTyper`'s `ExprType` erasure (`SIRTyper.scala:314-315`) — `ExprType` is also the type of parameterless methods (`def f: T`), and blanket-changing it breaks those (see audit finding E6).
- Error text must contain the exact phrase `By-name parameter` and the word `inline` (the tests match on both).
- `docs/internal/UPLC_CORRECTNESS_AUDIT.md` and `scalus-core/jvm/src/test/scala/scalus/compiler/PatternMatchAuditTest.scala` are intentionally untracked. NEVER `git add` or commit them. `PatternMatchAuditTest` has 5 intentionally failing tests (audit findings M1–M3) — expected failures, not regressions.
- CI note: the `lts-prev` CI job runs `++3.3.7; scalusPlugin/Test/compile` (compile only, no test execution) — the new test must compile against the 3.3.7 dotty API. `Driver().process(args, reporter)`, `Reporter.doReport(dia)(using Context)`, and `Diagnostic.level` are stable across 3.3.x and 3.8.x; do not use `StoreReporter` (its constructor signature drifted between versions).
- Commit directly to `master` (no branches/worktrees). Run `sbtn scalafmtAll` before committing (and format `build.sbt` by hand to match its style — scalafmtAll does not cover it). Rebase (`git pull --rebase`) before any push.
- Never add `Co-Authored-By: Claude` or any similar trailer to commit messages.

---

### Task 1: Negative-compilation test infra + RED tests

Build wiring hands the forked test JVM two system properties: the packaged plugin jar and scalus-core's full compile classpath. The dependency chain `scalusPlugin/Test → scalus.jvm/Compile → scalusPlugin/Compile` is acyclic (Test vs Compile configs), so referencing `scalus.jvm` tasks from `scalusPlugin`'s test settings is legal even though the plugin cannot depend on core at compile scope.

**Files:**
- Modify: `build.sbt` (the `lazy val scalusPlugin` first `.settings(...)` block, after the `libraryDependencies += "org.scala-lang" %% "scala3-compiler"` line)
- Create: `scalus-plugin/src/test/scala/scalus/compiler/plugin/ByNameParamErrorTest.scala`

**Interfaces:**
- Consumes: `dotty.tools.dotc.Driver#process(args: Array[String], reporter: Reporter): Reporter`; sbt task refs `(Compile / packageBin)`, `(scalus.jvm / Compile / fullClasspath)`.
- Produces: `compileSnippet(source: String): List[String]` test helper (error messages); system properties `scalus.plugin.jar` and `scalus.test.classpath` available to all future plugin diagnostic tests.

- [ ] **Step 1: Add the build wiring**

In `build.sbt`, inside `lazy val scalusPlugin`'s first `.settings(...)` block, after the `scala3-compiler` dependency line, add:

```scala
      // Negative-compilation tests drive dotc with the packaged plugin against the
      // real scalus-core classpath; both are handed to the forked test JVM below.
      Test / fork := true,
      Test / javaOptions ++= Seq(
        s"-Dscalus.plugin.jar=${(Compile / packageBin).value.getAbsolutePath}",
        s"-Dscalus.test.classpath=${(scalus.jvm / Compile / fullClasspath).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)}"
      ),
```

- [ ] **Step 2: Write the failing tests**

```scala
package scalus.compiler.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.collection.mutable.ListBuffer

/** Audit finding E2: by-name parameters (`b: => T`) were silently compiled strict, so
  * code passing JVM tests behaved differently on-chain. They must be rejected with a
  * positioned compile error instead.
  *
  * These tests compile snippets with the packaged plugin (system properties
  * `scalus.plugin.jar` / `scalus.test.classpath` are set in build.sbt) and assert on
  * the collected diagnostics.
  */
class ByNameParamErrorTest extends AnyFunSuite {

    private class ErrorCollector extends Reporter {
        val collected: ListBuffer[String] = ListBuffer.empty
        override def doReport(dia: Diagnostic)(using Context): Unit =
            if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
                collected += dia.message
    }

    private def compileSnippet(source: String): List[String] = {
        val pluginJar = sys.props("scalus.plugin.jar")
        val classpath = sys.props("scalus.test.classpath")
        val dir = Files.createTempDirectory("scalus-plugin-neg-test")
        val src = dir.resolve("snippet.scala")
        Files.writeString(src, source)
        val out = Files.createDirectories(dir.resolve("out"))
        val reporter = new ErrorCollector
        new Driver().process(
          Array(
            s"-Xplugin:$pluginJar",
            "-classpath",
            classpath,
            "-d",
            out.toString,
            src.toString
          ),
          reporter
        )
        reporter.collected.toList
    }

    test("control: strict parameters compile without errors") {
        val errors = compileSnippet(
          """import scalus.*
            |@Compile
            |object Control {
            |    def strictOr(a: Boolean, b: Boolean): Boolean = if a then true else b
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"control snippet must compile cleanly, got: $errors")
    }

    test("by-name parameter in @Compile object def is rejected") {
        val errors = compileSnippet(
          """import scalus.*
            |@Compile
            |object Repro {
            |    def lazyOr(a: Boolean, b: => Boolean): Boolean = if a then true else b
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(m => m.contains("By-name parameter") && m.contains("inline")),
          s"expected a by-name rejection error, got: $errors"
        )
    }

    test("by-name parameter in local def inside compile block is rejected") {
        val errors = compileSnippet(
          """import scalus.*
            |object Wrapper {
            |    val sir = Compiler.compile {
            |        def lazyOr(a: Boolean, b: => Boolean): Boolean = if a then true else b
            |        lazyOr(true, false)
            |    }
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(m => m.contains("By-name parameter") && m.contains("inline")),
          s"expected a by-name rejection error, got: $errors"
        )
    }
}
```

- [ ] **Step 3: Run and verify RED (control GREEN, by-name tests fail)**

Run: `sbtn "scalusPlugin/testOnly scalus.compiler.plugin.ByNameParamErrorTest"`
Expected: the control test PASSES (proves the Driver infra and classpath work — a broken classpath would fail it loudly, not vacuously pass the others), the two by-name tests FAIL with "expected a by-name rejection error, got: List()" — today the snippets compile without any diagnostic. If the control test fails instead, fix the infra before proceeding; do not weaken it.

- [ ] **Step 4: git add the new file**

```bash
git add scalus-plugin/src/test/scala/scalus/compiler/plugin/ByNameParamErrorTest.scala
```

---

### Task 2: The check in compileDefDef (GREEN)

**Files:**
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala` (~line 1351, right after `val params = dd.paramss.flatten.collect { case vd: ValDef => vd }` in `compileDefDef`)

**Interfaces:**
- Consumes: `GenericError(message, srcPos)` (`CompilationError.scala:241`), `error[A](err, default): A` (`SIRCompiler.scala:1061` — reports and continues, so ALL offending params in one def get reported); `ExprType` is in scope via the existing `import dotty.tools.dotc.core.Types.*`.
- Produces: the diagnostic Task 1's tests match on.

- [ ] **Step 1: Add the check**

Immediately after `val params = dd.paramss.flatten.collect { case vd: ValDef => vd }`:

```scala
            // By-name params would be compiled strictly (the phase runs before
            // ElimByName and SIRTyper erases ExprType), silently diverging from
            // Scala's call-by-name semantics on-chain (audit finding E2).
            params.foreach { vd =>
                if vd.tpt.tpe.isInstanceOf[ExprType] then
                    error(
                      GenericError(
                        s"By-name parameter '${vd.name.show}: ${vd.tpt.tpe.show}' is not supported: " +
                            "Scalus would evaluate the argument strictly at the call site, silently " +
                            "diverging from Scala's call-by-name semantics on-chain. Use an `inline` " +
                            "method with an `inline` parameter, or an explicit function parameter " +
                            "`() => T`.",
                        vd.srcPos
                      ),
                      ()
                    )
            }
```

- [ ] **Step 2: Run the suite — verify GREEN**

Run: `sbtn "scalusPlugin/testOnly scalus.compiler.plugin.ByNameParamErrorTest"`
Expected: 3/3 PASS (control still clean; both by-name snippets now produce the error) with Task 1's assertions unmodified.

- [ ] **Step 3: Run all plugin tests**

Run: `sbtn scalusPlugin/test`
Expected: all pass (pre-existing SIRFlatSerializationTest etc. now run forked — behavior unchanged).

---

### Task 3: Full regression

The new error could in principle fire on existing library/example code. Planning-time sweep found the ONLY by-name in on-chain code is `Order.ifEqualThen(inline other: => Order)` — an `inline` method, which `compileDefDef` skips before the check (inline defs are `Ignored`). Any surprise hit will show up as a loud compile error, not a wrong-code regression.

- [ ] **Step 1: Full quick gate**

Run: `sbtn quick`
Expected: everything compiles; tests green EXCEPT the 5 intentional `PatternMatchAuditTest` failures. If any project fails to compile with the new error, inspect the offending def: if it is genuinely by-name in compiled code, that is a real latent E2 bug in that code — fix it there (switch to `inline` or `() => T`) and note it in the commit message.

- [ ] **Step 2: Cross-compile canary for 3.3.7 API compatibility**

Run: `sbtn "++3.3.7 scalusPlugin/Test/compile"`
Expected: compiles (this is what the `lts-prev` CI job runs; it only compiles, never executes, the plugin tests).
Then restore: `sbtn "++3.3.8 scalusPlugin/Test/compile"` (or just proceed — CI resets versions per job).

---

### Task 4: Docs + commit

**Files:**
- Modify: `scalus-site/content/language-guide/support.mdx` (the "Unsupported Features" list, ~line 42)
- Modify: `docs/internal/UPLC_CORRECTNESS_AUDIT.md` — E2 section: append a Status line (file stays UNTRACKED — edit it, never `git add` it)

- [ ] **Step 1: Document in the unsupported-features list**

In `support.mdx` under `## Unsupported Features`, after the `var`s and `lazy val`s bullet, add:

```markdown
* by-name parameters (`b: => T`) - rejected at compile time, because Scalus would evaluate
  the argument strictly, unlike Scala; use an `inline` method with an `inline` parameter,
  or an explicit function parameter `b: () => T`
```

- [ ] **Step 2: Mark E2 fixed in the (untracked) audit doc**

Append to the E2 section of `docs/internal/UPLC_CORRECTNESS_AUDIT.md`:

```markdown
**Status: FIXED** — by-name parameters are now rejected with a positioned compile error in
`SIRCompiler.compileDefDef` (suggesting `inline` or `() => T`); negative-compilation suite:
`scalus-plugin/src/test/scala/scalus/compiler/plugin/ByNameParamErrorTest.scala`.
```

Also update the E2 row in the summary table (add `— **FIXED**` to the description, matching the X1/E1 rows).

- [ ] **Step 3: Format, commit, push**

```bash
# scalafmtAll first (memory: ci fails on a single unformatted file)
```
Run: `sbtn scalafmtAll`

```bash
git add build.sbt \
        scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala \
        scalus-plugin/src/test/scala/scalus/compiler/plugin/ByNameParamErrorTest.scala \
        scalus-site/content/language-guide/support.mdx \
        docs/superpowers/plans/2026-07-10-e2-byname-param-error.md
git status   # verify UPLC_CORRECTNESS_AUDIT.md and PatternMatchAuditTest.scala are NOT staged
git commit -m "fix(plugin): reject by-name parameters with a compile error

By-name parameters (b: => T) were silently compiled strict: the Scalus phase
runs before ElimByName and SIRTyper erases ExprType, so a lazy guard like
lazyOr(cond, throw ...) passed JVM tests but always failed on-chain, with no
diagnostic. compileDefDef now reports a positioned error for every by-name
parameter, suggesting an inline method with an inline parameter or an
explicit () => T function parameter.

Adds negative-compilation test infrastructure: scalusPlugin tests fork with
the packaged plugin jar and the scalus-core classpath as system properties,
and ByNameParamErrorTest drives dotty's Driver over snippets, asserting on
collected diagnostics.

Audit finding E2."
git pull --rebase && git push
```

---

## Follow-up (recorded, not part of this change)

Real by-name support (thunk encoding: params as `Unit => T`, call-site lambda wrapping, force at each use) remains possible as a feature once the type-machinery findings T1/T2/E6 are fixed; the error message and docs would then be relaxed. Release notes should mention that previously-compiling by-name defs in `@Compile` code now fail with a clear error — such code was already broken on-chain.
