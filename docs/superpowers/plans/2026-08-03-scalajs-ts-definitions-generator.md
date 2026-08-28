# Scala.js TypeScript Definitions Generator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `scalus-ts-exporter`, a TASTy-inspector-based tool that generates `scalus.d.ts` from the Scala.js facades' `@JSExport*` surface, wire it into the build with a CI drift gate, and migrate the npm package to the generated, flattened API.

**Architecture:** A JVM-only sbt module reads TASTy from the JS modules' class directories via `scala3-tasty-inspector`, builds a small TS declaration model, maps Scala types to TypeScript per Scala.js export semantics (strict errors on non-exportable types, `@TsType` escape hatch), converts Scaladoc to TSDoc, and emits one deterministic `.d.ts`. An sbt task regenerates the committed file; CI fails on diff.

**Tech Stack:** Scala 3.3.8, `scala3-tasty-inspector` (same version), scalatest, sbt, TypeScript 5.x (`tsc --noEmit` validation), vitest (existing npm tests).

**Spec:** `docs/superpowers/specs/2026-08-03-scalajs-typescript-definitions-generator-design.md` — read it first.

## Global Constraints

- Scala `3.3.8` (`scala3LtsVersion`), Scala.js `1.22.0`, sbt project ids: `scalusTsExporter` (new), `scalusTsExporterFixtures` (new), existing: `scalus.js`, `scalusCardanoLedger.js` (alias `scalusCardanoLedgerJS` from the CLI).
- This worktree: run sbt as `sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "<commands>"` (batch, never `sbtn`; see memory "sbtn no-output hang"). The worktree already has `zz-worktree-git-override.sbt` and the `plutus-conformance` symlink — do not commit the former.
- Run `sbt scalafmtAll` before EVERY commit (CI runs `scalafmtCheckAll`).
- Conventional commits (`feat:`, `fix:`, `docs:`, `test:`, `chore:`). NEVER add any Claude/Anthropic co-author trailer.
- `git add` every new file.
- Scala 3 code style per `CLAUDE.md`: `{}` for top-level defs, indentation syntax for small `if`/`match`, `then`/`do`.
- New public annotations (`scalus.interop.*`) are additions — MiMa-safe. The only intentional MiMa breakage is Task 9's two `JEmulator` return types; filters are part of that task.
- Generated file header must say it is generated; humans must not edit it.
- The generator core must stay Scalus-agnostic: no `scalus.*` hardcoding except the default `@TsType`/`@TsName` annotation FQNs (`scalus.interop.TsType`, `scalus.interop.TsName`), which are configurable constants in one place.

## File Structure

```
scalus-ts-exporter/
  src/main/scala/scalus/tsexport/
    TsModel.scala          # TS declaration AST + ExportError
    Emitter.scala          # model -> .d.ts text (pure)
    DocConverter.scala     # scaladoc text -> TSDoc lines (pure)
    TypeMapper.scala       # TypeRepr -> TsType (Quotes-dependent)
    ExportCollector.scala  # Inspector: TASTy -> TsModule
    Main.scala             # CLI
  src/test/scala/scalus/tsexport/
    EmitterTest.scala
    DocConverterTest.scala
    InspectorFixture.scala # shared helper to run the inspector over fixtures
    TypeMapperTest.scala
    ExportCollectorTest.scala
    GoldenTest.scala
  src/test/resources/golden/
    fixtures.d.ts          # committed golden output
    consumer.ts            # TS consumer exercising the golden declarations
  fixtures/
    src/main/scala/scalus/interop/TsAnnotations.scala   # same-FQN local copy
    src/main/scala/tsfixtures/Fixtures.scala            # every supported shape
    src/main/scala/tsfixtures/Errors.scala              # error shapes (separate file, excluded from golden run)
scalus-core/js/src/main/scala/scalus/interop/TsAnnotations.scala  # shipped annotations
build.sbt                  # two new projects + generateDts/checkDtsUpToDate wiring + MiMa filters + ci-js alias
scalus-cardano-ledger/js/src/main/scala/scalus/uplc/eval/JScalus.scala      # Task 9
scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala # Task 9
scalus-core/js/src/main/scala/scalus/cardano/ledger/SlotConfig.scala        # Task 9 (@TsName not needed; verify only)
scalus-cardano-ledger/js/src/main/npm/scalus.d.ts       # regenerated in Task 10
scalus-cardano-ledger/js/src/main/npm/__tests__/*.ts    # reconciled in Task 10
package.json               # root: add typescript devDep (Task 7)
.github: no change (ci-js alias covers the gate)
```

---

### Task 1: sbt scaffold — exporter module, fixtures project, inspector smoke test

**Files:**
- Modify: `build.sbt` (after `scalusUplcJitCompiler`, ~line 536; aggregates at lines 221–282)
- Create: `scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Fixtures.scala` (minimal seed)
- Create: `scalus-ts-exporter/src/test/scala/scalus/tsexport/InspectorFixture.scala`
- Create: `scalus-ts-exporter/src/test/scala/scalus/tsexport/SmokeTest.scala`

**Interfaces:**
- Produces: sbt projects `scalusTsExporter`, `scalusTsExporterFixtures`; test system properties `tsexport.fixtures.classes`, `tsexport.fixtures.classpath`, `tsexport.sourceroot`; test helper `InspectorFixture.withFixtureTastys[A](f: Quotes ?=> List[?] => A): A` — later tests build on the property names and the helper.

- [ ] **Step 1: Add the two projects to build.sbt**

Insert after the `scalusUplcJitCompiler` definition (`build.sbt:536`):

```scala
// TypeScript definitions generator: reads TASTy of the Scala.js modules and
// emits scalus.d.ts. See docs/superpowers/specs/2026-08-03-scalajs-typescript-definitions-generator-design.md
lazy val scalusTsExporter = project
    .in(file("scalus-ts-exporter"))
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-ts-exporter",
      scalacOptions ++= commonScalacOptions,
      libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      Test / fork := true,
      Test / javaOptions ++= Seq(
        s"-Dtsexport.fixtures.classes=${(scalusTsExporterFixtures / Compile / classDirectory).value.getAbsolutePath}",
        s"-Dtsexport.fixtures.classpath=${(scalusTsExporterFixtures / Compile / fullClasspath).value.map(_.data.getAbsolutePath).mkString(java.io.File.pathSeparator)}",
        s"-Dtsexport.sourceroot=${(ThisBuild / baseDirectory).value.getAbsolutePath}"
      ),
      publish / skip := true
    )

// Scala.js fixture code exercising every export shape the generator supports.
// Compiled only so its TASTy can be inspected by scalusTsExporter tests.
lazy val scalusTsExporterFixtures = project
    .in(file("scalus-ts-exporter/fixtures"))
    .enablePlugins(ScalaJSPlugin)
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-ts-exporter-fixtures",
      scalacOptions ++= commonScalacOptions,
      publish / skip := true
    )
```

Add `scalusTsExporter` and `scalusTsExporterFixtures` to the `root` aggregate (`build.sbt:223-243`) and `scalusTsExporter` to the `jvm` aggregate (`build.sbt:252-265`). Do NOT add the fixtures project to the `js` aggregate (it must not run in `ci-js` linking/tests).

- [ ] **Step 2: Seed one fixture**

`scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Fixtures.scala`:

```scala
package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** A point. */
@JSExportTopLevel("Point")
class Point(val x: Double, val y: Double) extends js.Object {
    def dist(other: Point): Double = 0.0
}
```

- [ ] **Step 3: Write the inspector test helper and the failing smoke test**

`scalus-ts-exporter/src/test/scala/scalus/tsexport/InspectorFixture.scala`:

```scala
package scalus.tsexport

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

object InspectorFixture {
    val fixtureClasses: String = sys.props("tsexport.fixtures.classes")
    val fixtureClasspath: List[String] =
        sys.props("tsexport.fixtures.classpath").split(File.pathSeparator).toList
    val sourceRoot: String = sys.props("tsexport.sourceroot")

    def tastyFilesUnder(root: String): List[String] = {
        val p = Paths.get(root)
        if !Files.exists(p) then Nil
        else
            Files
                .walk(p)
                .iterator()
                .asScala
                .filter(_.toString.endsWith(".tasty"))
                .map(_.toString)
                .toList
                .sorted
    }
}
```

`scalus-ts-exporter/src/test/scala/scalus/tsexport/SmokeTest.scala`:

```scala
package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite
import scala.tasty.inspector.*
import scala.quoted.*

class SmokeTest extends AnyFunSuite {
    test("inspector loads fixture TASTy and sees @JSExportTopLevel") {
        var foundExportNames = List.empty[String]
        val inspector = new Inspector {
            def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
                import quotes.reflect.*
                for tasty <- tastys do
                    object walker extends TreeAccumulator[Unit] {
                        def foldTree(u: Unit, tree: Tree)(owner: Symbol): Unit = tree match
                            case c: ClassDef =>
                                val names = c.symbol.annotations.collect {
                                    case a
                                        if a.tpe.typeSymbol.fullName ==
                                            "scala.scalajs.js.annotation.JSExportTopLevel" =>
                                        a match
                                            case Apply(_, List(Literal(StringConstant(n)))) => n
                                            case _ => c.symbol.name
                                }
                                foundExportNames = foundExportNames ++ names
                                foldOverTree(u, tree)(owner)
                            case _ => foldOverTree(u, tree)(owner)
                    }
                    walker.foldTree((), tasty.ast)(tasty.ast.symbol)
            }
        }
        val ok = TastyInspector.inspectAllTastyFiles(
          InspectorFixture.tastyFilesUnder(InspectorFixture.fixtureClasses),
          Nil,
          InspectorFixture.fixtureClasspath
        )(inspector)
        assert(ok, "inspector reported failure")
        assert(foundExportNames.contains("Point"))
    }
}
```

- [ ] **Step 4: Run the test, expect failure or pass-after-fixing**

```
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusTsExporter/test"
```

First run may fail on build wiring or on the `TastyInspector` API surface (it changed across 3.x minors — if `inspectAllTastyFiles` does not exist under that name, check the one shipped with 3.3.8: `scala.tasty.inspector.TastyInspector`). Fix until the smoke test passes. This step exists precisely to de-risk the inspector plumbing before any real code.

- [ ] **Step 5: Commit**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true scalafmtAll
git add build.sbt scalus-ts-exporter
git commit -m "feat(tsexport): scaffold scalus-ts-exporter with tasty-inspector smoke test"
```

---

### Task 2: TS declaration model + emitter (pure)

**Files:**
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/TsModel.scala`
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/Emitter.scala`
- Test: `scalus-ts-exporter/src/test/scala/scalus/tsexport/EmitterTest.scala`

**Interfaces:**
- Produces (used by every later task):

```scala
package scalus.tsexport

enum TsType {
    case Named(name: String)                    // "number", "Uint8Array", "EvaluationResult"
    case Union(members: List[TsType])
    case Arr(elem: TsType)
    case Func(params: List[TsParam], ret: TsType)
    case Index(value: TsType)                   // { [key: string]: V }
    case Verbatim(text: String)                 // @TsType override, emitted as-is
}

case class TsParam(name: String, tpe: TsType, optional: Boolean)

/** TSDoc body lines, already converted, without the comment frame. */
case class TsDoc(lines: List[String])

enum TsMember {
    case Ctor(overloads: List[List[TsParam]], doc: Option[TsDoc])
    case Method(
        name: String,
        overloads: List[(List[TsParam], TsType)],
        static: Boolean,
        doc: Option[TsDoc]
    )
    case Property(
        name: String,
        tpe: TsType,
        readonly: Boolean,
        optional: Boolean,
        static: Boolean,
        doc: Option[TsDoc]
    )
}

enum TsDecl {
    /** deprecatedAliases: extra top-level export names, emitted as deprecated alias exports. */
    case Cls(
        name: String,
        typeParams: List[String],
        members: List[TsMember],
        doc: Option[TsDoc],
        deprecatedAliases: List[String]
    )
    case Iface(name: String, members: List[TsMember], doc: Option[TsDoc])
    case Fun(name: String, overloads: List[(List[TsParam], TsType)], doc: Option[TsDoc])
    case ConstObj(name: String, members: List[TsMember], doc: Option[TsDoc])
}

case class TsModule(decls: List[TsDecl])

case class ExportError(member: String, message: String) {
    def render: String = s"$member: $message"
}

object Emitter {
    def emit(module: TsModule): String
    def render(t: TsType): String
}
```

- [ ] **Step 1: Write failing emitter tests**

`EmitterTest.scala` — key cases (write all of them):

```scala
package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite
import TsType.*

class EmitterTest extends AnyFunSuite {
    private val num = Named("number")
    private val str = Named("string")

    test("renders types") {
        assert(Emitter.render(Union(List(str, Named("undefined")))) == "string | undefined")
        assert(Emitter.render(Arr(Union(List(num, Named("null"))))) == "(number | null)[]")
        assert(Emitter.render(Index(str)) == "{ [key: string]: string }")
        assert(
          Emitter.render(Func(List(TsParam("x", num, false)), str)) == "(x: number) => string"
        )
        assert(Emitter.render(Verbatim("\"key\" | \"script\"")) == "\"key\" | \"script\"")
    }

    test("emits a class with ctor, method overloads, static and readonly members") {
        val cls = TsDecl.Cls(
          "Emu",
          Nil,
          List(
            TsMember.Ctor(List(List(TsParam("a", num, false), TsParam("b", str, true))), None),
            TsMember.Method(
              "submit",
              List(
                (List(TsParam("tx", Named("Uint8Array"), false)), Named("SubmitResult")),
                (
                  List(
                    TsParam("tx", Named("Uint8Array"), false),
                    TsParam("dbg", Index(str), false)
                  ),
                  Named("SubmitResult")
                )
              ),
              static = false,
              None
            ),
            TsMember.Property("slot", num, readonly = true, optional = false, static = false, None),
            TsMember.Property(
              "mainnet",
              Named("Emu"),
              readonly = true,
              optional = false,
              static = true,
              None
            )
          ),
          None,
          deprecatedAliases = Nil
        )
        val out = Emitter.emit(TsModule(List(cls)))
        assert(out.contains("export class Emu {"))
        assert(out.contains("constructor(a: number, b?: string);"))
        assert(out.contains("submit(tx: Uint8Array): SubmitResult;"))
        assert(out.contains("submit(tx: Uint8Array, dbg: { [key: string]: string }): SubmitResult;"))
        assert(out.contains("readonly slot: number;"))
        assert(out.contains("static readonly mainnet: Emu;"))
    }

    test("emits interface, function overloads, const object, docs, aliases; sorted; header") {
        val doc = TsDoc(List("Adds.", "@param x the x", "@returns sum"))
        val decls = List(
          TsDecl.Fun("zeta", List((List(TsParam("x", num, false)), num)), Some(doc)),
          TsDecl.Iface(
            "Alpha",
            List(TsMember.Property("v", num, readonly = false, optional = true, static = false, None)),
            None
          ),
          TsDecl.ConstObj(
            "Scalus",
            List(TsMember.Method("run", List((Nil, num)), static = false, None)),
            Some(TsDoc(List("@deprecated Use top-level functions.")))
          ),
          TsDecl.Cls("Beta", Nil, Nil, None, deprecatedAliases = List("OldBeta"))
        )
        val out = Emitter.emit(TsModule(decls))
        // header
        assert(out.startsWith("// Generated by scalus-ts-exporter. DO NOT EDIT."))
        // alphabetical order of declarations: Alpha, Beta, Scalus, zeta
        val order = List("interface Alpha", "class Beta", "const Scalus", "function zeta")
            .map(s => out.indexOf(s))
        assert(order == order.sorted && order.forall(_ >= 0), s"order was $order")
        // pieces
        assert(out.contains("export interface Alpha {"))
        assert(out.contains("v?: number;"))
        assert(out.contains("export function zeta(x: number): number;"))
        assert(out.contains("/**\n * Adds.\n * @param x the x\n * @returns sum\n */"))
        assert(out.contains("export const Scalus: {"))
        assert(out.contains("run(): number;"))
        // deprecated alias exported after everything else
        assert(out.contains("/** @deprecated Use Beta instead. */"))
        assert(out.contains("export { Beta as OldBeta };"))
        // no trailing whitespace anywhere, deterministic double-run
        assert(!out.linesIterator.exists(_.endsWith(" ")))
        assert(out == Emitter.emit(TsModule(decls)))
    }
}
```

- [ ] **Step 2: Run to verify failure**

`sbt ... "scalusTsExporter/testOnly scalus.tsexport.EmitterTest"` — FAIL: `Emitter` not found.

- [ ] **Step 3: Implement `TsModel.scala` (exactly the Interfaces block above) and `Emitter.scala`**

Emitter rules (implement all):
- Header line: `// Generated by scalus-ts-exporter. DO NOT EDIT.` then a blank line.
- Declarations sorted alphabetically by name (case-sensitive), rendered with one blank line between them. Alias exports (`export { X as Y };`, one per alias, each preceded by `/** @deprecated Use X instead. */`) collected from all `Cls.deprecatedAliases` and appended after all declarations, sorted by alias name.
- Docs: `TsDoc(lines)` renders as `/**`, then one ` * <line>` per line, then ` */`, indented to the member's indent level.
- `Cls` → `export class Name<T, U> { ... }`; ctor overloads each as `constructor(params);`; statics printed with `static ` prefix; `readonly ` before name for readonly properties; `?` suffix for optional properties and params.
- `Iface` → `export interface Name { ... }` (members: properties and methods; never static).
- `Fun` → one `export function name(params): ret;` line per overload.
- `ConstObj` → `export const Name: { ... };` — methods as `name(params): ret;` lines inside the object type.
- `render`: unions joined with ` | `; array of a union parenthesizes the element (`(a | b)[]`); `Func` params rendered like member params; `Index(v)` → `{ [key: string]: <v> }`; `Verbatim` as-is.
- Indent with 2 spaces (matches the existing hand-written d.ts and npm prettier defaults).

- [ ] **Step 4: Run to verify pass**

`sbt ... "scalusTsExporter/testOnly scalus.tsexport.EmitterTest"` — PASS.

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll
git add scalus-ts-exporter
git commit -m "feat(tsexport): TS declaration model and deterministic d.ts emitter"
```

---### Task 3: DocConverter — Scaladoc → TSDoc (pure)

**Files:**
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/DocConverter.scala`
- Test: `scalus-ts-exporter/src/test/scala/scalus/tsexport/DocConverterTest.scala`

**Interfaces:**
- Produces: `object DocConverter { def convert(rawScaladoc: String): Option[TsDoc] }` — input is the raw comment as found in TASTy/source including `/**` and `*/`; output `None` when the comment has no content.

- [ ] **Step 1: Write failing tests**

```scala
package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

class DocConverterTest extends AnyFunSuite {
    test("strips frame and stars, keeps body") {
        val doc = DocConverter.convert("/** Hello world. */").get
        assert(doc.lines == List("Hello world."))
    }

    test("multi-line with params, return, links, deprecated") {
        val raw =
            """/** Evaluates a script.
              |  *
              |  * See [[evaluateScript]] and [[scalus.uplc.eval.JScalus]].
              |  *
              |  * @param doubleCborHex
              |  *   the script hex
              |  * @param data
              |  *   the argument
              |  * @return
              |  *   the result
              |  * @deprecated Use evaluate instead
              |  */""".stripMargin
        val doc = DocConverter.convert(raw).get
        assert(doc.lines.head == "Evaluates a script.")
        assert(doc.lines.contains("See {@link evaluateScript} and {@link scalus.uplc.eval.JScalus}."))
        // scaladoc's continuation-line style folds into one TSDoc tag line
        assert(doc.lines.contains("@param doubleCborHex the script hex"))
        assert(doc.lines.contains("@param data the argument"))
        assert(doc.lines.contains("@returns the result"))
        assert(doc.lines.contains("@deprecated Use evaluate instead"))
    }

    test("empty comment is None") {
        assert(DocConverter.convert("/** */").isEmpty)
        assert(DocConverter.convert("").isEmpty)
    }
}
```

- [ ] **Step 2: Run to verify failure** (`testOnly scalus.tsexport.DocConverterTest` → FAIL: not found)

- [ ] **Step 3: Implement**

Algorithm (implement in `DocConverter.scala`):
1. Trim; drop leading `/**` and trailing `*/`; split lines; strip each line's leading whitespace + optional `*` + one space.
2. Fold tag continuations: a line starting with `@` begins a tag block; subsequent non-empty lines that do not start with `@` are appended to it with a single space (scaladoc indents tag bodies on the next line).
3. Rewrite: `@return` → `@returns`; `[[X]]` → `{@link X}` (regex `\[\[([^\]]+)\]\]`); all other tags pass through (`@param`, `@deprecated`, `@throws`).
4. Collapse runs of blank lines to one; drop leading/trailing blank lines. If nothing remains, `None`.

- [ ] **Step 4: Run to verify pass**

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll && git add scalus-ts-exporter && git commit -m "feat(tsexport): scaladoc to TSDoc converter"
```

---

### Task 4: Fixtures — full shape coverage + shipped `@TsType`/`@TsName` annotations

**Files:**
- Create: `scalus-core/js/src/main/scala/scalus/interop/TsAnnotations.scala`
- Create: `scalus-ts-exporter/fixtures/src/main/scala/scalus/interop/TsAnnotations.scala` (verbatim same-FQN copy so fixtures do not depend on scalus-core)
- Modify: `scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Fixtures.scala` (replace seed)
- Create: `scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Errors.scala`

**Interfaces:**
- Produces: annotation FQNs `scalus.interop.TsType` and `scalus.interop.TsName` (the collector matches these strings); fixture symbols referenced by name in Tasks 5–7 tests.

- [ ] **Step 1: Write the shipped annotations** (both copies identical):

```scala
package scalus.interop

import scala.annotation.StaticAnnotation

/** Overrides the TypeScript type emitted by scalus-ts-exporter for the annotated member,
  * parameter, or field — e.g. `@TsType("\"key\" | \"script\"")`. The string is emitted verbatim.
  */
final class TsType(val tsType: String) extends StaticAnnotation

/** Overrides the TypeScript declaration name emitted by scalus-ts-exporter for the annotated
  * class or trait — e.g. `@TsName("SubmitResult")` on `trait JSubmitResult`.
  */
final class TsName(val name: String) extends StaticAnnotation
```

- [ ] **Step 2: Write the full fixtures**

`tsfixtures/Fixtures.scala` (replaces the seed; keep `Point`):

```scala
package tsfixtures

import scalus.interop.{TsName, TsType}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

/** A point.
  *
  * @param x
  *   the x coordinate
  */
@JSExportTopLevel("Point")
class Point(val x: Double, val y: Double) extends js.Object {

    /** Distance to [[Point]] `other`. */
    def dist(other: Point): Double = 0.0
}

/** Exported under a canonical name plus a deprecated alias. */
@JSExportTopLevel("NewName")
@JSExportTopLevel("OldName")
class Renamed(val n: Int) extends js.Object

/** Non-js.Object class: only annotated members are exported. */
@JSExportTopLevel("Partial")
class Partial(secret: String) {
    @JSExport
    def visible(a: js.BigInt): js.BigInt = a
    def hidden(): String = secret
}

@JSExportTopLevel("Statics")
class Statics(val v: Double) extends js.Object
object Statics {
    @JSExportStatic
    val mainnet: Statics = new Statics(1)
    @JSExportStatic
    def make(v: Double, tag: String = "x"): Statics = new Statics(v)
}

/** Generic exported class. */
@JSExportTopLevel("Box")
class Box[A](val value: A) extends js.Object

/** Every mappable type in one class. */
@JSExportTopLevel("Kitchen")
class Kitchen extends js.Object {
    def prims(a: Boolean, b: Int, c: Double, d: String): Unit = ()
    def big(x: js.BigInt): js.BigInt = x
    def arr(xs: js.Array[String]): js.Array[js.Array[Double]] = js.Array()
    def undef(x: js.UndefOr[String]): js.UndefOr[Double] = 0.0
    def union(x: js.BigInt | Null): Uint8Array | Null = null
    def dict(d: js.Dictionary[String]): js.Dictionary[js.Array[Double]] = js.Dictionary()
    def promise(): js.Promise[String] = js.Promise.resolve("a")
    def fun(f: js.Function1[Double, String]): js.Function0[Unit] = () => ()
    def dyn(x: js.Dynamic): js.Any = x
    def bytes(u: Uint8Array): Uint8Array = u
    def obj(o: js.Object): js.Object = o
    def opt(a: Double, b: js.UndefOr[String], c: js.UndefOr[Double]): Unit = ()
    def dflt(a: Double, b: String = "x"): Double = a
    val ro: Double = 1.0
    var rw: String = "s"
    def getter: Double = 2.0
    def overloaded(a: Double): Double = a
    def overloaded(a: Double, b: String): String = b
    @TsType("\"key\" | \"script\"")
    def credType(): String = "key"
    def config(c: Conf): Conf = c
}

/** Referenced but not exported: becomes an interface, renamed via @TsName. */
@TsName("Config")
trait Conf extends js.Object {

    /** Nested reference chases transitively. */
    val nested: js.UndefOr[js.Array[Inner]]
    val flag: Boolean
}

trait Inner extends js.Object {
    val id: String
}

/** Top-level exported functions from an object. */
@JSExportTopLevel("Tools")
object Tools {
    /** Doubles. */
    @JSExport
    @JSExportTopLevel("twice")
    def twice(x: Double): Double = x * 2
    @JSExport
    def concat(a: String, b: js.UndefOr[String]): String = a
    private def internal(): Unit = ()
}
```

`tsfixtures/Errors.scala` (compiled but pointed at only by error tests):

```scala
package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("BadLong")
class BadLong extends js.Object {
    def bad(x: Long): Long = x
}

@JSExportTopLevel("BadOption")
class BadOption extends js.Object {
    def bad(x: Option[String]): Option[String] = x
}

@JSExportTopLevel("BadColl")
class BadColl extends js.Object {
    def bad(): List[String] = Nil
}

@JSExportTopLevel("BadOpaque")
class BadOpaque extends js.Object {
    def bad(): java.time.Instant = java.time.Instant.EPOCH
}
```

- [ ] **Step 3: Compile everything**

```
sbt ... "scalusTsExporterFixtures/compile" "scalusJS/compile"
```
Expected: success (annotations compile on both sides; fixtures need no scalus dependency).

- [ ] **Step 4: Commit**

```bash
sbt ... scalafmtAll
git add scalus-core/js/src/main/scala/scalus/interop scalus-ts-exporter/fixtures
git commit -m "feat(tsexport): TsType/TsName annotations and full fixture coverage"
```

---

### Task 5: TypeMapper — TypeRepr → TsType

**Files:**
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/TypeMapper.scala`
- Test: `scalus-ts-exporter/src/test/scala/scalus/tsexport/TypeMapperTest.scala`

**Interfaces:**
- Consumes: `TsType`, `ExportError` from Task 2; fixture types from Task 4.
- Produces:

```scala
class TypeMapper(using val quotes: Quotes)(
    /** typeSymbol.fullName -> emitted TS name, for exported/chased declarations */
    knownNames: Map[String, String],
    /** called when an unknown js.Object subtype is referenced; returns its future TS name */
    chase: quotes.reflect.Symbol => String
) {
    import quotes.reflect.*
    /** Left(error message) when the type is not exportable. */
    def map(tpe: TypeRepr, context: String): Either[ExportError, TsType]
}
```

- [ ] **Step 1: Write failing tests**

Test through the inspector: inspect fixture TASTy, look up `Kitchen`'s methods by name, map each parameter/result type, assert the rendered strings. Structure:

```scala
package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite
import scala.tasty.inspector.*
import scala.quoted.*

class TypeMapperTest extends AnyFunSuite {

    /** Runs `f` with the mapper inside an inspection of the fixtures. */
    private def withMapper(f: Quotes ?=> (String, String) => String): Map[String, String] = {
        // collect (methodName -> rendered result) pairs for assertions outside
        var results = Map.empty[String, String]
        val inspector = new Inspector {
            def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
                import quotes.reflect.*
                val kitchen = Symbol.requiredClass("tsfixtures.Kitchen")
                val mapper = new TypeMapper(
                  knownNames = Map(
                    "tsfixtures.Point" -> "Point",
                    "tsfixtures.Conf" -> "Config",
                    "tsfixtures.Inner" -> "Inner"
                  ),
                  chase = sym => sym.name
                )
                for m <- kitchen.declaredMethods do
                    val sig = m.tree match
                        case dd: DefDef =>
                            val ps = dd.termParamss.flatMap(_.params).map { p =>
                                mapper.map(p.tpt.tpe, m.name) match
                                    case Right(t)  => Emitter.render(t)
                                    case Left(err) => s"ERROR(${err.message})"
                            }
                            val ret = mapper.map(dd.returnTpt.tpe, m.name) match
                                case Right(t)  => Emitter.render(t)
                                case Left(err) => s"ERROR(${err.message})"
                            (ps :+ ret).mkString(" ; ")
                        case _ => "?"
                    results = results.updated(s"${m.name}/${results.count(_._1.startsWith(m.name))}", sig)
            }
        }
        TastyInspector.inspectAllTastyFiles(
          InspectorFixture.tastyFilesUnder(InspectorFixture.fixtureClasses),
          Nil,
          InspectorFixture.fixtureClasspath
        )(inspector)
        results
    }

    test("maps every Kitchen signature") {
        val r = withMapper((_, _) => "")
        assert(r("prims/0") == "boolean ; number ; number ; string ; void")
        assert(r("big/0") == "bigint ; bigint")
        assert(r("arr/0") == "string[] ; number[][]")
        assert(r("undef/0") == "string | undefined ; number | undefined")
        assert(r("union/0") == "bigint | null ; Uint8Array | null")
        assert(r("dict/0") == "{ [key: string]: string } ; { [key: string]: number[] }")
        assert(r("promise/0") == "Promise<string>")
        assert(r("fun/0") == "(arg0: number) => string ; () => void")
        assert(r("dyn/0") == "any ; any")
        assert(r("bytes/0") == "Uint8Array ; Uint8Array")
        assert(r("obj/0") == "object ; object")
        assert(r("getter/0") == "number")
        assert(r("config/0") == "Config ; Config")
    }

    test("errors on non-exportable types") {
        // inspect BadLong/BadOption/BadColl/BadOpaque the same way; each must produce Left
        // with a message containing the offending type name (e.g. "Long", "Option", "List",
        // "java.time.Instant") and the member context string.
        val r = withMapper((_, _) => "") // extend withMapper to also visit tsfixtures.Bad* classes
        // assertions analogous to above; the four Bad* methods all render as ERROR(...)
        assert(r.exists { case (k, v) => k.startsWith("bad") && v.contains("ERROR") })
    }
}
```

(Adjust the helper as needed while implementing — the assertions on rendered strings are the contract; `Promise<string>` requires `TsType.Named` to allow parameterized names or a dedicated `Generic` case — pick ONE representation: add `case Generic(name: String, args: List[TsType])` to `TsType` in Task 2's file and render as `name<a, b>`, updating `EmitterTest` accordingly.)

- [ ] **Step 2: Run to verify failure** (TypeMapper not found)

- [ ] **Step 3: Implement `TypeMapper`**

Match on `tpe.dealias` (dealias is REQUIRED: `js.UndefOr[A]` is the Scala 3 alias `A | Unit`):
- `OrType` → flatten all branches; map each; `Unit`/`Nothing?`→ `undefined` via: branch `scala.Unit` → `Named("undefined")`, `scala.Null` → `Named("null")`; dedupe; single member unwraps.
- `AppliedType(base, args)` — dispatch on `base.typeSymbol.fullName`:
  - `scala.scalajs.js.Array` → `Arr`
  - `scala.scalajs.js.Dictionary` → `Index(map(args(1)))`? No — Dictionary has ONE arg: `Index(map(args(0)))`
  - `scala.scalajs.js.Promise` → `Generic("Promise", ...)`
  - `scala.scalajs.js.|` → union of both args
  - `scala.scalajs.js.Function0..22` → `Func(argN named arg0..argN-1, last)`
  - `scala.scalajs.js.ThisFunctionN` → error (not used in Scalus; keep simple)
  - anything else applied → fall through to opaque handling below with args mapped if the base resolves.
- Ground types by `typeSymbol.fullName`:
  - `scala.Boolean` → boolean; `scala.Byte|Short|Int|Float|Double` → number; `java.lang.String` → string; `scala.Unit` → void; `scala.Long` → ERROR ("Long has no JavaScript representation; use Double, js.BigInt, or @TsType").
  - `scala.scalajs.js.BigInt` → bigint; `scala.scalajs.js.Dynamic` → any; `scala.scalajs.js.Any` → any; `scala.scalajs.js.Object` → object.
  - `scala.scalajs.js.typedarray.*` (Uint8Array, Int8Array, ArrayBuffer, …) → short name verbatim.
  - `scala.scalajs.js.Date` → Date; `scala.scalajs.js.RegExp` → RegExp.
- Symbol lookup: if `fullName` ∈ `knownNames` → `Named(knownNames(fullName))`.
- Otherwise, if the symbol's base types include `scala.scalajs.js.Object` (check `tpe.baseClasses.exists(_.fullName == "scala.scalajs.js.Object")`) → `Named(chase(sym))` (the collector registers it for interface emission).
- Type parameter references (`ParamRef`/type symbols that are type params) → `Named(sym.name)`.
- Everything else → `Left(ExportError(context, s"type ${tpe.show} is not exportable to TypeScript; export it, use a js.* type, or add @TsType"))`. Special-case the message for `scala.Option` ("use js.UndefOr") and `scala.collection.*` ("use js.Array or js.Dictionary").

`@TsType` handling lives in the collector (Task 6), not here — the mapper is annotation-free.

- [ ] **Step 4: Run to verify pass**, fixing the exact rendered strings as the source of truth (they define the emitter contract for the golden file).

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll && git add scalus-ts-exporter && git commit -m "feat(tsexport): TypeRepr to TsType mapper with strict exportability errors"
```

---

### Task 6: ExportCollector — TASTy → TsModule

**Files:**
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/ExportCollector.scala`
- Test: `scalus-ts-exporter/src/test/scala/scalus/tsexport/ExportCollectorTest.scala`

**Interfaces:**
- Consumes: `TypeMapper`, `DocConverter`, model from Task 2, fixtures from Task 4.
- Produces:

```scala
object ExportCollector {
    case class Result(module: TsModule, errors: List[ExportError])
    /** tastyRoots: dirs containing .tasty; classpath: dependency classpath;
      * sourceRoot: base for resolving relative TASTy source paths (docs fallback).
      */
    def collect(
        tastyRoots: List[String],
        classpath: List[String],
        sourceRoot: String
    ): Result
}
```

- [ ] **Step 1: Write failing tests** (assert against the fixtures; the important behaviors):

```scala
class ExportCollectorTest extends AnyFunSuite {
    lazy val result = ExportCollector.collect(
      List(InspectorFixture.fixtureClasses),
      InspectorFixture.fixtureClasspath,
      InspectorFixture.sourceRoot
    )
    def decl(name: String): TsDecl =
        result.module.decls.find {
            case d: TsDecl.Cls      => d.name == name
            case d: TsDecl.Iface    => d.name == name
            case d: TsDecl.Fun      => d.name == name
            case d: TsDecl.ConstObj => d.name == name
        }.get

    test("js.Object class exports all public members; ctor from primary constructor") {
        val p = decl("Point").asInstanceOf[TsDecl.Cls]
        assert(p.members.exists { case TsMember.Ctor(List(ps), _) => ps.map(_.name) == List("x", "y"); case _ => false })
        assert(p.members.exists { case m: TsMember.Method => m.name == "dist"; case _ => false })
        assert(p.doc.get.lines.head == "A point.")
    }
    test("non-js.Object class exports only @JSExport members") {
        val p = decl("Partial").asInstanceOf[TsDecl.Cls]
        assert(p.members.collect { case m: TsMember.Method => m.name } == List("visible"))
    }
    test("statics from companion @JSExportStatic; default param optional") {
        val s = decl("Statics").asInstanceOf[TsDecl.Cls]
        val make = s.members.collectFirst { case m: TsMember.Method if m.name == "make" => m }.get
        assert(make.static)
        assert(make.overloads.head._1.last.optional) // tag: String = "x"
        assert(s.members.exists { case p: TsMember.Property => p.name == "mainnet" && p.static; case _ => false })
    }
    test("multiple JSExportTopLevel: first canonical, rest deprecated aliases") {
        val r = decl("NewName").asInstanceOf[TsDecl.Cls]
        assert(r.deprecatedAliases == List("OldName"))
        assert(!result.module.decls.exists { case c: TsDecl.Cls => c.name == "OldName"; case _ => false })
    }
    test("generics; getters readonly; var mutable; trailing UndefOr optional") {
        assert(decl("Box").asInstanceOf[TsDecl.Cls].typeParams == List("A"))
        val k = decl("Kitchen").asInstanceOf[TsDecl.Cls]
        assert(k.members.exists { case p: TsMember.Property => p.name == "getter" && p.readonly; case _ => false })
        assert(k.members.exists { case p: TsMember.Property => p.name == "rw" && !p.readonly; case _ => false })
        val opt = k.members.collectFirst { case m: TsMember.Method if m.name == "opt" => m }.get
        assert(opt.overloads.head._1.map(_.optional) == List(false, true, true)) // b, c trailing UndefOr
        val overl = k.members.collectFirst { case m: TsMember.Method if m.name == "overloaded" => m }.get
        assert(overl.overloads.size == 2)
    }
    test("TsType override wins; TsName renames chased interfaces; transitive chase") {
        val k = decl("Kitchen").asInstanceOf[TsDecl.Cls]
        val ct = k.members.collectFirst { case m: TsMember.Method if m.name == "credType" => m }.get
        assert(ct.overloads.head._2 == TsType.Verbatim("\"key\" | \"script\""))
        val conf = decl("Config").asInstanceOf[TsDecl.Iface]
        assert(conf.members.exists { case p: TsMember.Property => p.name == "nested" && p.optional; case _ => false })
        assert(decl("Inner").isInstanceOf[TsDecl.Iface]) // chased transitively
    }
    test("object with JSExport members becomes ConstObj; JSExportTopLevel def becomes Fun") {
        val tools = decl("Tools").asInstanceOf[TsDecl.ConstObj]
        assert(tools.members.collect { case m: TsMember.Method => m.name }.sorted == List("concat", "twice"))
        val twice = decl("twice").asInstanceOf[TsDecl.Fun] // also a top-level function
        assert(twice.doc.get.lines.head == "Doubles.")
    }
    test("errors accumulate for Bad* fixtures with member context") {
        val msgs = result.errors.map(_.render)
        assert(msgs.exists(m => m.contains("BadLong") && m.contains("Long")))
        assert(msgs.exists(m => m.contains("BadOption") && m.contains("Option")))
        assert(msgs.exists(m => m.contains("BadColl")))
        assert(msgs.exists(m => m.contains("BadOpaque")))
    }
    test("docs flow through, including UndefOr-optional interface members") {
        assert(decl("Config").asInstanceOf[TsDecl.Iface].members.nonEmpty)
    }
}
```

- [ ] **Step 2: Run to verify failure**

- [ ] **Step 3: Implement `ExportCollector`**

Collection algorithm:
1. One `Inspector` pass over all TASTy roots. Walk each `tasty.ast` with a `TreeAccumulator`, collecting `ClassDef` (classes, traits, objects) and top-level `DefDef`/`ValDef` symbols carrying `@JSExportTopLevel`.
2. Export-name resolution: `@JSExportTopLevel` args in source order — first is canonical, rest become `deprecatedAliases`. Annotation arg extraction: `case Apply(_, List(Literal(StringConstant(n)), _*)) => n`; no-arg forms use the Scala name. `@TsName` (FQN `scalus.interop.TsName`) overrides the emitted name of NON-top-level-exported (chased) declarations; for exported ones the export annotation wins.
3. Member collection per class:
   - Class `<: js.Object` (`sym.typeRef.baseClasses.exists(_.fullName == "scala.scalajs.js.Object")`): all public, non-synthetic members declared in the class (skip `private`/`protected` via `sym.flags.is(Flags.Private) || sym.flags.is(Flags.Protected)`, skip `Flags.Synthetic` and `Flags.Artifact`, skip constructors here).
   - Otherwise: only members with `@JSExport` / class-level `@JSExportAll`.
   - Primary constructor: `sym.primaryConstructor` params → `TsMember.Ctor`; skip if the constructor is private.
   - Companion statics: `sym.companionModule` declarations annotated `@JSExportStatic` → same member kinds with `static = true`.
4. Member kinds: `DefDef` with `paramSymss` empty or only type params → readonly `Property` (getter); with `List(Nil)` → `Method` with zero params; `val` → readonly `Property`; `var` → mutable `Property`. Group same-name `DefDef`s into one `Method` with sorted-by-arity overloads.
5. Optionality: parameter is optional iff (`HasDefault` flag or its mapped type contains `undefined`) AND all following params are also optional; when optional via UndefOr, strip ` | undefined` from the rendered param type. Interface `val` of type `js.UndefOr[T]` → optional property with type `T`.
6. `@TsType` (FQN `scalus.interop.TsType`) on a member: the member's result type (or a val/var's type) becomes `TsType.Verbatim(arg)`; on a parameter: that parameter's type.
7. Chasing: `TypeMapper`'s `chase` callback registers the referenced symbol in a work queue; process until empty; each chased trait/class `<: js.Object` becomes `TsDecl.Iface` (name from `@TsName` or the Scala short name) with all public vals/defs mapped the same way.
8. Objects: `@JSExportTopLevel` object → `TsDecl.ConstObj` with its `@JSExport` members. A `def` inside an object carrying its own `@JSExportTopLevel("n")` ALSO yields a top-level `TsDecl.Fun("n", ...)`.
9. Docs: `sym.docstring` first; if `None`, fallback: `sym.pos` → source path (resolve relative paths against `sourceRoot`) → read the file, scan backwards from the symbol's start offset for a `/** ... */` block that ends (modulo whitespace/annotations) directly before the definition; give up quietly (`None`) on any miss. Convert with `DocConverter.convert`.
10. Errors: accumulate every `Left` from the mapper into `Result.errors`; a member with an error is omitted from the model.

- [ ] **Step 4: Run to verify pass** (iterate: this is the largest task; Quotes API friction is expected — adjust helper details, keep the test assertions as the contract)

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll && git add scalus-ts-exporter && git commit -m "feat(tsexport): export collector building the TS model from TASTy"
```

---

### Task 7: CLI + golden end-to-end test with tsc validation

**Files:**
- Create: `scalus-ts-exporter/src/main/scala/scalus/tsexport/Main.scala`
- Create: `scalus-ts-exporter/src/test/scala/scalus/tsexport/GoldenTest.scala`
- Create: `scalus-ts-exporter/src/test/resources/golden/fixtures.d.ts` (seeded in Step 4)
- Create: `scalus-ts-exporter/src/test/resources/golden/consumer.ts`
- Modify: `package.json` (root — add `"typescript": "^5.7.2"` to `devDependencies`)

**Interfaces:**
- Produces: `scalus.tsexport.Main` CLI: `--tasty-root <dir>` (repeatable), `--classpath <path-list>`, `--output <file>`, `--source-root <dir>` (default `.`), `--exclude <fqnPrefix>` (repeatable; used by the golden test to drop `tsfixtures.Bad*` and by nothing else yet). Exit 0 on success, 1 with all errors on stderr otherwise.

- [ ] **Step 1: Write `Main`**

```scala
package scalus.tsexport

import java.nio.file.{Files, Paths}

object Main {
    case class Config(
        tastyRoots: List[String] = Nil,
        classpath: List[String] = Nil,
        output: String = "",
        sourceRoot: String = ".",
        excludes: List[String] = Nil
    )

    def parse(args: List[String], cfg: Config = Config()): Either[String, Config] = args match
        case Nil =>
            if cfg.tastyRoots.isEmpty then Left("at least one --tasty-root is required")
            else if cfg.output.isEmpty then Left("--output is required")
            else Right(cfg.copy(tastyRoots = cfg.tastyRoots.reverse, excludes = cfg.excludes.reverse))
        case "--tasty-root" :: v :: rest => parse(rest, cfg.copy(tastyRoots = v :: cfg.tastyRoots))
        case "--classpath" :: v :: rest =>
            parse(rest, cfg.copy(classpath = v.split(java.io.File.pathSeparator).toList))
        case "--output" :: v :: rest      => parse(rest, cfg.copy(output = v))
        case "--source-root" :: v :: rest => parse(rest, cfg.copy(sourceRoot = v))
        case "--exclude" :: v :: rest     => parse(rest, cfg.copy(excludes = v :: cfg.excludes))
        case other :: _                   => Left(s"unknown argument: $other")

    def run(cfg: Config): Either[List[ExportError], String] = {
        val result = ExportCollector.collect(cfg.tastyRoots, cfg.classpath, cfg.sourceRoot)
        val filtered = result.copy(errors =
            result.errors.filterNot(e => cfg.excludes.exists(p => e.member.startsWith(p)))
        )
        // excludes also drop matching declarations from the module — implement in collect or here
        if filtered.errors.nonEmpty then Left(filtered.errors)
        else Right(Emitter.emit(filtered.module))
    }

    def main(args: Array[String]): Unit = parse(args.toList) match
        case Left(msg) =>
            System.err.println(s"scalus-ts-exporter: $msg"); sys.exit(1)
        case Right(cfg) =>
            run(cfg) match
                case Left(errors) =>
                    errors.foreach(e => System.err.println(s"error: ${e.render}"))
                    System.err.println(s"${errors.size} export error(s); no output written")
                    sys.exit(1)
                case Right(text) =>
                    Files.createDirectories(Paths.get(cfg.output).getParent)
                    Files.writeString(Paths.get(cfg.output), text)
                    println(s"wrote ${cfg.output}")
}
```

(`--exclude` must ALSO filter declarations whose Scala FQN starts with the prefix — thread the exclude list into `ExportCollector.collect` as a parameter with default `Nil`; the golden run excludes `tsfixtures.Bad`.)

- [ ] **Step 2: Write the failing golden test**

```scala
package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}

class GoldenTest extends AnyFunSuite {
    test("fixtures generate the committed golden d.ts") {
        val out = Files.createTempFile("fixtures", ".d.ts")
        val cfg = Main.Config(
          tastyRoots = List(InspectorFixture.fixtureClasses),
          classpath = InspectorFixture.fixtureClasspath,
          output = out.toString,
          sourceRoot = InspectorFixture.sourceRoot,
          excludes = List("tsfixtures.Bad")
        )
        Main.run(cfg) match
            case Left(errs) => fail(errs.map(_.render).mkString("\n"))
            case Right(text) =>
                val golden = Paths.get("scalus-ts-exporter/src/test/resources/golden/fixtures.d.ts")
                val expected = Files.readString(golden)
                assert(text == expected, "generated output differs from golden; if intended, regenerate the golden file")
    }

    test("golden d.ts + consumer.ts type-check with tsc") {
        val root = Paths.get(InspectorFixture.sourceRoot)
        val tsc = root.resolve("node_modules/.bin/tsc")
        assume(Files.exists(tsc), "tsc not installed; run sbt installNpmTestDeps")
        val golden = root.resolve("scalus-ts-exporter/src/test/resources/golden")
        val cmd = List(
          tsc.toString, "--noEmit", "--strict", "--target", "es2020",
          "--moduleResolution", "bundler", "--module", "esnext",
          golden.resolve("fixtures.d.ts").toString,
          golden.resolve("consumer.ts").toString
        )
        val proc = new ProcessBuilder(cmd*).inheritIO().start()
        assert(proc.waitFor() == 0, "tsc found type errors in golden output")
    }
}
```

`consumer.ts` (imports don't resolve across bare files, so reference types structurally via the ambient declarations — simplest correct form: make it a sibling `.ts` that imports from `./fixtures`):

```typescript
import { Point, NewName, Statics, Box, Kitchen, Tools, twice, Config } from "./fixtures";

const p: Point = new Point(1, 2);
const d: number = p.dist(p);
const n: NewName = new NewName(1);
const s: Statics = Statics.make(1);
const m: Statics = Statics.mainnet;
const b: Box<string> = new Box("x");
const k = new Kitchen();
const u: string | undefined = k.undef("a");
const big: bigint = k.big(1n);
const ct: "key" | "script" = k.credType();
const cfg: Config = { flag: true };
const t: number = twice(2);
const c: string = Tools.concat("a", undefined);
```

(For the `import from "./fixtures"` form to work, the golden file must be `fixtures.d.ts` in the same directory — it is. `tsc` treats `fixtures.d.ts` as the module `./fixtures`.)

- [ ] **Step 3: Add `"typescript": "^5.7.2"` to the root `package.json` `devDependencies`** (this makes `installNpmTestDeps` provide `tsc`), run `npm install` at the repo root once so local runs have it.

- [ ] **Step 4: Seed the golden file**

Run the generator over the fixtures once (via the failing test's tmp output or `scalusTsExporter/run` with the args from Step 2), copy the output to `scalus-ts-exporter/src/test/resources/golden/fixtures.d.ts`, then REVIEW IT LINE BY LINE against the fixture sources and the spec's mapping table before committing — the golden file is the reviewed contract, not a snapshot rubber-stamp. Verify at minimum: `Point` class with ctor + `dist`; `NewName` + deprecated `OldName` alias export; `Partial` shows only `visible`; `Statics.make(v: number, tag?: string)` static; `Box<A>`; every `Kitchen` member matches Task 5's rendered strings; `Config`/`Inner` interfaces with `nested?: Inner[]`; `Tools` const object; top-level `twice` function; TSDoc blocks present.

- [ ] **Step 5: Run both tests to verify pass**

```
sbt ... "scalusTsExporter/testOnly scalus.tsexport.GoldenTest"
```

- [ ] **Step 6: Full module test run + commit**

```bash
sbt ... "scalusTsExporter/test"
sbt ... scalafmtAll
git add scalus-ts-exporter package.json package-lock.json
git commit -m "feat(tsexport): CLI and golden end-to-end test with tsc validation"
```

---

### Task 8: sbt wiring — `generateDts`, `checkDtsUpToDate`, ci-js gate

**Files:**
- Modify: `build.sbt` (task keys near line 161; `scalusCardanoLedger` jsSettings near line 846; `ci-js` alias near line 1024)

**Interfaces:**
- Consumes: `scalus.tsexport.Main` CLI (Task 7).
- Produces: `scalusCardanoLedgerJS/generateDts`, `scalusCardanoLedgerJS/checkDtsUpToDate` tasks; updated `ci-js` alias.

- [ ] **Step 1: Add task keys** next to the existing ones (`build.sbt:161`):

```scala
lazy val generateDts = taskKey[Unit]("Generate scalus.d.ts from the Scala.js facades' TASTy")
lazy val checkDtsUpToDate = taskKey[Unit]("Fail if the committed scalus.d.ts is stale")
```

- [ ] **Step 2: Wire the tasks** into `scalusCardanoLedger`'s `.jsSettings` (the block that defines `prepareNpmPackage`, `build.sbt:847+`):

```scala
generateDts := Def.taskDyn {
    val coreClasses = (scalus.js / Compile / classDirectory).value.getAbsolutePath
    val ledgerCompile = (Compile / compile).value // ensure TASTy is fresh
    val ledgerClasses = (Compile / classDirectory).value.getAbsolutePath
    val cp = (Compile / fullClasspath).value.map(_.data.getAbsolutePath)
        .mkString(java.io.File.pathSeparator)
    val out = ((Compile / sourceDirectory).value / "npm" / "scalus.d.ts").getAbsolutePath
    val srcRoot = (ThisBuild / baseDirectory).value.getAbsolutePath
    val args = List(
      "--tasty-root", coreClasses,
      "--tasty-root", ledgerClasses,
      "--classpath", cp,
      "--output", out,
      "--source-root", srcRoot
    ).mkString(" ", " ", "")
    Def.task {
        (scalusTsExporter / Compile / runMain).toTask(s" scalus.tsexport.Main$args").value
    }
}.value,
checkDtsUpToDate := {
    generateDts.value
    val out = (Compile / sourceDirectory).value / "npm" / "scalus.d.ts"
    val code = scala.sys.process
        .Process(Seq("git", "diff", "--exit-code", "--", out.getAbsolutePath))
        .!
    if code != 0 then
        sys.error(
          "scalus.d.ts is out of date. Run scalusCardanoLedgerJS/generateDts and commit the result."
        )
},
prepareNpmPackage := prepareNpmPackage.dependsOn(generateDts).value,
```

(`scalus.js / Compile / classDirectory` does not itself compile scalus.js — add `(scalus.js / Compile / compile).value` alongside `ledgerCompile` in the taskDyn body.)

- [ ] **Step 3: Update the `ci-js` alias** (`build.sbt:1024-1027`):

```scala
addCommandAlias(
  "ci-js",
  "clean;js/Test/compile;js/test;scalusTsExporter/test;scalusCardanoLedgerJS/checkDtsUpToDate;scalusCardanoLedgerJS/runNpmTests"
)
```

- [ ] **Step 4: Verify it runs** (it WILL fail `checkDtsUpToDate` because the real facades still contain `js.Dynamic` etc. — expected at this point):

```
sbt ... "scalusCardanoLedgerJS/generateDts"
```
Expected right now: exit 1 with export errors mentioning `JEmulator.submitTx` (js.Dynamic maps to `any`, which is allowed — so the actual expected failures are none from Dynamic; the likely real errors are from any stray non-exportable types). Record what it prints — that is Task 9's work list. If it succeeds, inspect the diff of `scalus.d.ts` but do NOT commit it yet (Task 10 owns the regeneration commit). `git checkout -- scalus-cardano-ledger/js/src/main/npm/scalus.d.ts` afterwards.

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll && git add build.sbt && git commit -m "feat(tsexport): generateDts/checkDtsUpToDate sbt tasks and ci-js drift gate"
```

---

### Task 9: Facade upgrade — flatten exports, rename with aliases, typed returns, MiMa filters

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/uplc/eval/JScalus.scala`
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`
- Modify: `build.sbt` (MiMa filters in `scalusCardanoLedger` settings, near `mimaPreviousArtifacts` at line 818)
- Test: existing `scalus-cardano-ledger/js/src/test` suites + `sbt scalusCardanoLedgerJS/test`

**Interfaces:**
- Consumes: `scalus.interop.{TsName, TsType}` (Task 4).
- Produces: the JS surface Task 10 regenerates from — canonical names `EvaluationResult`, `RedeemerBudget`, top-level `applyDataArgToScript`/`evaluateScript`/`evaluateScriptProfile`/`evalPlutusScripts`, traits `JSubmitResult @TsName("SubmitResult")`, `JDelegationInfo @TsName("DelegationInfo")`.

- [ ] **Step 1: JScalus.scala changes**

- `JSResult`: replace `@JSExportTopLevel("Result")` with (order matters — first is canonical):

```scala
    @JSExportTopLevel("EvaluationResult")
    @JSExportTopLevel("Result")
    class JSResult(
```

- `Redeemer`: same pattern with `@JSExportTopLevel("RedeemerBudget")` then `@JSExportTopLevel("Redeemer")`.
- The four functions: add `@JSExportTopLevel("<name>")` next to the existing `@JSExport` on `applyDataArgToScript`, `evaluateScript`, `evaluateScriptProfile`, `evalPlutusScripts` (export name = Scala name).
- `object JScalus`: extend its scaladoc so the generated `Scalus` const carries a deprecation, e.g. append to the object's doc comment:

```scala
/** Main API exported by Scalus.
  *
  * @deprecated
  *   Use the top-level functions (`evaluateScript`, `evalPlutusScripts`, ...) instead; this
  *   namespace object remains for backwards compatibility.
  */
```

- [ ] **Step 2: JEmulator.scala changes**

Add the two result traits (near `JEmulatorInitialState`) and imports `scalus.interop.{TsName, TsType}`:

```scala
/** Result of [[JEmulator.submitTx]]. */
@TsName("SubmitResult")
trait JSubmitResult extends js.Object {
    val isSuccess: Boolean
    val txHash: js.UndefOr[String]
    val error: js.UndefOr[String]
    val logs: js.UndefOr[js.Array[String]]
}

/** Delegation info returned by [[JEmulator.getDelegation]]. */
@TsName("DelegationInfo")
trait JDelegationInfo extends js.Object {
    /** Pool key hash bytes, or null if not delegated. */
    val poolId: Uint8Array | Null
    /** Reward balance in lovelace. */
    val rewards: js.BigInt
}
```

Change signatures and casts (runtime objects already have these shapes):
- `def submitTx(txCborBytes: Uint8Array): JSubmitResult`
- `def submitTx(txCborBytes: Uint8Array, debugScripts: js.Dictionary[String]): JSubmitResult`
- `private def formatSubmitResult(...): JSubmitResult` — same body, final expression `.asInstanceOf[JSubmitResult]` on each `js.Dynamic.literal(...)`.
- `def getDelegation(stakeCredentialCbor: Uint8Array): JDelegationInfo` — `js.Dynamic.literal(poolId = pool, rewards = ...).asInstanceOf[JDelegationInfo]`.

Rename the emitted names of the existing config traits via `@TsName` (J-prefix must not leak into the d.ts): `@TsName("EmulatorInitialState")` on `JEmulatorInitialState`, `@TsName("StakeRegistration")` on `JStakeRegistration`, `@TsName("PoolRegistration")` on `JPoolRegistration`, `@TsName("DRepRegistration")` on `JDRepRegistration`, `@TsName("DatumEntry")` on `JDatumEntry`.

Tighten the literal unions with `@TsType` (matches the old hand-written d.ts):

```scala
    /** "key" or "script" */
    @TsType("\"key\" | \"script\"")
    val credentialType: String
```
(on both `JStakeRegistration` and `JDRepRegistration`.)

- [ ] **Step 3: MiMa filters** — in `scalusCardanoLedger`'s shared `.settings` block (next to `mimaPreviousArtifacts`, `build.sbt:818`):

```scala
      mimaBinaryIssueFilters ++= {
          import com.typesafe.tools.mima.core.*
          Seq(
            // JS-only facade: submitTx/getDelegation return types narrowed from js.Dynamic
            // to typed js.Object traits (spec 2026-08-03, decision with anemish).
            ProblemFilters.exclude[IncompatibleResultTypeProblem]("scalus.cardano.node.JEmulator.submitTx"),
            ProblemFilters.exclude[IncompatibleResultTypeProblem]("scalus.cardano.node.JEmulator.getDelegation"),
            ProblemFilters.exclude[IncompatibleMethTypeProblem]("scalus.cardano.node.JEmulator.submitTx"),
            ProblemFilters.exclude[IncompatibleMethTypeProblem]("scalus.cardano.node.JEmulator.getDelegation")
          )
      },
```

Run `sbt ... "scalusCardanoLedgerJS/mimaReportBinaryIssues"` and prune the filter list to exactly the problems MiMa reports (keep the comment).

- [ ] **Step 4: Compile + run JS tests**

```
sbt ... "scalusCardanoLedgerJS/test"
```
Expected: PASS (runtime behavior unchanged; only types/annotations moved).

- [ ] **Step 5: Commit**

```bash
sbt ... scalafmtAll
git add scalus-cardano-ledger build.sbt
git commit -m "feat(js): flatten Scala.js exports, typed submitTx/getDelegation results

Top-level exports for the four evaluation functions; EvaluationResult/
RedeemerBudget canonical names with deprecated Result/Redeemer aliases;
JSubmitResult/JDelegationInfo js.Object traits replace js.Dynamic returns
(2 MiMa filters, JS artifact only); TsName/TsType annotations for d.ts
generation. Spec: docs/superpowers/specs/2026-08-03-*.md decision 6."
```

---

### Task 10: Migration — regenerate scalus.d.ts, reconcile consumers

**Files:**
- Regenerate: `scalus-cardano-ledger/js/src/main/npm/scalus.d.ts`
- Modify: `scalus-cardano-ledger/js/src/main/npm/__tests__/*.ts`, `__tests__/shared-tests.ts` (whatever type errors surface)
- Modify: `scalus-examples/js/src/main/ts/evaluate.ts` (+ its `package.json`/`tsconfig.json` only if type errors surface)
- Modify: `scalus-cardano-ledger/js/src/main/npm/README.md`, `scalus-site/content/testing/js-emulator.mdx`, `scalus-site/content/multiplatform.mdx` (import examples)

- [ ] **Step 1: Regenerate**

```
sbt ... "scalusCardanoLedgerJS/generateDts"
```
Expected: exit 0, `scalus.d.ts` rewritten. If export errors surface, fix them at the facade (per Task 9 patterns — @TsType or js.* types), never by hand-editing the output.

- [ ] **Step 2: Review the diff against the old hand-written file**

`git diff scalus-cardano-ledger/js/src/main/npm/scalus.d.ts` — verify against the spec's Migration section, item by item: top-level `EvaluationResult`/`RedeemerBudget` classes + deprecated `Result`/`Redeemer` aliases; top-level function exports present; deprecated `Scalus` const still declared; `SubmitResult`/`DelegationInfo`/`EmulatorInitialState`/`StakeRegistration`/`PoolRegistration`/`DRepRegistration`/`DatumEntry` interfaces present (no J prefix); `"key" | "script"` literal unions preserved; `Emulator` constructor now shows the real third parameter `initialStakeRewards?: { [key: string]: string }` (drift fix — the old file omitted it); `SlotConfig` statics present; TSDoc present on the documented members. Anything missing = a generator bug: go back to the failing component's test, add a fixture case reproducing it, fix, regenerate.

- [ ] **Step 3: Reconcile npm tests and examples**

```
sbt ... "scalusCardanoLedgerJS/runNpmTests"
```
Fix type errors in `__tests__/*.ts` — prefer migrating to the canonical flat imports (`import { evaluateScript, EvaluationResult } from "scalus"`) so the tests double as canonical usage examples, but keep at least one test exercising the deprecated `Scalus.evaluateScript` path and one `new Result(...)`-typed reference to prove the aliases work. Then type-check/adjust `scalus-examples/js/src/main/ts/evaluate.ts` the same way.

- [ ] **Step 4: Update docs** — in `README.md` (npm), `js-emulator.mdx`, `multiplatform.mdx`: switch import examples to the flat API, add one line that `scalus.d.ts` is generated by `scalusCardanoLedgerJS/generateDts` (do not edit by hand).

- [ ] **Step 5: Full gate + commit**

```
sbt ... "scalusCardanoLedgerJS/checkDtsUpToDate"   # must pass now
sbt ... scalafmtAll
git add scalus-cardano-ledger scalus-examples scalus-site
git commit -m "feat(js): generated scalus.d.ts replaces hand-written definitions

scalus.d.ts is now produced by scalus-ts-exporter from the facades' TASTy
(checkDtsUpToDate gates drift in ci-js). npm tests and docs migrated to the
flat ESM API; deprecated Scalus namespace and Result/Redeemer aliases retained."
```

---

### Task 11: Final verification + memory

- [ ] **Step 1: Full local CI-relevant sweep**

```
sbt ... "scalafmtCheckAll" "scalusTsExporter/test" "js/Test/compile" "js/test" "scalusCardanoLedgerJS/checkDtsUpToDate" "scalusCardanoLedgerJS/runNpmTests" "scalusCardanoLedgerJVM/mimaReportBinaryIssues" "scalusCardanoLedgerJS/mimaReportBinaryIssues"
```
All must pass. Then `sbt ... quick` for the JVM side (Task 9 touched shared settings only; expect green).

- [ ] **Step 2: Spec coverage check** — re-read the spec top to bottom; confirm each section maps to landed code (survey/decisions → n/a; architecture/pipeline/mapping/policy/docs/wiring → Tasks 1–8; decision 6 + migration → Tasks 9–10; testing → Tasks 2–7; error handling → Tasks 5–7). Fix gaps before declaring done.

- [ ] **Step 3: Verify branch state** — `git log --oneline master..HEAD` shows the spec + plan + ~9 implementation commits; `git status` clean (the untracked `zz-worktree-git-override.sbt` is expected).

- [ ] **Step 4: Save a memory note** (per global memory instructions) recording: scalus-ts-exporter exists, checkDtsUpToDate is the drift gate, scalus.d.ts is generated — update the stale "hand-written d.ts" claims in existing memories if any.

---

## Self-Review Notes (already applied)

- The `TsType.Generic` case (for `Promise<T>`) is introduced in Task 5 Step 1 and must be added to Task 2's `TsType` enum and `EmitterTest` when reached — flagged in both places.
- `--exclude` filters both errors AND declarations (Task 7 Step 1 note).
- Golden seeding is generate-then-review, not blind snapshot (Task 7 Step 4).
- MiMa filter list is pruned to what MiMa actually reports (Task 9 Step 3).
- Old `Result`/`Redeemer`/`Scalus` deprecated paths keep one test each (Task 10 Step 3).
