# UPLC Source View Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit a `<key>.uplc.json` artifact (UPLC text + text-range → source-position span map) with profile reports, and add a bidirectional side-by-side UPLC view to the Scalus Profiler VS Code extension.

**Architecture:** Scalus stamps `functionName` into `UplcAnnotation` during V3 lowering, renders the evaluated in-memory `Term` with the existing paiges printer plus zero-width markers to recover exact text offsets, and writes the artifact via `ProfileReportWriter` into the existing `profile-manifest.json` (`format: "uplc"`). The extension parses the artifact and syncs cursor/highlights between the Scala editor and a read-only virtual UPLC document.

**Tech Stack:** Scala 3 (scalus-core shared), paiges 0.4.4 (`Doc.zeroWidth`), jsoniter-scala, TypeScript VS Code extension (zero runtime deps, esbuild).

**Spec:** `docs/superpowers/specs/2026-07-31-uplc-source-view-design.md`

## Global Constraints

- Two repos: `scalus` (this repo) and `/Users/nau/projects/lantr/scalus-vscode-extension`.
- Branch name in BOTH repos: `feature/uplc-source-view` (create from current master/main HEAD; do NOT commit to master).
- Conventional commits (`feat:`, `fix:`, `test:`, `docs:`). NEVER add a `Co-Authored-By: Claude` (or similar) trailer.
- No em dashes (—) in any authored text (docs, comments, commit messages); use en dash (–) if needed.
- Scalus repo: run `sbtn scalafmtAll` before EVERY commit (CI fails on one unformatted file).
- Scalus code style: Scala 3, `{}` for top-level defs, indentation syntax for small `if`/`match`, `then`/`do` keywords. 4-space indent (scalafmt enforces).
- MiMa: only ADD public API; never change existing public signatures. New public members on existing types are OK. `private[scalus]` additions are safe.
- Artifact schema constants (copy verbatim): file name `"$scriptHash-$redeemerTag-$redeemerIndex.uplc.json"`, manifest format string `"uplc"`, `schemaVersion: 1`, span fields `s`,`e`,`n`,`file`,`sl`,`sc`,`el`,`ec`,`fn` (0-based lines AND columns, character offsets, end-exclusive `e`), top-level fields `schemaVersion`,`uplc`,`files`,`functions`,`spans`.
- Render width: 80 (same as `Term.show`).
- Extension: zero runtime dependencies; new pure logic goes in vscode-free modules tested by `test/smoke.ts`; `npm run typecheck && npm test` must pass before each commit.

## Deviation from spec (approved during planning)

The spec says "New `ProfileFormat.Uplc` in `EvaluatorReportConfig`". Do NOT add that enum case: `ProfileFormat` values are rendered from `ProfilingData` by `ProfileReporting.render`, which cannot produce UPLC (it has no `Term`). Instead `ProfileReportWriter.write` takes the term as a new optional parameter and writes the artifact directly when the profile level is `Full`. Task 5 updates the spec file accordingly.

## Key file map (scalus)

| File | Role |
|---|---|
| `scalus-core/shared/src/main/scala/scalus/uplc/UplcAnnotation.scala` | annotation type (pos + functionName) |
| `scalus-core/shared/src/main/scala/scalus/uplc/Term.scala` | `Term` enum, fill passes (line ~111/198), `given Pretty[Term]` (line ~561) |
| `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweringContext.scala` | mutable lowering context (add `currentFunction`) |
| `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValue.scala` | ~40 `UplcAnnotation(pos)` stamp sites |
| `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/Lowering.scala` | `lowerSIR`, `SIR.Let`/`SIR.Decl` cases (set `currentFunction`) |
| `scalus-core/shared/src/main/scala/scalus/uplc/eval/ProfileReportWriter.scala` | artifact + manifest writer |
| `scalus-cardano-ledger/shared/.../PlutusScriptEvaluator.scala:382,722` | `renderProfile` + call site (has `plutusScript`) |
| `scalus-testkit/shared/.../ScalusTest.scala:94` | `runWithProfileReport` (has `self: Program`) |
| `scalus-core/shared/src/main/scala/scalus/compiler/Compiled.scala:89-134` | `toUplc` fill-pass call site |

Facts verified during planning:
- paiges 0.4.4 has `Doc.zeroWidth(s: String): Doc` (zero layout width, emitted in render output). The printer already uses paiges styling (zero-width ANSI) for XTerm mode.
- The `Apply` printer case flattens chains via `a.applyToList`; inner `Apply` nodes of a chain never pass through `prettyTermWithDepth`, so they get no spans. Accepted: the outermost application's span covers the chain.
- Annotated terms reach the ledger evaluator via `Script.PlutusV3(program)` factories caching `_cachedProgram` (`scalus-core/shared/.../cardano/ledger/Script.scala:26-39`); CBOR-decoded scripts have empty annotations, so the writer must skip the artifact when the term carries no source info.
- `Pretty[Term].pretty` calls `TermSanitizer.sanitizeNames` (annotation-preserving, structure-preserving) before printing.

---

# Part A: scalus repo

### Task 1: Branch + `functionName` stamping during lowering

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweringContext.scala`
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValue.scala`
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/Lowering.scala`
- Test: `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/FunctionNameAnnotationTest.scala` (create)

**Interfaces:**
- Produces: `LoweringContext.currentFunction: String` (var, default `""`), `LoweringContext.ann(pos: SIRPosition): UplcAnnotation` returning `UplcAnnotation(pos, currentFunction)`. Lowered terms carry `annotation.functionName` for code inside named `Let`-bound lambdas / top-level defs.

- [ ] **Step 1: Create the branch**

```bash
cd /Users/nau/projects/lantr/scalus && git checkout -b feature/uplc-source-view
```

- [ ] **Step 2: Write the failing test**

Look at existing lowering tests in `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/` for the established way to compile-and-lower in tests (most use `scalus.Compiler.compile { ... }` then `.toUplc()` via `import scalus.*`). Follow that pattern:

```scala
package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Term

class FunctionNameAnnotationTest extends AnyFunSuite {

    private def collectFunctionNames(t: Term): Set[String] = {
        def go(t: Term, acc: Set[String]): Set[String] = {
            val acc1 = if t.annotation.functionName.nonEmpty then acc + t.annotation.functionName else acc
            t match
                case Term.LamAbs(_, body, _)   => go(body, acc1)
                case Term.Apply(f, arg, _)     => go(arg, go(f, acc1))
                case Term.Force(b, _)          => go(b, acc1)
                case Term.Delay(b, _)          => go(b, acc1)
                case Term.Constr(_, args, _)   => args.foldLeft(acc1)((a, x) => go(x, a))
                case Term.Case(arg, cases, _)  => cases.foldLeft(go(arg, acc1))((a, x) => go(x, a))
                case _                         => acc1
        }
        go(t, Set.empty)
    }

    test("lowered terms carry the enclosing function name") {
        val sir = Compiler.compile {
            def double(x: BigInt): BigInt = x + x
            double(21)
        }
        val term = sir.toUplc()
        val names = collectFunctionNames(term)
        assert(names.contains("double"), s"expected 'double' in $names")
    }
}
```

Note: the compiled `def double` becomes a `SIR.Let` binding whose name may be qualified or suffixed. If the assert fails only because of a name prefix (e.g. `"...double"`), relax to `names.exists(_.endsWith("double"))` and keep that as the contract.

- [ ] **Step 3: Run the test, verify it fails**

```bash
sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.FunctionNameAnnotationTest"
```
Expected: FAIL (empty set – nothing populates `functionName` today).

- [ ] **Step 4: Implement**

1. `LoweringContext.scala` – add to the class body (not the constructor):

```scala
    /** Name of the innermost enclosing user function being lowered. Stamped into
      * [[scalus.uplc.UplcAnnotation.functionName]] by [[ann]] so tooling (VS Code
      * UPLC source view) can group compiled UPLC by source function. Empty when
      * lowering code outside any named binding.
      */
    var currentFunction: String = ""

    /** Annotation for a lowered term: position plus the enclosing function name. */
    def ann(pos: SIRPosition): UplcAnnotation = UplcAnnotation(pos, currentFunction)

    /** Run `body` with [[currentFunction]] set to `name`, restoring the previous value. */
    def withFunction[A](name: String)(body: => A): A = {
        val saved = currentFunction
        currentFunction = name
        try body
        finally currentFunction = saved
    }
```

Add `import scalus.uplc.UplcAnnotation` to the file's imports.

2. `Lowering.scala` – find where `SIR.Let` bindings are lowered (grep `case SIR.Let` / `Binding(`). For each binding whose rhs is lowered, wrap the rhs lowering in `lctx.withFunction(binding.name) { ... }` ONLY when the rhs is a lambda (`SIR.LamAbs`) – value bindings keep the enclosing function. Also find where top-level module definitions are lowered (the driver that iterates `Module.defs` or lowers the root `SIR.Decl`/`Let` chain produced by the plugin) and do the same there. Use the binding's simple name: `binding.name.split('.').last` if names are dot-qualified.

3. `LoweredValue.scala` – mechanically replace `UplcAnnotation(pos)` / `UplcAnnotation(<expr>)` term-construction sites with `lctx.ann(pos)` (the enclosing methods have `(using lctx: LoweringContext)` or a `lctx` in scope; check each of the ~40 sites; where no context is in scope, leave `UplcAnnotation(pos)` unchanged and note it in the commit message).

- [ ] **Step 5: Run the test, verify it passes; run the lowering test suite**

```bash
sbtn "scalusJVM/testOnly scalus.compiler.sir.lowering.*"
```
Expected: PASS, no regressions.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add -A && git commit -m "feat(compiler): stamp enclosing function name into UplcAnnotation during V3 lowering"
```

---

### Task 2: Annotation-preserving fill passes

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/Term.scala` (fill passes, lines ~111-230)
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/Compiled.scala` (line ~133)
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/FillAnnotationsTest.scala` (create)

**Interfaces:**
- Consumes: `UplcAnnotation(pos, functionName)` from Task 1.
- Produces: `private[scalus] def fillEmptyAnnotationsBottomUp: (Term, UplcAnnotation)` and `private[scalus] def fillEmptyAnnotationsTopDown(inherited: UplcAnnotation): Term` on `Term`. Existing public `fillEmptyPosBottomUp`/`fillEmptyPosTopDown` keep their exact signatures and become thin delegates.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.ScalusSourcePos

class FillAnnotationsTest extends AnyFunSuite {
    private val pos = ScalusSourcePos("Foo.scala", 10, 0, 10, 20)
    private val ann = UplcAnnotation(pos, "validate")

    test("bottom-up fill propagates functionName to spine nodes") {
        val leaf = Term.Var(NamedDeBruijn("x"), ann)
        val spine = Term.Force(Term.Delay(leaf)) // spine has empty annotations
        val (filled, _) = spine.fillEmptyAnnotationsBottomUp
        assert(filled.annotation.functionName == "validate")
        assert(filled.annotation.pos == pos)
    }

    test("top-down fill propagates functionName downward") {
        val inner = Term.Delay(Term.Var(NamedDeBruijn("x")))
        val filled = inner.fillEmptyAnnotationsTopDown(ann)
        assert(filled.annotation.functionName == "validate")
        val Term.Delay(v, _) = filled: @unchecked
        assert(v.annotation.functionName == "validate")
    }

    test("existing annotations are never overwritten") {
        val other = UplcAnnotation(ScalusSourcePos("Bar.scala", 1, 0, 1, 5), "other")
        val leaf = Term.Var(NamedDeBruijn("x"), other)
        val filled = leaf.fillEmptyAnnotationsTopDown(ann)
        assert(filled.annotation == other)
    }
}
```

- [ ] **Step 2: Run, verify it fails to compile** (methods don't exist)

```bash
sbtn "scalusJVM/testOnly scalus.uplc.FillAnnotationsTest"
```

- [ ] **Step 3: Implement**

In `Term.scala`, generalize the two existing fill passes from `ScalusSourcePos` to `UplcAnnotation`:

- `fillEmptyAnnotationsBottomUp: (Term, UplcAnnotation)` – same traversal as `fillEmptyPosBottomUp`, but `firstNonEmpty` picks the first annotation whose `pos.effectivePos` is not effectively empty (preserving that annotation's `functionName`), and `stamp` writes the whole representative annotation:

```scala
        def firstNonEmpty(as: UplcAnnotation*): UplcAnnotation =
            as.iterator
                .map(a => a.copy(pos = a.pos.effectivePos))
                .find(!_.pos.isEffectivelyEmpty)
                .getOrElse(UplcAnnotation.empty)
        def stamp(t: Term, rep: UplcAnnotation): UplcAnnotation =
            if t.annotation.isEffectivelyEmpty && !rep.pos.isEffectivelyEmpty then rep
            else t.annotation
```

Each case mirrors the existing one, with `t.annotation` in place of `t.annotation.pos` for the recursion results.

- `fillEmptyAnnotationsTopDown(inherited: UplcAnnotation): Term` – same as `fillEmptyPosTopDown` with `UplcAnnotation` threaded instead of `ScalusSourcePos`.

- Rewrite the two existing public methods as delegates (signatures unchanged):

```scala
    def fillEmptyPosBottomUp: (Term, ScalusSourcePos) =
        val (t, a) = fillEmptyAnnotationsBottomUp
        (t, a.pos)

    def fillEmptyPosTopDown(inherited: ScalusSourcePos): Term =
        fillEmptyAnnotationsTopDown(UplcAnnotation(inherited))
```

- `Compiled.scala:133` – replace
  `optimized.fillEmptyPosBottomUp._1.fillEmptyPosTopDown(scalus.utils.ScalusSourcePos.empty)`
  with
  `optimized.fillEmptyAnnotationsBottomUp._1.fillEmptyAnnotationsTopDown(UplcAnnotation.empty)` (import `scalus.uplc.UplcAnnotation`).

- [ ] **Step 4: Run tests**

```bash
sbtn "scalusJVM/testOnly scalus.uplc.FillAnnotationsTest scalus.uplc.eval.CekSourcePosTest"
```
Expected: PASS (CekSourcePosTest guards the existing pos-fill behavior).

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -A && git commit -m "feat(uplc): annotation-preserving fill passes carrying functionName"
```

---

### Task 3: Decorator hook in the Pretty[Term] printer

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/Term.scala` (`given Pretty[Term]`, lines ~560-655)
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/PrettyDecoratedTest.scala` (create)

**Interfaces:**
- Produces: `private[scalus] object TermPrinter { def prettySanitized(term: Term, style: Style, decorate: (Term, Doc) => Doc): Doc }` where `term` must already be name-sanitized. `Term.pretty` behavior is byte-identical to before.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.paiges.Doc
import scalus.utils.Style
import scalus.uplc.DefaultFun.AddInteger

class PrettyDecoratedTest extends AnyFunSuite {
    private val term = Term.Apply(
      Term.Apply(Term.Builtin(AddInteger), Term.Const(Constant.Integer(1))),
      Term.Const(Constant.Integer(2))
    )

    test("identity decorator renders identically to pretty") {
        val sanitized = TermSanitizer.sanitizeNames(term)
        val doc = TermPrinter.prettySanitized(sanitized, Style.Normal, (_, d) => d)
        assert(doc.render(80) == term.show)
    }

    test("decorator wraps every printed node") {
        var count = 0
        val sanitized = TermSanitizer.sanitizeNames(term)
        TermPrinter
            .prettySanitized(sanitized, Style.Normal, (_, d) => { count += 1; d })
            .render(80)
        // builtin + 2 consts + outermost Apply of the flattened chain = 4
        assert(count == 4)
    }
}
```

- [ ] **Step 2: Run, verify compile failure** (`TermPrinter` doesn't exist)

```bash
sbtn "scalusJVM/testOnly scalus.uplc.PrettyDecoratedTest"
```

- [ ] **Step 3: Implement**

Move the body of the `given Pretty[Term]`'s `prettyTermWithDepth` into a new `private[scalus] object TermPrinter` in `Term.scala` (same file, below the given):

```scala
private[scalus] object TermPrinter {
    /** Pretty-print an already-sanitized term, passing every printed node's Doc through
      * `decorate`. `(term, doc) => doc` reproduces `Term.pretty` exactly. Inner `Apply`
      * nodes of a flattened application chain are not printed individually and are not
      * decorated.
      */
    def prettySanitized(term: Term, style: Style, decorate: (Term, Doc) => Doc): Doc =
        prettyTermWithDepth(term, style, depth = 0, decorate)

    private def prettyTermWithDepth(
        term: Term,
        style: Style,
        depth: Int,
        decorate: (Term, Doc) => Doc
    ): Doc = { ... existing body, every recursive call passes decorate,
               and the final Doc of each case is wrapped: decorate(term, doc) }
}
```

Concretely: each `case` in the existing match builds its `Doc` exactly as today; bind it to `val doc = ...` and return `decorate(term, doc)`. Recursive calls become `prettyTermWithDepth(x, style, depth + 1, decorate)`.

The `given Pretty[Term]` becomes:

```scala
    given Pretty[Term] with
        def pretty(term: Term, style: Style): Doc =
            TermPrinter.prettySanitized(TermSanitizer.sanitizeNames(term), style, (_, d) => d)
```

- [ ] **Step 4: Run the test plus printer-sensitive suites**

```bash
sbtn "scalusJVM/testOnly scalus.uplc.PrettyDecoratedTest scalus.uplc.*Pretty* scalus.uplc.UplcParserTest"
```
Expected: PASS. Also run `sbtn "scalusJVM/testQuick"` to catch golden-output tests that assert on `show` strings.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -A && git commit -m "refactor(uplc): extract TermPrinter with a per-node decorator hook"
```

---

### Task 4: UplcSourceMapRenderer

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/uplc/eval/UplcSourceMap.scala`
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/eval/UplcSourceMapRendererTest.scala`

**Interfaces:**
- Consumes: `TermPrinter.prettySanitized` (Task 3), `Term.annotation` with `functionName` (Tasks 1-2).
- Produces:

```scala
case class UplcSpan(s: Int, e: Int, n: Int, file: Int, sl: Int, sc: Int, el: Int, ec: Int, fn: Option[Int])
case class UplcSourceMap(schemaVersion: Int, uplc: String, files: Seq[String], functions: Seq[String], spans: Seq[UplcSpan])
object UplcSourceMapRenderer {
    val SchemaVersion = 1
    def hasSourceInfo(term: Term): Boolean
    def render(term: Term): UplcSourceMap
    def toJson(map: UplcSourceMap): Array[Byte]   // jsoniter, indented
}
```

- [ ] **Step 1: Write the failing test**

```scala
package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.utils.ScalusSourcePos

class UplcSourceMapRendererTest extends AnyFunSuite {
    private val posA = ScalusSourcePos("/src/Foo.scala", 10, 2, 10, 7)
    private val posB = ScalusSourcePos("/src/Foo.scala", 12, 4, 12, 9)
    private val annA = UplcAnnotation(posA, "validate")
    private val annB = UplcAnnotation(posB, "")

    private val term = Term.Apply(
      Term.Apply(Term.Builtin(AddInteger, annA), Term.Const(Constant.Integer(1), annB)),
      Term.Const(Constant.Integer(2)),
      annA
    )

    test("uplc text equals plain show (markers fully stripped)") {
        val map = UplcSourceMapRenderer.render(term)
        assert(map.uplc == term.show)
    }

    test("spans point at the printed node text") {
        val map = UplcSourceMapRenderer.render(term)
        val builtinSpan = map.spans.find(sp => map.uplc.substring(sp.s, sp.e).contains("addInteger")).get
        assert(map.files(builtinSpan.file) == "/src/Foo.scala")
        assert(builtinSpan.sl == 10 && builtinSpan.sc == 2 && builtinSpan.el == 10 && builtinSpan.ec == 7)
        assert(builtinSpan.fn.map(map.functions) == Some("validate"))
    }

    test("nodes without positions produce no spans") {
        val map = UplcSourceMapRenderer.render(term)
        // the '2' const has an empty annotation
        assert(!map.spans.exists(sp => map.uplc.substring(sp.s, sp.e) == "(con integer 2)"))
    }

    test("spans nest and offsets are within bounds") {
        val map = UplcSourceMapRenderer.render(term)
        map.spans.foreach { sp =>
            assert(sp.s >= 0 && sp.e <= map.uplc.length && sp.s < sp.e)
        }
    }

    test("post-order indices are stable under Apply wrapping") {
        val wrapped = Term.Apply(term, Term.Const(Constant.Integer(3)))
        val base = UplcSourceMapRenderer.render(term)
        val wrap = UplcSourceMapRenderer.render(wrapped)
        val baseByPos = base.spans.map(sp => (sp.sl, sp.sc, sp.n)).toSet
        // every base span keeps its node index in the wrapped program
        baseByPos.foreach { case (sl, sc, n) =>
            assert(wrap.spans.exists(sp => sp.sl == sl && sp.sc == sc && sp.n == n))
        }
    }

    test("hasSourceInfo") {
        assert(UplcSourceMapRenderer.hasSourceInfo(term))
        assert(!UplcSourceMapRenderer.hasSourceInfo(Term.Const(Constant.Integer(1))))
    }

    test("json round-trip") {
        val map = UplcSourceMapRenderer.render(term)
        val json = new String(UplcSourceMapRenderer.toJson(map), "UTF-8")
        assert(json.contains("\"schemaVersion\": 1") || json.contains("\"schemaVersion\":1"))
        assert(json.contains("\"uplc\""))
    }
}
```

- [ ] **Step 2: Run, verify compile failure**

```bash
sbtn "scalusJVM/testOnly scalus.uplc.eval.UplcSourceMapRendererTest"
```

- [ ] **Step 3: Implement `UplcSourceMap.scala`**

```scala
package scalus.uplc.eval

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.typelevel.paiges.Doc
import scalus.uplc.{Term, TermPrinter, TermSanitizer}
import scalus.utils.Style

/** One mapped region of rendered UPLC text.
  *
  * Offsets `s`/`e` are character offsets into [[UplcSourceMap.uplc]] (end-exclusive).
  * `n` is the node's post-order index in the term tree (children before parent, fields
  * in declaration order) – stable when the program is later wrapped in `Apply` nodes.
  * `sl`/`sc`/`el`/`ec` are 0-based source lines/columns (raw ScalusSourcePos values;
  * note profile.json uses 1-based lines). `file`/`fn` index [[UplcSourceMap.files]] /
  * [[UplcSourceMap.functions]].
  */
case class UplcSpan(s: Int, e: Int, n: Int, file: Int, sl: Int, sc: Int, el: Int, ec: Int, fn: Option[Int])

/** The `<key>.uplc.json` document consumed by the Scalus VS Code extension. */
case class UplcSourceMap(
    schemaVersion: Int,
    uplc: String,
    files: Seq[String],
    functions: Seq[String],
    spans: Seq[UplcSpan]
)

object UplcSourceMapRenderer {
    val SchemaVersion = 1

    private given JsonValueCodec[UplcSourceMap] = JsonCodecMaker.make

    private val MarkerStart = '\u0001'
    private val MarkerEnd = '\u0002'

    /** True when at least one node carries a usable source position. */
    def hasSourceInfo(term: Term): Boolean =
        !term.annotation.pos.effectivePos.isEffectivelyEmpty || (term match
            case Term.LamAbs(_, b, _)    => hasSourceInfo(b)
            case Term.Apply(f, a, _)     => hasSourceInfo(f) || hasSourceInfo(a)
            case Term.Force(b, _)        => hasSourceInfo(b)
            case Term.Delay(b, _)        => hasSourceInfo(b)
            case Term.Constr(_, as, _)   => as.exists(hasSourceInfo)
            case Term.Case(a, cs, _)     => hasSourceInfo(a) || cs.exists(hasSourceInfo)
            case _                       => false
        )

    def render(term: Term): UplcSourceMap = {
        val sanitized = TermSanitizer.sanitizeNames(term)

        // Post-order index per node (identity-based: the tree may contain equal subterms).
        val postOrder = new java.util.IdentityHashMap[Term, Integer]()
        var next = 0
        def index(t: Term): Unit = {
            t match
                case Term.LamAbs(_, b, _)  => index(b)
                case Term.Apply(f, a, _)   => index(f); index(a)
                case Term.Force(b, _)      => index(b)
                case Term.Delay(b, _)      => index(b)
                case Term.Constr(_, as, _) => as.foreach(index)
                case Term.Case(a, cs, _)   => index(a); cs.foreach(index)
                case _                     => ()
            postOrder.put(t, next)
            next += 1
        }
        index(sanitized)

        // Collect annotations per marker id; decorate with zero-width markers.
        val nodes = scala.collection.mutable.ArrayBuffer.empty[Term]
        val doc = TermPrinter.prettySanitized(
          sanitized,
          Style.Normal,
          (t, d) =>
              if t.annotation.pos.effectivePos.isEffectivelyEmpty then d
              else {
                  val id = nodes.length
                  nodes += t
                  Doc.zeroWidth(s"$MarkerStart$id$MarkerEnd") + d +
                      Doc.zeroWidth(s"$MarkerStart/$id$MarkerEnd")
              }
        )
        val marked = doc.render(80)

        // Strip markers, recording clean offsets.
        val clean = new StringBuilder(marked.length)
        val starts = new Array[Int](nodes.length)
        val ends = new Array[Int](nodes.length)
        var i = 0
        while i < marked.length do {
            val c = marked.charAt(i)
            if c == MarkerStart then {
                val stop = marked.indexOf(MarkerEnd, i + 1)
                val body = marked.substring(i + 1, stop)
                if body.startsWith("/") then ends(body.drop(1).toInt) = clean.length
                else starts(body.toInt) = clean.length
                i = stop + 1
            } else {
                clean.append(c)
                i += 1
            }
        }

        val files = scala.collection.mutable.LinkedHashMap.empty[String, Int]
        val functions = scala.collection.mutable.LinkedHashMap.empty[String, Int]
        def intern(m: scala.collection.mutable.LinkedHashMap[String, Int], s: String): Int =
            m.getOrElseUpdate(s, m.size)

        val spans = nodes.indices.map { id =>
            val t = nodes(id)
            val pos = t.annotation.pos.effectivePos
            val fn = t.annotation.functionName
            UplcSpan(
              s = starts(id),
              e = ends(id),
              n = postOrder.get(t),
              file = intern(files, pos.file),
              sl = pos.startLine,
              sc = pos.startColumn,
              el = pos.endLine,
              ec = pos.endColumn,
              fn = if fn.isEmpty then None else Some(intern(functions, fn))
            )
        }

        UplcSourceMap(SchemaVersion, clean.toString, files.keys.toSeq, functions.keys.toSeq, spans)
    }

    def toJson(map: UplcSourceMap): Array[Byte] =
        writeToArray(map, WriterConfig.withIndentionStep(2))
}
```

Check the actual `ScalusSourcePos` field names (`startLine`, `startColumn`, `endLine`, `endColumn`) and `effectivePos`/`isEffectivelyEmpty` before compiling; adjust if they differ.

- [ ] **Step 4: Run the test, verify PASS**

```bash
sbtn "scalusJVM/testOnly scalus.uplc.eval.UplcSourceMapRendererTest"
```

- [ ] **Step 5: Add an integration invariant test on a real compiled program**

Append to the same test file:

```scala
    test("invariant holds for a compiled program") {
        import scalus.*
        val sir = Compiler.compile {
            def double(x: BigInt): BigInt = x + x
            double(21)
        }
        val t = sir.toUplc()
        val map = UplcSourceMapRenderer.render(t)
        assert(map.uplc == t.show)
        assert(map.spans.nonEmpty)
        assert(map.functions.exists(_.endsWith("double")))
    }
```

Run again; expected PASS.

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add -A && git commit -m "feat(uplc): UplcSourceMapRenderer – UPLC text with source-position span map"
```

---

### Task 5: Write the artifact with profile reports

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/eval/ProfileReportWriter.scala`
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/PlutusScriptEvaluator.scala` (renderProfile ~382, call site ~722)
- Modify: `scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala` (~94)
- Modify: `docs/superpowers/specs/2026-07-31-uplc-source-view-design.md` (remove the `ProfileFormat.Uplc` sentence, describe the writer-parameter approach)
- Test: `scalus-core/shared/src/test/scala/scalus/uplc/eval/ProfileReportWriterUplcTest.scala` (create; put it next to any existing ProfileReportWriter test and follow its temp-dir pattern if one exists)

**Interfaces:**
- Consumes: `UplcSourceMapRenderer` (Task 4).
- Produces: `ProfileReportWriter.write(data, report, scriptHash, language, redeemerTag, redeemerIndex, onConsole, uplcTerm: Option[Term] = None)`. When `report.profile == ProfileLevel.Full`, `uplcTerm` is defined, and `UplcSourceMapRenderer.hasSourceInfo(term)`, writes `<key>.uplc.json` under `report.outputDir` and adds `("uplc", fileName)` to the manifest run's files.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.uplc.eval

import com.github.plokhotnyuk.jsoniter_scala.core.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{EvaluatorReportConfig, ProfileLevel}
import scalus.uplc.*
import scalus.uplc.DefaultFun.AddInteger
import scalus.utils.ScalusSourcePos

import java.nio.file.{Files, Path}

class ProfileReportWriterUplcTest extends AnyFunSuite {
    private def annotated: Term =
        Term.Builtin(AddInteger, UplcAnnotation(ScalusSourcePos("/src/A.scala", 3, 0, 3, 5), "f"))

    private def emptyProfile: ProfilingData = ProfilingData.empty // if no such member exists,
    // construct the minimal ProfilingData the same way existing ProfileReportWriter/Formatter
    // tests do – check those tests first.

    test("uplc.json is written and indexed in the manifest") {
        val dir = Files.createTempDirectory("scalus-uplc-test")
        val report = EvaluatorReportConfig(
          enabled = true,
          outputDir = dir.toString,
          profile = ProfileLevel.Full
        )
        ProfileReportWriter.write(
          emptyProfile, report, "cafe01", "PlutusV3", "Spend", 0, _ => (), Some(annotated)
        )
        val uplcFile = dir.resolve("cafe01-Spend-0.uplc.json")
        assert(Files.exists(uplcFile))
        val manifest = new String(Files.readAllBytes(dir.resolve("profile-manifest.json")), "UTF-8")
        assert(manifest.contains("\"uplc\""))
        assert(manifest.contains("cafe01-Spend-0.uplc.json"))
    }

    test("no artifact for a term without source info") {
        val dir = Files.createTempDirectory("scalus-uplc-test2")
        val report = EvaluatorReportConfig(enabled = true, outputDir = dir.toString, profile = ProfileLevel.Full)
        ProfileReportWriter.write(
          emptyProfile, report, "cafe02", "PlutusV3", "Spend", 0, _ => (),
          Some(Term.Const(Constant.Integer(1)))
        )
        assert(!Files.exists(dir.resolve("cafe02-Spend-0.uplc.json")))
    }
}
```

Before running: check how existing tests build a `ProfilingData` (grep `ProfilingData(` in test sources) and use that instead of the `ProfilingData.empty` placeholder if it does not exist. Note: `ProfilingData` totals feed the manifest budget; zeros are fine.

- [ ] **Step 2: Run, verify failure** (no such parameter)

```bash
sbtn "scalusJVM/testOnly scalus.uplc.eval.ProfileReportWriterUplcTest"
```

- [ ] **Step 3: Implement**

In `ProfileReportWriter.write`, add the parameter `uplcTerm: Option[scalus.uplc.Term] = None` (last, after `onConsole`). After the `outputs.foreach` loop and before `val files = written.result()`:

```scala
        uplcTerm.foreach { term =>
            if report.profile == ProfileLevel.Full && UplcSourceMapRenderer.hasSourceInfo(term)
            then {
                val file = s"$key.uplc.json"
                platform.createDirectories(report.outputDir)
                platform.writeFile(
                  reportPath(report, file),
                  UplcSourceMapRenderer.toJson(UplcSourceMapRenderer.render(term))
                )
                written += "uplc" -> file
            }
        }
```

Add `import scalus.cardano.ledger.ProfileLevel` if missing.

`PlutusScriptEvaluator.scala`: add a `term: Term` parameter to `renderProfile` and pass it through:

```scala
        private def renderProfile(
            result: Result,
            scriptHash: ScriptHash,
            redeemer: Redeemer,
            language: Language,
            uplcTerm: => Term
        ): Unit = result.profile.foreach { data =>
            ProfileReportWriter.write(
              data, report, scriptHash.toHex, language.toString,
              redeemer.tag.toString, redeemer.index, log.info(_),
              Some(uplcTerm)
            )
        }
```

Call site (~line 722): pass `plutusScript.program.term` as the new argument. (By-name so the CBOR decode only happens when a profile was actually produced.)

`ScalusTest.runWithProfileReport` (~line 97): add `Some(self.term)` as the last `write` argument.

- [ ] **Step 4: Run tests**

```bash
sbtn "scalusJVM/testOnly scalus.uplc.eval.ProfileReportWriterUplcTest"
sbtn "scalusJVM/testQuick"
```
Expected: PASS.

- [ ] **Step 5: Update the spec file**

In `docs/superpowers/specs/2026-07-31-uplc-source-view-design.md`, replace the two lines

> - New `ProfileFormat.Uplc` in `EvaluatorReportConfig`. `ProfileLevel.Full` writes it.

with

> - `ProfileReportWriter.write` takes the evaluated term as an optional parameter and
>   writes the artifact when the profile level is `Full` and the term carries source
>   info (no new `ProfileFormat` case: those are rendered from `ProfilingData`, which
>   has no `Term`).

- [ ] **Step 6: Full check, format and commit**

```bash
sbtn scalafmtAll
sbtn quick
git add -A && git commit -m "feat(profiler): write <key>.uplc.json UPLC source map with profile reports"
```

- [ ] **Step 7: Verify MiMa**

```bash
sbtn mima
```
Expected: clean (all changed members are private/`private[scalus]`; `Term` additions are additive). If `fillEmptyAnnotations*` additions are flagged (they should not be – additions are compatible), report back instead of adding filters.

---

# Part B: scalus-vscode-extension repo

All paths below are relative to `/Users/nau/projects/lantr/scalus-vscode-extension`.

### Task 6: uplcMap.ts (pure model + queries) with tests

**Files:**
- Create: `src/uplcMap.ts`
- Modify: `test/smoke.ts` (append a test section; follow its existing plain-assert style)

**Interfaces:**
- Produces:

```ts
export const UPLC_MAP_SCHEMA_VERSION = 1;
export interface UplcSpan { s: number; e: number; n: number; file: number; sl: number; sc: number; el: number; ec: number; fn?: number }
export interface UplcSourceMap { schemaVersion: number; uplc: string; files: string[]; functions: string[]; spans: UplcSpan[] }
export function parseUplcMap(text: string): UplcSourceMap;           // throws on malformed/wrong version
export function spansAtSource(map: UplcSourceMap, file: number, line0: number, col0: number): UplcSpan[];
export function innermostSpanAt(map: UplcSourceMap, offset: number): UplcSpan | undefined;
export function spansForFunction(map: UplcSourceMap, fn: number): UplcSpan[];
```

- [ ] **Step 1: Create the branch**

```bash
cd /Users/nau/projects/lantr/scalus-vscode-extension && git checkout -b feature/uplc-source-view
```

- [ ] **Step 2: Write failing tests in `test/smoke.ts`**

Follow the file's existing pattern (plain `assert` helpers, no vscode import). Append:

```ts
// --- uplcMap ---
import {
  parseUplcMap,
  spansAtSource,
  innermostSpanAt,
  spansForFunction,
} from "../src/uplcMap";

{
  const map = parseUplcMap(
    JSON.stringify({
      schemaVersion: 1,
      uplc: "(program 1.1.0 [(builtin addInteger) (con integer 1) (con integer 2)])",
      files: ["/src/Foo.scala"],
      functions: ["validate"],
      spans: [
        { s: 15, e: 74, n: 3, file: 0, sl: 10, sc: 0, el: 12, ec: 5 },
        { s: 16, e: 36, n: 0, file: 0, sl: 10, sc: 2, el: 10, ec: 7, fn: 0 },
      ],
    })
  );
  assertEq(map.spans.length, 2, "uplcMap parses spans");

  // spansAtSource: line 10 col 3 hits both (outer covers 10..12, inner covers 10:2-10:7)
  assertEq(spansAtSource(map, 0, 10, 3).length, 2, "spansAtSource hits nested spans");
  // line 11 hits only the outer span
  assertEq(spansAtSource(map, 0, 11, 0).length, 1, "spansAtSource line containment");
  // line 10 col 1 is before the inner span's start column
  assertEq(spansAtSource(map, 0, 10, 1).length, 1, "spansAtSource column boundary");

  // innermost: offset 20 is inside both spans; the smaller one wins
  assertEq(innermostSpanAt(map, 20)?.n, 0, "innermostSpanAt picks smallest");
  assertEq(innermostSpanAt(map, 40)?.n, 3, "innermostSpanAt falls back to outer");
  assertEq(innermostSpanAt(map, 0), undefined, "innermostSpanAt outside all spans");

  assertEq(spansForFunction(map, 0).length, 1, "spansForFunction");

  let threw = false;
  try {
    parseUplcMap(JSON.stringify({ schemaVersion: 99, uplc: "", files: [], functions: [], spans: [] }));
  } catch {
    threw = true;
  }
  assertEq(threw, true, "parseUplcMap rejects wrong schemaVersion");
}
```

Adapt `assertEq` to whatever helper `test/smoke.ts` actually defines (read it first).

- [ ] **Step 3: Run, verify failure**

```bash
npm test
```
Expected: compile error (module missing).

- [ ] **Step 4: Implement `src/uplcMap.ts`**

```ts
// TypeScript mirror of <key>.uplc.json written by Scalus UplcSourceMapRenderer
// (scalus-core/.../uplc/eval/UplcSourceMap.scala). Offsets s/e are character offsets
// into `uplc` (end-exclusive); sl/sc/el/ec are 0-BASED source lines/columns (unlike
// profile.json, which is 1-based); n is the node's post-order index in the term tree.

export const UPLC_MAP_SCHEMA_VERSION = 1;

export interface UplcSpan {
  s: number;
  e: number;
  n: number;
  file: number;
  sl: number;
  sc: number;
  el: number;
  ec: number;
  fn?: number;
}

export interface UplcSourceMap {
  schemaVersion: number;
  uplc: string;
  files: string[];
  functions: string[];
  spans: UplcSpan[];
}

/** Parse and validate a .uplc.json document. Throws on malformed input or an
 *  unsupported schema version. */
export function parseUplcMap(text: string): UplcSourceMap {
  const raw = JSON.parse(text) as Partial<UplcSourceMap>;
  if (!raw || typeof raw.uplc !== "string" || !Array.isArray(raw.spans)) {
    throw new Error("not a Scalus UPLC source map");
  }
  if (raw.schemaVersion !== UPLC_MAP_SCHEMA_VERSION) {
    throw new Error(
      `unsupported UPLC map schemaVersion ${raw.schemaVersion} (expected ${UPLC_MAP_SCHEMA_VERSION})`
    );
  }
  return {
    schemaVersion: raw.schemaVersion,
    uplc: raw.uplc,
    files: raw.files ?? [],
    functions: raw.functions ?? [],
    spans: raw.spans.filter(validSpan),
  };
}

function validSpan(sp: unknown): sp is UplcSpan {
  const x = sp as UplcSpan;
  return (
    !!x &&
    typeof x.s === "number" &&
    typeof x.e === "number" &&
    typeof x.file === "number" &&
    typeof x.sl === "number"
  );
}

/** True when the 0-based source position (line0, col0) falls inside the span's range. */
function containsSource(sp: UplcSpan, line0: number, col0: number): boolean {
  if (line0 < sp.sl || line0 > sp.el) {
    return false;
  }
  if (line0 === sp.sl && col0 < sp.sc) {
    return false;
  }
  if (line0 === sp.el && col0 > sp.ec) {
    return false;
  }
  return true;
}

/** All spans of `file` whose source range contains the cursor. */
export function spansAtSource(
  map: UplcSourceMap,
  file: number,
  line0: number,
  col0: number
): UplcSpan[] {
  return map.spans.filter((sp) => sp.file === file && containsSource(sp, line0, col0));
}

/** The smallest span containing the UPLC text offset, or undefined. */
export function innermostSpanAt(map: UplcSourceMap, offset: number): UplcSpan | undefined {
  let best: UplcSpan | undefined;
  for (const sp of map.spans) {
    if (offset >= sp.s && offset < sp.e) {
      if (!best || sp.e - sp.s < best.e - best.s) {
        best = sp;
      }
    }
  }
  return best;
}

/** Every span attributed to the given function index. */
export function spansForFunction(map: UplcSourceMap, fn: number): UplcSpan[] {
  return map.spans.filter((sp) => sp.fn === fn);
}
```

- [ ] **Step 5: Run tests + typecheck, verify PASS**

```bash
npm run typecheck && npm test
```

- [ ] **Step 6: Commit**

```bash
git add src/uplcMap.ts test/smoke.ts && git commit -m "feat: uplcMap model and span queries for the UPLC source view"
```

---

### Task 7: UPLC virtual document + Show UPLC command

**Files:**
- Modify: `src/profileStore.ts` (add `uplcUri` getter after `htmlUri`, ~line 60)
- Create: `src/uplcView.ts`
- Modify: `src/extension.ts` (wire UplcView + command)
- Modify: `package.json` (command contribution)

**Interfaces:**
- Consumes: `parseUplcMap`, `UplcSourceMap` (Task 6); `ProfileStore` (`run`, `onDidChange`, existing getters); `runFile`/`resolveManifestFile` from `src/manifest.ts`.
- Produces: `class UplcView implements vscode.Disposable` with `constructor()`, `setSource(uri: vscode.Uri | undefined): Promise<void>` (loads + parses the artifact, refreshes an open document), `show(): Promise<void>` (opens the virtual doc beside), `get map(): UplcSourceMap | undefined`, `readonly docUri: vscode.Uri`. `ProfileStore.uplcUri: vscode.Uri | undefined`. Command `scalusProfile.showUplc` ("Scalus Profile: Show Compiled UPLC").

- [ ] **Step 1: Add `uplcUri` to ProfileStore** (mirror of `htmlUri`, lines 52-60):

```ts
  /** The selected run's UPLC source map, when the manifest lists one. */
  get uplcUri(): vscode.Uri | undefined {
    if (!this._run || !this._manifestDir) {
      return undefined;
    }
    const uplc = runFile(this._run, "uplc");
    return uplc
      ? vscode.Uri.file(resolveManifestFile(this._manifestDir.fsPath, uplc))
      : undefined;
  }
```

- [ ] **Step 2: Implement `src/uplcView.ts`**

```ts
import * as vscode from "vscode";
import { UplcSourceMap, parseUplcMap } from "./uplcMap";

export const UPLC_SCHEME = "scalus-uplc";

/** Read-only virtual document showing the compiled UPLC of the selected profile run,
 *  backed by the run's .uplc.json source map. */
export class UplcView implements vscode.Disposable {
  readonly docUri = vscode.Uri.parse(`${UPLC_SCHEME}:/compiled.uplc`);
  private _map: UplcSourceMap | undefined;
  private readonly onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
  private readonly providerReg: vscode.Disposable;

  constructor() {
    this.providerReg = vscode.workspace.registerTextDocumentContentProvider(UPLC_SCHEME, {
      onDidChange: this.onDidChangeEmitter.event,
      provideTextDocumentContent: () => this._map?.uplc ?? "",
    });
  }

  get map(): UplcSourceMap | undefined {
    return this._map;
  }

  /** Load (or clear) the source map from the run's .uplc.json. */
  async setSource(uri: vscode.Uri | undefined): Promise<void> {
    if (!uri) {
      this._map = undefined;
    } else {
      try {
        const bytes = await vscode.workspace.fs.readFile(uri);
        this._map = parseUplcMap(Buffer.from(bytes).toString("utf8"));
      } catch (e) {
        this._map = undefined;
        console.warn(`Scalus Profile: ignoring UPLC map ${uri.fsPath}: ${(e as Error).message}`);
      }
    }
    this.onDidChangeEmitter.fire(this.docUri);
  }

  /** Open the UPLC document beside the active editor. */
  async show(): Promise<void> {
    if (!this._map) {
      vscode.window.showInformationMessage(
        "Scalus Profile: no UPLC map for this run. Re-run a profiled test with a Scalus version that emits it (format \"uplc\" in profile-manifest.json)."
      );
      return;
    }
    const doc = await vscode.workspace.openTextDocument(this.docUri);
    await vscode.window.showTextDocument(doc, {
      viewColumn: vscode.ViewColumn.Beside,
      preserveFocus: true,
      preview: false,
    });
  }

  dispose(): void {
    this.providerReg.dispose();
    this.onDidChangeEmitter.dispose();
  }
}
```

- [ ] **Step 3: Wire into `src/extension.ts`**

- `const uplcView = new UplcView();` after the other components; push onto `context.subscriptions`.
- Inside `refreshViews()` add: `void uplcView.setSource(store.uplcUri);`
- Register the command with the others:

```ts
    vscode.commands.registerCommand("scalusProfile.showUplc", () => uplcView.show()),
```

- [ ] **Step 4: package.json** – add to `contributes.commands`:

```json
{ "command": "scalusProfile.showUplc", "title": "Show Compiled UPLC", "category": "Scalus Profile" }
```

- [ ] **Step 5: Verify**

```bash
npm run typecheck && npm test && npm run compile
```
Expected: all pass.

- [ ] **Step 6: Commit**

```bash
git add -A && git commit -m "feat: Show Compiled UPLC command with scalus-uplc virtual document"
```

---

### Task 8: Bidirectional cursor sync

**Files:**
- Modify: `src/uplcView.ts`
- Modify: `src/extension.ts`

**Interfaces:**
- Consumes: `spansAtSource`, `innermostSpanAt` (Task 6), `bestMatchingFile` from `src/pathMatch.ts` (existing: `bestMatchingFile(candidates: string[], editorPath: string): string | undefined`).
- Produces: `UplcView.onSelectionChanged(e: vscode.TextEditorSelectionChangeEvent): void` – the single sync entry point wired to `vscode.window.onDidChangeTextEditorSelection`.

- [ ] **Step 1: Add decorations and sync to `UplcView`**

Add to the class:

```ts
  private readonly highlight = vscode.window.createTextEditorDecorationType({
    backgroundColor: new vscode.ThemeColor("editor.findMatchHighlightBackground"),
  });

  private uplcEditor(): vscode.TextEditor | undefined {
    return vscode.window.visibleTextEditors.find((e) => e.document.uri.scheme === UPLC_SCHEME);
  }

  /** Sync highlights on any selection change: Scala -> UPLC or UPLC -> Scala. */
  onSelectionChanged(e: vscode.TextEditorSelectionChangeEvent): void {
    const map = this._map;
    if (!map) {
      return;
    }
    if (e.textEditor.document.uri.scheme === UPLC_SCHEME) {
      void this.syncToSource(e.textEditor, map);
    } else if (e.textEditor.document.languageId === "scala") {
      this.syncToUplc(e.textEditor, map);
    }
  }

  /** Scala cursor -> highlight matching UPLC spans. */
  private syncToUplc(editor: vscode.TextEditor, map: UplcSourceMap): void {
    const target = this.uplcEditor();
    if (!target) {
      return;
    }
    const match = bestMatchingFile(map.files, editor.document.uri.fsPath);
    const file = match ? map.files.indexOf(match) : -1;
    if (file < 0) {
      target.setDecorations(this.highlight, []);
      return;
    }
    const pos = editor.selection.active;
    const spans = spansAtSource(map, file, pos.line, pos.character);
    const ranges = spans.map(
      (sp) =>
        new vscode.Range(
          target.document.positionAt(sp.s),
          target.document.positionAt(sp.e)
        )
    );
    target.setDecorations(this.highlight, ranges);
    if (ranges.length > 0) {
      // Reveal the tightest (innermost) match.
      const tightest = ranges.reduce((a, b) =>
        b.end.character - b.start.character + (b.end.line - b.start.line) * 1e6 <
        a.end.character - a.start.character + (a.end.line - a.start.line) * 1e6
          ? b
          : a
      );
      target.revealRange(tightest, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    }
  }

  /** UPLC cursor -> highlight the originating Scala range. */
  private async syncToSource(editor: vscode.TextEditor, map: UplcSourceMap): Promise<void> {
    const offset = editor.document.offsetAt(editor.selection.active);
    const span = innermostSpanAt(map, offset);
    if (!span) {
      return;
    }
    const mapped = map.files[span.file];
    const source = vscode.window.visibleTextEditors.find(
      (ed) =>
        ed.document.languageId === "scala" &&
        bestMatchingFile(map.files, ed.document.uri.fsPath) === mapped
    );
    if (!source) {
      return; // v1: only highlight already-visible Scala editors
    }
    const last = source.document.lineCount - 1;
    const clamp = (line: number) => Math.max(0, Math.min(line, last));
    const range = new vscode.Range(clamp(span.sl), span.sc, clamp(span.el), span.ec);
    source.setDecorations(this.highlight, [range]);
    source.revealRange(range, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
  }
```

Imports to add in `uplcView.ts`: `spansAtSource`, `innermostSpanAt` from `./uplcMap`; `bestMatchingFile` from `./pathMatch`. Dispose the decoration type in `dispose()`: `this.highlight.dispose();`.

- [ ] **Step 2: Wire the listener in `extension.ts`**

```ts
  context.subscriptions.push(
    vscode.window.onDidChangeTextEditorSelection((e) => uplcView.onSelectionChanged(e))
  );
```

- [ ] **Step 3: Verify + manual smoke**

```bash
npm run typecheck && npm test && npm run compile
```
Then a quick manual test in the Extension Development Host (F5) against a scalus checkout with a generated artifact – see Task 10 Step 3 for how to generate one. Verify: cursor in the Scala file highlights UPLC; cursor in UPLC highlights Scala.

- [ ] **Step 4: Commit**

```bash
git add -A && git commit -m "feat: bidirectional cursor sync between Scala source and compiled UPLC"
```

---

### Task 9: Function-level highlight command

**Files:**
- Modify: `src/uplcView.ts`
- Modify: `src/extension.ts`, `package.json`

**Interfaces:**
- Consumes: `spansForFunction`, `spansAtSource` (Task 6).
- Produces: command `scalusProfile.showUplcForFunction` ("Show Compiled UPLC for Function"): from the cursor (Scala or UPLC editor), resolves the function of the span under the cursor and highlights ALL its spans in the UPLC editor.

- [ ] **Step 1: Add to `UplcView`:**

```ts
  /** Highlight every UPLC span belonging to the function under the cursor. */
  async showFunction(): Promise<void> {
    const map = this._map;
    if (!map) {
      await this.show();
      return;
    }
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      return;
    }
    let fn: number | undefined;
    if (editor.document.uri.scheme === UPLC_SCHEME) {
      fn = innermostSpanAt(map, editor.document.offsetAt(editor.selection.active))?.fn;
    } else {
      const match = bestMatchingFile(map.files, editor.document.uri.fsPath);
      const file = match ? map.files.indexOf(match) : -1;
      const pos = editor.selection.active;
      const withFn = file < 0 ? [] : spansAtSource(map, file, pos.line, pos.character)
        .filter((sp) => sp.fn !== undefined)
        .sort((a, b) => (a.el - a.sl) - (b.el - b.sl)); // innermost (smallest) first
      fn = withFn[0]?.fn;
    }
    if (fn === undefined) {
      vscode.window.showInformationMessage("Scalus Profile: no function found at the cursor.");
      return;
    }
    await this.show();
    const target = this.uplcEditor();
    if (!target) {
      return;
    }
    const ranges = spansForFunction(map, fn).map(
      (sp) => new vscode.Range(target.document.positionAt(sp.s), target.document.positionAt(sp.e))
    );
    target.setDecorations(this.highlight, ranges);
    if (ranges.length > 0) {
      target.revealRange(ranges[0], vscode.TextEditorRevealType.InCenterIfOutsideViewport);
    }
    vscode.window.setStatusBarMessage(
      `UPLC: ${ranges.length} region(s) from ${map.functions[fn]}`,
      5000
    );
  }
```

- [ ] **Step 2: Register command** in `extension.ts`:

```ts
    vscode.commands.registerCommand("scalusProfile.showUplcForFunction", () =>
      uplcView.showFunction()
    ),
```

and in `package.json` `contributes.commands`:

```json
{ "command": "scalusProfile.showUplcForFunction", "title": "Show Compiled UPLC for Function", "category": "Scalus Profile" }
```

- [ ] **Step 3: Verify and commit**

```bash
npm run typecheck && npm test && npm run compile
git add -A && git commit -m "feat: highlight all compiled UPLC of the function under the cursor"
```

---

### Task 10: UPLC syntax grammar, docs, end-to-end check

**Files:**
- Create: `syntaxes/uplc.tmLanguage.json`
- Modify: `package.json` (`contributes.languages`, `contributes.grammars`), `src/uplcView.ts` (set language), `README.md`, `CHANGELOG.md`

- [ ] **Step 1: Grammar + contributions**

`syntaxes/uplc.tmLanguage.json`:

```json
{
  "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
  "name": "Untyped Plutus Core",
  "scopeName": "source.uplc",
  "patterns": [
    { "match": "\\b(program|lam|delay|force|error|constr|case|con|builtin)\\b", "name": "keyword.control.uplc" },
    { "match": "(?<=\\(builtin\\s)[A-Za-z0-9_']+", "name": "support.function.uplc" },
    { "match": "(?<=\\(con\\s)[A-Za-z]+(\\s*\\([^)]*\\))?", "name": "storage.type.uplc" },
    { "match": "-?\\b\\d+\\b", "name": "constant.numeric.uplc" },
    { "match": "#[0-9a-fA-F]*", "name": "constant.other.uplc" },
    { "match": "\\b(True|False)\\b", "name": "constant.language.uplc" },
    { "match": "\"(\\\\.|[^\"])*\"", "name": "string.quoted.double.uplc" },
    { "match": "[\\[\\]()]", "name": "punctuation.section.uplc" }
  ]
}
```

`package.json`:

```json
  "contributes": {
    "languages": [
      { "id": "uplc", "aliases": ["Untyped Plutus Core", "UPLC"], "extensions": [".uplc"] }
    ],
    "grammars": [
      { "language": "uplc", "scopeName": "source.uplc", "path": "./syntaxes/uplc.tmLanguage.json" }
    ]
  }
```

(merge into the existing `contributes` object). In `UplcView.show()`, after `openTextDocument`, set the language:

```ts
    await vscode.languages.setTextDocumentLanguage(doc, "uplc");
```

- [ ] **Step 2: README + CHANGELOG**

README: add a "Compiled UPLC view" section documenting the two commands, the sync behavior, and the requirement (a Scalus version that writes `format: "uplc"` into `profile-manifest.json`, profile level `full`). CHANGELOG: add an Unreleased entry.

- [ ] **Step 3: End-to-end verification (both repos)**

In the scalus repo (branch `feature/uplc-source-view`), generate a real artifact by running any profiled example test, e.g.:

```bash
cd /Users/nau/projects/lantr/scalus
SCALUS_PROFILE=full sbtn "scalusExamplesJVM/testOnly *HtlcValidator*"
ls scalus-examples/jvm/target/scalus/ | grep uplc
```

(If that suite does not produce profile reports, grep for `runWithProfileReport` usages in test sources and run one of those suites instead.) Expected: a `*.uplc.json` next to the profile files, and `"uplc"` inside `profile-manifest.json`.

Then launch the Extension Development Host (F5) on the scalus workspace and verify the full flow: Show Compiled UPLC, both sync directions, function highlight, syntax colors.

- [ ] **Step 4: Final verify + commit**

```bash
npm run typecheck && npm test && npm run compile
git add -A && git commit -m "feat: uplc grammar, docs for the compiled UPLC view"
```

---

## Completion

Both branches stay unmerged; report back with:
1. scalus branch: commits + `sbtn quick` and `sbtn mima` results.
2. extension branch: commits + `npm test` results + what was manually verified end-to-end.
3. Any deviations from this plan (annotate which task and why).
