# Mutual Recursion Support Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make top-level mutually recursive functions in `@Compile` objects compile and run on all three lowering backends, per `docs/superpowers/specs/2026-08-04-mutual-recursion-design.md`.

**Architecture:** The linker groups mutually recursive top-level defs (SCCs) into multi-binding rec `SIR.Let`s. A new SIR pass `MutualRecursionElimination` rewrites each group into nested single-binding lets (peers-as-params) at the entry of both lowering backends, so the existing T2 self-application encoding does the rest. `PrettyPrinter` learns to render multi-binding lets. The plugin reports a clear error for local (in-block) mutual recursion, which stays unsupported.

**Tech Stack:** Scala 3.3.8, sbt (`sbtn`), ScalaTest. Modules: `scalus-core` (shared), `scalus-plugin`.

## Global Constraints

- Commit directly on `master` (no branches). Run `sbtn scalafmtAll` before every commit; CI runs `scalafmtCheckAll`.
- Never add a `Co-Authored-By: Claude` trailer to commits.
- MiMa: additions only, zero new filters. Verify with `sbtn mima`.
- Scala style per `CLAUDE.md`: `{}` for top-level defs, indentation syntax for small `if`/`match`, `then`/`do` keywords.
- Imports, not fully-qualified names.
- Test compile+run cycle: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly <ClassName>" 2>&1 | grep -v '^\[warn\]' | tail -60`.
- The probe file `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/MutualRecursionProbeTest.scala` exists uncommitted; Task 3 replaces it.

---

### Task 1: PrettyPrinter multi-binding lets

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/PrettyPrinter.scala:189-209`
- Test: create `scalus-core/shared/src/test/scala/scalus/compiler/sir/MultiBindingLetPrettyPrinterTest.scala`

**Interfaces:**
- Consumes: `SIR.Let(bindings: List[Binding], body, flags: LetFlags, anns)`, `Binding(name, tp, value)`, `flags.isRec`.
- Produces: `sir.show` renders any multi-binding `Let` without throwing. Rec groups render members joined by `and`; non-rec bindings stack under one `let`. The `sys.error("Multiple bindings not supported")` case is gone. Task 2/3 tests call `sir.show` on groups.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Constant

class MultiBindingLetPrettyPrinterTest extends AnyFunSuite {
    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val intToInt = SIRType.Fun(intTp, intTp)
    private def intConst(v: Int) = SIR.Const(Constant.Integer(v), intTp, ann)
    private def vr(name: String, tp: SIRType = intTp) = SIR.Var(name, tp, ann)

    test("non-rec multi-binding let renders without error") {
        val let = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          vr("a"),
          SIR.LetFlags.None,
          ann
        )
        val rendered = let.show
        assert(rendered.contains("let"), rendered)
        assert(rendered.contains("a: Int"), rendered)
        assert(rendered.contains("b: Int"), rendered)
        assert(rendered.contains("in"), rendered)
    }

    test("rec multi-binding let renders as fun group joined by 'and'") {
        def lam(body: SIR) = SIR.LamAbs(vr("n"), body, List.empty, ann)
        val let = SIR.Let(
          List(
            Binding("f", intToInt, lam(vr("g", intToInt))),
            Binding("g", intToInt, lam(vr("f", intToInt)))
          ),
          vr("f", intToInt),
          SIR.LetFlags.Recursivity,
          ann
        )
        val rendered = let.show
        assert(rendered.contains("fun f"), rendered)
        assert(rendered.contains("and"), rendered)
        assert(rendered.contains("fun g"), rendered)
        assert(rendered.contains("in"), rendered)
    }
}
```

Note: `SIR.LamAbs(vr("g", intToInt))` bodies are type-nonsense but the printer never type-checks; this is fine for a printer test.

- [ ] **Step 2: Run test to verify it fails**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.MultiBindingLetPrettyPrinterTest" 2>&1 | tail -30`
Expected: FAIL with `RuntimeException: Multiple bindings not supported` (from `PrettyPrinter.scala:209`).

- [ ] **Step 3: Implement the rendering**

In `PrettyPrinter.pretty(sir, style)`, add a local helper after the `typedName` def (around line 125):

```scala
        def prettyFunBinding(name: String, tp: SIRType, rhs: SIR, eqText: Doc): Doc =
            val (args, body1) = SirDSL.lamAbsToList(rhs)
            val prettyArgs = inParens(intercalate(text(",") + space, args.map(text)))
            val signatureLine =
                (kw("fun") & text(name) + (prettyArgs + char(':') & typ(
                  pretty(tp)
                ) & eqText).nested(2)).grouped
            (signatureLine + (line + pretty(body1, style))
                .nested(4)
                .grouped).grouped.aligned
```

Rewrite the single-binding rec case (lines 195-207) to use the helper, and replace the TODO + `sys.error` (lines 208-209) with the two multi-binding cases:

```scala
            case Let(List(Binding(name, tp, body)), inExpr, flags, anns) if flags.isRec =>
                val eqText = if flags.isLazy then text("=[lazy]") else text("=")
                prettyFunBinding(name, tp, body, eqText) / kw(
                  "in"
                ) & pretty(inExpr, style)
            case Let(Nil, _, _, _) => sys.error(s"Empty let binding: $sir")
            case Let(bindings, inExpr, flags, anns) if !flags.isRec =>
                val eqText = if flags.isLazy then text("=[lazy]") else text("=")
                val prettyBindings = stack(bindings.map { case Binding(name, tp, rhs) =>
                    (typedName(name, tp) & eqText + (line + pretty(rhs, style)).nested(2)).grouped
                })
                ((kw("let") & prettyBindings.aligned) / kw("in")).aligned / pretty(inExpr, style)
            case Let(bindings, inExpr, flags, anns) =>
                val eqText = if flags.isLazy then text("=[lazy]") else text("=")
                val funDocs = bindings.map { case Binding(name, tp, rhs) =>
                    prettyFunBinding(name, tp, rhs, eqText)
                }
                intercalate(line + kw("and") + space, funDocs) / kw(
                  "in"
                ) & pretty(inExpr, style)
```

Keep the single-binding non-rec case (lines 189-194) unchanged so existing golden output does not move.

- [ ] **Step 4: Run test to verify it passes**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.MultiBindingLetPrettyPrinterTest" 2>&1 | tail -20`
Expected: PASS (2 tests).

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/sir/PrettyPrinter.scala \
        scalus-core/shared/src/test/scala/scalus/compiler/sir/MultiBindingLetPrettyPrinterTest.scala
git commit -m "feat(sir): PrettyPrinter renders multi-binding lets

Non-rec bindings stack under one let; rec groups render as fun
definitions joined by 'and'. Removes the sys.error placeholder."
```

---

### Task 2: MutualRecursionElimination pass + backend wiring

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/compiler/sir/MutualRecursionElimination.scala`
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/SirToUplcV3Lowering.scala:28` (wrap `sir`)
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/simple/BaseSimpleLowering.scala:50-53` (wrap `sir`) and the guard comment at `:623-624`
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/Lowering.scala:587-590` (guard message only)
- Test: create `scalus-core/shared/src/test/scala/scalus/compiler/sir/MutualRecursionEliminationTest.scala`

**Interfaces:**
- Consumes: `SIR`, `AnnotatedSIR`, `SIR.Let`, `Binding(name, tp, value)`, `SIR.LetFlags` (opaque Int: `isRec`, `remove`, `Recursivity`), `RemoveRecursivity.isRecursive(name, term)`, `SIRType.Fun`.
- Produces: `object MutualRecursionElimination { def apply(sir: SIR): SIR }` in package `scalus.compiler.sir`. After it runs, no rec `Let` has 2+ bindings. Fresh binding names use suffix `$mutrec` (e.g. `isOdd$mutrec`). Task 3 relies on `apply` and on the wiring into both backends.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.Options
import scalus.compiler.sir.SirDSL.{*, given}
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

class MutualRecursionEliminationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val boolTp = SIRType.Boolean
    private val intToBool = SIRType.Fun(intTp, boolTp)
    private val intToInt = SIRType.Fun(intTp, intTp)

    private def nVar = SIR.Var("n", intTp, ann)
    private def intConst(v: Int) = SIR.Const(Constant.Integer(v), intTp, ann)
    private def boolConst(v: Boolean) = SIR.Const(Constant.Bool(v), boolTp, ann)

    /** λn. if n == 0 then base else callee(n - 1) */
    private def stepBool(callee: String, base: Boolean): SIR =
        SIR.LamAbs(
          nVar,
          SIR.IfThenElse(
            SIRBuiltins.equalsInteger $ nVar $ intConst(0),
            boolConst(base),
            SIR.Var(callee, intToBool, ann) $
                (SIRBuiltins.subtractInteger $ nVar $ intConst(1)),
            boolTp,
            ann
          ),
          List.empty,
          ann
        )

    private def evenOddGroup(body: AnnotatedSIR): AnnotatedSIR =
        SIR.Let(
          List(
            Binding("isEven", intToBool, stepBool("isOdd", base = true)),
            Binding("isOdd", intToBool, stepBool("isEven", base = false))
          ),
          body,
          SIR.LetFlags.Recursivity,
          ann
        )

    private def callIsEven(n: Int): AnnotatedSIR =
        extractAnnotated(SIR.Var("isEven", intToBool, ann) $ intConst(n))

    test("2-group: rewritten to nested single lets with $mutrec peer") {
        val out = MutualRecursionElimination(evenOddGroup(callIsEven(4)))
        out match
            case SIR.Let(List(Binding(oddP, oddTp, oddRhs)), inner, oddFlags, _) =>
                assert(oddP == "isOdd$mutrec")
                assert(!oddFlags.isRec)
                assert(oddTp == SIRType.Fun(intToBool, intToBool))
                oddRhs match
                    case SIR.LamAbs(param, _, _, _) => assert(param.name == "isEven")
                    case other                      => fail(s"expected LamAbs, got $other")
                inner match
                    case SIR.Let(List(Binding("isEven", _, _)), _, evenFlags, _) =>
                        assert(evenFlags.isRec)
                    case other => fail(s"expected isEven let, got $other")
            case other => fail(s"expected outer isOdd-mutrec let, got $other")
    }

    test("2-group: evaluates correctly on all three backends") {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            val sir = evenOddGroup(callIsEven(4))
            val uplc = sir.toUplc(using opts)()
            uplc.evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Bool(true)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    /** λn. if n == 0 then base else callee(n - 1), Int result */
    private def stepInt(callee: String, base: Int): SIR =
        SIR.LamAbs(
          nVar,
          SIR.IfThenElse(
            SIRBuiltins.equalsInteger $ nVar $ intConst(0),
            intConst(base),
            SIR.Var(callee, intToInt, ann) $
                (SIRBuiltins.subtractInteger $ nVar $ intConst(1)),
            intTp,
            ann
          ),
          List.empty,
          ann
        )

    test("3-group: a -> b -> c -> a evaluates correctly on all backends") {
        val group = SIR.Let(
          List(
            Binding("rotA", intToInt, stepInt("rotB", base = 0)),
            Binding("rotB", intToInt, stepInt("rotC", base = 1)),
            Binding("rotC", intToInt, stepInt("rotA", base = 2))
          ),
          extractAnnotated(SIR.Var("rotA", intToInt, ann) $ intConst(7)),
          SIR.LetFlags.Recursivity,
          ann
        )
        // rotA(7)->rotB(6)->rotC(5)->rotA(4)->rotB(3)->rotC(2)->rotA(1)->rotB(0) = 1
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            group.toUplc(using opts)().evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(1)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    test("group of non-lambda values is rejected") {
        val group = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          SIR.Var("a", intTp, ann),
          SIR.LetFlags.Recursivity,
          ann
        )
        val e = intercept[IllegalArgumentException] { MutualRecursionElimination(group) }
        assert(e.getMessage.contains("mutually recursive values"))
    }

    test("single-binding and non-rec lets are unchanged") {
        val singleRec = SIR.Let(
          List(Binding("f", intToInt, stepInt("f", base = 0))),
          extractAnnotated(SIR.Var("f", intToInt, ann) $ intConst(3)),
          SIR.LetFlags.Recursivity,
          ann
        )
        assert(MutualRecursionElimination(singleRec) == singleRec)
        val nonRec = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          SIR.Var("a", intTp, ann),
          SIR.LetFlags.None,
          ann
        )
        assert(MutualRecursionElimination(nonRec) == nonRec)
    }
}
```

Notes for the implementer:
- `SirDSL.$` builds `SIR.Apply` with computed types; `extractAnnotated` is from `SirDSL`.
- `sir.toUplc(using opts)()` is the extension in `scalus/package.scala`; the backend comes from `opts.targetLoweringBackend`. It works because this task also wires the pass into all backends (Step 3). Until then the V3 backend throws `LoweringException` and simple backends `sys.error`.
- If `TargetLoweringBackend.values` clashes with imports, qualify: `scalus.compiler.sir.TargetLoweringBackend.values`.

- [ ] **Step 2: Run test to verify it fails**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.MutualRecursionEliminationTest" 2>&1 | tail -30`
Expected: FAIL to compile with "Not found: MutualRecursionElimination".

- [ ] **Step 3: Implement the pass**

Create `MutualRecursionElimination.scala`:

```scala
package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

/** Eliminates multi-binding recursive `Let` groups by rewriting each group into
  * nested single-binding lets (peers-as-params), so the lowering backends only
  * ever see single self-recursion, which they encode via self-application.
  *
  * For a group `f1..fN` (binding order) it emits, outermost first:
  * {{{
  *   let fNp = λf1...f(N-1). rhsN'   // fresh name fN$mutrec
  *   ...
  *   let f2p = λf1. rhs2'
  *   let f1  = rhs1'
  *   in body'
  * }}}
  * where inside `rhs_i'` (context `i`; context 1 also covers `rhs1'` and the body):
  *   - a reference to `fj` with `j < i` stays the (param) variable `fj`;
  *   - a reference to `fj` with `j >= i` becomes `fjp E(1) ... E(j-1)` with the
  *     same rule applied to the arguments recursively.
  * Each member's rhs must be a lambda; a cyclic group of plain values is rejected.
  * See docs/superpowers/specs/2026-08-04-mutual-recursion-design.md.
  */
object MutualRecursionElimination {

    def apply(sir: SIR): SIR = transform(sir)

    private def transform(sir: SIR): SIR = sir match
        case Decl(data, term)   => Decl(data, transform(term))
        case expr: AnnotatedSIR => transformExpr(expr)

    private def transformExpr(sir: AnnotatedSIR): AnnotatedSIR = sir match
        case Let(bindings, body, flags, anns) if flags.isRec && bindings.sizeIs >= 2 =>
            val nBindings = bindings.map(b => Binding(b.name, b.tp, transform(b.value)))
            eliminate(nBindings, transformExpr(body), flags, anns)
        case Let(bindings, body, flags, anns) =>
            Let(
              bindings.map(b => Binding(b.name, b.tp, transform(b.value))),
              transformExpr(body),
              flags,
              anns
            )
        case LamAbs(param, term, tps, anns) => LamAbs(param, transform(term), tps, anns)
        case Apply(f, arg, tp, anns) =>
            Apply(transformExpr(f), transformExpr(arg), tp, anns)
        case Select(s, field, tp, anns) => Select(transform(s), field, tp, anns)
        case IfThenElse(c, t, f, tp, anns) =>
            IfThenElse(transformExpr(c), transformExpr(t), transformExpr(f), tp, anns)
        case And(a, b, anns) => And(transformExpr(a), transformExpr(b), anns)
        case Or(a, b, anns)  => Or(transformExpr(a), transformExpr(b), anns)
        case Not(a, anns)    => Not(transformExpr(a), anns)
        case Match(scrutinee, cases, tp, anns) =>
            Match(
              transformExpr(scrutinee),
              cases.map(c => c.copy(body = transform(c.body))),
              tp,
              anns
            )
        case Constr(name, data, args, tp, anns) =>
            Constr(name, data, args.map(transform), tp, anns)
        case Cast(expr, tp, anns) => Cast(transformExpr(expr), tp, anns)
        case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    private def eliminate(
        bindings: List[Binding],
        body: AnnotatedSIR,
        flags: SIR.LetFlags,
        anns: AnnotationsDecl
    ): AnnotatedSIR = {
        val n = bindings.size
        val names = bindings.map(_.name).toVector
        val tps = bindings.map(_.tp).toVector
        bindings.foreach { b =>
            b.value match
                case _: LamAbs => ()
                case other =>
                    throw new IllegalArgumentException(
                      s"mutually recursive values are not supported (only functions): " +
                          s"'${b.name}' in group ${names.mkString(", ")} at ${anns.pos.file}:${anns.pos.startLine}"
                    )
        }
        val pNames = names.map(_ + "$mutrec")
        // fip takes f1..f(i-1) as params: tps(0) -> ... -> tps(i-2) -> tps(i-1); i is 1-based
        def pTp(i: Int): SIRType =
            (0 until i - 1).foldRight(tps(i - 1))((k, acc) => SIRType.Fun(tps(k), acc))

        def applyChain(f: AnnotatedSIR, args: Seq[AnnotatedSIR], anns: AnnotationsDecl) =
            args.foldLeft(f) { (acc, arg) =>
                val resTp = acc.tp match
                    case SIRType.Fun(_, out) => out
                    case t                   => t
                Apply(acc, arg, resTp, anns)
            }

        // E(j) in context i, 1-based: the expression denoting fj where the vars
        // f1..f(i-1) (or f1 itself for i == 1) are in scope.
        def peerExpr(j: Int, i: Int, occAnns: AnnotationsDecl): AnnotatedSIR =
            if j < i || (i == 1 && j == 1) then Var(names(j - 1), tps(j - 1), occAnns)
            else
                applyChain(
                  Var(pNames(j - 1), pTp(j), occAnns),
                  (1 until j).map(k => peerExpr(k, i, occAnns)),
                  occAnns
                )

        def rewrite(sir: SIR, i: Int, shadowed: Set[String]): SIR = sir match
            case Decl(data, term)   => Decl(data, rewrite(term, i, shadowed))
            case expr: AnnotatedSIR => rewriteExpr(expr, i, shadowed)

        def rewriteExpr(sir: AnnotatedSIR, i: Int, shadowed: Set[String]): AnnotatedSIR =
            sir match
                case v @ Var(name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then peerExpr(j + 1, i, occAnns) else v
                case v @ ExternalVar(_, name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then peerExpr(j + 1, i, occAnns) else v
                case v: Var         => v
                case v: ExternalVar => v
                case Let(bs, b, fl, a) =>
                    val newShadowed = shadowed ++ bs.map(_.name)
                    // rec bindings see themselves; non-rec rhs uses the outer scope,
                    // but group names are full names that locals cannot collide with,
                    // so shadowing both sides is safe and simple
                    Let(
                      bs.map(bd => Binding(bd.name, bd.tp, rewrite(bd.value, i, newShadowed))),
                      rewriteExpr(b, i, newShadowed),
                      fl,
                      a
                    )
                case LamAbs(param, term, tps, a) =>
                    LamAbs(param, rewrite(term, i, shadowed + param.name), tps, a)
                case Apply(f, arg, tp, a) =>
                    Apply(rewriteExpr(f, i, shadowed), rewriteExpr(arg, i, shadowed), tp, a)
                case Select(s, field, tp, a) => Select(rewrite(s, i, shadowed), field, tp, a)
                case IfThenElse(c, t, f, tp, a) =>
                    IfThenElse(
                      rewriteExpr(c, i, shadowed),
                      rewriteExpr(t, i, shadowed),
                      rewriteExpr(f, i, shadowed),
                      tp,
                      a
                    )
                case And(x, y, a) => And(rewriteExpr(x, i, shadowed), rewriteExpr(y, i, shadowed), a)
                case Or(x, y, a)  => Or(rewriteExpr(x, i, shadowed), rewriteExpr(y, i, shadowed), a)
                case Not(x, a)    => Not(rewriteExpr(x, i, shadowed), a)
                case Match(scrutinee, cases, tp, a) =>
                    Match(
                      rewriteExpr(scrutinee, i, shadowed),
                      cases.map { c =>
                          val caseShadowed = c.pattern match
                              case Pattern.Constr(_, patBindings, _) => shadowed ++ patBindings
                              case _                                 => shadowed
                          c.copy(body = rewrite(c.body, i, caseShadowed))
                      },
                      tp,
                      a
                    )
                case Constr(name, data, args, tp, a) =>
                    Constr(name, data, args.map(rewrite(_, i, shadowed)), tp, a)
                case Cast(expr, tp, a) => Cast(rewriteExpr(expr, i, shadowed), tp, a)
                case _: Builtin | _: Error | _: Const => sir

        def recFlagsFor(letName: String, rhs: SIR): SIR.LetFlags =
            if RemoveRecursivity.isRecursive(letName, rhs) then flags
            else flags.remove(SIR.LetFlags.Recursivity)

        // innermost: f1 under its original name
        val rhs1 = rewrite(bindings.head.value, 1, Set.empty)
        val body1 = rewriteExpr(body, 1, Set.empty)
        val innermost =
            Let(List(Binding(names(0), tps(0), rhs1)), body1, recFlagsFor(names(0), rhs1), anns)

        // wrap with f2p .. fNp, fNp outermost
        (2 to n).foldLeft(innermost: AnnotatedSIR) { (acc, i) =>
            val rewritten = rewrite(bindings(i - 1).value, i, Set.empty)
            val paramWrapped = (0 until i - 1).foldRight(rewritten) { (k, b) =>
                LamAbs(Var(names(k), tps(k), anns), b, List.empty, anns)
            }
            val pName = pNames(i - 1)
            Let(
              List(Binding(pName, pTp(i), paramWrapped)),
              acc,
              recFlagsFor(pName, rewritten),
              anns
            )
        }
    }
}
```

Implementation notes (verify against actual code while writing):
- Check `SIR.Case` field names (`pattern`, `body`, `anns`) and `Pattern.Constr(constr, bindings, typeBindings)` - mirror how `RemoveRecursivity.removeRecursivityInExpr` matches them and adjust `c.copy(...)`/pattern access accordingly.
- `paramWrapped` must be a `SIR` acceptable as `Binding.value` - `LamAbs` is `AnnotatedSIR`, fine.
- `rewrite` on `bindings(i - 1).value` returns `SIR`; `LamAbs` body parameter accepts `SIR`.

- [ ] **Step 4: Wire into both backends**

`SirToUplcV3Lowering.scala` line 28, inside `toLoweredValue`:

```scala
        val v0 = Lowering.lowerSIR(MutualRecursionElimination(sir))
```

Check the import list of the file; `MutualRecursionElimination` is in `scalus.compiler.sir` which is already imported (`import scalus.compiler.sir.{Module, SIR, SIRType}` - add it there).

Note: `Lowering.lowerSIR` takes `AnnotatedSIR`. If `MutualRecursionElimination(sir): SIR` does not typecheck there, follow the file's existing handling of `SIR` vs `AnnotatedSIR` (see how `sir` reaches `lowerSIR` today) - the elimination preserves the `Decl`/`AnnotatedSIR` split, so the same pattern applies.

`BaseSimpleLowering.scala` `lower()` (line 50):

```scala
    def lower(): Term =
        // Apply let floating to optimize lazy let bindings
        val transformed = LetFloating(MutualRecursionElimination(sir))
        lowerInner(transformed)
```

(Check how `sir` and `LetFloating` interact - `LetFloating(...)` accepts `SIR`.)

Update the two dead-guard messages to reflect reality:

`BaseSimpleLowering.scala:623-624` - replace

```scala
                // TODO: implement mutual recursion
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
```

with

```scala
                // Multi-binding rec groups are rewritten to single-binding lets by
                // MutualRecursionElimination before lowering; this is an unreachable guard.
                sys.error(s"Unexpected multi-binding recursive let (should have been eliminated): $bindings")
```

`Lowering.scala:587-590` - replace the message in the `case _ =>` branch of the rec bindings match with the same "should have been eliminated" wording (keep position info).

- [ ] **Step 5: Run test to verify it passes**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.MutualRecursionEliminationTest" 2>&1 | tail -30`
Expected: PASS (5 tests).

- [ ] **Step 6: Run the affected suites to catch regressions**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testQuick" 2>&1 | tail -20`
Expected: PASS. The wiring is a no-op for SIR without multi-binding rec groups, so no budget drift.

- [ ] **Step 7: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/sir/MutualRecursionElimination.scala \
        scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/SirToUplcV3Lowering.scala \
        scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/simple/BaseSimpleLowering.scala \
        scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/Lowering.scala \
        scalus-core/shared/src/test/scala/scalus/compiler/sir/MutualRecursionEliminationTest.scala
git commit -m "feat(sir): MutualRecursionElimination pass

Rewrites multi-binding recursive Let groups into nested single-binding
lets (peers-as-params) at the entry of all lowering backends, so the
self-application encoding handles the recursion. One fixpoint per group,
~1-2 extra applies per cross-call."
```

---

### Task 3: Linker SCC grouping + end-to-end test

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/linking/SIRLinker.scala:66-107` (the `link` fold) plus new private helpers
- Create: `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/MutualRecursionTest.scala`
- Delete: `scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/MutualRecursionProbeTest.scala` (uncommitted probe; superseded)

**Interfaces:**
- Consumes: `MutualRecursionElimination` wiring from Task 2 (the linker's groups must lower), `PrettyPrinter` multi-binding rendering from Task 1 (`sir.show` assertion).
- Produces: `SIRLinker.link` emits one multi-binding rec `SIR.Let` per mutually recursive SCC of top-level defs; acyclic defs keep today's nesting and order exactly.

- [ ] **Step 1: Write the failing end-to-end test**

Delete the probe file, then create `MutualRecursionTest.scala`:

```scala
package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.{compile, Compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

@Compile
object MutualRecursionTestDefs {
    def isEven(n: BigInt): Boolean =
        if n == BigInt(0) then true else isOdd(n - 1)
    def isOdd(n: BigInt): Boolean =
        if n == BigInt(0) then false else isEven(n - 1)

    def rotA(n: BigInt): BigInt = if n == BigInt(0) then BigInt(0) else rotB(n - 1)
    def rotB(n: BigInt): BigInt = if n == BigInt(0) then BigInt(1) else rotC(n - 1)
    def rotC(n: BigInt): BigInt = if n == BigInt(0) then BigInt(2) else rotA(n - 1)

    /** Self-recursive AND cross-recursive member. */
    def evenSum(n: BigInt): BigInt =
        if n == BigInt(0) then BigInt(0)
        else if n == BigInt(1) then oddSkip(n)
        else n + evenSum(n - BigInt(2))
    def oddSkip(n: BigInt): BigInt = evenSum(n - 1)
}

class MutualRecursionTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val backends = List(
      TargetLoweringBackend.SirToUplcV3Lowering,
      TargetLoweringBackend.ScottEncodingLowering,
      TargetLoweringBackend.SumOfProductsLowering
    )

    private def opts(backend: TargetLoweringBackend) = Options(
      targetLoweringBackend = backend,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV
    )

    private def evalInt(sir: scalus.compiler.sir.SIR, backend: TargetLoweringBackend): BigInt =
        sir.toUplc(using opts(backend))().evaluateDebug match
            case s: Result.Success =>
                s.term match
                    case Term.Const(Constant.Integer(v)) => v
                    case other                           => fail(s"not an integer: $other")
            case f => fail(s"backend $backend failed: $f")

    private def evalBool(sir: scalus.compiler.sir.SIR, backend: TargetLoweringBackend): Boolean =
        sir.toUplc(using opts(backend))().evaluateDebug match
            case s: Result.Success =>
                s.term match
                    case Term.Const(Constant.Bool(v)) => v
                    case other                        => fail(s"not a boolean: $other")
            case f => fail(s"backend $backend failed: $f")

    test("even/odd 2-cycle works on all backends") {
        val sirTrue = compile { MutualRecursionTestDefs.isEven(BigInt(10)) }
        val sirFalse = compile { MutualRecursionTestDefs.isEven(BigInt(9)) }
        for backend <- backends do {
            assert(evalBool(sirTrue, backend), s"backend $backend")
            assert(!evalBool(sirFalse, backend), s"backend $backend")
        }
    }

    test("even/odd group renders via PrettyPrinter") {
        val sir = compile { MutualRecursionTestDefs.isEven(BigInt(2)) }
        val rendered = sir.show
        assert(rendered.contains("and"), rendered)
        assert(rendered.contains("isEven"), rendered)
        assert(rendered.contains("isOdd"), rendered)
    }

    test("3-cycle works on all backends") {
        // rotA(7): 7 steps around the cycle ends in rotB's base = 1
        val sir = compile { MutualRecursionTestDefs.rotA(BigInt(7)) }
        for backend <- backends do assert(evalInt(sir, backend) == BigInt(1), s"backend $backend")
    }

    test("self- and cross-recursive member works on all backends") {
        // evenSum(6) = 6 + 4 + 2 + 0 = 12
        val sir = compile { MutualRecursionTestDefs.evenSum(BigInt(6)) }
        for backend <- backends do assert(evalInt(sir, backend) == BigInt(12), s"backend $backend")
    }

    test("body calling only one member of the group works") {
        val sir = compile { MutualRecursionTestDefs.isOdd(BigInt(3)) }
        for backend <- backends do assert(evalBool(sir, backend), s"backend $backend")
    }

    test("V3 recursion budget stays sane") {
        val sir = compile { MutualRecursionTestDefs.isEven(BigInt(20)) }
        sir.toUplc(using opts(TargetLoweringBackend.SirToUplcV3Lowering))().evaluateDebug match
            case s: Result.Success =>
                // ~21 recursive calls; generous ceiling proves no pathological encoding.
                assert(s.budget.cpu < 50_000_000L, s.budget)
            case f => fail(s"failed: $f")
    }
}
```

Note: `evenSum` avoids `%` on purpose (keeps prelude dependencies out). Check `Result.Success` field for the budget (`s.budget`) against `SelfApplicationRecursionTest` usage and adjust.

- [ ] **Step 2: Run test to verify it fails**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.lowering.MutualRecursionTest" 2>&1 | tail -40`
Expected: FAIL with `LoweringException: External variable ... not found in the scope` (the linker still emits broken nested lets).

- [ ] **Step 3: Implement SCC grouping in the linker**

In `SIRLinker`, add two private helpers:

```scala
    /** Names of global defs referenced from `sir` (syntactic, no shadow tracking:
      * global names are dot-qualified full names that locals never collide with). */
    private def collectGlobalRefs(sir: SIR, keys: Set[String]): Set[String] = {
        val acc = mutable.Set.empty[String]
        def go(s: SIR): Unit = s match
            case SIR.Decl(_, term)              => go(term)
            case SIR.Var(name, _, _)            => if keys.contains(name) then acc += name
            case SIR.ExternalVar(_, name, _, _) => if keys.contains(name) then acc += name
            case SIR.Let(bindings, body, _, _)  => bindings.foreach(b => go(b.value)); go(body)
            case SIR.LamAbs(_, term, _, _)      => go(term)
            case SIR.Apply(f, arg, _, _)        => go(f); go(arg)
            case SIR.Select(s1, _, _, _)        => go(s1)
            case SIR.IfThenElse(c, t, f, _, _)  => go(c); go(t); go(f)
            case SIR.And(a, b, _)               => go(a); go(b)
            case SIR.Or(a, b, _)                => go(a); go(b)
            case SIR.Not(a, _)                  => go(a)
            case SIR.Match(scrutinee, cases, _, _) => go(scrutinee); cases.foreach(c => go(c.body))
            case SIR.Constr(_, _, args, _, _)   => args.foreach(go)
            case SIR.Cast(expr, _, _)           => go(expr)
            case _: SIR.Builtin | _: SIR.Error | _: SIR.Const => ()
        go(sir)
        acc.toSet
    }

    /** Tarjan strongly connected components; nodes in `nodes` order, edges by name. */
    private def stronglyConnectedComponents(
        nodes: List[String],
        edges: Map[String, Set[String]]
    ): List[List[String]] = {
        val indexOf = mutable.Map.empty[String, Int]
        val lowlink = mutable.Map.empty[String, Int]
        val onStack = mutable.Set.empty[String]
        val stack = mutable.Stack.empty[String]
        val result = mutable.ListBuffer.empty[List[String]]
        var counter = 0

        def strongConnect(v: String): Unit = {
            indexOf(v) = counter
            lowlink(v) = counter
            counter += 1
            stack.push(v)
            onStack += v
            for w <- edges.getOrElse(v, Set.empty) do
                if !indexOf.contains(w) then
                    strongConnect(w)
                    lowlink(v) = math.min(lowlink(v), lowlink(w))
                else if onStack(w) then lowlink(v) = math.min(lowlink(v), indexOf(w))
            if lowlink(v) == indexOf(v) then
                val component = mutable.ListBuffer.empty[String]
                var w = ""
                while {
                    w = stack.pop()
                    onStack -= w
                    component += w
                    w != v
                } do ()
                result += component.toList
        }
        nodes.foreach(v => if !indexOf.contains(v) then strongConnect(v))
        result.toList
    }
```

Rewrite the fold in `link` (lines 72-102). Replace the `globalDefs.values.foldRight(processed)` block with:

```scala
        // Preserve today's behavior for the error path: a def still in Linking
        // state means a linking failure; report and degrade like before.
        val states = globalDefs.values.toList
        val stillLinking = states.exists {
            case LinkingDefState.Linking => true
            case _                       => false
        }
        val full: SIR =
            if stillLinking then
                val message = s"Linking in progress at end of linking"
                error(message, pos, SIR.Error(message, AnnotationsDecl.empty.copy(pos = pos)))
            else {
                val linked = states.collect { case LinkingDefState.Linked(b) => b }
                val keys = linked.map(_.name).toSet
                val edges: Map[String, Set[String]] =
                    linked.map(b => b.name -> (collectGlobalRefs(b.body, keys) - b.name)).toMap
                val sccs = stronglyConnectedComponents(linked.map(_.name), edges)
                val groupOf: Map[String, List[String]] =
                    sccs.filter(_.sizeIs >= 2).flatMap(g => g.map(_ -> g)).toMap
                val byName = linked.map(b => b.name -> b).toMap
                // Completion (insertion) order, merging each cyclic group into
                // the slot of its earliest member.
                val emitted = mutable.Set.empty[String]
                val slots = mutable.ListBuffer.empty[List[SIRLinkedBinding]]
                for b <- linked do
                    if !emitted.contains(b.name) then
                        groupOf.get(b.name) match
                            case Some(group) =>
                                val members = linked.filter(m => group.contains(m.name))
                                members.foreach(m => emitted += m.name)
                                slots += members
                            case None =>
                                emitted += b.name
                                slots += List(b)
                def asAnnotated(acc: SIR, name: String): AnnotatedSIR = acc match
                    case annssir: AnnotatedSIR => annssir
                    case _ =>
                        val msg = s"Unexpected Decl. In binding $name in SIRLinker.link"
                        error(msg, pos, SIR.Error(msg, AnnotationsDecl.empty.copy(pos = pos)))
                slots.toList.foldRight(processed) {
                    case (List(b), acc) =>
                        SIR.Let(
                          List(Binding(b.name, b.declaredTp.getOrElse(b.body.tp), b.body)),
                          asAnnotated(acc, b.name),
                          b.flags,
                          AnnotationsDecl.empty.copy(pos = pos)
                        )
                    case (group, acc) =>
                        SIR.Let(
                          group.map(b =>
                              Binding(b.name, b.declaredTp.getOrElse(b.body.tp), b.body)
                          ),
                          asAnnotated(acc, group.head.name),
                          SIR.LetFlags.Recursivity,
                          AnnotationsDecl.empty.copy(pos = pos)
                        )
                }
            }
```

Implementation notes:
- Keep the existing `dataDecls` fold after `full` unchanged.
- The old code reported "Linking in progress for ..." per-state; the new single check may need the state name - keep the message informative but the exact wording is free.
- Single-binding slots must produce *byte-identical* SIR to today (same `Binding`, same `b.flags`, same anns) - this is what keeps every existing budget pin and golden test green.

- [ ] **Step 4: Run the end-to-end test**

Run: `sbtn -Dsbt.supershell=false "scalusJVM/testOnly scalus.compiler.sir.lowering.MutualRecursionTest" 2>&1 | tail -40`
Expected: PASS (6 tests).

- [ ] **Step 5: Run the full JVM suite (linker touches everything)**

Run: `sbtn -Dsbt.supershell=false "jvm/test" 2>&1 | tail -30`
Expected: PASS. If any budget-pinned test fails, the single-binding path is NOT byte-identical - fix the linker (do not re-pin budgets; the design requires unchanged acyclic output).

- [ ] **Step 6: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/sir/linking/SIRLinker.scala \
        scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/MutualRecursionTest.scala
git rm --cached -q scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/MutualRecursionProbeTest.scala 2>/dev/null || true
git commit -m "feat(sir): mutually recursive top-level defs

SIRLinker groups strongly connected top-level defs into multi-binding
recursive Lets (Tarjan SCC over linked defs); MutualRecursionElimination
lowers them through the self-application encoding on all backends.
Acyclic defs keep byte-identical output."
```

(If the probe file was never `git add`ed, just `rm` it.)

---

### Task 4: Plugin error for local mutual recursion

**Files:**
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/CompilationError.scala` (new error case)
- Modify: `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala:1192-1205` (the `case _ =>` fallback in `compileIdentOrQualifiedSelect`)

**Interfaces:**
- Consumes: `CompilationError` trait (`message`, `srcPos`), `error(error: CompilationError, defaultValue)` helper (`SIRCompiler.scala:1145`, reports + throws).
- Produces: compiling a forward reference to a local def fails with a clear message instead of "Bad symbolic reference ... Module `..._$_$sir` not found".

- [ ] **Step 1: Add the error case**

In `CompilationError.scala`, after `ExpressionNotSupported`:

```scala
case class LocalMutualRecursionNotSupported(name: String, srcPos: SrcPos)
    extends CompilationError {
    def message: String =
        s"""Forward reference to local definition '$name'.
           |Local mutual recursion is not supported in compile blocks.
           |Move the mutually recursive functions to a top-level object annotated with @Compile.""".stripMargin
}
```

- [ ] **Step 2: Detect the forward reference**

In `compileIdentOrQualifiedSelect` (`SIRCompiler.scala`), the `case _ =>` fallback of the `widenedDealias match` (around line 1203) currently returns
`(e.symbol.owner.fullName.toString, e.symbol.fullName.toString)`. A symbol owned by a *term* (a method or anonymous function, not a module class) that is not in the local env is a forward reference to a local def. Replace with:

```scala
                    case _ =>
                        if e.symbol.owner.isTerm then
                            error(LocalMutualRecursionNotSupported(name, e.srcPos), ("", ""))
                        else (e.symbol.owner.fullName.toString, e.symbol.fullName.toString)
```

Verify the exact owner predicate against dotty's API in this codebase (`isTerm` on `Symbol`; if unavailable, use `!e.symbol.owner.isClass`). The full test suite (Step 4) confirms no legitimate reference path regresses.

- [ ] **Step 3: Verify manually (no negative-test infra exists for plugin errors)**

Temporarily add to any scalus-core test file (do NOT commit):

```scala
    val bad = compile {
        def isEven(n: BigInt): Boolean = if n == BigInt(0) then true else isOdd(n - 1)
        def isOdd(n: BigInt): Boolean = if n == BigInt(0) then false else isEven(n - 1)
        isEven(BigInt(4))
    }
```

Run: `sbtn -Dsbt.supershell=false "scalusJVM/Test/compile" 2>&1 | tail -20`
Expected: a single clear error containing "Local mutual recursion is not supported in compile blocks" (plus the RuntimeException from the `error` helper). No "Bad symbolic reference". Then remove the scratch code and recompile.

- [ ] **Step 4: Run the full suite to catch false positives**

Run: `sbtn -Dsbt.supershell=false "jvm/test" 2>&1 | tail -20`
Expected: PASS - no legitimate identifier path hits the new error.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-plugin/src/main/scala/scalus/compiler/plugin/CompilationError.scala \
        scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala
git commit -m "feat(plugin): clear error for local mutual recursion

Forward references to local defs in compile blocks now report
'local mutual recursion is not supported; move the functions to a
@Compile object' instead of a phantom-module Bad symbolic reference."
```

---

### Task 5: Docs, plan-doc correction, MiMa, memory

**Files:**
- Modify: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (T2 section, lines ~283-304; section 4.7)
- Modify: `~/.claude/projects/-Users-nau-projects-lantr-scalus/memory/scalus-t2-recursion-encoding.md` (+ its `MEMORY.md` index line)

**Interfaces:**
- Consumes: everything landed in Tasks 1-4.
- Produces: accurate docs; verified MiMa/CI state.

- [ ] **Step 1: Correct the plan document**

In `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` T2:
- Delete the sentence "Keep Z only where needed (e.g. polymorphic recursion corner cases)." - verified false: no Z fallback exists or is needed; UPLC is untyped, self-application covers polymorphic recursion.
- Replace the mutual-recursion sentence with a done-note: mutual recursion is supported since this change - `SIRLinker` groups SCCs into multi-binding rec Lets and `MutualRecursionElimination` rewrites them to nested single lets (peers-as-params, one fixpoint per group), lowered by the self-application encoding on all backends. Local (in-block) mutual recursion reports a clear error.
- Mark T2 as done (self-application landed earlier; mutual recursion landed now), and drop the stale `Lowering.scala:588-591` / `BaseSimpleLowering.scala:613` line references.

- [ ] **Step 2: MiMa and full checks**

Run: `sbtn -Dsbt.supershell=false mima 2>&1 | tail -10`
Expected: PASS with zero new filters (all changes are additions or private).

Run: `sbtn -Dsbt.supershell=false quick 2>&1 | tail -10`
Expected: PASS.

- [ ] **Step 3: Update memory**

Update `scalus-t2-recursion-encoding.md`: mutual recursion (T2b-mutual) is DONE - linker SCC grouping + `MutualRecursionElimination` peers-as-params, all backends, local mutual recursion = clear plugin error. Remove the "pending" wording; update the `MEMORY.md` index line to match.

- [ ] **Step 4: Commit**

```bash
git add docs/internal/CODEGEN_IMPROVEMENT_PLAN.md
git commit -m "docs: mark T2 mutual recursion done, correct Z-combinator note

No Z fallback was ever kept - self-application covers all recursion
shapes in untyped UPLC, including polymorphic recursion."
```

- [ ] **Step 5: Rebase and push**

```bash
git pull --rebase origin master && git push origin master
```

(The user commits in parallel - always rebase before push.)
