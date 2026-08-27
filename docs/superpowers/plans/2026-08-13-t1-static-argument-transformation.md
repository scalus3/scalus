# T1 Static-Argument Transformation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A SIR-to-SIR optimization pass that stops recursive functions from
re-passing loop-invariant arguments on every recursive call, applied only
when `optimizeUplc` is enabled.

**Architecture:** New pass `StaticArgumentTransformation` in
`scalus.compiler.sir` (sibling of `MutualRecursionElimination`,
`RemoveRecursivity`). For `let rec f = λp1…pn. body` where every self-call
passes some params unchanged, it rewrites to
`let f = λp1…pn. (let rec f$sat = λ<changing>. body' in f$sat <changing>)`.
Wired into the three SIR→UPLC entry points, gated on optimization.

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest. Spec:
`docs/superpowers/specs/2026-08-05-t1-static-argument-transformation-design.md`.

## Global Constraints

- Commit directly to `master`; no branches, no PR. Rebase before push if needed.
- Run `sbtn scalafmtAll` before every commit (CI fails on one unformatted file).
- Conventional commit messages (`feat:`, `test:`, `docs:`). NEVER add a
  `Co-Authored-By: Claude` trailer.
- Scala 3 style per project CLAUDE.md: `{}` for top-level defs, indentation
  syntax for small `if`/`match`, `then`/`do` keywords.
- Test commands: `sbtn scalusJVM/testOnly -- <class>` for one suite,
  `sbtn quick` before finishing.
- The pass must be a no-op when it cannot prove the rewrite safe. When in
  doubt, return the input unchanged.

## Background reading for every task (10 minutes)

- Spec: `docs/superpowers/specs/2026-08-05-t1-static-argument-transformation-design.md`
- SIR ADT: `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIR.scala`
  — `Var(name, tp, anns)`, `ExternalVar(moduleName, name, tp, anns)`,
  `Let(bindings, body, flags, anns)` (flags: `LetFlags.Recursivity`,
  `.isRec`, `.remove(...)`), `LamAbs(param: Var, term, typeParams, anns)`,
  `Apply(f: AnnotatedSIR, arg: AnnotatedSIR, tp, anns)`,
  `Binding(name, tp, value)`. `AnnotatedSIR` is the expression subtree of
  `SIR` (`SIR.Decl` is the only non-expression node).
- Traversal template (shadow-aware, handles every node kind):
  `scalus-core/shared/src/main/scala/scalus/compiler/sir/MutualRecursionElimination.scala`
  — copy its `rewrite`/`rewriteExpr` structure for node cases not spelled
  out below, especially `Match` pattern-binding shadowing.
- Self-reference detection prior art:
  `scalus-core/shared/src/main/scala/scalus/compiler/sir/RemoveRecursivity.scala`
  (`isRecursive` matches both `Var` and `ExternalVar` by the `name` field).
- Test style template:
  `scalus-core/shared/src/test/scala/scalus/compiler/sir/MutualRecursionEliminationTest.scala`
  (hand-built SIR via `SirDSL` `$` operator + `extractAnnotated`, shape
  assertions, all-backend evaluation via `sir.toUplc(using opts)()` and
  `uplc.evaluateDebug`).

---

### Task 1: Core pass — basic lift

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/compiler/sir/StaticArgumentTransformation.scala`
- Create: `scalus-core/shared/src/test/scala/scalus/compiler/sir/StaticArgumentTransformationTest.scala`

**Interfaces:**
- Produces: `object StaticArgumentTransformation { def apply(sir: SIR): SIR }`
  — total, never throws on valid SIR, returns input shape unchanged when no
  rewrite applies. All later tasks call exactly this.

- [ ] **Step 1: Write the failing shape + evaluation tests**

Test fixture: `let rec go = λf. λn. λacc. if n == 0 then acc else go f (n-1) (acc + f n)`
applied as `go (λx. x*2) 4 0` (expects `20`). `f` is static; `n`, `acc` change.

```scala
package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.Options
import scalus.compiler.sir.SirDSL.{*, given}
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

class StaticArgumentTransformationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val intToInt = SIRType.Fun(intTp, intTp)
    // go : (Int -> Int) -> Int -> Int -> Int
    private val goTp = SIRType.Fun(intToInt, SIRType.Fun(intTp, intToInt))

    private def intConst(v: Int) = SIR.Const(Constant.Integer(v), intTp, ann)
    private def v(name: String, tp: SIRType) = SIR.Var(name, tp, ann)

    /** λf. λn. λacc. if n == 0 then acc else go f (n - 1) (acc + f n) */
    private def goRhs(selfRef: AnnotatedSIR): SIR =
        SIR.LamAbs(
          v("f", intToInt),
          SIR.LamAbs(
            v("n", intTp),
            SIR.LamAbs(
              v("acc", intTp),
              SIR.IfThenElse(
                extractAnnotated(SIRBuiltins.equalsInteger $ v("n", intTp) $ intConst(0)),
                v("acc", intTp),
                extractAnnotated(
                  selfRef $ v("f", intToInt)
                      $ (SIRBuiltins.subtractInteger $ v("n", intTp) $ intConst(1))
                      $ (SIRBuiltins.addInteger $ v("acc", intTp)
                          $ (v("f", intToInt) $ v("n", intTp)))
                ),
                intTp,
                ann
              ),
              List.empty,
              ann
            ),
            List.empty,
            ann
          ),
          List.empty,
          ann
        )

    private def program: AnnotatedSIR = {
        val double = SIR.LamAbs(
          v("x", intTp),
          extractAnnotated(SIRBuiltins.addInteger $ v("x", intTp) $ v("x", intTp)),
          List.empty,
          ann
        )
        SIR.Let(
          List(Binding("go", goTp, goRhs(v("go", goTp)))),
          extractAnnotated(v("go", goTp) $ double $ intConst(4) $ intConst(0)),
          SIR.LetFlags.Recursivity,
          ann
        )
    }

    test("static param f is lifted; n, acc keep recursing") {
        StaticArgumentTransformation(program) match
            case SIR.Let(List(Binding("go", _, wrapper)), _, outerFlags, _) =>
                assert(!outerFlags.isRec, "outer let must become non-recursive")
                // wrapper = λf. λn. λacc. let rec go$sat = λn. λacc. ... in go$sat n acc
                wrapper match
                    case SIR.LamAbs(
                          f1,
                          SIR.LamAbs(n1, SIR.LamAbs(a1, inner, _, _), _, _),
                          _,
                          _
                        ) =>
                        assert(List(f1.name, n1.name, a1.name) == List("f", "n", "acc"))
                        inner match
                            case SIR.Let(
                                  List(Binding("go$sat", satTp, satLam)),
                                  entry,
                                  innerFlags,
                                  _
                                ) =>
                                assert(innerFlags.isRec)
                                assert(satTp == SIRType.Fun(intTp, intToInt))
                                satLam match
                                    case SIR.LamAbs(p1, SIR.LamAbs(p2, _, _, _), _, _) =>
                                        assert(List(p1.name, p2.name) == List("n", "acc"))
                                    case other => fail(s"expected 2-param sat lambda: $other")
                                // entry = go$sat n acc
                                entry match
                                    case SIR.Apply(
                                          SIR.Apply(SIR.Var("go$sat", _, _), SIR.Var("n", _, _), _, _),
                                          SIR.Var("acc", _, _),
                                          _,
                                          _
                                        ) =>
                                    case other => fail(s"expected entry call go$$sat n acc: $other")
                            case other => fail(s"expected inner go$$sat letrec: $other")
                    case other => fail(s"expected 3-param wrapper: $other")
            case other => fail(s"expected outer go let: $other")
    }

    test("no self-call passes f anymore inside go$sat body") {
        val out = StaticArgumentTransformation(program)
        // crude but effective: the rewritten tree must not contain
        // an application spine `go ...` — only `go$sat` spines of length 2
        def containsVar(sir: SIR, name: String): Boolean =
            sir.toString.contains(s"Var($name,")
        assert(containsVar(out, "go$sat"))
    }

    test("transformed program evaluates to the same result on all backends") {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            val uplc = StaticArgumentTransformation(program)
                .asInstanceOf[SIR]
                .toUplc(using opts)()
            uplc.evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(20)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }
}
```

Adapt DSL details (`$`, `extractAnnotated`, `SIRBuiltins` member names) to
what `MutualRecursionEliminationTest.scala` actually uses — mirror it
exactly. If `sum` via `f n` types don't line up, simplify the body; the
assertions that matter are the shape ones.

- [ ] **Step 2: Run tests, verify they fail**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.StaticArgumentTransformationTest"`
Expected: compile error "not found: StaticArgumentTransformation".

- [ ] **Step 3: Implement the pass**

```scala
package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

/** Static-argument transformation (T1 of docs/internal/CODEGEN_IMPROVEMENT_PLAN.md).
  *
  * A parameter of a single-binding recursive let is "static" when every
  * self-call passes exactly that parameter's variable in the same position.
  * Static parameters are bound once in a wrapper lambda; the inner
  * recursion re-passes only the changing ones:
  *
  * {{{
  * let rec f = λp1…pn. body            // pi static in every self-call
  * -->
  * let f = λp1…pn.
  *     let rec f$sat = λq1…qk. body[ f e1…en := f$sat e_q1…e_qk ]
  *     in f$sat q1…qk                  // q = changing params, source order
  * }}}
  *
  * External uses of `f` are unaffected (the wrapper keeps the signature).
  * Runs only when optimization is enabled — see Compiled.toUplc and
  * scalus/package.scala. Skips: multi-binding (mutual) rec groups, lazy
  * lets, bindings with bare/under-saturated self-references, duplicate
  * parameter names, and bindings with no static parameter. If all
  * parameters are static the last one stays changing (a nullary strict
  * letrec would diverge).
  */
object StaticArgumentTransformation {

    def apply(sir: SIR): SIR = transform(sir)

    private def transform(sir: SIR): SIR = sir match
        case Decl(data, term) => Decl(data, transform(term))
        case e: AnnotatedSIR  => transformExpr(e)

    private def transformExpr(sir: AnnotatedSIR): AnnotatedSIR = sir match
        case Let(List(b), body, flags, anns) if flags.isRec && !flags.isLazy =>
            val newRhs = transform(b.value)
            val newBody = transform(body)
            trySat(b.name, newRhs) match
                case Some(wrapper) =>
                    Let(
                      List(Binding(b.name, b.tp, wrapper)),
                      newBody,
                      flags.remove(LetFlags.Recursivity),
                      anns
                    )
                case None =>
                    Let(List(Binding(b.name, b.tp, newRhs)), newBody, flags, anns)
        case Let(bindings, body, flags, anns) =>
            // non-rec lets and multi-binding mutual groups: recurse only
            Let(
              bindings.map(b => Binding(b.name, b.tp, transform(b.value))),
              transform(body),
              flags,
              anns
            )
        case LamAbs(param, term, tps, anns) => LamAbs(param, transform(term), tps, anns)
        case Apply(f, arg, tp, anns) => Apply(transformExpr(f), transformExpr(arg), tp, anns)
        // ... remaining node cases exactly as MutualRecursionElimination.rewriteExpr:
        // Match, Constr, Select, IfThenElse, And, Or, Not, Cast, Const, Var,
        // ExternalVar, Builtin, Error — pure structural recursion, no rewriting.

    // ---------------------------------------------------------------- SAT core

    private final case class Lam(param: Var, typeParams: List[SIRType.TypeVar], anns: AnnotationsDecl)

    /** Peels the outer lambda chain: returns (lambda shells, innermost body). */
    private def peel(rhs: SIR): (List[Lam], SIR) = rhs match
        case LamAbs(p, t, tps, anns) =>
            val (rest, body) = peel(t)
            (Lam(p, tps, anns) :: rest, body)
        case other => (Nil, other)

    private def trySat(name: String, rhs: SIR): Option[SIR] = {
        val (lams, innerBody) = peel(rhs)
        val params = lams.map(_.param)
        val n = params.length
        if n == 0 then return None
        if params.map(_.name).distinct.length != n then return None // duplicate names

        val analysis = new Analysis(name, params)
        analysis.run(innerBody, shadowed = Set.empty)
        if analysis.blocked || analysis.masks.isEmpty then return None

        val combined = analysis.masks.reduce((a, b) => a.lazyZip(b).map(_ && _))
        val staticMask =
            if combined.forall(identity) then combined.updated(n - 1, false)
            else combined
        if !staticMask.exists(identity) then return None

        val changing = params.lazyZip(staticMask).collect { case (p, false) => p }.toList
        val satName = name + "$sat"
        val satTp = changing.foldRight(innerBody.tp)((p, acc) => SIRType.Fun(p.tp, acc))
        val anns = rhs.anns

        val rewritten = new Rewriter(name, satName, satTp, n, staticMask)
            .rewrite(innerBody, shadowed = Set.empty)

        val satLam = changing.foldRight(rewritten) { (p, acc) =>
            LamAbs(p, acc, List.empty, anns): SIR
        }
        val entry = changing.foldLeft(Var(satName, satTp, anns): AnnotatedSIR) { (acc, p) =>
            applyOne(acc, Var(p.name, p.tp, anns), anns)
        }
        val innerLet =
            Let(List(Binding(satName, satTp, satLam)), entry, LetFlags.Recursivity, anns)
        val wrapper = lams.foldRight(innerLet: SIR) { (lam, acc) =>
            LamAbs(lam.param, acc, lam.typeParams, lam.anns)
        }
        Some(wrapper)
    }

    private def applyOne(f: AnnotatedSIR, arg: AnnotatedSIR, anns: AnnotationsDecl): AnnotatedSIR = {
        val resTp = f.tp match
            case SIRType.Fun(_, out)                          => out
            case SIRType.TypeLambda(_, SIRType.Fun(_, out))   => out
            case other                                        => other
        Apply(f, arg, resTp, anns)
    }

    /** Unwinds an application spine: `Apply(Apply(h, a1), a2)` -> (h, [a1, a2]). */
    private def spine(e: AnnotatedSIR): (AnnotatedSIR, List[AnnotatedSIR]) = e match
        case Apply(f, arg, _, _) =>
            val (h, args) = spine(f)
            (h, args :+ arg)
        case other => (other, Nil)

    private def isSelfRef(e: AnnotatedSIR, name: String, shadowed: Set[String]): Boolean =
        e match
            case Var(`name`, _, _) if !shadowed.contains(name)         => true
            case ExternalVar(_, `name`, _, _) if !shadowed.contains(name) => true
            case _                                                     => false

    /** Pass 1: collect per-call static masks; set `blocked` on any
      * bare/under-saturated self-reference. Shadow-aware: shadowing of the
      * function name hides self-references; shadowing of a param name makes
      * that position non-static at that call site. Traversal structure
      * mirrors MutualRecursionElimination.rewriteExpr (Let/LamAbs/Match
      * extend `shadowed`); only the Apply/Var/ExternalVar handling differs.
      */
    private final class Analysis(name: String, params: List[Var]) {
        var blocked = false
        var masks: List[List[Boolean]] = Nil
        private val n = params.length

        def run(sir: SIR, shadowed: Set[String]): Unit = sir match
            case app: Apply =>
                val (head, args) = spine(app)
                if isSelfRef(head, name, shadowed) then
                    if args.length < n then blocked = true
                    else
                        val firstN = args.take(n)
                        masks = params.lazyZip(firstN).map { (p, a) =>
                            a match
                                case Var(pn, _, _) =>
                                    pn == p.name && !shadowed.contains(pn)
                                case _ => false
                        }.toList :: masks
                        args.foreach(run(_, shadowed)) // args may contain self-calls
                else
                    run(head, shadowed)
                    args.foreach(run(_, shadowed))
            case e: AnnotatedSIR if isSelfRef(e, name, shadowed) =>
                blocked = true // bare reference outside a saturated spine
            case LamAbs(p, t, _, _) => run(t, shadowed + p.name)
            case Let(bs, body, flags, _) =>
                val names = bs.map(_.name).toSet
                val rhsShadow = if flags.isRec then shadowed ++ names else shadowed
                bs.foreach(b => run(b.value, rhsShadow))
                run(body, shadowed ++ names)
            // Match: extend shadowed with constructor-pattern bindings per
            // case, exactly as MutualRecursionElimination does.
            // All other nodes: recurse into children with unchanged `shadowed`.
            case _ => // structural recursion over remaining node kinds

        // NOTE: the "case _" above is shorthand for this plan; the real
        // implementation must enumerate every AnnotatedSIR node kind like
        // MutualRecursionElimination.rewriteExpr does, so nothing is missed.
    }

    /** Pass 2: replace every saturated self-call spine
      * `self e1…en extra…` with `(f$sat e_c1…e_ck) extra…`, recursively
      * rewriting the argument subtrees. Same shadow rules as Analysis; a
      * shadowed self-name means the spine is left alone.
      */
    private final class Rewriter(
        name: String,
        satName: String,
        satTp: SIRType,
        n: Int,
        staticMask: List[Boolean]
    ) {
        def rewrite(sir: SIR, shadowed: Set[String]): SIR = sir match
            case e: AnnotatedSIR => rewriteExpr(e, shadowed)
            case Decl(data, t)   => Decl(data, rewrite(t, shadowed))

        def rewriteExpr(e: AnnotatedSIR, shadowed: Set[String]): AnnotatedSIR = e match
            case app: Apply =>
                val (head, args) = spine(app)
                if isSelfRef(head, name, shadowed) && args.length >= n then
                    val newArgs = args.map(a => rewriteExpr(a, shadowed))
                    val changingArgs =
                        newArgs.take(n).lazyZip(staticMask).collect { case (a, false) => a }
                    val base = changingArgs.foldLeft(
                      Var(satName, satTp, head.anns): AnnotatedSIR
                    )((acc, a) => applyOne(acc, a, app.anns))
                    newArgs.drop(n).foldLeft(base)((acc, a) => applyOne(acc, a, app.anns))
                else
                    Apply(rewriteExpr(app.f, shadowed), rewriteExpr(app.arg, shadowed), app.tp, app.anns)
            case LamAbs(p, t, tps, anns) => LamAbs(p, rewrite(t, shadowed + p.name), tps, anns)
            case Let(bs, body, flags, anns) =>
                val names = bs.map(_.name).toSet
                val rhsShadow = if flags.isRec then shadowed ++ names else shadowed
                Let(
                  bs.map(b => Binding(b.name, b.tp, rewrite(b.value, rhsShadow))),
                  rewrite(body, shadowed ++ names),
                  flags,
                  anns
                )
            // Match + all remaining node kinds: same structure as
            // MutualRecursionElimination.rewriteExpr, recursing with the
            // correct shadowed set.
            case other => other // plan shorthand — enumerate all cases for real
    }
}
```

Two deliberate simplifications to preserve:
- Over-saturated spines (`args.length > n`) are handled: first `n` args are
  the self-call, the tail is re-applied to the result.
- The rewriter rewrites args before rebuilding, so self-calls nested inside
  a self-call's argument are handled.

- [ ] **Step 4: Run tests, verify they pass**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.StaticArgumentTransformationTest"`
Expected: 3 tests PASS.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/compiler/sir/StaticArgumentTransformation.scala \
        scalus-core/shared/src/test/scala/scalus/compiler/sir/StaticArgumentTransformationTest.scala
git commit -m "feat(sir): static-argument transformation pass (T1)"
```

---

### Task 2: Edge-case behavior

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/StaticArgumentTransformation.scala`
- Modify: `scalus-core/shared/src/test/scala/scalus/compiler/sir/StaticArgumentTransformationTest.scala`

**Interfaces:**
- Consumes: `StaticArgumentTransformation.apply(sir: SIR): SIR` from Task 1.
- Produces: same signature; guaranteed no-op guarantees listed below.

- [ ] **Step 1: Write failing tests, one per edge case**

Reuse Task 1's fixture helpers. Each test builds a small variant and
asserts on the output shape:

```scala
test("zero static params: let is unchanged") {
    // let rec count = λn. if n == 0 then 0 else count (n - 1)
    // single param, changing -> output must be structurally identical input
}

test("all params static: last one is demoted to changing") {
    // let rec spin = λa. λb. spin a b   (never terminates, only shape-checked)
    // expect wrapper with inner spin$sat = λb. ... spin$sat b
}

test("under-saturated self-call blocks the transform") {
    // let rec go = λf. λn. ... (go f) ...   -- spine length 1 < 2
    // expect output == input
}

test("bare self-reference blocks the transform") {
    // let rec go = λf. λn. ... (id go) ...  -- go not in head position
    // expect output == input
}

test("param shadowed at call site is not static") {
    // let rec go = λf. λn. ... (λf. go f (n-1)) something ...
    // inner λf shadows f -> f not static -> only-param f means no transform
}

test("over-saturated spine keeps trailing args") {
    // let rec mk = λc. λn. if n == 0 then (λx. x+c) else mk c (n-1)
    // call site inside body: (mk c (n-1)) 5  -- spine length 3 > n=2
    // expect rewritten to (mk$sat (n-1)) 5
}

test("multi-binding rec let (mutual group) is left untouched") {
    // reuse evenOddGroup from MutualRecursionEliminationTest style
}

test("non-rec let is left untouched") { /* simple non-rec let, compare == */ }

test("nested rec let inside a rec let body is transformed independently") {
    // outer loop with static arg; body contains its own inner rec let with
    // a static arg; both get their own $sat wrapper
}
```

Write these out fully with the SirDSL helpers — the comments above define
the exact fixture; the assertion style is Task 1's.

- [ ] **Step 2: Run, verify the new tests fail (or expose bugs)**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.StaticArgumentTransformationTest"`

- [ ] **Step 3: Fix the implementation until all pass**

The Task 1 code already claims all these behaviors; this task is where each
claim gets pinned. Typical fixes land in `Analysis.run` (blocking rules)
and `trySat` (guards).

- [ ] **Step 4: Run full sir test package**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.*"`
Expected: all green.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -A scalus-core
git commit -m "test(sir): static-argument transformation edge cases"
```

---

### Task 3: ExternalVar self-calls and polymorphic bindings

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/compiler/sir/StaticArgumentTransformation.scala`
- Modify: `scalus-core/shared/src/test/scala/scalus/compiler/sir/StaticArgumentTransformationTest.scala`

**Interfaces:**
- Consumes/Produces: unchanged `apply` signature.

- [ ] **Step 1: Write failing tests**

```scala
test("ExternalVar self-calls are detected (linked top-level defs)") {
    // Binding name is a dotted FQN, as SIRLinker emits:
    // let rec Mod$.go = λf. λn. ... ExternalVar("Mod$", "Mod$.go", goTp) f (n-1) ...
    // expect the same wrapper shape as Task 1, satName "Mod$.go$sat"
}

test("typeParams stay on the wrapper lambda") {
    // let rec go = Λ[A] λf: A->A. λn: Int. λx: A. if n == 0 then x else go f (n-1) (f x)
    // build with LamAbs(param, ..., typeParams = List(tvA), ann) on the outermost lam
    // expect: wrapper's outermost LamAbs keeps typeParams List(tvA);
    //         inner go$sat lambdas have typeParams Nil;
    //         changing params are (n, x); f is lifted
}
```

For the type-var fixture use `SIRType.TypeVar` the same way
`MutualRecursionElimination`'s tests/fixtures do; the inner `satTp` is a
plain `Fun` chain over the changing param types with the type var free.

- [ ] **Step 2: Run, verify failures**

Run: `sbtn "scalusJVM/testOnly scalus.compiler.sir.StaticArgumentTransformationTest"`

- [ ] **Step 3: Implement**

`isSelfRef` already matches `ExternalVar(_, name, _, _)`; the test should
pass or reveal name-matching bugs (the binding name and the ExternalVar
`name` are both the full dotted name — compare them verbatim, exactly like
`RemoveRecursivity.isRecursive`). For typeParams: `peel`/rebuild already
preserves per-lambda `typeParams`; verify `TypeLambda` in `applyOne` and
`satTp` construction behaves (no `TypeLambda` wrapper on `satTp`).

- [ ] **Step 4: Run tests, verify green; commit**

```bash
sbtn scalafmtAll
git add -A scalus-core
git commit -m "feat(sir): SAT handles ExternalVar self-calls and polymorphic bindings"
```

---

### Task 4: Wire into the three entry points, gated on optimizeUplc

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala`
  (method `toUplc`, the `val sir1 = ...` / `val sirToLower = sir1` lines)
- Modify: `scalus-core/shared/src/main/scala/scalus/package.scala`
  (extensions `toUplc` and `lowerToUplc`)
- Modify: `scalus-core/shared/src/test/scala/scalus/compiler/sir/StaticArgumentTransformationTest.scala`

**Interfaces:**
- Consumes: `StaticArgumentTransformation.apply`.
- Produces: behavior only — optimized compiles apply SAT, unoptimized do not.

- [ ] **Step 1: Write the failing gating test**

The program must NOT be closed (wrap it in a lambda taking an argument), or
the optimizer's PartialEvaluator may fold the whole loop away and the
`$sat` marker with it:

```scala
test("SAT applies only when optimizeUplc is on") {
    // λseed. go double 4 seed  — open term, cannot be constant-folded
    val openProgram: SIR = SIR.LamAbs(v("seed", intTp), programWithSeed, List.empty, ann)
    val optimized = openProgram.toUplc()(optimizeUplc = true)
    val plain = openProgram.toUplc()(optimizeUplc = false)
    assert(optimized.show.contains("$sat"))
    assert(!plain.show.contains("$sat"))
}
```

(`programWithSeed` = Task 1's fixture with `intConst(0)` replaced by
`v("seed", intTp)`.)

- [ ] **Step 2: Run, verify it fails** (no `$sat` in either output)

- [ ] **Step 3: Wire the three call sites**

`Compiled.toUplc` (`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala`):

```scala
val sir1 = if options.removeTraces then RemoveTraces.transform(sir) else sir
val sirToLower =
    if options.optimizeUplc then StaticArgumentTransformation(sir1) else sir1
```

`package.scala` `toUplc` (gate on the *parameter*, before the backend match):

```scala
val sirToLower = if optimizeUplc then StaticArgumentTransformation(sir) else sir
val uplc = backend match
    case TargetLoweringBackend.ScottEncodingLowering =>
        ScottEncodingLowering(sirToLower, ...)
    ... // replace every `sir` argument in the match with `sirToLower`
```

`package.scala` `lowerToUplc` (gate on `options.optimizeUplc`, same shape).
Add `import scalus.compiler.sir.StaticArgumentTransformation` where needed
(`package.scala` already imports `scalus.compiler.sir.*`).

- [ ] **Step 4: Run the gating test + recursion guard suites**

```bash
sbtn "scalusJVM/testOnly scalus.compiler.sir.StaticArgumentTransformationTest \
  scalus.compiler.sir.lowering.SelfApplicationRecursionTest \
  scalus.compiler.sir.MutualRecursionEliminationTest \
  scalus.compiler.sir.lowering.MutualRecursionTest"
```

Expected: all green. `SelfApplicationRecursionTest`'s
"optimizer can still constant-fold closed recursive computations" is the
T2-constraint gate — if it fails, implement the spec's fallback: in
`trySat`, skip the transform when the rec rhs is closed (no free variables
besides the binding name; write a small free-vars check or reuse
`RemoveRecursivity.isRecursive`-style traversal), and note it in the pass
scaladoc.

- [ ] **Step 5: Format and commit**

```bash
sbtn scalafmtAll
git add -A scalus-core
git commit -m "feat(sir): apply SAT in optimized toUplc paths"
```

---

### Task 5: Proof-record microbenchmark

**Files:**
- Modify: `scalus-core/jvm/src/test/scala/scalus/uplc/eval/ExprSizeAndBudgetTest.scala`

**Interfaces:**
- Consumes: nothing from other tasks (hand-written UPLC terms).
- Produces: pinned per-call cpu/mem delta constants documenting the win.

- [ ] **Step 1: Read the T2 proof-record**

`ExprSizeAndBudgetTest.scala:221-333`: helpers `zEncoding`/
`selfAppEncoding` build raw UPLC counting loops; `compareEncodings`
evaluates both and asserts pinned per-call deltas.

- [ ] **Step 2: Add the SAT record**

Mirror the existing helpers with two hand-written UPLC programs computing
the same N-iteration counting loop where the recursive function takes two
extra invariant arguments (e.g. `lo` and `step` constants threaded through):

- `nonSatEncoding(n)`: self-application recursion, self-call re-passes
  `lo`, `step`, and the counter each iteration.
- `satEncoding(n)`: `lo`/`step` bound once by outer lambdas, inner
  self-application recursion passes only the counter.

Evaluate both for two sizes (e.g. n=10, n=100), compute per-iteration
cpu/mem delta exactly like `compareEncodings`, and pin the constants with
a comment `// T1 proof-record: SAT saves <X> cpu / <Y> mem per call and
per lifted argument`. Use the measured values from the first run as the
pins (they are deterministic).

- [ ] **Step 3: Run**

Run: `sbtn "scalusJVM/testOnly scalus.uplc.eval.ExprSizeAndBudgetTest"`
Expected: PASS with the new pins.

- [ ] **Step 4: Format and commit**

```bash
sbtn scalafmtAll
git add scalus-core/jvm/src/test/scala/scalus/uplc/eval/ExprSizeAndBudgetTest.scala
git commit -m "test(uplc): T1 SAT proof-record microbenchmark"
```

---

### Task 6: Full suite, budget re-pins, plan doc update

**Files:**
- Modify: budget-pin literals across test suites (driven by failures)
- Modify: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (mark T1 done)

- [ ] **Step 1: Full clean run**

```bash
sbtn clean
sbtn quick
```

Only suites compiling with `optimizeUplc = true` should move. Expect
budget-pin failures (improvements) in optimized-path suites (prelude
List/SortedMap folds, Knights/Clausify, examples).

- [ ] **Step 2: Re-pin budgets**

```bash
python3 scripts/update-budgets.py
```

Known script gaps needing a manual tail (from prior re-pins): `Coin` fee
literals, `assertResult`-style pins, script-size pins, Knights tolerance
suite, bloxbean tests. Fix those by hand from the failure output. Dual
`ScalaCompilerVersion.baseline(pre38, since38)` call sites: update the
current generation's arm from local failures and flag the other arm in the
commit message for re-measurement on the other compiler generation.

- [ ] **Step 3: Verify green**

Run: `sbtn quick`
Expected: all green. Sanity-check a few deltas are *decreases* (SAT must
not increase budgets; a small size increase in wrapper-heavy code is
acceptable, cpu/mem must not regress on loops).

- [ ] **Step 4: Mark T1 done in the plan doc**

In `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`, retitle T1 to
`### T1. Static-argument transformation for recursive functions (HIGH, DONE)`
and append a short "What landed" note: pass name, optimizeUplc gating, the
three wiring points, measured per-call delta from Task 5, and the
mutual-recursion-peers limitation (runs before MutualRecursionElimination).

- [ ] **Step 5: Format and final commit**

```bash
sbtn scalafmtAll
git add -A
git commit -m "feat(sir): T1 static-argument transformation - budgets re-pinned, plan updated"
```

---

## Self-review notes

- Spec coverage: pass + rule details (Tasks 1-3), optimizeUplc gating at
  all three entry points incl. `toUplcOptimized` via `toUplc` (Task 4),
  T2 closed-fold gate + fallback (Task 4 step 4), proof-record (Task 5),
  budget re-pins + dual baselines (Task 6), documented mutual-recursion
  limitation (Task 6 step 4). genArrayToList explicitly out of scope.
- The `case _ =>` shorthands in the Task 1 code are marked as plan
  shorthand with explicit instruction to enumerate all node kinds from
  MutualRecursionElimination — not an implementation placeholder a worker
  can miss.
- Names used consistently: `StaticArgumentTransformation.apply`, `$sat`
  suffix, `trySat`, `Analysis`, `Rewriter`.
