package scalus.compiler.sir.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, SIR, SIRType}
import scalus.compiler.sir.transform.BooleanOptimizer.optimize
import scalus.uplc.Constant

import scala.annotation.nowarn

/** Test suite for SIRBooleanOptimizer. Tests all implemented boolean optimization rules using
  * AnyFunSuite style.
  */
class BooleanOptimizerTest extends AnyFunSuite:

    // Create empty annotations for test simplicity
    private val emptyAnns = AnnotationsDecl.empty

    // Helper methods to create SIR nodes for testing
    private def const(value: Boolean): SIR.Const =
        SIR.Const(Constant.Bool(value), SIRType.Boolean, emptyAnns)

    private def not(expr: AnnotatedSIR): SIR.Not =
        SIR.Not(expr, emptyAnns)

    private def and(a: AnnotatedSIR, b: AnnotatedSIR): SIR.And =
        SIR.And(a, b, emptyAnns)

    private def or(a: AnnotatedSIR, b: AnnotatedSIR): SIR.Or =
        SIR.Or(a, b, emptyAnns)

    private def ifThenElse(cond: AnnotatedSIR, t: AnnotatedSIR, f: AnnotatedSIR): SIR.IfThenElse =
        SIR.IfThenElse(cond, t, f, SIRType.Boolean, emptyAnns)

    // Mock variables for testing
    private def varA: SIR.Var = SIR.Var("a", SIRType.Boolean, emptyAnns)
    private def varB: SIR.Var = SIR.Var("b", SIRType.Boolean, emptyAnns)
    private def varC: SIR.Var = SIR.Var("c", SIRType.Boolean, emptyAnns)

    test("eliminate double negation") {
        val expr = not(not(varA))
        val optimized = optimize(expr)
        assert(optimized == varA)
    }

    test("optimize nested double negations") {
        val expr = not(not(not(not(varA))))
        val optimized = optimize(expr)
        assert(optimized == varA)
    }

    test("apply reverse De Morgan for OR with negated operands") {
        val expr = or(not(varA), not(varB))
        val expected = not(and(varA, varB))
        val optimized = optimize(expr)
        assert(optimized == expected)
    }

    test("apply reverse De Morgan for AND with negated operands") {
        val expr = and(not(varA), not(varB))
        val expected = not(or(varA, varB))
        val optimized = optimize(expr)
        assert(optimized == expected)
    }

    test("optimize negated conditionals") {
        val expr = ifThenElse(not(varA), varB, varC)
        val expected = ifThenElse(varA, varC, varB)
        val optimized = optimize(expr)
        assert(optimized == expected)
    }

    test("optimize conditionals with constant conditions") {
        val trueExpr = ifThenElse(const(true), varA, varB)
        val falseExpr = ifThenElse(const(false), varA, varB)

        assert(optimize(trueExpr) == varA)
        assert(optimize(falseExpr) == varB)
    }

    test("keep conditionals with identical branches (cond may Error or trace)") {
        val expr = ifThenElse(varA, varB, varB)
        assert(optimize(expr) == expr)
    }

    test("apply constant folding for AND operations") {
        // false && a => false: `a` is the unevaluated branch, so dropping it is safe
        assert(optimize(and(const(false), varA)) == const(false))

        // a && true => a
        assert(optimize(and(varA, const(true))) == varA)

        // true && a => a
        assert(optimize(and(const(true), varA)) == varA)
    }

    test("do not fold `a && false`: it would drop the evaluation of `a`") {
        val expr = and(varA, const(false))
        assert(optimize(expr) == expr)
    }

    test("apply constant folding for OR operations") {
        // true || a => true: `a` is the unevaluated branch, so dropping it is safe
        assert(optimize(or(const(true), varA)) == const(true))

        // a || false => a
        assert(optimize(or(varA, const(false))) == varA)

        // false || a => a
        assert(optimize(or(const(false), varA)) == varA)
    }

    test("do not fold `a || true`: it would drop the evaluation of `a`") {
        val expr = or(varA, const(true))
        assert(optimize(expr) == expr)
    }

    test("do not apply idempotence: `a && a` evaluates `a` twice, `a` evaluates it once") {
        val and1 = and(varA, varA)
        val or1 = or(varA, varA)
        assert(optimize(and1) == and1)
        assert(optimize(or1) == or1)
    }

    test("do not apply the complementation law: it would drop the evaluation of `a`") {
        // `a && !a` and `a || !a` evaluate `a`, which may Error or trace
        val and1 = and(varA, not(varA))
        val or1 = or(varA, not(varA))
        assert(optimize(and1) == and1)
        assert(optimize(or1) == or1)
    }

    test("optimize constant NOT operations") {
        assert(optimize(not(const(true))) == const(false))
        assert(optimize(not(const(false))) == const(true))
    }

    test("recursively optimize in complex expressions") {
        // (!a && !b) || false => !(a || b)
        val expr = or(and(not(varA), not(varB)), const(false))
        assert(optimize(expr) == not(or(varA, varB)))
    }

    test("recursively optimize in conditional expressions") {
        // if (false && a) then b else c => if false then b else c => c
        val expr = ifThenElse(and(const(false), varA), varB, varC)
        assert(optimize(expr) == varC)
    }

    test("handle nested De Morgan's law with double negation") {
        // !(!(a && b)) => a && b
        val expr = not(not(and(varA, varB)))
        val optimized = optimize(expr)
        assert(optimized == and(varA, varB))
    }

    test("optimize negated conditional with constant") {
        // if (!true) then a else b => if (false) then a else b => b
        val expr = ifThenElse(not(const(true)), varA, varB)
        val optimized = optimize(expr)
        assert(optimized == varB)
    }

    test("collapse a double negation created by a rewrite") {
        // !(!a || !b) => !(!(a && b)) => a && b
        val expr = not(or(not(varA), not(varB)))
        assert(optimize(expr) == and(varA, varB))
    }

    test("optimize inside Let binding values, not just the body") {
        val expr = SIR.Let(
          List(Binding("x", SIRType.Boolean, not(not(varA)))),
          not(not(varB)),
          SIR.LetFlags.None,
          emptyAnns
        )
        val expected = SIR.Let(
          List(Binding("x", SIRType.Boolean, varA)),
          varB,
          SIR.LetFlags.None,
          emptyAnns
        )
        assert(optimize(expr) == expected)
    }

    test("optimize inside Cast") {
        val expr = SIR.Cast(not(not(varA)), SIRType.Boolean, emptyAnns)
        assert(optimize(expr) == SIR.Cast(varA, SIRType.Boolean, emptyAnns))
    }

    test("optimize inside LamAbs and Apply") {
        val param = SIR.Var("p", SIRType.Boolean, emptyAnns)
        val lam = SIR.LamAbs(param, not(not(varA)), Nil, emptyAnns)
        val app = SIR.Apply(lam, not(not(varB)), SIRType.Boolean, emptyAnns)
        val expected = SIR.Apply(
          SIR.LamAbs(param, varA, Nil, emptyAnns),
          varB,
          SIRType.Boolean,
          emptyAnns
        )
        assert(optimize(app) == expected)
    }

    test("stay linear on deeply nested negated conditionals") {
        // The old implementation re-optimized already-optimized children on every If(Not)
        // and De Morgan rewrite, which is exponential in the nesting depth.
        val deep = (1 to 60).foldLeft[AnnotatedSIR](varA) { (acc, _) =>
            ifThenElse(not(acc), varB, varC)
        }
        val optimized = optimize(deep)
        assert(optimized != null)
    }

    test("count the rules that fire") {
        val expr = or(not(varA), not(varB))
        val (optimized, stats) = BooleanOptimizer.optimizeCounting(expr)
        assert(optimized == not(and(varA, varB)))
        assert(stats.hits("de-morgan-or") == 1)
        assert(stats.total == 1)
    }

    test("the deprecated alias at the old location forwards here") {
        val expr = or(not(varA), not(varB))
        val viaAlias =
            scalus.compiler.sir.BooleanOptimizer.optimize(expr): @nowarn("cat=deprecation")
        assert(viaAlias == optimize(expr))
    }
