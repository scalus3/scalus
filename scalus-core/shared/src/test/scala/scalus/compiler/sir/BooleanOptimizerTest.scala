package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.BooleanOptimizer.optimize
import scalus.uplc.Constant

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

    test("optimize conditionals with identical branches") {
        val expr = ifThenElse(varA, varB, varB)
        val optimized = optimize(expr)
        assert(optimized == varB)
    }

    test("apply constant folding for AND operations") {
        // a && false => false
        assert(optimize(and(varA, const(false))) == const(false))

        // false && a => false
        assert(optimize(and(const(false), varA)) == const(false))

        // a && true => a
        assert(optimize(and(varA, const(true))) == varA)

        // true && a => a
        assert(optimize(and(const(true), varA)) == varA)
    }

    test("apply constant folding for OR operations") {
        // a || true => true
        assert(optimize(or(varA, const(true))) == const(true))

        // true || a => true
        assert(optimize(or(const(true), varA)) == const(true))

        // a || false => a
        assert(optimize(or(varA, const(false))) == varA)

        // false || a => a
        assert(optimize(or(const(false), varA)) == varA)
    }

    test("optimize idempotent AND operations") {
        // a && a => a
        val expr = and(varA, varA)
        val optimized = optimize(expr)
        assert(optimized == varA)
    }

    test("optimize idempotent OR operations") {
        // a || a => a
        val expr = or(varA, varA)
        val optimized = optimize(expr)
        assert(optimized == varA)
    }

    test("apply complementation law for AND") {
        // a && !a => false
        val expr1 = and(varA, not(varA))
        val expr2 = and(not(varA), varA)

        assert(optimize(expr1) == const(false))
        assert(optimize(expr2) == const(false))
    }

    test("apply complementation law for OR") {
        // a || !a => true
        val expr1 = or(varA, not(varA))
        val expr2 = or(not(varA), varA)

        assert(optimize(expr1) == const(true))
        assert(optimize(expr2) == const(true))
    }

    test("optimize constant NOT operations") {
        assert(optimize(not(const(true))) == const(false))
        assert(optimize(not(const(false))) == const(true))
    }

    test("recursively optimize in complex expressions") {
        // (a && !a) || b => false || b => b
        val expr = or(and(varA, not(varA)), varB)
        val optimized = optimize(expr)
        assert(optimized == varB)
    }

    test("recursively optimize in conditional expressions") {
        // if (a && !a) then b else c => if (false) then b else c => c
        val expr = ifThenElse(and(varA, not(varA)), varB, varC)
        val optimized = optimize(expr)
        assert(optimized == varC)
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
