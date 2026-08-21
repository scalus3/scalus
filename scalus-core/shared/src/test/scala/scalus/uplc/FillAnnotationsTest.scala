package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.ScalusSourcePos

class FillAnnotationsTest extends AnyFunSuite {
    private val pos = ScalusSourcePos("Foo.scala", 10, 0, 10, 20)
    private val ann = UplcAnnotation(pos, "validate")

    /** An annotation carrying only a function name, as lowering stamps it on terms that have no
      * position of their own.
      */
    private def nameOnly(n: String) = UplcAnnotation(ScalusSourcePos.empty, n)

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

    test("bottom-up fills a missing position without touching an existing functionName") {
        val leaf = Term.Var(NamedDeBruijn("x"), ann)
        val spine = Term.Apply(leaf, Term.Const(Constant.Integer(1)), nameOnly("helper"))
        val (filled, rep) = spine.fillEmptyAnnotationsBottomUp
        assert(filled.annotation == UplcAnnotation(pos, "helper"))
        assert(rep == ann) // the representative is the whole annotation of the positioned leaf
    }

    test("top-down fills a missing position without touching an existing functionName") {
        val leaf = Term.Var(NamedDeBruijn("x"), nameOnly("helper"))
        val filled = leaf.fillEmptyAnnotationsTopDown(ann)
        assert(filled.annotation == UplcAnnotation(pos, "helper"))
    }

    test("top-down: an inner functionName wins for its own subtree") {
        val inner = Term.Delay(
          Term.Var(NamedDeBruijn("x")),
          UplcAnnotation(ScalusSourcePos("Bar.scala", 1, 0, 1, 5), "inner")
        )
        val filled = Term.Force(inner).fillEmptyAnnotationsTopDown(ann)
        assert(filled.annotation == ann) // the Force spine node inherits the root annotation
        val Term.Force(Term.Delay(v, _), _) = filled: @unchecked
        assert(v.annotation.functionName == "inner")
    }

    test("fills leave a fully-annotated term untouched (identity, no realloc)") {
        val t =
            Term.Apply(Term.Var(NamedDeBruijn("x"), ann), Term.Const(Constant.Integer(0), ann), ann)
        assert(t.fillEmptyAnnotationsBottomUp._1 eq t)
        assert(t.fillEmptyAnnotationsTopDown(UplcAnnotation.empty) eq t)
    }
}
