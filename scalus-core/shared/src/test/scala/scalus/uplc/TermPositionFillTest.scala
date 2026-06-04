package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Term.*
import scalus.utils.ScalusSourcePos

class TermPositionFillTest extends AnyFunSuite {

    private def pos(line: Int) = ScalusSourcePos("F.scala", line, 0, line, 5)
    private def ann(line: Int) = UplcAnnotation(pos(line))
    private val x = NamedDeBruijn("x", 1)

    private def allLocated(t: Term): Boolean =
        !t.annotation.isEmpty && (t match
            case Apply(f, a, _)     => allLocated(f) && allLocated(a)
            case LamAbs(_, b, _)    => allLocated(b)
            case Force(b, _)        => allLocated(b)
            case Delay(b, _)        => allLocated(b)
            case Constr(_, args, _) => args.forall(allLocated)
            case Case(s, cs, _)     => allLocated(s) && cs.forall(allLocated)
            case _                  => true)

    test("fillEmptyPosBottomUp: a spine node inherits the function leaf's position") {
        val t = Apply(Var(x, ann(10)), Const(Constant.Integer(1)))
        val (filled, rep) = t.fillEmptyPosBottomUp
        assert(rep == pos(10))
        assert(filled.annotation.pos == pos(10)) // Apply got the function's pos
        val Apply(f, a, _) = filled: @unchecked
        assert(f.annotation.pos == pos(10)) // function Var unchanged
        assert(a.annotation.isEmpty) // arg leaf has no descendant → still empty after bottom-up
    }

    test("fillEmptyPosBottomUp: prefers the function side, falls back to the argument") {
        val t = Apply(Var(NamedDeBruijn("f", 2)), Var(x, ann(20)))
        val (filled, rep) = t.fillEmptyPosBottomUp
        assert(rep == pos(20))
        assert(filled.annotation.pos == pos(20))
    }

    test("fillEmptyPosBottomUp never overwrites an existing position") {
        val t = Apply(Var(x, ann(10)), Const(Constant.Integer(1)), ann(99))
        val (filled, _) = t.fillEmptyPosBottomUp
        assert(filled.annotation.pos == pos(99))
    }

    test("fillEmptyPosTopDown: descendants inherit the nearest located ancestor") {
        val t = LamAbs("x", Apply(Var(x), Var(x)), ann(5))
        val LamAbs(_, Apply(f, a, ap), _) = t.fillEmptyPosTopDown(ScalusSourcePos.empty): @unchecked
        assert(ap.pos == pos(5))
        assert(f.annotation.pos == pos(5))
        assert(a.annotation.pos == pos(5))
    }

    test("fillEmptyPosTopDown never overwrites; an inner position wins for its own subtree") {
        val t = LamAbs("x", Apply(Var(x, ann(7)), Const(Constant.Integer(0))), ann(5))
        val LamAbs(_, Apply(f, a, ap), _) = t.fillEmptyPosTopDown(ScalusSourcePos.empty): @unchecked
        assert(ap.pos == pos(5))
        assert(f.annotation.pos == pos(7)) // kept
        assert(a.annotation.pos == pos(5)) // inherited
    }

    test("bottom-up then top-down locate the whole spine from leaf positions") {
        val t = Apply(
          Apply(Builtin(DefaultFun.AddInteger), Var(x, ann(10))),
          Const(Constant.Integer(2))
        )
        assert(!allLocated(t))
        val filled = t.fillEmptyPosBottomUp._1.fillEmptyPosTopDown(ScalusSourcePos.empty)
        assert(allLocated(filled))
    }

    test("fills leave a fully-located term untouched (identity, no realloc)") {
        val t = Apply(Var(x, ann(1)), Const(Constant.Integer(0), ann(2)), ann(3))
        assert(t.fillEmptyPosBottomUp._1 eq t)
        assert(t.fillEmptyPosTopDown(ScalusSourcePos.empty) eq t)
    }
}
