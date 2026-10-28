package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, DefaultFun, DefaultUni, NamedDeBruijn, Term}
import scalus.uplc.eval.{PlutusVM, Result}

class DropListsTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val dropList = Term.Force(Term.Builtin(DefaultFun.DropList))

    private def list(size: Int): Term =
        Term.Const(Constant.List(DefaultUni.Integer, (1 to size).map(Constant.Integer(_)).toList))

    private def dropped(function: Term, k: BigInt, xs: Term): Term =
        Term.Apply(Term.Apply(function, Term.Const(Constant.Integer(k))), xs)

    /** What a term evaluates to on the Scalus CEK: its value, or that it failed. */
    private def outcome(term: Term): Either[String, Term] = term.evaluateDebug match
        case Result.Success(value, _, _, _) => Right(value)
        case Result.Failure(_, _, _, _)     => Left("failed")

    test("a dropList of a constant is the steps of a list's tail, also past the end") {
        for
            k <- 0 to 5
            size <- 0 to 6
        do
            val written = dropped(dropList, k, list(size))
            val unrolled = DropLists.unrolled(written)
            assert(!unrolled.toString.contains("DropList"), unrolled)
            assert(outcome(unrolled) == outcome(written), s"k = $k, size = $size")
            assert(
              outcome(unrolled) == Right(
                Term.Const(
                  Constant.List(DefaultUni.Integer, (k + 1 to size).map(Constant.Integer(_)).toList)
                )
              )
            )
    }

    test("a dropList bound to a variable is found where the program uses it") {
        val variable = Term.Var(NamedDeBruijn("d"))
        // As `(lam d body) builtin`, and as a field of `case (constr 0 builtins)`.
        val applied = Term.Apply(Term.LamAbs("d", dropped(variable, 2, list(3))), dropList)
        val cased = Term.Case(
          Term.Constr(Word64.Zero, List(dropList)),
          List(Term.LamAbs("d", dropped(variable, 2, list(3))))
        )
        for written <- List(applied, cased) do
            val unrolled = DropLists.unrolled(written)
            assert(unrolled.toString.split("DropList").length == 2, unrolled)
            assert(outcome(unrolled) == outcome(written))
        // A variable of the same name bound to another value is not the builtin.
        val shadowed = Term.Apply(
          Term.LamAbs("d", Term.LamAbs("d", dropped(variable, 2, list(3)))),
          dropList
        )
        assert(DropLists.unrolled(shadowed) == shadowed)
    }

    test("a dropList of a count below zero, above the most steps, or not a constant is kept") {
        val negative = dropped(dropList, -1, list(3))
        assert(DropLists.unrolled(negative) == negative)
        val many = dropped(dropList, DropLists.maxSteps + 1, list(3))
        assert(DropLists.unrolled(many) == many)
        val computed = Term.Apply(
          Term.Apply(
            dropList,
            Term.Apply(
              Term.LamAbs("n", Term.Var(NamedDeBruijn("n"))),
              Term.Const(Constant.Integer(1))
            )
          ),
          list(3)
        )
        assert(DropLists.unrolled(computed) == computed)
    }
}
