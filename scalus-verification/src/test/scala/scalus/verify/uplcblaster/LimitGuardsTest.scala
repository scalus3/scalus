package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.uplc.Constant
import scalus.uplc.builtin.Data
import scalus.verify.*
import scalus.verify.Props.*

class LimitGuardsTest extends AnyFunSuite {

    private def ident(prop: Prop): PropExpr.Ident[?] = prop match
        case Prop.Forall(ident, _) => ident
        case other                 => fail(s"expected a universal statement, got $other")

    private def guarded(prop: Prop): LimitGuards.Guard = {
        val tp = ident(prop).tp
        LimitGuards
            .guard(DataLayout.of(DataEncoding.valueType(tp)), DataEncoding.limits(tp))
            .fold(fail(_), identity)
    }

    /** The elements of the lists of a list are limited, and the list itself has no most. */
    private val emptyLists =
        forAllDataLimited[prelude.List[prelude.List[BigInt]]](xss =>
            xss.forall(xs => xs.length <= 0)
        )(xss => xss.length >= BigInt(0))

    private val atMostTwo =
        forAllDataLimited[prelude.List[BigInt]](xs => xs.length <= 2)(xs => xs.length >= BigInt(0))

    test("a guard is exact where it checks all that the limits say, and not where it checks less") {
        assert(guarded(atMostTwo).exact)
        // A list that has no most is not walked to its end, and its elements are not looked at.
        assert(!guarded(emptyLists).exact)
        val bounded = forAllDataLimited[prelude.List[prelude.List[BigInt]]](xss =>
            xss.length <= 2 && xss.forall(xs => xs.length <= 0)
        )(xss => xss.length >= BigInt(0))
        assert(guarded(bounded).exact)
    }

    test("a variable whose guard checks less stands only for every value") {
        // As a forAll in a positive position it is lowered: a proof of more values holds of those
        // the limits allow.
        assert(UplcBlaster.lower(emptyLists, FunctionTable.empty).isRight)
        // Negated, it asks for a witness, which the guard could let through outside the limits.
        UplcBlaster.lower(Prop.Not(emptyLists), FunctionTable.empty) match
            case Left(reason) => assert(reason.contains("checks less than its limits say"), reason)
            case Right(_) => fail("expected a negated statement of an inexact guard to be refused")
        // An exact guard may be negated.
        assert(UplcBlaster.lower(Prop.Not(atMostTwo), FunctionTable.empty).isRight)
    }

    test("a value outside the limits is no counterexample") {
        val tp = ident(emptyLists).tp
        val inside = Data.List(prelude.List(Data.List(prelude.List.Nil)))
        val outside = Data.List(prelude.List(Data.List(prelude.List(Data.I(1)))))
        assert(UplcBlaster.outsideType(Constant.Data(inside), tp).isEmpty)
        assert(
          UplcBlaster
              .outsideType(Constant.Data(outside), tp)
              .exists(_.contains("outside the variable's limits")),
          UplcBlaster.outsideType(Constant.Data(outside), tp)
        )
    }

    test("a guard too large is refused before it is written") {
        // Four lists within each other, of 64 elements each: some 16 million cells.
        val nested =
            forAllDataLimited[prelude.List[prelude.List[prelude.List[prelude.List[BigInt]]]]](x =>
                maxCollectionLength(x) <= 64
            )(x => x.length >= BigInt(0))
        val tp = ident(nested).tp
        val started = System.nanoTime()
        LimitGuards.guard(DataLayout.of(DataEncoding.valueType(tp)), DataEncoding.limits(tp)) match
            case Left(reason) => assert(reason.contains("more than 20000 variables"), reason)
            case Right(_)     => fail("expected the guard to be refused")
        assert(
          (System.nanoTime() - started) / 1e9 < 5,
          "the refusal is told before the guard is written"
        )
    }
}
