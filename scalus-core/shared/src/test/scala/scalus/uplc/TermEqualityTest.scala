package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.test.ArbitraryInstances
import scalus.utils.ScalusSourcePos

import scala.language.implicitConversions

class TermEqualityTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    private val x = vr"x"
    private val y = vr"y"

    test("~=~ ignores annotations") {
        val pos = UplcAnnotation(ScalusSourcePos("A.scala", 1, 2, 1, 5))
        assert(Constr(Word64.Zero, List(x.withAnnotation(pos))) ~=~ Constr(Word64.Zero, List(x)))
    }

    test("~=~ compares the number of Constr fields") {
        assert(Constr(Word64.Zero, List(x)) ~!=~ Constr(Word64.Zero, List(x, y)))
        assert(Constr(Word64.Zero, List(x, y)) ~!=~ Constr(Word64.Zero, List(x)))
        assert(Constr(Word64.Zero, Nil) ~!=~ Constr(Word64.Zero, List(x)))
    }

    test("~=~ compares the number of Case branches") {
        assert(Case(x, List(y)) ~!=~ Case(x, List(y, y)))
        assert(Case(x, List(y, y)) ~!=~ Case(x, List(y)))
        assert(Case(x, Nil) ~!=~ Case(x, List(y)))
    }

    test("~=~ is reflexive, and an extra field or branch breaks it") {
        forAll { (t: Term, extra: Term) =>
            assert(t ~=~ t)
            assert(Constr(Word64.One, List(t)) ~!=~ Constr(Word64.One, List(t, extra)))
            assert(Case(t, List(t)) ~!=~ Case(t, List(t, extra)))
        }
    }
}
