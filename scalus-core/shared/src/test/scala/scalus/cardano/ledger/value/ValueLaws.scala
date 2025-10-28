package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

class ValueLaws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    import Gen.Arb.given
    import spire.laws.arb.given

    checkAll("Value Partial Order", OrderLaws[Value].partialOrder)

    checkAll("Value Unbounded Partial Order", OrderLaws[Value.Unbounded].partialOrder)

    checkAll("Value Fractional Partial Order", OrderLaws[Value.Fractional].partialOrder)

    checkAll("Value Unbounded CModule", VectorSpaceLaws[Value.Unbounded, SafeLong].cModule)

    checkAll("Value Fractional VectorSpace", VectorSpaceLaws[Value.Fractional, Rational].vectorSpace)

}
