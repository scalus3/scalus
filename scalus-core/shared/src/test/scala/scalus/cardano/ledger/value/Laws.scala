package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.laws
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

import Gen.Arb.given
import spire.laws.arb.given

class Laws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    implicit val config: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 10_000, workers = 10)

    checkAll("Value Partial Order", OrderLaws[Value].partialOrder)

    checkAll("Value Unbounded Partial Order", OrderLaws[Value.Unbounded].partialOrder)

    checkAll("Value Fractional Partial Order", OrderLaws[Value.Fractional].partialOrder)

    checkAll("Value Unbounded CModule", VectorSpaceLaws[Value.Unbounded, SafeLong].cModule)

    checkAll(
      "Value Fractional VectorSpace",
      VectorSpaceLaws[Value.Fractional, Rational].vectorSpace
    )
}
