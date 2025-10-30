package scalus.cardano.ledger.value.coin

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.laws
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

import Gen.Arb.given
import spire.laws.arb.given

class Laws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    implicit val config : PropertyCheckConfiguration = PropertyCheckConfiguration(
        minSuccessful = 100_000, workers = 10)

    checkAll("Coin.Coin ordered", OrderLaws[Coin.Coin].order)

    checkAll("Coin.Unbounded cModule", VectorSpaceLaws[Coin.Unbounded, SafeLong].cModule)

    checkAll("Coin.Unbounded ordered", OrderLaws[Coin.Unbounded].order)

    checkAll("Coin.Fractional vector space", VectorSpaceLaws[Coin.Fractional, Rational].vectorSpace)

    checkAll("Coin.Fractional ordered", OrderLaws[Coin.Fractional].order)
}
