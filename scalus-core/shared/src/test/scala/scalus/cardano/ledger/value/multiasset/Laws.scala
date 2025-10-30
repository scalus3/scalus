package scalus.cardano.ledger.value.multiasset

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import scalus.cardano.ledger.value.multiasset.MultiAsset
import spire.laws
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

import Gen.Arb.given
import spire.laws.arb.given

class Laws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    implicit val config: PropertyCheckConfiguration = PropertyCheckConfiguration(
        minSuccessful = 10_000, workers = 10)

    checkAll("MultiAsset Partial Order", OrderLaws[MultiAsset].partialOrder)

    checkAll("MultiAsset Unbounded Partial Order", OrderLaws[MultiAsset.Unbounded].partialOrder)

    checkAll("MultiAsset Fractional Partial Order", OrderLaws[MultiAsset.Fractional].partialOrder)

    checkAll(
      "MultiAsset Unbounded Inner CModule",
      VectorSpaceLaws[MultiAsset.Inner.Unbounded, SafeLong].cModule
    )

    checkAll(
      "MultiAsset Fractional Inner VectorSpace",
      VectorSpaceLaws[MultiAsset.Inner.Fractional, Rational].vectorSpace
    )

    checkAll(
      "MultiAsset Unbounded CModule",
      VectorSpaceLaws[MultiAsset.Unbounded, SafeLong].cModule
    )

    checkAll(
      "MultiAsset Fractional VectorSpace",
      VectorSpaceLaws[MultiAsset.Fractional, Rational].vectorSpace
    )
}
