package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.laws
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

class Laws extends AnyFunSuite with FunSuiteDiscipline with Checkers {

    import Gen.Arb.given
    import spire.laws.arb.given

    // ===================================
    // Coin
    // ===================================
    checkAll("Coin.Coin ordered", OrderLaws[Coin.Coin].order)

    checkAll("Coin.Unbounded cModule", VectorSpaceLaws[Coin.Unbounded, SafeLong].cModule)

    checkAll("Coin.Unbounded ordered", OrderLaws[Coin.Unbounded].order)

    checkAll("Coin.Fractional vector space", VectorSpaceLaws[Coin.Fractional, Rational].vectorSpace)

    checkAll("Coin.Fractional ordered", OrderLaws[Coin.Fractional].order)

    // ===================================
    // MultiAsset
    // ===================================

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

    // ===================================
    // Value
    // ===================================
    checkAll("Value Partial Order", OrderLaws[Value].partialOrder)

    checkAll("Value Unbounded Partial Order", OrderLaws[Value.Unbounded].partialOrder)

    checkAll("Value Fractional Partial Order", OrderLaws[Value.Fractional].partialOrder)

    checkAll("Value Unbounded CModule", VectorSpaceLaws[Value.Unbounded, SafeLong].cModule)

    checkAll(
        "Value Fractional VectorSpace",
        VectorSpaceLaws[Value.Fractional, Rational].vectorSpace
    )
}
