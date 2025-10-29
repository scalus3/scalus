package scalus.cardano.ledger.value

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Prop, Properties, Gen as Gen0}
import spire.algebra.NormedVectorSpace.InnerProductSpaceIsNormedVectorSpace
import spire.implicits.*
import spire.syntax.all._
import spire.laws.arb.given
import spire.math.{Rational, SafeLong}

import scala.util.Try


/**
 * These tests primarily test functions that mix underlying representations.
 * Functions that test within a single representation are test withing in the Laws module.
 */
object Property extends Properties("Coin/MultiAsset/Value") {
    import Arbitrary.arbitrary
    import Gen.Arb.given
    import Prop.forAll

    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    // ===================================
    // Coin
    // ===================================

    property("Long constructors behave for Coin.Coin") = forAll(arbitrary[Long]) {
        case l if 0 <= l => Coin(l).isRight && Try(Coin.unsafeApply(l)).isSuccess
        case l => Coin(l) == Left(Coin.ArithmeticError.Underflow)
            && Try(Coin.unsafeApply(l)).isFailure
    }

    property("SafeLong constructor for Coin.Coin behaves") = forAll(arbitrary[SafeLong]) {
        case sl if 0 <= sl && sl <= Long.MaxValue => Coin(sl).isRight && Try(Coin.unsafeApply(sl)).isSuccess
        case sl if sl < 0 => Coin(sl) == Left(Coin.ArithmeticError.Underflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
        case sl => Coin(sl) == Left(Coin.ArithmeticError.Overflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
    }

    // ===================================
    // MultiAsset
    // ===================================

    property("MultiAsset.Unbounded subtracted from itself equals zero") = forAll(arbitrary[MultiAsset.Unbounded]) {
        ma => ma - ma == MultiAsset.Unbounded.zero
    }

    property("MultiAsset.Fractional subtracted from itself equals zero") = forAll(arbitrary[MultiAsset.Fractional]) {
        ma => ma - ma == MultiAsset.Fractional.zero
    }

    // ===================================
    // Injection/projection round-trips
    // ===================================

    ///////////
    // For Coin
    property("Coin => Unbounded => Coin round trips") = forAll(arbitrary[Coin]) {
        c => c.toCoinUnbounded.toCoin == Right(c)
    }

    property("Coin => Fractional => Coin round trips") = forAll(arbitrary[Coin]){
        c => c.toCoinFractional.toCoin == Right(c)
    }

    property("∀ (c : Coin) => Coin(c.underlying) == Right(c)") = forAll(arbitrary[Coin]) { c =>
        Coin(c.underlying) == Right(c)
    }


    property("∀ (u : Unbounded) => u.toFractional.toUnbounded == u)") =
        forAll(Arbitrary.arbitrary[Coin.Unbounded]) { u =>
        {
            u.toCoinFractional.toUnbounded === u
        }
        }

    /////////////////////////////
    // Scaling round trips
    property("Scale bounded coin by integral and inverse fractional") =
        forAll(arbitrary[Coin], arbitrary[SafeLong].suchThat(sl => sl != 0)) {
            (coin, i) => coin.scaleIntegral(i).scaleFractional(Rational(1, i)).toCoin == Right(coin)
    }

    property("Scale bounded multiasset by integral an inverse fractional") =
        forAll(arbitrary[MultiAsset], arbitrary[SafeLong].suchThat(sl => sl != 0)){
            (ma, i) => ma.scaleIntegral(i).scaleFractional(Rational(1, i)).toMultiAsset == Right(ma)
        }


    /////////////////////////
    // Average coin
    property("Average coin, long constructor") =
        forAll(Gen0.listOf(Gen0.posNum[Long])){
            longs => {
                longs.map(Coin.unsafeApply).averageCoin == {
                    if longs.nonEmpty then
                        Some(Coin.Fractional(Rational(longs.sum, longs.length)))
                    else None
                }
            }
        }

    property(
      "∀ (sl : SafeLong , c: Coin) => c.scaleIntegral(sl) == c.scaleFractional(Rational(sl, 1)).toUnbounded"
    ) = forAll(Arbitrary.arbitrary[SafeLong], arbitrary[Coin]) { (sl, c) =>
        {
            c.scaleIntegral(sl) == c.scaleFractional(Rational(sl, 1)).toUnbounded
        }
    }

}
