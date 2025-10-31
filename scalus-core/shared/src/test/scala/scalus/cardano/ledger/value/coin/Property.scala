package scalus.cardano.ledger.value.coin

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen as Gen0, Prop, Properties}
import scalus.cardano.ledger.value.coin.Coin
// import spire.implicits.*
import spire.syntax.all.*
import spire.laws.arb.given
import spire.math.{Rational, SafeLong}

import scalus.cardano.ledger.value.coin.Gen.Arb.given

import scala.util.Try

import Arbitrary.arbitrary
import Prop.forAll

import Gen.*

/** These tests primarily test functions that mix underlying representations. Functions that test
  * within a single representation are test withing in the Laws module.
  */
object Property extends Properties("Coin") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    property("Long constructors behave for Coin.Coin") = forAll(arbitrary[Long]) {
        case l if 0 <= l => Coin(l).isRight && Try(Coin.unsafeApply(l)).isSuccess
        case l =>
            Coin(l) == Left(Coin.ArithmeticError.Underflow)
            && Try(Coin.unsafeApply(l)).isFailure
    }

    property("SafeLong constructor for Coin.Coin behaves") = forAll(arbitrary[SafeLong]) {
        case sl if 0 <= sl && sl <= Long.MaxValue =>
            Coin(sl).isRight && Try(Coin.unsafeApply(sl)).isSuccess
        case sl if sl < 0 =>
            Coin(sl) == Left(Coin.ArithmeticError.Underflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
        case sl =>
            Coin(sl) == Left(Coin.ArithmeticError.Overflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
    }

    // ===================================
    // Injection/projection round-trips
    // ===================================

    ///////////
    // For Coin
    property("Coin => Unbounded => Coin round trips") = forAll(arbitrary[Coin]) { c =>
        c.toCoinUnbounded.toCoin == Right(c)
    }

    property("Coin => Fractional => Coin round trips") = forAll(arbitrary[Coin]) { c =>
        c.toCoinFractional.toCoin == Right(c)
    }

    property("∀ (c : Coin) => Coin(c.underlying) == Right(c)") = forAll(arbitrary[Coin]) { c =>
        Coin(c.underlying) == Right(c)
    }

    property("∀ (u : Unbounded) => u.toFractional.toUnbounded == u)") =
        forAll(Arbitrary.arbitrary[Coin.Unbounded]) { u =>
            {
                u.toFractional.toUnbounded === u
            }
        }

    /////////////////////////////
    // Scaling round trips
    property("Scale bounded coin by integral and inverse fractional") =
        forAll(arbitrary[Coin], arbitrary[SafeLong].suchThat(sl => sl != 0)) { (coin, i) =>
            coin.scaleIntegral(i).scaleFractional(Rational(1, i)).toCoin == Right(coin)
        }

    /////////////////////////
    // Average coin
    property("Average coin, long constructor") = forAll(Gen0.listOf(Gen0.posNum[Long])) { longs =>
        {
            longs.map(Coin.unsafeApply).averageCoin == {
                if longs.nonEmpty then Some(Coin.Fractional(Rational(longs.sum, longs.length)))
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

    // ===================================
    // Coin distribution
    // ===================================
    property("Coin distribution weights are normalized.") = forAll(genRawWeights()) { rw =>
        Distribution.normalizeWeights(rw).fold(false)(weights => weights.totalWeight == 1)
    }

    property("Coin.Unbounded distribution sums to amount distributed.") =
        forAll(arbitrary[Coin.Unbounded], genNormalizedWeights()) { (coin, weights) =>
            coin.distribute(weights).toList.sumCoins == coin
        }

    property("Coin distribution sums to amount distributed.") =
        forAll(arbitrary[Coin], genNormalizedWeights()) { (coin, weights) =>
            coin.distribute(weights).toList.sumCoins == coin
        }

}
