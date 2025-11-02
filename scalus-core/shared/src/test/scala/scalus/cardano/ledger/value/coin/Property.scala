package scalus.cardano.ledger.value.coin

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen as Gen0, Prop, Properties}
import scalus.cardano.ledger.value.coin.Coin
import spire.syntax.all.*
import spire.laws.arb.given
import spire.math.{Rational, SafeLong}
import spire.syntax.eq.*
import cats.implicits.catsSyntaxEither
import scalus.cardano.ledger.value.coin.Gen.Arb.given

import scala.util.Try
import Arbitrary.arbitrary
import Prop.forAll
import Gen.*

/** These tests primarily test functions that mix underlying representations. Functions that test
  * within a single representation are test withing in the Laws module.
  *
  * These tests use the cats.Eq instances when comparing things with `===`.
  */
object Property extends Properties("Coin") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    property("Long constructors behave for Coin.Coin") = forAll(arbitrary[Long]) {
        case l if 0 <= l => Coin(l).isRight && Try(Coin.unsafeApply(l)).isSuccess
        case l =>
            Coin(l) === Left(Coin.ArithmeticError.Underflow)
            && Try(Coin.unsafeApply(l)).isFailure
    }

    property("SafeLong constructor for Coin.Coin behaves") = forAll(arbitrary[SafeLong]) {
        case sl if 0 <= sl && sl <= Long.MaxValue =>
            Coin(sl).isRight && Try(Coin.unsafeApply(sl)).isSuccess
        case sl if sl < 0 =>
            Coin(sl) === Left(Coin.ArithmeticError.Underflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
        case sl =>
            Coin(sl) === Left(Coin.ArithmeticError.Overflow) &&
            Try(Coin.unsafeApply(sl)).isFailure
    }

    // ===================================
    // Injection/projection round-trips
    // ===================================

    ///////////
    // For Coin
    property("Coin => Unbounded => Coin round trips") = forAll(arbitrary[Coin]) { c =>
        c.toUnbounded.toCoin === Right(c)
    }

    property("Coin => Fractional => Coin round trips") = forAll(arbitrary[Coin]) { c =>
        c.toFractional.toCoin === Right(c)
    }

    property("∀ (c : Coin) => Coin(c.underlying) === Right(c)") = forAll(arbitrary[Coin]) { c =>
        Coin(c.underlying) === Right(c)
    }

    property("∀ (u : Unbounded) => u.toFractional.toUnbounded === u)") =
        forAll(Arbitrary.arbitrary[Coin.Unbounded]) { u =>
            {
                u.toFractional.toUnbounded === u
            }
        }

    /////////////////////////////
    // Scaling round trips
    property("Scale bounded coin by integral and inverse fractional") =
        forAll(arbitrary[Coin], arbitrary[SafeLong].suchThat(sl => sl != 0)) { (coin, i) =>
            (coin *~ i /~ i).toCoin === Right(coin)
        }

    /////////////////////////
    // Average coin
    property("Average coin, long constructor") = forAll(Gen0.listOf(Gen0.posNum[Long])) { longs =>
        {
            longs.map(Coin.unsafeApply).coinAverage === {
                if longs.nonEmpty then Some(Coin.Fractional(Rational(longs.sum, longs.length)))
                else None
            }
        }
    }

    property(
      "∀ (sl : SafeLong , c: Coin) => c.scaleIntegral(sl) === c.scaleFractional(Rational(sl, 1)).toUnbounded"
    ) = forAll(Arbitrary.arbitrary[SafeLong], arbitrary[Coin]) { (sl, c) =>
        {
            c *~ sl === (c *~ Rational(sl, 1)).toUnbounded
        }
    }

    // ===================================
    // Coin distribution
    // ===================================
    property("Coin distribution weights are normalized.") = forAll(genRawWeights()) { rw =>
        Distribution
            .normalizeWeights(rw)
            .fold(false)(weights => weights.totalWeight === Rational(1))
    }

    property("Coin.Unbounded distribution sums to amount distributed.") =
        forAll(arbitrary[Coin.Unbounded], arbitrary[Distribution.NormalizedWeights]) {
            (coin, weights) =>
                coin.distribute(weights).toList.coinSum === coin
        }

    property("Coin distribution sums to amount distributed.") =
        forAll(arbitrary[Coin], arbitrary[Distribution.NormalizedWeights]) { (coin, weights) =>
            coin.distribute(weights).toList.coinSum === coin.toUnbounded
        }

}
