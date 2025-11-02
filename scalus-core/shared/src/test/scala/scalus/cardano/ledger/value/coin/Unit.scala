package scalus.cardano.ledger.value.coin

import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.ledger.value.coin.Coin
import spire.math.{Rational, SafeLong}

import scala.util.Try

/** These tests use the org.scalactic.Equals instances when comparing things with `===`. */
class Unit extends AnyFunSuite, Matchers {
    // =====================
    // Long constructor
    // =====================
    test("Coin(0L) === Right(Coin.zero)") {
        assert(Right(Coin.zero) === Coin(0L))
    }

    test("Coin(1L) succeeds") {
        assert(Coin(1L).isRight)
    }

    test("Coin(-1L) fails") {
        assert(Coin(-1L) === Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MaxValue + 1 fails") {
        assert(Coin(Long.MaxValue + 1) === Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MinValue - 1 succeeds") {
        assert(Coin(Long.MinValue - 1).isRight)
    }

    // =====================
    // SafeLong constructor
    // =====================

    test("Coin(SafeLong(0L) === Right(Coin.zero)") {
        assert(Right(Coin.zero) === Coin(SafeLong(0L)))
    }

    test("Coin(SafeLong(1L)) succeeds") {
        assert(Coin(SafeLong(1L)).isRight)
    }

    test("Coin(SafeLong(-1L)) fails") {
        assert(Coin(SafeLong(-1L)) === Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MaxValue + 1 : SafeLong) fails") {
        val tooBig = SafeLong(BigInt(Long.MaxValue) + 1)
        assert(Coin(tooBig) === Left(Coin.ArithmeticError.Overflow))
    }

    test("Coin(Long.MinValue - 1 : SafeLong) fails") {
        val tooSmall = SafeLong(BigInt(Long.MinValue) - 1)
        assert(Coin(tooSmall) === Left(Coin.ArithmeticError.Underflow))
    }

    // =====================

    test("Genuinely massive coin construction fails for bounded coin") {
        assert(
          Coin(SafeLong(spire.math.pow(BigInt(2), BigInt(128)))) === Left(
            Coin.ArithmeticError.Overflow
          )
        )
    }

    test("Coin(pow(2,32)) round trips") {
        assert(Coin(math.pow(2, 32).toLong).map(_.underlying) === Right(math.pow(2, 32)))
    }

    test("Coin 1 + 2 === 3") {
        assert(Coin.unsafeApply(1L) +~ Coin.unsafeApply(2L) === Coin.unsafeApply(3L))
    }

    test("zero denominator fractional should fail")(
      assert(Try(Coin.Fractional(Rational(1, 0))).isFailure)
    )

    test("Coin signum") {
        assert(Coin.unsafeApply(0).signum === 0)
        assert(Coin.unsafeApply(100).signum === 1)
    }

    // =====================
    // Coin.Unbounded
    // =====================

    test("Coin.Unbounded => Coin.Coin fails on big positive coin") {
        val bigCoin = Coin.Unbounded(SafeLong(Long.MaxValue) * 10)
        assert(bigCoin.toCoin === Left(Coin.ArithmeticError.Overflow))
        assert(Try(bigCoin.unsafeToCoin).isFailure)
    }

    test("Coin.Unbounded => Coin.Coin fails on big negative coin") {
        val bigCoin = Coin.Unbounded(SafeLong(Long.MaxValue) * -10)
        assert(bigCoin.toCoin === Left(Coin.ArithmeticError.Underflow))
        assert(Try(bigCoin.unsafeToCoin).isFailure)
    }

    // =====================
    // Banker's rounding
    // =====================
    test("Coin.Fractional(1/2) rounds to 0") {
        val oneHalf = Coin.Fractional(Rational(1, 2))
        assert(oneHalf.toCoin === Right(Coin.zero))
        assert(oneHalf.unsafeToCoin === Coin.zero)
        assert(oneHalf.toUnbounded === Coin.Unbounded.zero)
    }

    test("Coin.Fractional(-1/2) rounds to 0") {
        val oneHalf = Coin.Fractional(Rational(-1, 2))
        assert(oneHalf.toCoin === Right(Coin.zero))
        assert(oneHalf.unsafeToCoin === Coin.zero)
        assert(oneHalf.toUnbounded === Coin.Unbounded.zero)
    }

    test("Coin.Fractional(3/2) rounds to 2") {
        val x = Coin.Fractional(Rational(3, 2))
        assert(x.toCoin === Coin(2))
        assert(x.toCoin === Right(Coin.unsafeApply(2)))
        assert(x.toUnbounded === Coin.Unbounded(2))
    }

    test("Coin.Fraction(-3/2) rounds to -2") {
        val x = Coin.Fractional(Rational(-3, 2))
        assert(x.toCoin === Left(Coin.ArithmeticError.Underflow))
        assert(x.toUnbounded === Coin.Unbounded(-2))
    }

    // =====================
    // Coin distribution
    // =====================
    test("Weights normalization") {
        val rawWeights = NonEmptyList(Rational(33), List(Rational(8812911823L), Rational(51)))
        val md = Distribution.normalizeWeights(rawWeights)
        md.fold(fail())(d =>
            assert(d.numberOfWeights === 3)
            assert(d.totalWeight === 1)
        )
    }

    test("Coin distribution") {
        val x = Coin.unsafeApply(873_012_309_810_298L)
        val rawWeights =
            NonEmptyList(Rational(53, 81), List(Rational(37, 123), Rational(67, 3329)))
        val md = Distribution.normalizeWeights(rawWeights)
        md.fold(fail())(d => assert(x.distribute(d).toList.coinSum === x))
    }

    test("Coin.Unbounded distribution") {
        val x = Coin.Unbounded(SafeLong(BigInt("12301230981029831298019283")))
        val rawWeights = NonEmptyList(
          Rational(23, 77),
          List(Rational(11, 23), Rational(17, 101), Rational(9788, 178877))
        )
        val md = Distribution.normalizeWeights(rawWeights)
        md.fold(fail())(d => assert(x.distribute(d).toList.coinSum === x))
    }

}
