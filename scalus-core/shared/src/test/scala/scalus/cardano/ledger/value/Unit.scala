package scalus.cardano.ledger.value

import algebra.CommutativeGroup
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.value.MultiAsset.Unbounded.Unbounded
import scalus.cardano.ledger.{AssetName, ScriptHash}
import spire.laws.GroupLaws
import spire.math.{Rational, SafeLong}
import spire.syntax.all.*

import scala.collection.immutable.TreeMap
import scala.util.Try

class Unit extends AnyFunSuite {

    // ===================================
    // Coin.Coin tests
    // ===================================

    // =====================
    // Long constructor
    // =====================
    test("Coin(0L) == Right(Coin.zero)") { assert(Right(Coin.zero) == Coin(0L)) }

    test("Coin(1L) succeeds") { assert(Coin(1L).isRight) }

    test("Coin(-1L) fails") {
        assert(Coin(-1L) == Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MaxValue + 1 fails") {
        assert(Coin(Long.MaxValue + 1) == Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MinValue - 1 succeeds") {
        assert(Coin(Long.MinValue - 1).isRight)
    }

    // =====================
    // SafeLong constructor
    // =====================

    test("Coin(SafeLong(0L) == Right(Coin.zero)") {
        assert(Right(Coin.zero) == Coin(SafeLong(0L)))
    }

    test("Coin(SafeLong(1L)) succeeds") {
        assert(Coin(SafeLong(1L)).isRight)
    }

    test("Coin(SafeLong(-1L)) fails") {
        assert(Coin(SafeLong(-1L)) == Left(Coin.ArithmeticError.Underflow))
    }

    test("Coin(Long.MaxValue + 1 : SafeLong) fails") {
        val tooBig = SafeLong(BigInt(Long.MaxValue) + 1)
        assert(Coin(tooBig) == Left(Coin.ArithmeticError.Overflow))
    }

    test("Coin(Long.MinValue - 1 : SafeLong) fails") {
        val tooSmall = SafeLong(BigInt(Long.MinValue) - 1)
        assert(Coin(tooSmall) == Left(Coin.ArithmeticError.Underflow))
    }

    ///////////

    test("Genuinely massive coin construction fails for bounded coin") {
        assert(
          Coin(SafeLong(spire.math.pow(BigInt(2), BigInt(128)))) == Left(
            Coin.ArithmeticError.Overflow
          )
        )
    }

    test("Coin(pow(2,32)) round trips") {
        assert(Coin(math.pow(2, 32).toLong).map(_.underlying) === Right(math.pow(2, 32)))
    }

    test("Coin 1 + 2 == 3") {
        assert(Coin.unsafeApply(1L) +~ Coin.unsafeApply(2L) == Coin.unsafeApply(3L))
    }

    test("zero denominator fractional should fail")(
      assert(Try(Coin.Fractional(Rational(1, 0))).isFailure)
    )

    test("Coin signum") {
        assert(Coin.unsafeApply(0).signum == 0)
        assert(Coin.unsafeApply(100).signum == 1)
    }

    //////////////////////////
    // Coin.Unbounded

    test("Coin.Unbounded => Coin.Coin fails on big positive coin") {
        val bigCoin = Coin.Unbounded(SafeLong(Long.MaxValue) * 10)
        assert(bigCoin.toCoin == Left(Coin.ArithmeticError.Overflow))
        assert(Try(bigCoin.unsafeToCoin).isFailure)
    }

    test("Coin.Unbouned => Coin.Coin fails on big negative coin") {
        {
            val bigCoin = Coin.Unbounded(SafeLong(Long.MaxValue) * -10)
            assert(bigCoin.toCoin == Left(Coin.ArithmeticError.Underflow))
            assert(Try(bigCoin.unsafeToCoin).isFailure)
        }
    }

    ////////////////////////
    // Banker's rounding
    test("Coin.Fractional(1/2) rounds to 0") {
        val oneHalf = Coin.Fractional(Rational(1, 2))
        assert(oneHalf.toCoin == Right(Coin.zero))
        assert(oneHalf.unsafeToCoin == Coin.zero)
        assert(oneHalf.toUnbounded == Coin.Unbounded.zero)
    }

    test("Coin.Fractional(-1/2) rounds to 0") {
        val oneHalf = Coin.Fractional(Rational(-1, 2))
        assert(oneHalf.toCoin == Right(Coin.zero))
        assert(oneHalf.unsafeToCoin == Coin.zero)
        assert(oneHalf.toUnbounded == Coin.Unbounded.zero)
    }

    test("Coin.Fractional(3/2) rounds to 2") {
        val x = Coin.Fractional(Rational(3, 2))
        assert(x.toCoin == Coin(2))
        assert(x.toCoin == Right(Coin.unsafeApply(2)))
        assert(x.toUnbounded == Coin.Unbounded(2))
    }

    test("Coin.Fraction(-3/2) rounds to -2") {
        val x = Coin.Fractional(Rational(-3, 2))
        assert(x.toCoin == Left(Coin.ArithmeticError.Underflow))
        assert(x.toUnbounded == Coin.Unbounded(-2))
    }

    // ===================================
    // MultiAsset
    // ===================================

    val mai: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(
      TreeMap(
        AssetName.fromHex("") -> Coin
            .Unbounded(SafeLong(BigInt(1)))
      )
    )

    test("MultiAsset.Unbounded subtracted from itself equals zero") {
        implicit val alg: CommutativeGroup[MultiAsset.Inner.Unbounded] =
            MultiAsset.Inner.Unbounded.algebra.additive
        assert(alg.empty === (alg.inverse(mai) |+| mai))
    }

    test("MultiAsset.Unbounded subtracted from itself equals zero, 2") {
        implicit val alg: CommutativeGroup[MultiAsset.Inner.Unbounded] = MultiAsset.Inner.Unbounded.algebra.additive
        assert(alg.empty === mai - mai)
    }

    test("MultiAsset.Unbounded.Inner added to zero is identity"){
        assert(mai + MultiAsset.Inner.Unbounded.zero === mai)
        assert(MultiAsset.Inner.Unbounded.zero + mai === mai)
    }

    test("MultiAsset.Unbounded.Inner minus zero") {
        assert(mai - MultiAsset.Inner.Unbounded.zero === mai)
        assert(MultiAsset.Inner.Unbounded.zero - mai === -mai)
    }

    import Gen.Arb.given
    test("left inverse") {
        GroupLaws[MultiAsset.Inner.Unbounded]
            .group(using MultiAsset.Inner.Unbounded.algebra.additive)
            .props
            .head
            ._2
            .check()
    }
}
