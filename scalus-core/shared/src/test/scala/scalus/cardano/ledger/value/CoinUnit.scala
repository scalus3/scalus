package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import spire.math.Rational

import scala.util.Try

class CoinUnit extends AnyFunSuite {

    // ===================================
    // Coin.Coin tests
    // ===================================
    test("Coin(0L) == Right(Coin.zero)") { assert(Right(Coin.zero) == Coin(0L)) }

    test("Coin(1L) succeeds") { assert(Coin(1L).isRight) }

    test("Coin(-1L) fails") {
        assert(Coin(-1L).isLeft)
    }

    test("Coin(pow(2,32)) round trips") {
        assert(Coin(math.pow(2, 32).toLong).map(_.underlying) === Right(math.pow(2, 32)))
    }

    test("Coin 1 + 2 == 3") {
        assert(Coin.unsafeApply(1L) +~ Coin.unsafeApply(2L) == Coin.unsafeApply(3L))
    }

    // ===================================
    // Coin.Unbounded
    // ===================================

    // ===================================
    // Coin.Fractional
    // ===================================

    test("zero denominator fractional should fail")(
      assert(Try(Coin.Fractional(Rational(1, 0))).isFailure)
    )

}

/*

       The inverse property must not hold, see the list of prescribed failures above.

        Turning a floating point or rational amount into an amount of money should fail whenever it introduces an error.

    Addition

        Addition of two amounts must be able to fail.

            It must succeed for:

                1 + 2, and equal 3

                0 + money for any money (and equal money).

                money + 0 for any money (and equal money).

            It must fail for:

                maxBound + 1

                maxBound + maxBound

                minBound + minBound

                minBound + (- 1)

        Adding two amounts in different currencies must always fail.

        It must be associative if both sides succeed: a + (b + c) == (a + b) + c Note that either side might fail separately because of overflow.

        It must be commutative: a + b == b + a.

    Subtraction

        Subtraction of two amounts must be able to fail.

            It must succeed for:

                3 - 2, and equal 1.

                0 - money for any money (and equal - money).

                money - 0 for any money (and equal money).

            It must fail for:

                minBound - 1

                maxBound - minBound

                minBound - maxBound

                maxBound - (- 1)

    Summation

        Summation must succeed for:

            sum [1, 2, 3] == 6

        Summation must fail for:

            [maxBound, 1]

            [maxBound, 1, -2] (in order to maintain bounded complexities per element).

    Integer multiplication

        It must succeed for:

            3 x 6, and equal 18

            1 x money, and equal money

            0 x money, and equal a zero amount

        It must fail for:

            2 x maxBound

            3 x minBound

        It must be distributive when both sides succeed: a x (b + c) == (a x b) + (a x c).

    Integer distribution

        It distributes 3 into 3 as 3 chunks of 1.

        It distributes 5 into 3 as 2 chunks of 2 and 1 chunk of 1.

        It distributes 10 into 4 as 2 chunks of 3 and 2 chunk of 2.

        It distributes any amount into chunks that sum up to the input amount successfully.

    Fractional multiplication

        100 minimal quantisations times 1/100 must equal 1 minimal quantisation.

        101 minimal quantisations times 1/100 must equal 1 minimal quantisation with the rate changed to 1/101.

        It must produce results that can be multiplied back to the input amount successfully.
 */
