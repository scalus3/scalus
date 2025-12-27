package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Tests for NonNegativeInterval arithmetic, including golden tests that match the Haskell
  * cardano-ledger implementation.
  *
  * Reference: cardano-ledger/eras/alonzo/test-suite/test/Test/Cardano/Ledger/Alonzo/Golden.hs
  * https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/alonzo/test-suite/test/Test/Cardano/Ledger/Alonzo/Golden.hs
  *
  * The fee calculation formula from cardano-ledger is:
  * {{{
  * txscriptfee :: Prices -> ExUnits -> Coin
  * txscriptfee Prices {prMem, prSteps} ExUnits {exUnitsMem = m, exUnitsSteps = s} =
  *   Coin $ ceiling $ (fromIntegral m * unboundRational prMem)
  *                  + (fromIntegral s * unboundRational prSteps)
  * }}}
  *
  * Reference: cardano-ledger/libs/cardano-ledger-core/src/Cardano/Ledger/Plutus/ExUnits.hs
  */
class NonNegativeIntervalTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    test("NonNegativeInterval from double preserves exact fractions after reduction"):
        // Haskell: boundRational 0.0577 produces 577/10000
        val priceMem = NonNegativeInterval(0.0577, precision = 15)
        val reducedMem = priceMem.reduce
        assert(reducedMem.numerator == 577)
        assert(reducedMem.denominator == 10000)

        // Haskell: boundRational 0.0000721 produces 721/10000000
        val priceSteps = NonNegativeInterval(0.0000721, precision = 15)
        val reducedSteps = priceSteps.reduce
        assert(reducedSteps.numerator == 721)
        assert(reducedSteps.denominator == 10000000)

    test("NonNegativeInterval multiplication with Long handles large values without overflow"):
        // Test case: price_step * large_steps should not overflow
        // price_step = 0.0000721 = 721/10000000
        // steps = 10_000_000_000 (10 billion)
        // Expected: 721 * 10^10 / 10^7 = 721 * 10^3 = 721000
        val priceSteps = NonNegativeInterval(0.0000721, precision = 15)
        val steps = 10_000_000_000L
        val result = priceSteps * steps
        assert(result.reduce == NonNegativeInterval(721000, 1))

    test("fee calculation matches Haskell txscriptfee formula"):
        // This test verifies our fee calculation matches the Haskell cardano-ledger implementation.
        //
        // Reference: cardano-ledger/eras/alonzo/test-suite/test/Test/Cardano/Ledger/Alonzo/Golden.hs
        // The goldenMinFee test uses these exact protocol parameters from mainnet:
        //   priceMem = fromJust $ boundRational 0.0577
        //   priceSteps = fromJust $ boundRational 0.0000721
        //
        // Formula: fee = ceiling(memory * priceMem + steps * priceSteps)

        val priceMem = NonNegativeInterval(0.0577, precision = 15)
        val priceSteps = NonNegativeInterval(0.0000721, precision = 15)

        // Test case 1: Simple known values
        // memory = 1000, steps = 1000000
        // fee = ceil(1000 * 0.0577 + 1000000 * 0.0000721)
        //     = ceil(57.7 + 72.1) = ceil(129.8) = 130
        val fee1 = (priceMem * 1000L + priceSteps * 1000000L).ceil
        assert(fee1 == 130)

        // Test case 2: Values similar to real transaction execution units
        // memory = 100000, steps = 50000000
        // fee = ceil(100000 * 0.0577 + 50000000 * 0.0000721)
        //     = ceil(5770 + 3605) = ceil(9375) = 9375
        val fee2 = (priceMem * 100000L + priceSteps * 50000000L).ceil
        assert(fee2 == 9375)

        // Test case 3: Large execution units (stress test for overflow handling)
        // memory = 14000000 (max per tx), steps = 10000000000 (max per tx)
        // fee = ceil(14000000 * 0.0577 + 10000000000 * 0.0000721)
        //     = ceil(807800 + 721000) = ceil(1528800) = 1528800
        val fee3 = (priceMem * 14000000L + priceSteps * 10000000000L).ceil
        assert(fee3 == 1528800)

    test("ExUnits.fee matches Haskell txscriptfee"):
        // This test uses the ExUnits.fee method which is used in production code.
        // It should match the Haskell txscriptfee function exactly.
        //
        // Reference protocol parameters from mainnet (epoch 544):
        //   price_mem: 0.0577
        //   price_step: 0.0000721
        val prices = ExUnitPrices(
          priceMemory = NonNegativeInterval(0.0577, precision = 15),
          priceSteps = NonNegativeInterval(0.0000721, precision = 15)
        )

        // Test with known execution units
        val exUnits = ExUnits(memory = 100000L, steps = 50000000L)
        val fee = exUnits.fee(prices)
        // fee = ceil(100000 * 0.0577 + 50000000 * 0.0000721) = ceil(9375) = 9375
        assert(fee.value == 9375)

    test("arithmetic operations preserve precision with BigInt reduction"):
        // Test that intermediate calculations don't overflow and produce correct results
        val a = NonNegativeInterval(1, 1000000000000000L) // 1e-15
        val b = NonNegativeInterval(1, 1000000000000000L) // 1e-15

        // Addition should work without overflow
        val sum = a + b
        assert(sum.reduce == NonNegativeInterval(2, 1000000000000000L).reduce)

        // Multiplication should work without overflow
        val product = a * 1000000000000000L
        assert(product.reduce == NonNegativeInterval(1, 1))

    test("reduce handles edge cases"):
        // Zero numerator
        val zero = NonNegativeInterval.reduce(0, 1000000)
        assert(zero == NonNegativeInterval.zero)

        // Already reduced
        val reduced = NonNegativeInterval.reduce(7, 11)
        assert(reduced.numerator == 7)
        assert(reduced.denominator == 11)

        // Large GCD
        val large = NonNegativeInterval.reduce(1000000000, 1000000000)
        assert(large == NonNegativeInterval(1, 1))
