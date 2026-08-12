package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class ExUnitsTest extends AnyFunSuite {

    private val maxTxExUnits = ExUnits(memory = 16_500_000L, steps = 10_000_000_000L)

    test("fitsWithin accepts units under both components") {
        assert(ExUnits(6_966_471L, 1_606_514_781L).fitsWithin(maxTxExUnits))
    }

    test("fitsWithin accepts units equal to the budget") {
        assert(maxTxExUnits.fitsWithin(maxTxExUnits))
    }

    test("fitsWithin rejects units over on steps but under on memory") {
        val actual = ExUnits(memory = 6_966_471L, steps = 11_606_514_781L)
        assert(!actual.fitsWithin(maxTxExUnits))
        assert(actual.exceeds(maxTxExUnits))
    }

    test("fitsWithin rejects units over on memory but under on steps") {
        val actual = ExUnits(memory = 16_500_001L, steps = 1L)
        assert(!actual.fitsWithin(maxTxExUnits))
        assert(actual.exceeds(maxTxExUnits))
    }

    test("fitsWithin rejects units over on both components") {
        assert(ExUnits.enormous.exceeds(maxTxExUnits))
    }
}
