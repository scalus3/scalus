package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class RefScriptFeeTest extends AnyFunSuite {

    test("tier 1: linear 15 lovelace per byte") {
        assert(RefScriptFee.fee(0, 15L) == Coin(0))
        assert(RefScriptFee.fee(1, 15L) == Coin(15))
        assert(RefScriptFee.fee(49, 15L) == Coin(735))
        assert(RefScriptFee.fee(25599, 15L) == Coin(25599L * 15))
    }

    test("tier boundaries: x1.2 per 25,600-byte stride") {
        // 25_600 * 15 = 384_000; second tier price = 18
        assert(RefScriptFee.fee(25600, 15L) == Coin(384000))
        assert(RefScriptFee.fee(25700, 15L) == Coin(384000 + 100L * 18))
        // three full tiers: 15, 18, 21.6 lovelace/byte
        val threeTiers = 25600L * 15 + 25600L * 18 + (BigDecimal(25600) * BigDecimal("21.6")).toLong
        assert(RefScriptFee.fee(3 * 25600, 15L) == Coin(threeTiers))
        // One byte in the third tier costs 21.6 lovelace; round down only the final total.
        assert(RefScriptFee.fee(2 * 25600 + 1, 15L) == Coin(844821))
    }

    test("constructor uses the supplied protocol parameters") {
        val params = CardanoInfo.mainnet.protocolParams.copy(minFeeRefScriptCostPerByte = 7L)
        val calculator = new RefScriptFee(params)
        assert(calculator.calculate(1109) == Coin(7763))
        assert(calculator.calculate(25601) == Coin(179208))
    }

    test("contextual constructor uses CardanoInfo") {
        given CardanoInfo = CardanoInfo.mainnet
        val calculator = new RefScriptFee()
        assert(calculator.calculate(1109) == Coin(16635))
    }

    test("large reference scripts use exact intermediates and floor only the total") {
        assert(RefScriptFee.fee(204800, 15L) == Coin(6335648))
        assert(RefScriptFee.fee(460800, 15L) == Coin(49196799))
        assert(RefScriptFee.fee(460801, 15L) == Coin(49197199))
        assert(RefScriptFee.fee(512000, 15L) == Coin(71688191))
    }

    test("large base prices remain valid when the final fee fits Coin") {
        assert(RefScriptFee.fee(1, Long.MaxValue) == Coin(Long.MaxValue))
        // The unused next tier must not overflow at an exact boundary.
        assert(
          RefScriptFee.fee(25600, Long.MaxValue / 25600) ==
              Coin((Long.MaxValue / 25600) * 25600)
        )
    }

    test("fees outside Coin range fail explicitly") {
        val error = intercept[ArithmeticException](RefScriptFee.fee(2, Long.MaxValue))
        assert(error.getMessage.contains("Coin range"))
    }

    test("negative arguments are rejected and zero price is free") {
        intercept[IllegalArgumentException](RefScriptFee.fee(-1, 15L))
        intercept[IllegalArgumentException](RefScriptFee.fee(0, -1L))
        assert(RefScriptFee.fee(Int.MaxValue, 0L) == Coin(0))
    }
}
