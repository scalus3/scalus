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
    }

    test("ProtocolParams overload uses minFeeRefScriptCostPerByte") {
        val params = CardanoInfo.mainnet.protocolParams
        assert(params.minFeeRefScriptCostPerByte == 15L)
        // pinned value from docs/internal/UNROLLING_REF_SCRIPT_FEE_TRADEOFF.md (1,109 B script)
        assert(RefScriptFee.fee(1109, params) == Coin(16635))
        assert(RefScriptFee.fee(1109, params) == RefScriptFee.fee(1109, 15L))
    }

    test("constants") {
        assert(RefScriptFee.sizeIncrement == 25600)
        assert(RefScriptFee.multiplier == NonNegativeInterval(12, 10))
    }
}
