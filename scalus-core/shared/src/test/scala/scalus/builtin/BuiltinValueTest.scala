package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{BuiltinValue, ByteString, Data}
import scalus.uplc.Constant
import scalus.uplc.eval.{BuiltinException, BuiltinValueOps}

/** Tests for CIP-0153 MaryEraValue (BuiltinValue) operations. */
class BuiltinValueTest extends AnyFunSuite {

    // Test currencies and tokens
    val adaSymbol: ByteString = ByteString.empty
    val adaToken: ByteString = ByteString.empty
    val policyA: ByteString = ByteString.fromHex("aabbccdd")
    val tokenA: ByteString = ByteString.fromString("TokenA")
    val tokenB: ByteString = ByteString.fromString("TokenB")

    test("empty BuiltinValue has no coins") {
        val empty = BuiltinValue.empty
        val result = BuiltinValueOps.lookupCoin(adaSymbol, adaToken, empty)
        assert(result == BigInt(0))
    }

    test("insertCoin creates value with single token") {
        val value =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val result = BuiltinValueOps.lookupCoin(adaSymbol, adaToken, value)
        assert(result == BigInt(1000000))
    }

    test("insertCoin with multiple tokens") {
        val v1 =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), v1)
        val v3 = BuiltinValueOps.insertCoin(policyA, tokenB, BigInt(200), v2)

        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, v3) == BigInt(1000000))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, v3) == BigInt(100))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenB, v3) == BigInt(200))
    }

    test("insertCoin with zero removes token") {
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(0), v1)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, v2) == BigInt(0))
    }

    test("lookupCoin returns 0 for missing token") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        assert(BuiltinValueOps.lookupCoin(policyA, tokenB, value) == BigInt(0))
        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, value) == BigInt(0))
    }

    test("unionValue merges two values") {
        val v1 =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val merged = BuiltinValueOps.unionValue(v1, v2)

        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, merged) == BigInt(1000000))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, merged) == BigInt(100))
    }

    test("unionValue adds amounts for same token") {
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)
        val merged = BuiltinValueOps.unionValue(v1, v2)

        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, merged) == BigInt(150))
    }

    test("unionValue removes zero-sum tokens") {
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(-100), BuiltinValue.empty)
        val merged = BuiltinValueOps.unionValue(v1, v2)

        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, merged) == BigInt(0))
    }

    test("valueContains returns true when first contains second") {
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)

        assert(BuiltinValueOps.valueContains(v1, v2))
    }

    test("valueContains returns false when second has more") {
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(50), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)

        assert(!BuiltinValueOps.valueContains(v1, v2))
    }

    test("valueContains with empty always succeeds") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        assert(BuiltinValueOps.valueContains(value, BuiltinValue.empty))
        assert(BuiltinValueOps.valueContains(BuiltinValue.empty, BuiltinValue.empty))
    }

    test("scaleValue multiplies all amounts") {
        val v1 =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), v1)
        val scaled = BuiltinValueOps.scaleValue(BigInt(3), v2)

        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, scaled) == BigInt(3000000))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, scaled) == BigInt(300))
    }

    test("scaleValue with zero returns empty") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val scaled = BuiltinValueOps.scaleValue(BigInt(0), value)

        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, scaled) == BigInt(0))
    }

    test("scaleValue with one returns same value") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val scaled = BuiltinValueOps.scaleValue(BigInt(1), value)

        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, scaled) == BigInt(100))
    }

    test("scaleValue with negative inverts amounts") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val scaled = BuiltinValueOps.scaleValue(BigInt(-1), value)

        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, scaled) == BigInt(-100))
    }

    test("toData and fromData roundtrip") {
        val v1 =
            BuiltinValueOps.insertCoin(adaSymbol, adaToken, BigInt(1000000), BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), v1)
        val v3 = BuiltinValueOps.insertCoin(policyA, tokenB, BigInt(200), v2)

        val data = BuiltinValueOps.toData(v3)
        val restored = BuiltinValueOps.fromData(data)

        assert(BuiltinValueOps.lookupCoin(adaSymbol, adaToken, restored) == BigInt(1000000))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenA, restored) == BigInt(100))
        assert(BuiltinValueOps.lookupCoin(policyA, tokenB, restored) == BigInt(200))
    }

    test("insertCoin rejects key exceeding 32 bytes") {
        val longKey = ByteString.fromArray(Array.fill(33)(0.toByte))
        assertThrows[BuiltinException] {
            BuiltinValueOps.insertCoin(longKey, adaToken, BigInt(100), BuiltinValue.empty)
        }
        assertThrows[BuiltinException] {
            BuiltinValueOps.insertCoin(policyA, longKey, BigInt(100), BuiltinValue.empty)
        }
    }

    test("insertCoin rejects quantity out of 128-bit range") {
        val tooBig = BigInt(2).pow(127)
        assertThrows[BuiltinException] {
            BuiltinValueOps.insertCoin(policyA, tokenA, tooBig, BuiltinValue.empty)
        }

        val tooSmall = -BigInt(2).pow(127) - 1
        assertThrows[BuiltinException] {
            BuiltinValueOps.insertCoin(policyA, tokenA, tooSmall, BuiltinValue.empty)
        }
    }

    test("unionValue rejects overflow") {
        val maxVal = BigInt(2).pow(127) - 1
        val v1 = BuiltinValueOps.insertCoin(policyA, tokenA, maxVal, BuiltinValue.empty)
        val v2 = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(1), BuiltinValue.empty)

        assertThrows[BuiltinException] {
            BuiltinValueOps.unionValue(v1, v2)
        }
    }

    test("scaleValue rejects overflow") {
        val largeVal = BigInt(2).pow(100)
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, largeVal, BuiltinValue.empty)

        assertThrows[BuiltinException] {
            BuiltinValueOps.scaleValue(BigInt(2).pow(30), value)
        }
    }

    test("fromData rejects invalid key length") {
        val longKey = ByteString.fromArray(Array.fill(33)(0.toByte))
        val invalidData = Data.Map(
          scalus.cardano.onchain.plutus.prelude.List(
            (Data.B(longKey), Data.Map(scalus.cardano.onchain.plutus.prelude.List.empty))
          )
        )

        assertThrows[BuiltinException] {
            BuiltinValueOps.fromData(invalidData)
        }
    }

    test("fromData rejects invalid Data structure") {
        assertThrows[BuiltinException] {
            BuiltinValueOps.fromData(Data.I(BigInt(42)))
        }
    }

    test("BuiltinValue Constant integration") {
        val value = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(100), BuiltinValue.empty)
        val constant = Constant.BuiltinValue(value)

        assert(constant.tpe == scalus.uplc.DefaultUni.BuiltinValue)
    }
}
