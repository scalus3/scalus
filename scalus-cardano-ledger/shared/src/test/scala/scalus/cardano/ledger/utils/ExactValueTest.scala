package scalus.cardano.ledger
package utils

import org.scalatest.funsuite.AnyFunSuite

class ExactValueTest extends AnyFunSuite {

    private val policy: PolicyId = ScriptHash.fromHex("0" * 56)
    private val name = AssetName.fromString("token")
    private def tokens(amount: Long): Value =
        Value(Coin.zero, MultiAsset.asset(policy, name, amount))

    test("sums past Long.MaxValue are exact") {
        val sum = ExactValue.sum(Seq.fill(4)(tokens(1L << 62) + Value.lovelace(Long.MaxValue)))
        assert(sum.coin == BigInt(Long.MaxValue) * 4)
        assert(sum.assets(policy)(name) == BigInt(2).pow(64))
        assert(sum - ExactValue.sum(Seq.fill(4)(tokens(1L << 62))) == ExactValue.coin(sum.coin))
    }

    test("amounts that cancel out are dropped, so equal values compare equal") {
        val value = Value(Coin(5), MultiAsset.asset(policy, name, 3))
        assert(ExactValue(value) - ExactValue(value) == ExactValue.zero)
        assert(ExactValue(value) + ExactValue(tokens(-3)) == ExactValue.coin(5))
    }

    test("toValue returns a value that fits and throws for one that does not") {
        val value = Value(Coin(5), MultiAsset.asset(policy, name, 3))
        assert((ExactValue(value) + value).toValue == value + value)
        val tooBig = ExactValue(tokens(Long.MaxValue)) + tokens(1)
        assertThrows[ArithmeticException](tooBig.toValue)
    }

    test("toClampedValue clamps each amount to the Long range") {
        val tooBig = ExactValue(Value.lovelace(Long.MaxValue) + tokens(Long.MinValue)) +
            Value.lovelace(1) - ExactValue(tokens(1))
        assert(tooBig.toClampedValue == Value.lovelace(Long.MaxValue) + tokens(Long.MinValue))
    }
}
