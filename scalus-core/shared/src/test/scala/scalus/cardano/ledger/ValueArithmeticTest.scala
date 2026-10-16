package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

/** `Coin`, `MultiAsset` and `Value` arithmetic throws on a `Long` overflow instead of wrapping. */
class ValueArithmeticTest extends AnyFunSuite {

    private val policy: PolicyId = ScriptHash.fromHex("0" * 56)
    private val name = AssetName.fromString("token")
    private def tokens(amount: Long): MultiAsset = MultiAsset.asset(policy, name, amount)

    test("coin arithmetic within the Long range is unchanged") {
        assert(Coin(2) + Coin(3) == Coin(5))
        assert(Coin(2) - Coin(3) == Coin(-1))
        assert(-Coin(7) == Coin(-7))
        assert(Coin.ada(2) == Coin(2_000_000))
    }

    test("coin arithmetic throws instead of wrapping") {
        assertThrows[ArithmeticException](Coin(Long.MaxValue) + Coin(1))
        assertThrows[ArithmeticException](Coin(Long.MinValue) - Coin(1))
        assertThrows[ArithmeticException](-Coin(Long.MinValue))
        assertThrows[ArithmeticException](Coin.ada(Long.MaxValue / 1_000_000 + 1))
    }

    test("multi-asset arithmetic within the Long range is unchanged") {
        assert(tokens(2) + tokens(3) == tokens(5))
        assert(tokens(2) - tokens(2) == MultiAsset.zero)
        assert(-tokens(7) == tokens(-7))
    }

    test("multi-asset arithmetic throws instead of wrapping") {
        assertThrows[ArithmeticException](tokens(Long.MaxValue) + tokens(1))
        assertThrows[ArithmeticException](tokens(Long.MinValue) - tokens(1))
        assertThrows[ArithmeticException](-tokens(Long.MinValue))
    }

    test("value arithmetic throws instead of wrapping") {
        assertThrows[ArithmeticException](Value.lovelace(Long.MaxValue) + Value.lovelace(1))
        assertThrows[ArithmeticException](
          Value(Coin.zero, tokens(Long.MaxValue)) + Value(Coin.zero, tokens(1))
        )
        assertThrows[ArithmeticException](
          Value(Coin.zero, tokens(Long.MinValue)) - Value(Coin.zero, tokens(1))
        )
        assertThrows[ArithmeticException](
          Value.combine(Seq.fill(4)(Value(Coin.zero, tokens(1L << 62))))
        )
    }
}
