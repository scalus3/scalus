package scalus.utils.scalajs.internal

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.JsValue

import scala.scalajs.js

class NumbersTest extends AnyFunSuite {

    private def typeError(body: => Any): Unit =
        intercept[js.JavaScriptException](body).exception match
            case _: js.TypeError => ()
            case other           => fail(s"expected TypeError, got $other")

    test("Long and bigint convert exactly, up to the 64-bit bounds") {
        for n <- Seq(0L, 1L, -1L, (1L << 53) + 1, Long.MaxValue, Long.MinValue) do
            assert(n.toJsBigInt.toString == n.toString)
            assert(longOf(n.toJsBigInt, "n") == n)
        assert(
          BigInt(
            "123456789012345678901234567890"
          ).toJsBigInt.toString == "123456789012345678901234567890"
        )
    }

    test("a safe-integer number is read too, and anything else is a TypeError") {
        assert(longOf(42, "n") == 42L)
        typeError(longOf(js.BigInt("9223372036854775808"), "n"))
        typeError(longOf(1.5, "n"))
        typeError(longOf(2e21, "n"))
        typeError(longOf("42", "n"))
        typeError(longOf(js.undefined, "n"))
    }

    test("a lovelace amount past 64 bits is rejected, not wrapped") {
        // `BigInt(x.toString).toLong` used to wrap this to a negative number.
        typeError(new JsValue(js.BigInt("18446744073709551616")))
        assert(new JsValue(js.BigInt("9223372036854775807")).coin.toString == "9223372036854775807")
    }
}
