package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.serialization.flat
import scalus.serialization.flat.{DecoderState, EncoderState}

class Word64FlatTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private def roundTripFlat(value: Word64) =
        try {
            val enc = EncoderState(Word64.flatBytesCount(value))
            flat.encode(value, enc)
            val dec = DecoderState(enc.buffer)
            val decoded = flat.decode[Word64](dec)
            assert(decoded.toUnsignedString == value.toUnsignedString)
        } catch {
            case e: StackOverflowError => fail(s"$e at ${e.getStackTrace()(0)} ...")
        }

    test("round trip flat Word64.Zero"):
        roundTripFlat(Word64.Zero)

    test("round trip flat Word64.One"):
        roundTripFlat(Word64.One)

    test("round trip flat Word64(2)"):
        roundTripFlat(Word64(2))

    test("round trip flat Word64(10)"):
        roundTripFlat(Word64(10))

    test("round trip flat Word64(12345)"):
        roundTripFlat(Word64(12345))

    test("round trip flat Word64(Int.MaxValue)"):
        roundTripFlat(Word64(Int.MaxValue))

    test("round trip flat Word64(Long.MaxValue)"):
        roundTripFlat(Word64(Long.MaxValue))

    test("round trip flat Word64.MaxValue"):
        roundTripFlat(Word64.MaxValue)

    test("""round trip flat Word64.fromUnsignedString("9223372036854775808")"""):
        roundTripFlat(Word64.fromUnsignedString("9223372036854775808"))

    test("""round trip flat Word64.fromUnsignedString("12345678901234567890")"""):
        roundTripFlat(Word64.fromUnsignedString("12345678901234567890"))

    test("property: round trip flat for all Word64"):
        forAll(roundTripFlat)

}
