package scalus.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}

/** `Utf8.isWellFormed` is written out so that every platform decides alike; on the JVM it must
  * agree with the JDK's strict decoder, which implements the same Table 3-7 of the Unicode
  * standard.
  */
class Utf8Test extends AnyFunSuite with ScalaCheckPropertyChecks {

    // The JDK's decoder tells malformed input only by throwing.
    private def jdkAccepts(bytes: Array[Byte]): Boolean =
        try
            StandardCharsets.UTF_8
                .newDecoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(ByteBuffer.wrap(bytes))
            true
        catch case _: CharacterCodingException => false

    // bytes drawn from the lead and boundary values of every range of Table 3-7, so that random
    // sequences reach the overlong, surrogate and above-U+10FFFF cases
    private val interesting = Seq(0x00, 0x41, 0x7f, 0x80, 0x8f, 0x90, 0x9f, 0xa0, 0xbf, 0xc0, 0xc1,
      0xc2, 0xdf, 0xe0, 0xe1, 0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf3, 0xf4, 0xf5, 0xff)

    test("agrees with the JDK's strict decoder on sequences of boundary bytes") {
        val bytes = Gen.listOf(Gen.oneOf(interesting)).map(_.map(_.toByte).toArray)
        forAll(bytes, minSuccessful(20000))(b => assert(Utf8.isWellFormed(b) == jdkAccepts(b)))
    }

    test("agrees with the JDK's strict decoder on arbitrary bytes") {
        forAll(minSuccessful(5000))((b: Array[Byte]) =>
            assert(Utf8.isWellFormed(b) == jdkAccepts(b))
        )
    }

    test("accepts the encoding of any text") {
        val text = Gen.listOf(Gen.oneOf("a", "é", "€", "𝄞", "\u0000")).map(_.mkString)
        forAll(text)(s => assert(Utf8.isWellFormed(s.getBytes(StandardCharsets.UTF_8))))
    }
}
