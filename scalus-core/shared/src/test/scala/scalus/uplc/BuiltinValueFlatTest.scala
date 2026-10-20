package scalus.uplc

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.{BuiltinValue, ByteString}
import scalus.uplc.eval.BuiltinValueOps
import scalus.serialization.flat.{listFlat, DecoderState, EncoderState, Flat, FlatDecodingError, given}

import scala.collection.immutable.List

/** Audit finding F2: a `con value` (BuiltinValue) constant must flat-encode in Plutus's structural
  * format – a list of `(currency, list of (token, quantity))` (`PlutusCore/Value.hs`, `Flat Value`)
  * – not as a `Flat[Data]` CBOR-style blob.
  */
class BuiltinValueFlatTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    // The real `con value` flat codec (the BuiltinValue arm of flatForUni).
    private val valueFlat: Flat[BuiltinValue] =
        DefaultUni.flatForUni(DefaultUni.BuiltinValue).asInstanceOf[Flat[BuiltinValue]]

    private def toBytes[A](a: A)(using fl: Flat[A]): Array[Byte] =
        val enc = EncoderState(fl.bitSize(a) / 8 + 1)
        fl.encode(a, enc)
        enc.result

    private def roundtrip(v: BuiltinValue): BuiltinValue =
        valueFlat.decode(DecoderState(toBytes(v)(using valueFlat)))

    private val policyA = ByteString.fromHex("aabbccdd")
    private val tokenA = ByteString.fromString("TokenA")
    private val tokenB = ByteString.fromString("TokenB")

    // The crux of F2: the wire bytes must be the structural entry-list encoding, not a Data blob.
    test("con value flat-encodes as the structural entry list, not a Data blob") {
        val v = BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(5), BuiltinValue.empty)
        val entries: List[(ByteString, List[(ByteString, BigInt)])] =
            List((policyA, List((tokenA, BigInt(5)))))
        assert(
          toBytes(v)(using valueFlat).sameElements(toBytes(entries)),
          "BuiltinValue flat encoding must equal the structural (currency, [(token, qty)]) list encoding"
        )
    }

    test("round-trips empty / single / multi-token / negative quantities") {
        val empty = BuiltinValue.empty
        val single =
            BuiltinValueOps.insertCoin(policyA, tokenA, BigInt(1000000), BuiltinValue.empty)
        val multi =
            BuiltinValueOps.insertCoin(
              policyA,
              tokenB,
              BigInt(-7),
              BuiltinValueOps.insertCoin(
                policyA,
                tokenA,
                BigInt(100),
                BuiltinValueOps.insertCoin(
                  ByteString.empty,
                  ByteString.empty,
                  BigInt(42),
                  BuiltinValue.empty
                )
              )
            )
        for v <- List(empty, single, multi) do
            assert(roundtrip(v) == v, s"round-trip failed for $v")
    }

    private val keyGen: Gen[ByteString] =
        Gen.choose(0, 32)
            .flatMap(n => Gen.containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte]))
            .map(ByteString.fromArray)
    private val quantityGen: Gen[BigInt] =
        Gen.oneOf(
          Gen.choose(-1000L, 1000L).map(BigInt(_)),
          Gen.const(BigInt(2).pow(127) - 1),
          Gen.const(-BigInt(2).pow(127))
        ).suchThat(_ != 0)

    // insertCoin keeps a value canonical, so any value built from it must survive the codec.
    test("round-trips any canonical value") {
        val valueGen = Gen.listOf(Gen.zip(keyGen, keyGen, quantityGen)).map { entries =>
            entries.foldLeft(BuiltinValue.empty) { case (v, (c, t, q)) =>
                BuiltinValueOps.insertCoin(c, t, q, v)
            }
        }
        forAll(valueGen) { v => assert(roundtrip(v) == v) }
    }

    /** Flat-encodes `entries` as given, unsorted and unmerged, and decodes them as a `con value`.
      */
    private def decodeEntries(
        entries: List[(ByteString, List[(ByteString, BigInt)])]
    ): BuiltinValue =
        valueFlat.decode(DecoderState(toBytes(entries)))

    // The rules of Plutus's flat decoder for Value (`buildValueWith`, `K`, `Quantity` in
    // PlutusCore/Value.hs): the decoder must reject, not repair.
    test("rejects an entry list that is not canonical, as Plutus does") {
        val a = ByteString.fromHex("aa")
        val b = ByteString.fromHex("bb")
        val tooLong = ByteString.fromArray(Array.fill[Byte](33)(1))
        val rejected = List(
          "currency symbols out of order" -> List(
            (b, List((a, BigInt(1)))),
            (a, List((a, BigInt(1))))
          ),
          "duplicate currency symbol" -> List((a, List((a, BigInt(1)))), (a, List((b, BigInt(1))))),
          "token names out of order" -> List((a, List((b, BigInt(1)), (a, BigInt(1))))),
          "duplicate token name" -> List((a, List((a, BigInt(1)), (a, BigInt(2))))),
          "empty token list" -> List((a, List())),
          "zero quantity" -> List((a, List((a, BigInt(0))))),
          "currency symbol over 32 bytes" -> List((tooLong, List((a, BigInt(1))))),
          "token name over 32 bytes" -> List((a, List((tooLong, BigInt(1))))),
          "quantity above 2^127 - 1" -> List((a, List((a, BigInt(2).pow(127))))),
          "quantity below -2^127" -> List((a, List((a, -BigInt(2).pow(127) - 1))))
        )
        for (reason, entries) <- rejected do
            withClue(reason)(assertThrows[FlatDecodingError](decodeEntries(entries)))
    }
}
