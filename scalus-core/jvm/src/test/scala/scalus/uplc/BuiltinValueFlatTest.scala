package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{BuiltinValue, ByteString}
import scalus.uplc.eval.BuiltinValueOps
import scalus.serialization.flat.{listFlat, DecoderState, EncoderState, Flat, given}

import scala.collection.immutable.List

/** Audit finding F2: a `con value` (BuiltinValue) constant must flat-encode in Plutus's structural
  * format — a list of `(currency, list of (token, quantity))` (`PlutusCore/Value.hs`, `Flat Value`)
  * — not as a `Flat[Data]` CBOR-style blob.
  */
class BuiltinValueFlatTest extends AnyFunSuite {

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
}
