package scalus.uplc.eval

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Constant.given
import scalus.uplc.Term.*
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.builtin.{ByteString, Data}
import scalus.serialization.flat.{EncoderState, Flat, Natural, given}
import scalus.utils.Hex

import scalus.utils.scalajs.internal.*
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

class JUplcTest extends AnyFunSuite {

    private val program = λ("x")(vr"x").plutusV1.deBruijnedProgram

    private def typeError(body: => Any): js.TypeError =
        intercept[js.JavaScriptException](body).exception match
            case e: js.TypeError => e
            case other           => fail(s"expected TypeError, got $other")

    test("decodes CBOR Data arguments and applies them left to right") {
        val data = JUplc.decodeArgs(js.Array("182a", "42182a"))
        assert(data == List(Data.I(BigInt(42)), Data.B(ByteString.fromHex("182a"))))

        val applied = JUplc.applyAll(program, data)
        assert(applied.version == program.version)
        assert(applied.term == (program.term $ data(0) $ data(1)))
    }

    test("accepts raw flat, single and double CBOR, as uppercase hex and Uint8Array views") {
        val single = Cbor.encode(program.flatEncoded).toByteArray
        val double = Cbor.encode(single).toByteArray
        val padded = Array[Byte](0x55.toByte) ++ double ++ Array[Byte](0x66.toByte)
        val sliced = new Uint8Array(byteArray2Int8Array(padded).buffer, 1, double.length)

        List[scala.scalajs.js.Any](
          Hex.bytesToHex(program.flatEncoded),
          program.flatEncoded.toUint8Array,
          Hex.bytesToHex(single).toUpperCase,
          Hex.bytesToHex(double),
          single.toUint8Array,
          sliced
        ).foreach { input =>
            val decoded = JUplc.decodeScript(input)
            assert(decoded.alphaEq(program))
            assert(decoded.flatEncoded.sameElements(program.flatEncoded))
        }
    }

    test("rejects malformed representations and identifies the failing argument") {
        List[(String, scala.scalajs.js.Any)](
          "empty" -> "",
          "odd length" -> "0",
          "leading space" -> " 40",
          "0x prefix" -> "0x40",
          "not a program" -> "00"
        ).foreach { (label, input) =>
            withClue(label) { typeError { JUplc.decodeScript(input) } }
        }
        val failure = typeError(JUplc.decodeArgs(js.Array("182a", "0x00")))
        assert(failure.message.startsWith("args[1]"))
        typeError { JUplc.decodeArgs("not an array") }
    }

    test("preserves Data constructor boundaries, duplicates, and large integers") {
        val data = JUplc.decodeArgs(
          js.Array(
            "d866821b7fffffffffffffff80",
            "d866821b800000000000000080",
            "d866821bffffffffffffffff80",
            "d866820680",
            "d866821b000000000000008080",
            "a200010002",
            "c249010000000000000000"
          )
        )
        assert(data(0).toString.startsWith("<9223372036854775807"))
        assert(data(1).toString.startsWith("<9223372036854775808"))
        assert(data(2).toString.startsWith("<18446744073709551615"))
        assert(data(3).toString.startsWith("<6,"))
        assert(data(4).toString.startsWith("<128,"))
        assert(data(5).toString == "{0: 1, 0: 2}")
        assert(data(6) == Data.I(BigInt(1) << 64))
        typeError {
            JUplc.decodeArgs(js.Array("d86682c24901000000000000000080"))
        }
    }

    test(
      "rejects unsupported constants and truncated Flat byte-array chunks"
    ) {
        typeError { JUplc.decodeScript("450100004a81") }
        List("47010000488102aa", "48010000488101aa").foreach { script =>
            typeError { JUplc.decodeScript(script) }
        }
    }

    test("rejects Data shape boundaries and strict Flat completion") {
        val decoded = JUplc.decodeArgs(
          js.Array("d866820780", "d86682187f80", "d8669f0080ff", "c349010000000000000000")
        )
        assert(
          decoded.map(_.toString) == List(
            "<7, []>",
            "<127, []>",
            "<0, []>",
            s"-${(BigInt(1) << 64) + 1}"
          )
        )
        List("d8668100", "d86682c24901000000000000000080").foreach { value =>
            typeError { JUplc.decodeArgs(js.Array(value)) }
        }
    }

    test("reads the program without vetting what it reads") {
        // Whether a chain would admit the script is `PlutusScript.isWellFormed`'s question, asked
        // of a whole transaction elsewhere. These four all decode.
        val trailing = Cbor.encode(program.flatEncoded ++ Array[Byte](0)).toByteArray
        assert(JUplc.decodeScript(trailing.toUint8Array).alphaEq(program))

        val badPadding = program.flatEncoded.clone()
        badPadding(badPadding.length - 1) = 2
        assert(
          JUplc
              .decodeScript(JsCbor.encode(badPadding))
              .alphaEq(program)
        )

        // A version outside the two the ledger allows is not this layer's to refuse.
        val encoder = EncoderState(20)
        summon[Flat[Natural]].encode(Natural(BigInt(Int.MaxValue) + 1), encoder)
        summon[Flat[Natural]].encode(Natural(0), encoder)
        summon[Flat[Natural]].encode(Natural(0), encoder)
        summon[Flat[Term]].encode(Term.Error(), encoder)
        encoder.filler()
        assert(
          JUplc
              .decodeScript(JsCbor.encode(encoder.result))
              .term == Term.Error()
        )

        // Every CBOR byte-string layer is stripped, since a program never starts with a
        // byte-string header, so even a third wrap reaches the program.
        val single = Cbor.encode(program.flatEncoded).toByteArray
        val triple = Cbor.encode(Cbor.encode(single).toByteArray).toByteArray
        assert(JUplc.decodeScript(triple.toUint8Array).alphaEq(program))
    }

    test("the primitives change the envelope and never the script's own bytes") {
        // A script hashes over exactly these bytes, so a conversion that re-encoded from the
        // decoded program could hand back a script with a different hash. Non-canonical padding
        // is the cheapest input where re-encoding and passing through differ.
        val quirky = program.flatEncoded.clone()
        quirky(quirky.length - 1) = 2
        assert(!quirky.sameElements(program.flatEncoded))
        val single = Cbor.encode(quirky).toByteArray
        val double = Cbor.encode(single).toByteArray

        for input <- Seq(quirky, single, double) do
            assert(JUplc.decodeToFlat(input.toUint8Array).toByteArray.sameElements(quirky))
        assert(JCbor.wrapBytes(quirky.toUint8Array).toByteArray.sameElements(single))
        assert(JCbor.wrapBytes(single.toUint8Array).toByteArray.sameElements(double))
        assert(JCbor.unwrapBytes(double.toUint8Array).toByteArray.sameElements(single))
        assert(JCbor.unwrapBytes(single.toUint8Array).toByteArray.sameElements(quirky))
        typeError(JCbor.unwrapBytes(Array[Byte](0).toUint8Array))
    }

    test("hex round-trips, in either case, and rejects what is not hex") {
        val bytes = Array[Byte](0, 1, -1, 127, -128)
        assert(JHex.bytesToHex(bytes.toUint8Array) == "0001ff7f80")
        assert(JHex.hexToBytes("0001FF7f80").toByteArray.sameElements(bytes))
        typeError(JHex.hexToBytes("abc"))
        typeError(JHex.hexToBytes("zz"))
        typeError(JHex.hexToBytes(42))
    }

    test("applyParamsToScript is the composition, and what applyDataArgToScript returned") {
        val double = Cbor.encode(Cbor.encode(program.flatEncoded).toByteArray).toByteArray
        val composed = JHex.bytesToHex(
          JCbor.wrapBytes(
            JCbor.wrapBytes(
              JUplc.applyArgs(JUplc.decodeToFlat(double.toUint8Array), js.Array("182a"))
            )
          )
        )
        for script <- Seq[js.Any](double.toUint8Array, program.flatEncoded.toUint8Array) do
            assert(JUplc.applyParamsToScript(script, js.Array("182a")) == composed)
        assert(
          JScalus.applyDataArgToScript(Hex.bytesToHex(double), "{\"int\":42}") == composed
        )
        typeError(JUplc.applyParamsToScript(double.toUint8Array, js.Array("zz")))
        val failure = typeError(JUplc.applyParamsToScript(double.toUint8Array, js.Array("0x")))
        assert(failure.message.startsWith("params[0]"), failure.message)
    }

    test("exported methods work detached from their object") {
        val flat = program.flatEncoded.toUint8Array
        val detached = JUplc
            .asInstanceOf[js.Dynamic]
            .applyArgs
            .asInstanceOf[js.Function2[
              Uint8Array,
              js.Array[String],
              Uint8Array
            ]]
        assert(detached(flat, js.Array()).toByteArray.sameElements(program.flatEncoded))
    }

    test("rejects wrong JS inputs and preserves caller ownership") {
        List[scala.scalajs.js.Any](js.Dynamic.literal(), 1.asInstanceOf[js.Any], js.Array())
            .foreach { input =>
                typeError { JUplc.decodeScript(input) }
            }
        typeError { JUplc.decodeArgs(js.undefined) }
        val script = program.flatEncoded.toUint8Array
        val before = script.toArray.toList
        val first = JUplc.applyArgs(script, js.Array())
        val second = JUplc.applyArgs(script, js.Array())
        assert(!(first eq second)); assert(script.toArray.toList == before)
    }
}
