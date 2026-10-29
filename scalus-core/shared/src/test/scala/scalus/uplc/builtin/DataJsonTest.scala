package scalus.uplc.builtin

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.test.ArbitraryInstances

/** Pins the JSON encoding of [[Data]], which is public API and, through `Data.fromJson`, backs the
  * exported JavaScript function `applyDataArgToScript`.
  */
class DataJsonTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

    private def roundTrip(data: Data, json: String): Unit = {
        assert(Data.toJson(data) == json)
        assert(Data.fromJson(json) == data)
    }

    test("integers") {
        roundTrip(Data.I(42), """{"int":42}""")
        roundTrip(Data.I(0), """{"int":0}""")
        roundTrip(Data.I(-42), """{"int":-42}""")
        roundTrip(Data.I(BigInt("9999999999999")), """{"int":9999999999999}""")
    }

    test("integers beyond Double precision survive a round trip") {
        // The previous upickle reader took "int" through ujson's Double-valued `.num`, so
        // anything past 2^53 came back wrong.
        val big = BigInt(2).pow(64) + 1
        assert(Data.fromJson(Data.toJson(Data.I(big))) == Data.I(big))
    }

    test("integers longer than jsoniter's default digit limit survive a round trip") {
        // A 617-digit value, the size of an RSA-2048 modulus, which `expModInteger` exists to
        // work with. jsoniter's default bigIntDigitsLimit is 308.
        val huge = BigInt(10).pow(616) + 7
        assert(huge.toString.length == 617)
        assert(Data.fromJson(Data.toJson(Data.I(huge))) == Data.I(huge))
    }

    test("bytes") {
        roundTrip(Data.B(ByteString.fromHex("deadbeef")), """{"bytes":"deadbeef"}""")
        roundTrip(Data.B(ByteString.empty), """{"bytes":""}""")
    }

    test("lists") {
        roundTrip(
          Data.List(PList.from(scala.List(Data.I(1), Data.I(2)))),
          """{"list":[{"int":1},{"int":2}]}"""
        )
        roundTrip(Data.List(PList.from(scala.List.empty[Data])), """{"list":[]}""")
    }

    test("maps") {
        roundTrip(
          Data.Map(PList.from(scala.List(Data.I(1) -> Data.B(ByteString.fromHex("aa"))))),
          """{"map":[{"k":{"int":1},"v":{"bytes":"aa"}}]}"""
        )
        roundTrip(Data.Map(PList.from(scala.List.empty[(Data, Data)])), """{"map":[]}""")
    }

    test("constructors") {
        roundTrip(
          Data.Constr(0, PList.from(scala.List(Data.I(42)))),
          """{"constructor":0,"fields":[{"int":42}]}"""
        )
        roundTrip(
          Data.Constr(1, PList.from(scala.List.empty[Data])),
          """{"constructor":1,"fields":[]}"""
        )
    }

    test("nested structures") {
        val data = Data.Constr(
          0,
          PList.from(
            scala.List(
              Data.List(PList.from(scala.List(Data.I(1), Data.I(2)))),
              Data.Map(PList.from(scala.List(Data.B(ByteString.fromHex("abcd")) -> Data.I(100))))
            )
          )
        )
        val json =
            """{"constructor":0,"fields":[{"list":[{"int":1},{"int":2}]},""" +
                """{"map":[{"k":{"bytes":"abcd"},"v":{"int":100}}]}]}"""
        roundTrip(data, json)
    }

    test("object keys are accepted in any order") {
        assert(
          Data.fromJson("""{"fields":[{"int":1}],"constructor":2}""") ==
              Data.Constr(2, PList.from(scala.List(Data.I(1))))
        )
    }

    test("a constructor without fields decodes as one with no fields") {
        assert(
          Data.fromJson("""{"constructor":0}""") == Data.Constr(0, PList.from(scala.List.empty))
        )
    }

    test("indented output") {
        assert(Data.toJson(Data.I(1), 2).contains("\n"))
    }

    test("junk is rejected") {
        assertThrows[Exception](Data.fromJson("""{"nope":1}"""))
        assertThrows[Exception](Data.fromJson("""{}"""))
        assertThrows[Exception](Data.fromJson("""[]"""))
        // A fractional "int" is rejected rather than silently truncated, as the old reader did.
        assertThrows[Exception](Data.fromJson("""{"int":1.5}"""))
    }

    test("the output is that of the derived codec it replaced, indented or not") {
        forAll { (data: Data) =>
            val mirror = recursiveMirror(data)
            assert(Data.toJson(data) == writeToString(mirror)(using DataJson.codec))
            assert(
              Data.toJson(data, 2) ==
                  writeToString(mirror, WriterConfig.withIndentionStep(2))(using DataJson.codec)
            )
        }
    }

    /** The recursive mapping to [[DataJson]] that `toJson` wrote through before it stopped
      * recursing (#423), kept as an oracle.
      */
    private def recursiveMirror(data: Data): DataJson = data match
        case Data.Constr(constructor, args) =>
            DataJson(
              constructor = Some(constructor),
              fields = Some(args.toScalaList.map(recursiveMirror))
            )
        case Data.Map(entries) =>
            DataJson(map =
                Some(
                  entries.toScalaList.map((k, v) =>
                      DataJsonMapEntry(recursiveMirror(k), recursiveMirror(v))
                  )
                )
            )
        case Data.List(values) => DataJson(list = Some(values.toScalaList.map(recursiveMirror)))
        case Data.I(value)     => DataJson(int = Some(value))
        case Data.B(value)     => DataJson(bytes = Some(value.toHex))
}
