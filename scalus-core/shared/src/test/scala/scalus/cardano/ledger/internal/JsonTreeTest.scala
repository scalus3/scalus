package scalus.cardano.ledger.internal

import org.scalatest.funsuite.AnyFunSuite

import scalus.cardano.ledger.internal.JsonTree.{Arr, Bool, Null, Num, Obj, Str}

class JsonTreeTest extends AnyFunSuite {

    test("numbers keep the exact value of their text") {
        val tree = JsonTree.parse("""[0.123456789012345678, 9007199254740993, 1e-30, -7.21E-5]""")
        assert(
          tree == Arr(
            Vector(
              Num(BigDecimal("0.123456789012345678")),
              Num(BigDecimal(9007199254740993L)),
              Num(BigDecimal("1e-30")),
              Num(BigDecimal("-0.0000721"))
            )
          )
        )
    }

    test("objects keep their keys in document order, and a repeated key keeps its last value") {
        val tree = JsonTree.parse("""{"b": 1, "a": "x", "c": [true, null], "b": 2}""")
        assert(tree.obj.keys.toList == List("b", "a", "c"))
        assert(tree("b") == Num(2))
        assert(tree("a") == Str("x"))
        assert(tree("c") == Arr(Vector(Bool(true), Null)))
        assert(tree.get("missing").isEmpty)
    }

    test("render writes what parse reads") {
        val text = """{"n":9007199254740993,"d":0.0000721,"s":"q\"uote","e":{},"a":[],"z":null}"""
        assert(JsonTree.render(JsonTree.parse(text)) == text)
    }

    test("the macro parser builds the same tree, numbers exact") {
        val text = """{"n": 9007199254740993, "d": [0.123456789012345678, -7.21E-5, 1e-30],
                     |"s": "q\"uote", "o": {"t": true, "f": false, "z": null}, "e": {}, "a": []}""".stripMargin
        assert(JsonTree.parseInMacro(text) == JsonTree.parse(text))
        assert(JsonTree.parseInMacro(text)("n") == Num(BigDecimal(9007199254740993L)))
    }

    test("a missing key fails and names it") {
        val thrown = intercept[NoSuchElementException](JsonTree.parse("{}")("rho"))
        assert(thrown.getMessage == "key not found: rho")
    }

    test("a value of the wrong kind fails and says what it was") {
        val thrown = intercept[IllegalArgumentException](JsonTree.parse("\"0.3\"").num)
        assert(thrown.getMessage == "expected a JSON number, got a string")
        assert(JsonTree.parse("[]").get("x").isEmpty)
    }

    test("nesting past the bound fails instead of exhausting the stack") {
        val thrown = intercept[Exception](JsonTree.parse("[" * 100 + "]" * 100))
        assert(thrown.getMessage.contains("nested deeper"), thrown.getMessage)
    }

    test("malformed JSON and trailing text fail") {
        intercept[Exception](JsonTree.parse("""{"a": }"""))
        intercept[Exception](JsonTree.parse("""{"a": 1} x"""))
        intercept[Exception](JsonTree.parse("""[1, 2"""))
    }

    test("an object built in code renders its fields in order") {
        val tree = JsonTree.obj("z" -> JsonTree.num(1L), "a" -> JsonTree.obj())
        assert(tree.isInstanceOf[Obj])
        assert(JsonTree.render(tree) == """{"z":1,"a":{}}""")
    }
}
