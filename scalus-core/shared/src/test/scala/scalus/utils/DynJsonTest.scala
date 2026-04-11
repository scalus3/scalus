package scalus.utils

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.DynJson.*

class DynJsonTest extends AnyFunSuite {

    private val json = ujson
        .read("""{
        "name": "Alice",
        "age": 30,
        "active": true,
        "scores": [10, 20, 30],
        "address": {
            "city": "Wonderland",
            "zip": "12345"
        },
        "matrix": [[1, 2], [3, 4]],
        "empty": null
    }""")
        .dyn

    test("selectDynamic accesses object fields") {
        assert(json.name.str == "Alice")
        assert(json.age.num == 30.0)
        assert(json.active.bool == true)
    }

    test("selectDynamic chains for nested objects") {
        assert(json.address.city.str == "Wonderland")
        assert(json.address.zip.str == "12345")
    }

    test("applyDynamic accesses field and indexes array") {
        assert(json.scores(0).num == 10.0)
        assert(json.scores(1).num == 20.0)
        assert(json.scores(2).num == 30.0)
    }

    test("apply indexes into array value") {
        val scores = json.scores
        assert(scores(1).num == 20.0)
    }

    test("chaining field access, array index, and nested arrays") {
        assert(json.matrix(0)(1).num == 2.0)
        assert(json.matrix(1)(0).num == 3.0)
    }

    test("arr returns ujson.Arr") {
        assert(json.scores.arr.value.length == 3)
    }

    test("obj returns ujson.Obj") {
        assert(json.address.obj.value.size == 2)
    }

    test("isNull checks null values") {
        assert(json.empty.isNull)
        assert(!json.name.isNull)
    }

    test("strOpt returns Some for strings, None for others") {
        assert(json.name.strOpt == Some("Alice"))
        assert(json.age.strOpt == None)
    }

    test("numOpt returns Some for numbers, None for others") {
        assert(json.age.numOpt == Some(30.0))
        assert(json.name.numOpt == None)
    }

    test("boolOpt returns Some for booleans, None for others") {
        assert(json.active.boolOpt == Some(true))
        assert(json.name.boolOpt == None)
    }

    test("value field exposes underlying ujson.Value") {
        assert(json.name.value == ujson.Str("Alice"))
    }

    test("toString renders JSON") {
        assert(json.address.toString == """{"city":"Wonderland","zip":"12345"}""")
    }

    test("dyn extension method works on ujson.Value") {
        val j = ujson.read("""{"x": 1}""")
        assert(j.dyn.x.num == 1.0)
    }

    test("missing field throws exception") {
        assertThrows[Exception] {
            json.nonexistent.str
        }
    }

    test("wrong type accessor throws exception") {
        assertThrows[Exception] {
            json.name.num
        }
    }
}
