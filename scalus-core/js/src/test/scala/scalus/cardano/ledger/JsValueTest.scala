package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

import scala.scalajs.js

class JsValueTest extends AnyFunSuite {

    test("Value.ada exposes lovelace as a BigInt and round-trips to the Scala value") {
        val v = JsValue.ada(js.BigInt("10"))
        assert(v.coin.toString == "10000000")
        assert(v.assets.length == 0)
        assert(JsValue.wrap(v.underlying).coin.toString == "10000000")
    }

    test("assets surface policy id, asset name and quantity as hex and BigInt") {
        val policy = "0" * 56
        val name = "abcd"
        val v = new JsValue(js.BigInt("0"), js.Array(new JsAsset(policy, name, js.BigInt("5"))))
        assert(v.assets.length == 1)
        val a = v.assets(0)
        assert(a.policyId == policy)
        assert(a.assetName == name)
        assert(a.quantity.toString == "5")
        assert(a.unit == policy + name)
    }

    test("plus adds coin and merges assets") {
        val sum = JsValue.ada(js.BigInt("2")).plus(JsValue.ada(js.BigInt("3")))
        assert(sum.coin.toString == "5000000")

        val policy = "0" * 56
        def five =
            new JsValue(js.BigInt("1"), js.Array(new JsAsset(policy, "abcd", js.BigInt("5"))))
        def other = new JsValue(js.BigInt("1"), js.Array(new JsAsset(policy, "ef", js.BigInt("7"))))
        val merged = five.plus(five).plus(other)
        assert(merged.coin.toString == "3")
        // the same asset sums; a different name under the same policy stays a separate entry
        assert(
          merged.assets.map(a => a.unit -> a.quantity.toString).toList.sorted == List(
            (policy + "abcd") -> "10",
            (policy + "ef") -> "7"
          ).sorted
        )
    }

    test("wrap stores the ledger value by reference, so a round trip copies nothing") {
        val value = Value(Coin(42L))
        assert(JsValue.wrap(value).underlying eq value)
    }

    test("toObject yields own enumerable properties, which the handle does not") {
        val v = JsValue.ada(js.BigInt("1"))
        assert(js.Object.keys(v).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = v.toObject()
        assert(js.Object.keys(plain).toSet == Set("coin", "assets"))
        assert(plain.coin.toString == "1000000")
    }

    test("an Asset handle exposes nothing either, and toObject() carries every field") {
        val policy = "0" * 56
        val a = new JsAsset(policy, "abcd", js.BigInt("5"))
        assert(js.Object.keys(a).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = a.toObject()
        assert(js.Object.keys(plain).toSet == Set("policyId", "assetName", "quantity", "unit"))
        assert(plain.policyId == policy)
        assert(plain.assetName == "abcd")
        assert(plain.quantity.toString == "5")
        assert(plain.unit == policy + "abcd")
    }

    test("the public constructor takes lovelace") {
        assert(new JsValue(js.BigInt("250")).coin.toString == "250")
    }
}
