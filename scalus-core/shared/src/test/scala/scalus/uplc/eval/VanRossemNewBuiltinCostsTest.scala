package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite

/** [[VanRossemNewBuiltinCosts]] is generated from `builtinCostModelD.json` and
  * `builtinCostModelE.json`. These literals are what the machine actually charges for the van
  * Rossem builtins when a pre-van-Rossem cost model is in force, so they must not drift from the
  * vendored Plutus reference models. Regenerate the object if this fails: the resources changed.
  */
class VanRossemNewBuiltinCostsTest extends AnyFunSuite {

    private val d = BuiltinCostModel.vanRossemReferenceD
    private val e = BuiltinCostModel.vanRossemReferenceE

    private def check(name: String, literal: Any, fromD: Any, fromE: Any): Unit =
        test(s"$name matches both vendored reference models") {
            assert(literal == fromD, s"$name differs from builtinCostModelD.json")
            assert(literal == fromE, s"$name differs from builtinCostModelE.json")
        }

    check("expModInteger", VanRossemNewBuiltinCosts.expModInteger, d.expModInteger, e.expModInteger)
    check("dropList", VanRossemNewBuiltinCosts.dropList, d.dropList, e.dropList)
    check("lengthOfArray", VanRossemNewBuiltinCosts.lengthOfArray, d.lengthOfArray, e.lengthOfArray)
    check("listToArray", VanRossemNewBuiltinCosts.listToArray, d.listToArray, e.listToArray)
    check("indexArray", VanRossemNewBuiltinCosts.indexArray, d.indexArray, e.indexArray)
    check(
      "bls12_381_G1_multiScalarMul",
      VanRossemNewBuiltinCosts.bls12_381_G1_multiScalarMul,
      d.bls12_381_G1_multiScalarMul,
      e.bls12_381_G1_multiScalarMul
    )
    check(
      "bls12_381_G2_multiScalarMul",
      VanRossemNewBuiltinCosts.bls12_381_G2_multiScalarMul,
      d.bls12_381_G2_multiScalarMul,
      e.bls12_381_G2_multiScalarMul
    )
    check("insertCoin", VanRossemNewBuiltinCosts.insertCoin, d.insertCoin, e.insertCoin)
    check("lookupCoin", VanRossemNewBuiltinCosts.lookupCoin, d.lookupCoin, e.lookupCoin)
    check("unionValue", VanRossemNewBuiltinCosts.unionValue, d.unionValue, e.unionValue)
    check("valueContains", VanRossemNewBuiltinCosts.valueContains, d.valueContains, e.valueContains)
    check("valueData", VanRossemNewBuiltinCosts.valueData, d.valueData, e.valueData)
    check("unValueData", VanRossemNewBuiltinCosts.unValueData, d.unValueData, e.unValueData)
    check("scaleValue", VanRossemNewBuiltinCosts.scaleValue, d.scaleValue, e.scaleValue)
}
