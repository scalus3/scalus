package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}
import scalus.uplc.eval.{BuiltinException, CekValue, NoLogger, PlutusVM}

/** Plutus semantics variants D and E (`ensurable`) denote `bls12_381_G{1,2}_scalarMul` with
  * `scalarMulE`, which rejects a scalar outside the signed 4096-bit range. Variants A, B and C use
  * the unbounded `scalarMul`.
  */
class Bls12381ScalarMulBoundTest extends AnyFunSuite {
    private val costModel = PlutusVM.makePlutusV3VM().machineParams.builtinCostModel
    private val msg = ByteString.fromString("p")
    private val dst = ByteString.fromString("DST")
    private val g1 = Constant.BLS12_381_G1_Element(platform.bls12_381_G1_hashToGroup(msg, dst))
    private val g2 = Constant.BLS12_381_G2_Element(platform.bls12_381_G2_hashToGroup(msg, dst))

    private val ub = (BigInt(1) << 4095) - 1
    private val lb = -(BigInt(1) << 4095)

    private def scalarMul(
        variant: BuiltinSemanticsVariant,
        group: String,
        scalar: BigInt
    ): CekValue = {
        val builtins = new CardanoBuiltins(costModel, platform, variant)
        val (runtime, point) =
            if group == "G1" then (builtins.Bls12_381_G1_scalarMul, g1)
            else (builtins.Bls12_381_G2_scalarMul, g2)
        runtime.f(NoLogger, Seq(CekValue.VCon(Constant.Integer(scalar)), CekValue.VCon(point)))
    }

    for group <- Seq("G1", "G2") do {
        for variant <- Seq(BuiltinSemanticsVariant.D, BuiltinSemanticsVariant.E) do {
            test(s"$group.scalarMul rejects an out-of-bounds scalar in variant $variant") {
                for scalar <- Seq(ub + 1, lb - 1) do {
                    val e = intercept[BuiltinException](scalarMul(variant, group, scalar))
                    assert(e.getMessage == s"Scalar exceeds 512-byte bound for $group.scalarMul")
                }
            }

            test(s"$group.scalarMul accepts the bound scalars in variant $variant") {
                scalarMul(variant, group, ub)
                scalarMul(variant, group, lb)
            }
        }

        for variant <- Seq(
              BuiltinSemanticsVariant.A,
              BuiltinSemanticsVariant.B,
              BuiltinSemanticsVariant.C
            )
        do {
            test(s"$group.scalarMul has no scalar bound in variant $variant") {
                scalarMul(variant, group, ub + 1)
                scalarMul(variant, group, lb - 1)
            }
        }
    }
}
