package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}
import scalus.uplc.eval.{BuiltinException, CekValue, NoLogger, PlutusVM}

/** Plutus semantics variants D and E (`ensurable`) denote `bls12_381_G{1,2}_scalarMul` with
  * `scalarMulE`, which rejects a scalar outside the signed 4096-bit range. Variants A, B and C use
  * the unbounded `scalarMul`.
  */
class BLS12_381ScalarMulBoundTest extends AnyFunSuite {
    private val costModel = PlutusVM.makePlutusV3VM().machineParams.builtinCostModel
    private val msg = ByteString.fromString("p")
    private val dst = ByteString.fromString("DST")

    private val ub = (BigInt(1) << 4095) - 1
    private val lb = -(BigInt(1) << 4095)

    /** One group: its name in Plutus messages, its builtin, a point, and the unbounded platform
      * scalarMul that the builtin must agree with whenever it accepts the scalar.
      */
    private case class Group(
        name: String,
        runtime: CardanoBuiltins => BuiltinRuntime,
        point: Constant,
        expected: BigInt => Constant
    )

    private val groups = {
        val p1 = platform.bls12_381_G1_hashToGroup(msg, dst)
        val p2 = platform.bls12_381_G2_hashToGroup(msg, dst)
        Seq(
          Group(
            "G1",
            _.Bls12_381_G1_scalarMul,
            Constant.BLS12_381_G1_Element(p1),
            s => Constant.BLS12_381_G1_Element(platform.bls12_381_G1_scalarMul(s, p1))
          ),
          Group(
            "G2",
            _.Bls12_381_G2_scalarMul,
            Constant.BLS12_381_G2_Element(p2),
            s => Constant.BLS12_381_G2_Element(platform.bls12_381_G2_scalarMul(s, p2))
          )
        )
    }

    private def scalarMul(
        variant: BuiltinSemanticsVariant,
        group: Group,
        scalar: BigInt
    ): CekValue =
        group
            .runtime(new CardanoBuiltins(costModel, platform, variant))
            .f(NoLogger, Seq(CekValue.VCon(Constant.Integer(scalar)), CekValue.VCon(group.point)))

    private def assertComputes(
        variant: BuiltinSemanticsVariant,
        group: Group,
        scalar: BigInt
    ): Unit =
        assert(scalarMul(variant, group, scalar) == CekValue.VCon(group.expected(scalar)))

    for group <- groups do {
        for variant <- Seq(BuiltinSemanticsVariant.D, BuiltinSemanticsVariant.E) do {
            test(s"${group.name}.scalarMul rejects an out-of-bounds scalar in variant $variant") {
                for scalar <- Seq(ub + 1, lb - 1) do {
                    val e = intercept[BuiltinException](scalarMul(variant, group, scalar))
                    assert(
                      e.getMessage == s"Scalar exceeds 512-byte bound for ${group.name}.scalarMul"
                    )
                }
            }

            test(s"${group.name}.scalarMul computes the bound scalars in variant $variant") {
                assertComputes(variant, group, ub)
                assertComputes(variant, group, lb)
            }
        }

        for variant <- Seq(
              BuiltinSemanticsVariant.A,
              BuiltinSemanticsVariant.B,
              BuiltinSemanticsVariant.C
            )
        do {
            test(s"${group.name}.scalarMul has no scalar bound in variant $variant") {
                assertComputes(variant, group, ub + 1)
                assertComputes(variant, group, lb - 1)
            }
        }
    }
}
