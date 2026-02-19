package scalus.cardano.onchain.plutus.prelude.crypto.accumulator

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.crypto.accumulator.BilinearAccumulatorProver.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm

class G1AccumulatorTest extends AnyFunSuite, EvalTestKit {

    import scalus.cardano.onchain.plutus.prelude.List as PList

    private def toScalusList[A](vec: Vector[A]): PList[A] =
        vec.foldRight(PList.Nil: PList[A])((elem, acc) => PList.Cons(elem, acc))

    // Test-only tau. In production, tau must be random and destroyed after setup.
    private val tau: BigInt = BigInt("12345678901234567890")

    test("G1 membership: single element subset of 5-element set") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30), BigInt(40), BigInt(50))
        val subset = Vector(BigInt(20))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG1(setup, fullSet)
        val proof = membershipProofG1(setup, fullSet, subset)
        assert(verifyMembershipG1(setup, acc, subset, proof))
    }

    test("G1 membership: multi-element subset of 10-element set") {
        val fullSet = (1 to 10).map(i => BigInt(i * 100)).toVector
        val subset = Vector(BigInt(200), BigInt(500), BigInt(800))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG1(setup, fullSet)
        val proof = membershipProofG1(setup, fullSet, subset)
        assert(verifyMembershipG1(setup, acc, subset, proof))
    }

    test("G1 membership: full set as subset") {
        val fullSet = Vector(BigInt(1), BigInt(2), BigInt(3))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG1(setup, fullSet)
        val proof = membershipProofG1(setup, fullSet, fullSet)
        assert(verifyMembershipG1(setup, acc, fullSet, proof))
    }

    test("G1 non-membership: single disjoint element") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(99))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulateG1(setup, fullSet)
        val proof = nonMembershipProofG1(setup, fullSet, disjointSet)
        assert(verifyNonMembershipG1(setup, acc, disjointSet, proof))
    }

    test("G1 non-membership: multiple disjoint elements") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(40), BigInt(50))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulateG1(setup, fullSet)
        val proof = nonMembershipProofG1(setup, fullSet, disjointSet)
        assert(verifyNonMembershipG1(setup, acc, disjointSet, proof))
    }

    test("G1 on-chain UPLC membership pairing verification") {
        given compilerOptions: Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true
        )

        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val subset = Vector(BigInt(10), BigInt(20))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG1(setup, fullSet)
        val proof = membershipProofG1(setup, fullSet, subset)
        // Compute the G2 commitment offchain
        val commitment = scalus.cardano.onchain.plutus.prelude.crypto.accumulator.Poly
            .getG2Commitment(
              toScalusList(setup.g2Powers.take(subset.size + 1)),
              toScalusList(subset)
            )

        // e(acc, g2) == e(proof, commitment)
        val fn = PlutusV3.compile:
            (acc: G1Element, g2: G2Element, proof: G1Element, commitment: G2Element) =>
                bls12_381_finalVerify(
                  bls12_381_millerLoop(acc, g2),
                  bls12_381_millerLoop(proof, commitment)
                )

        val applied = fn.program.term $
            Term.Const(Constant.BLS12_381_G1_Element(acc)) $
            Term.Const(Constant.BLS12_381_G2_Element(setup.g2Generator)) $
            Term.Const(Constant.BLS12_381_G1_Element(proof)) $
            Term.Const(Constant.BLS12_381_G2_Element(commitment))

        val result = applied.evaluate
        assert(result α_== true.asTerm)
    }

    test("G1 on-chain UPLC non-membership pairing verification") {
        given compilerOptions: Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true
        )

        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(99))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulateG1(setup, fullSet)
        val (proofG2, proofG1) = nonMembershipProofG1(setup, fullSet, disjointSet)
        val disjointCommitment =
            scalus.cardano.onchain.plutus.prelude.crypto.accumulator.Poly.getG2Commitment(
              toScalusList(setup.g2Powers.take(disjointSet.size + 1)),
              toScalusList(disjointSet)
            )

        // e(acc, proofG2) * e(proofG1, disjointCommitment) == e(g1, g2)
        val fn = PlutusV3.compile:
            (
                acc: G1Element,
                pG2: G2Element,
                pG1: G1Element,
                disjointC: G2Element,
                g1: G1Element,
                g2: G2Element
            ) =>
                bls12_381_finalVerify(
                  bls12_381_mulMlResult(
                    bls12_381_millerLoop(acc, pG2),
                    bls12_381_millerLoop(pG1, disjointC)
                  ),
                  bls12_381_millerLoop(g1, g2)
                )

        val applied = fn.program.term $
            Term.Const(Constant.BLS12_381_G1_Element(acc)) $
            Term.Const(Constant.BLS12_381_G2_Element(proofG2)) $
            Term.Const(Constant.BLS12_381_G1_Element(proofG1)) $
            Term.Const(Constant.BLS12_381_G2_Element(disjointCommitment)) $
            Term.Const(Constant.BLS12_381_G1_Element(setup.g1Generator)) $
            Term.Const(Constant.BLS12_381_G2_Element(setup.g2Generator))

        val result = applied.evaluate
        assert(result α_== true.asTerm)
    }
}
