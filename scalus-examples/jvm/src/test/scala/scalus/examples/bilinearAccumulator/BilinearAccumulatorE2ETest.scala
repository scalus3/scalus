package scalus.examples.bilinearAccumulator

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.examples.bilinearAccumulator.BilinearAccumulator.*
import scalus.examples.bilinearAccumulator.BilinearAccumulatorProver.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

class BilinearAccumulatorE2ETest extends AnyFunSuite, EvalTestKit {

    // Test-only tau. In production, tau must be random and destroyed after setup.
    val tau: BigInt = BigInt("12345678901234567890")

    // -- Polynomial arithmetic tests --

    test("polyMul: (x+2)(x+3) = x²+5x+6") {
        val a = Poly(2, 1) // x + 2
        val b = Poly(3, 1) // x + 3
        val result = a * b
        assert(result == Poly(6, 5, 1))
    }

    test("polyDivMod exact: (x²+5x+6)/(x+2) = (x+3), remainder 0") {
        val dividend = Poly(6, 5, 1)
        val divisor = Poly(2, 1)
        val (q, r) = dividend /% divisor
        assert(q == Poly(3, 1))
        assert(r.isZero)
    }

    test("polyDivMod with remainder: (x²+5x+7)/(x+2) = (x+3) remainder 1") {
        val dividend = Poly(7, 5, 1)
        val divisor = Poly(2, 1)
        val (q, r) = dividend /% divisor
        assert(q == Poly(3, 1))
        assert(r == Poly(1))
    }

    test("polyExtGcd coprime: s*a + t*b = 1") {
        val a = Poly(2, 1) // x + 2
        val b = Poly(3, 1) // x + 3
        val (gcd, s, t) = a.extGcd(b)
        assert(gcd == Poly.one)
        // Verify Bezout identity: s*a + t*b = 1
        val check = s * a + t * b
        assert(check == Poly.one)
    }

    test("productPoly matches on-chain getFinalPoly") {
        val elements = Vector(BigInt(2), BigInt(3), BigInt(5))
        val offchain = Poly.product(elements)
        // (x+2)(x+3)(x+5) = x³ + 10x² + 31x + 30
        assert(offchain == Poly(30, 31, 10, 1))

        val onchain = getFinalPoly(List(BigInt(2), BigInt(3), BigInt(5)))
        assert(onchain == List(BigInt(30), BigInt(31), BigInt(10), BigInt(1)))
    }

    // -- E2E membership tests --

    test("membership: single element subset of 5-element set") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30), BigInt(40), BigInt(50))
        val subset = Vector(BigInt(20))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulate(setup, fullSet)
        val proof = membershipProof(setup, fullSet, subset)
        assert(verifyMembership(setup, acc, subset, proof))
    }

    test("membership: multi-element subset of 10-element set") {
        val fullSet = (1 to 10).map(i => BigInt(i * 100)).toVector
        val subset = Vector(BigInt(200), BigInt(500), BigInt(800))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulate(setup, fullSet)
        val proof = membershipProof(setup, fullSet, subset)
        assert(verifyMembership(setup, acc, subset, proof))
    }

    test("membership: full set as subset") {
        val fullSet = Vector(BigInt(1), BigInt(2), BigInt(3))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulate(setup, fullSet)
        val proof = membershipProof(setup, fullSet, fullSet)
        assert(verifyMembership(setup, acc, fullSet, proof))
    }

    test("membership: wrong subset fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val setup = trustedSetup(tau, fullSet.size)
        val wrongSubset = Vector(BigInt(99))
        assertThrows[IllegalArgumentException] {
            membershipProof(setup, fullSet, wrongSubset)
        }
    }

    // -- E2E non-membership tests --

    test("non-membership: single disjoint element") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(99))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulate(setup, fullSet)
        val proof = nonMembershipProof(setup, fullSet, disjointSet)
        assert(verifyNonMembership(setup, acc, disjointSet, proof))
    }

    test("non-membership: multiple disjoint elements") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(40), BigInt(50))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulate(setup, fullSet)
        val proof = nonMembershipProof(setup, fullSet, disjointSet)
        assert(verifyNonMembership(setup, acc, disjointSet, proof))
    }

    test("non-membership: overlapping set fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val overlappingSet = Vector(BigInt(20), BigInt(40))
        val setup = trustedSetup(tau, math.max(fullSet.size, overlappingSet.size))
        assertThrows[IllegalArgumentException] {
            nonMembershipProof(setup, fullSet, overlappingSet)
        }
    }

    // -- On-chain UPLC verification tests --
    // Note: List[G1Element] operations in UPLC require compile-time-inlined values (the scalus
    // compiler lowers List operations to Data-typed builtins). So we test the core pairing
    // verification with prover-generated scalar arguments, computing commitments offchain.

    test("on-chain UPLC membership pairing verification") {
        import scalus.uplc.builtin.Builtins.*

        given compilerOptions: Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true
        )

        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val subset = Vector(BigInt(10), BigInt(20))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulate(setup, fullSet)
        val proof = membershipProof(setup, fullSet, subset)
        // Compute the G1 commitment offchain using the on-chain function
        val commitment = BilinearAccumulator.getG1Commitment(
          toScalusList(setup.g1Powers.take(subset.size + 1)),
          toScalusList(subset)
        )

        // Compile the core pairing check: e(g1, acc) == e(commitment, proof)
        val fn = PlutusV3.compile:
            (g1: G1Element, acc: G2Element, commitment: G1Element, proof: G2Element) =>
                bls12_381_finalVerify(
                  bls12_381_millerLoop(g1, acc),
                  bls12_381_millerLoop(commitment, proof)
                )

        val applied = fn.program.term $
            Term.Const(Constant.BLS12_381_G1_Element(setup.g1Powers.head)) $
            Term.Const(Constant.BLS12_381_G2_Element(acc)) $
            Term.Const(Constant.BLS12_381_G1_Element(commitment)) $
            Term.Const(Constant.BLS12_381_G2_Element(proof))

        val result = applied.evaluate
        assert(result α_== true.asTerm)
    }

    test("on-chain UPLC non-membership pairing verification") {
        import scalus.uplc.builtin.Builtins.*

        given compilerOptions: Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = true
        )

        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(99))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulate(setup, fullSet)
        val (proofG1, proofG2) = nonMembershipProof(setup, fullSet, disjointSet)
        // Compute the G1 commitment for the disjoint set offchain
        val disjointCommitment = BilinearAccumulator.getG1Commitment(
          toScalusList(setup.g1Powers.take(disjointSet.size + 1)),
          toScalusList(disjointSet)
        )

        // Compile the core non-membership pairing check:
        // e(proofG1, acc) * e(disjointCommitment, proofG2) == e(g1, g2)
        val fn = PlutusV3.compile:
            (
                g1: G1Element,
                g2Gen: G2Element,
                acc: G2Element,
                disjointC: G1Element,
                pG1: G1Element,
                pG2: G2Element
            ) =>
                bls12_381_finalVerify(
                  bls12_381_mulMlResult(
                    bls12_381_millerLoop(pG1, acc),
                    bls12_381_millerLoop(disjointC, pG2)
                  ),
                  bls12_381_millerLoop(g1, g2Gen)
                )

        val applied = fn.program.term $
            Term.Const(Constant.BLS12_381_G1_Element(setup.g1Powers.head)) $
            Term.Const(Constant.BLS12_381_G2_Element(setup.g2Generator)) $
            Term.Const(Constant.BLS12_381_G2_Element(acc)) $
            Term.Const(Constant.BLS12_381_G1_Element(disjointCommitment)) $
            Term.Const(Constant.BLS12_381_G1_Element(proofG1)) $
            Term.Const(Constant.BLS12_381_G2_Element(proofG2))

        val result = applied.evaluate
        assert(result α_== true.asTerm)
    }
}
