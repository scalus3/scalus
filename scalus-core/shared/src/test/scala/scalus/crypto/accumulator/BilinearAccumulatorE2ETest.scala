package scalus.crypto.accumulator

import org.scalatest.funsuite.AnyFunSuite
import scalus.crypto.accumulator.BilinearAccumulatorProver.*
import scalus.testing.kit.EvalTestKit

class BilinearAccumulatorE2ETest extends AnyFunSuite, EvalTestKit {

    // Test-only tau. In production, tau must be random and destroyed after setup.
    val tau: BigInt = BigInt("12345678901234567890")

    // -- G2 accumulator membership tests --

    test("G2 membership: single element subset of 5-element set") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30), BigInt(40), BigInt(50))
        val subset = Vector(BigInt(20))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG2(setup, fullSet)
        val proof = membershipProofG2(setup, fullSet, subset)
        assert(verifyMembershipG2(setup, acc, subset, proof))
    }

    test("G2 membership: multi-element subset of 10-element set") {
        val fullSet = (1 to 10).map(i => BigInt(i * 100)).toVector
        val subset = Vector(BigInt(200), BigInt(500), BigInt(800))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG2(setup, fullSet)
        val proof = membershipProofG2(setup, fullSet, subset)
        assert(verifyMembershipG2(setup, acc, subset, proof))
    }

    test("G2 membership: full set as subset") {
        val fullSet = Vector(BigInt(1), BigInt(2), BigInt(3))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulateG2(setup, fullSet)
        val proof = membershipProofG2(setup, fullSet, fullSet)
        assert(verifyMembershipG2(setup, acc, fullSet, proof))
    }

    test("G2 membership: wrong subset fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val setup = trustedSetup(tau, fullSet.size)
        val wrongSubset = Vector(BigInt(99))
        assertThrows[IllegalArgumentException] {
            membershipProofG2(setup, fullSet, wrongSubset)
        }
    }

    // -- G2 accumulator non-membership tests --

    test("G2 non-membership: single disjoint element") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(99))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulateG2(setup, fullSet)
        val proof = nonMembershipProofG2(setup, fullSet, disjointSet)
        assert(verifyNonMembershipG2(setup, acc, disjointSet, proof))
    }

    test("G2 non-membership: multiple disjoint elements") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val disjointSet = Vector(BigInt(40), BigInt(50))
        val setup = trustedSetup(tau, math.max(fullSet.size, disjointSet.size))
        val acc = accumulateG2(setup, fullSet)
        val proof = nonMembershipProofG2(setup, fullSet, disjointSet)
        assert(verifyNonMembershipG2(setup, acc, disjointSet, proof))
    }

    test("G2 non-membership: overlapping set fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val overlappingSet = Vector(BigInt(20), BigInt(40))
        val setup = trustedSetup(tau, math.max(fullSet.size, overlappingSet.size))
        assertThrows[IllegalArgumentException] {
            nonMembershipProofG2(setup, fullSet, overlappingSet)
        }
    }

    // -- G1 accumulator membership tests --

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

    test("G1 membership: wrong subset fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val setup = trustedSetup(tau, fullSet.size)
        val wrongSubset = Vector(BigInt(99))
        assertThrows[IllegalArgumentException] {
            membershipProofG1(setup, fullSet, wrongSubset)
        }
    }

    // -- G1 accumulator non-membership tests --

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

    test("G1 non-membership: overlapping set fails") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val overlappingSet = Vector(BigInt(20), BigInt(40))
        val setup = trustedSetup(tau, math.max(fullSet.size, overlappingSet.size))
        assertThrows[IllegalArgumentException] {
            nonMembershipProofG1(setup, fullSet, overlappingSet)
        }
    }

    // -- Legacy API tests --

    test("legacy accumulate delegates to G2") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val setup = trustedSetup(tau, fullSet.size)
        val acc = accumulate(setup, fullSet)
        val accG2 = accumulateG2(setup, fullSet)
        // Both should produce the same G2 element
        assert(acc == accG2)
    }
}
