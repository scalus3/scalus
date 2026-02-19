package scalus.cardano.onchain.plutus.prelude.crypto.accumulator

import scalus.Compile
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.builtin.Builtins.*
import scalus.cardano.onchain.plutus.prelude.*

/** On-chain bilinear accumulator with the accumulator value on G1.
  *
  * The setup (CRS) uses G2 powers of tau. The accumulator and membership proofs are G1 points. This
  * variant is compatible with Ethereum KZG ceremonies (which provide G1 and G2 powers) and
  * hydrozoa.
  *
  * G1 accumulator is more expensive on-chain (G2 commitment in verification) but allows using
  * existing ceremony data without re-generation.
  */
@Compile
object G1Accumulator {

    /** Verify membership of a subset in the accumulated set.
      *
      * Checks the pairing equation: e(acc, g2) == e(proof, getG2Commitment(setup, subset))
      *
      * @param setup
      *   G2 powers of tau: [τ⁰·G2, τ¹·G2, ...]
      * @param acc
      *   the accumulator value (G1 point)
      * @param subset
      *   elements to prove membership for
      * @param proof
      *   the membership proof (G1 point)
      * @return
      *   true if the subset is in the accumulated set
      */
    def checkMembership(
        setup: List[G2Element],
        acc: G1Element,
        subset: List[BigInt],
        proof: G1Element
    ): Boolean = {
        val g2 = setup !! 0
        val lhs = bls12_381_millerLoop(acc, g2)
        val rhs = bls12_381_millerLoop(proof, Poly.getG2Commitment(setup, subset))
        bls12_381_finalVerify(lhs, rhs)
    }

    /** Verify non-membership of a disjoint set against the accumulated set.
      *
      * Checks the pairing equation: e(acc, proof._1) · e(proof._2, getG2Commitment(setup,
      * disjointSet)) == e(g1, g2)
      *
      * @param setup
      *   G2 powers of tau
      * @param g1
      *   the G1 generator point
      * @param acc
      *   the accumulator value (G1 point)
      * @param disjointSet
      *   elements to prove non-membership for
      * @param proof
      *   the non-membership proof (G2, G1) from extended GCD: (S on G2, T on G1)
      * @return
      *   true if the disjoint set has no elements in common with the accumulated set
      */
    def checkNonMembership(
        setup: List[G2Element],
        g1: G1Element,
        acc: G1Element,
        disjointSet: List[BigInt],
        proof: (G2Element, G1Element)
    ): Boolean = {
        val g2 = setup !! 0
        val lhs1 = bls12_381_millerLoop(acc, proof._1)
        val lhs2 = bls12_381_millerLoop(proof._2, Poly.getG2Commitment(setup, disjointSet))
        val lhs = bls12_381_mulMlResult(lhs1, lhs2)
        val rhs = bls12_381_millerLoop(g1, g2)
        bls12_381_finalVerify(lhs, rhs)
    }
}
