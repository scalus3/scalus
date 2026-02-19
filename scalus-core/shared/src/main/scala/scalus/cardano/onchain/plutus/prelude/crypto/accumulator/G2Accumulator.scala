package scalus.cardano.onchain.plutus.prelude.crypto.accumulator

import scalus.Compile
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.builtin.Builtins.*
import scalus.cardano.onchain.plutus.prelude.*

/** On-chain bilinear accumulator with the accumulator value on G2.
  *
  * The setup (CRS) uses G1 powers of tau. The accumulator and membership proofs are G2 points.
  * Verification uses pairings to check polynomial relationships.
  *
  * This variant is cheaper on-chain because the G1 commitment (multi-scalar multiplication on G1)
  * is less expensive than G2 operations.
  *
  * @see
  *   [[https://github.com/perturbing/plutus-accumulator]]
  */
@Compile
object G2Accumulator {

    /** Verify membership of a subset in the accumulated set.
      *
      * Checks the pairing equation: e(g1, acc) == e(getG1Commitment(setup, subset), proof)
      *
      * @param setup
      *   G1 powers of tau: [τ⁰·G1, τ¹·G1, ...]
      * @param acc
      *   the accumulator value (G2 point)
      * @param subset
      *   elements to prove membership for
      * @param proof
      *   the membership proof (G2 point)
      * @return
      *   true if the subset is in the accumulated set
      */
    def checkMembership(
        setup: List[G1Element],
        acc: G2Element,
        subset: List[BigInt],
        proof: G2Element
    ): Boolean = {
        val g1 = setup !! 0
        val lhs = bls12_381_millerLoop(g1, acc)
        val rhs = bls12_381_millerLoop(Poly.getG1Commitment(setup, subset), proof)
        bls12_381_finalVerify(lhs, rhs)
    }

    /** Verify non-membership of a disjoint set against the accumulated set.
      *
      * Checks the pairing equation: e(proof._1, acc) · e(getG1Commitment(setup, disjointSet),
      * proof._2) == e(g1, g2)
      *
      * @param setup
      *   G1 powers of tau
      * @param g2
      *   the G2 generator point
      * @param acc
      *   the accumulator value (G2 point)
      * @param disjointSet
      *   elements to prove non-membership for
      * @param proof
      *   the non-membership proof (G1, G2) from extended GCD
      * @return
      *   true if the disjoint set has no elements in common with the accumulated set
      */
    def checkNonMembership(
        setup: List[G1Element],
        g2: G2Element,
        acc: G2Element,
        disjointSet: List[BigInt],
        proof: (G1Element, G2Element)
    ): Boolean = {
        val g1 = setup !! 0
        val lhs1 = bls12_381_millerLoop(proof._1, acc)
        val lhs2 = bls12_381_millerLoop(Poly.getG1Commitment(setup, disjointSet), proof._2)
        val lhs = bls12_381_mulMlResult(lhs1, lhs2)
        val rhs = bls12_381_millerLoop(g1, g2)
        bls12_381_finalVerify(lhs, rhs)
    }
}
