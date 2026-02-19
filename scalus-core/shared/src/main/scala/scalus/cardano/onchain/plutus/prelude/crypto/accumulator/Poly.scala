package scalus.cardano.onchain.plutus.prelude.crypto.accumulator

import scalus.Compile
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.builtin.Builtins.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.{G1, G2}

/** On-chain polynomial operations for bilinear accumulator verification.
  *
  * Computes product polynomials and polynomial commitments on G1/G2 curve groups. Used by both
  * [[G1Accumulator]] and [[G2Accumulator]] for membership and non-membership verification.
  */
@Compile
object Poly {

    /** Compute the product polynomial ∏(x + aᵢ) from a list of elements.
      *
      * Returns coefficients in ascending degree order [c₀, c₁, ..., cₙ].
      *
      * @param binomials
      *   the elements aᵢ to multiply as (x + aᵢ) factors
      * @return
      *   the product polynomial coefficients
      */
    def getFinalPoly(binomials: List[BigInt]): List[BigInt] = {
        binomials.foldLeft(List.single(BigInt(1))): (acc, term) =>
            val shiftedPoly: List[BigInt] = List.Cons(0, acc)
            val multipliedPoly = acc.appended(BigInt(0)).map(_ * term)
            List.map2(shiftedPoly, multipliedPoly)(_ + _)
    }

    /** Compute a polynomial commitment on G1: Σ(coeffᵢ · g1Powersᵢ).
      *
      * @param setup
      *   powers of tau on G1: [τ⁰·G1, τ¹·G1, ...]
      * @param subset
      *   elements whose product polynomial to commit
      * @return
      *   the G1 commitment point
      */
    def getG1Commitment(
        setup: List[G1Element],
        subset: List[BigInt]
    ): G1Element = {
        val subsetInG1 =
            List.map2(getFinalPoly(subset), setup): (sb, st) =>
                bls12_381_G1_scalarMul(sb, st)
        subsetInG1.foldLeft(G1.zero): (a, b) =>
            bls12_381_G1_add(a, b)
    }

    /** Compute a polynomial commitment on G2: Σ(coeffᵢ · g2Powersᵢ).
      *
      * @param setup
      *   powers of tau on G2: [τ⁰·G2, τ¹·G2, ...]
      * @param subset
      *   elements whose product polynomial to commit
      * @return
      *   the G2 commitment point
      */
    def getG2Commitment(
        setup: List[G2Element],
        subset: List[BigInt]
    ): G2Element = {
        val subsetInG2 =
            List.map2(getFinalPoly(subset), setup): (sb, st) =>
                bls12_381_G2_scalarMul(sb, st)
        subsetInG2.foldLeft(G2.zero): (a, b) =>
            bls12_381_G2_add(a, b)
    }
}
