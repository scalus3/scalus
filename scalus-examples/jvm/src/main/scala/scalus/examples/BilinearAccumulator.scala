package scalus.examples
import scalus.Compile
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.builtin.Builtins.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G1.*

/** A Bilinear Accumulator is a cryptographic primitive that allows for efficient membership and
  * non-membership proofs for a set of elements.
  *
  * @see
  *   [[https://github.com/perturbing/plutus-accumulator]]
  */
@Compile
object BilinearAccumulator {

    def getFinalPoly(binomial_poly: List[BigInt]): List[BigInt] = {
        binomial_poly.foldLeft(List.single(BigInt(1))): (acc, term) =>
            val shiftedPoly: List[BigInt] = List.Cons(0, acc)
            val multipliedPoly = acc.appended(BigInt(0)).map(_ * term)
            List.map2(shiftedPoly, multipliedPoly)(_ + _)
    }

    def getG1Commitment(
        setup: List[G1Element],
        subset: List[BigInt]
    ): G1Element = {
        val subsetInG1 =
            List.map2(getFinalPoly(subset), setup): (sb, st) =>
                st.scale(sb)

        subsetInG1.foldLeft(zero): (a, b) =>
            a + b
    }

    /** Checks if the given `acc` is a valid accumulator for the given `subset` of the `setup`.
      * @param setup
      *   The setup of the accumulator.
      * @param acc
      *   The accumulator to check.
      * @param subset
      *   The subset of the setup.
      * @return
      *   True if the accumulator is valid, false otherwise.
      */
    def checkMembership(
        setup: List[G1Element],
        acc: G2Element,
        subset: List[BigInt],
        proof: G2Element
    ): Boolean = {
        val g1 = setup !! 0
        val lhs = bls12_381_millerLoop(g1, acc)
        val rhs = bls12_381_millerLoop(getG1Commitment(setup, subset), proof)
        bls12_381_finalVerify(lhs, rhs)
    }

    /** Checks if the given `acc` is a valid non-membership proof for the given `disjointSet` of the
      * `setup`.
      * @param setup
      *   The setup of the accumulator.
      * @param acc
      *   The accumulator to check.
      * @param disjointSet
      *   The disjoint set of the setup.
      * @return
      *   True if the accumulator is valid, false otherwise.
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
        val lhs2 = bls12_381_millerLoop(getG1Commitment(setup, disjointSet), proof._2)
        val lhs = bls12_381_mulMlResult(lhs1, lhs2)
        val rhs = bls12_381_millerLoop(g1, g2)

        bls12_381_finalVerify(lhs, rhs)
    }
}
