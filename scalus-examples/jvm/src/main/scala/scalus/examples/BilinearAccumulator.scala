package scalus.examples
import scalus.Compile
import scalus.uplc.builtin.BLS12_381_G1_Element
import scalus.uplc.builtin.BLS12_381_G2_Element
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
        setup: List[BLS12_381_G1_Element],
        subset: List[BigInt]
    ): BLS12_381_G1_Element = {
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
        setup: List[BLS12_381_G1_Element],
        acc: BLS12_381_G2_Element,
        subset: List[BigInt],
        proof: BLS12_381_G2_Element
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
        setup: List[BLS12_381_G1_Element],
        g2: BLS12_381_G2_Element,
        acc: BLS12_381_G2_Element,
        disjointSet: List[BigInt],
        proof: (BLS12_381_G1_Element, BLS12_381_G2_Element)
    ): Boolean = {
        val g1 = setup !! 0

        val lhs1 = bls12_381_millerLoop(proof._1, acc)
        val lhs2 = bls12_381_millerLoop(getG1Commitment(setup, disjointSet), proof._2)
        val lhs = bls12_381_mulMlResult(lhs1, lhs2)
        val rhs = bls12_381_millerLoop(g1, g2)

        bls12_381_finalVerify(lhs, rhs)
    }
}
