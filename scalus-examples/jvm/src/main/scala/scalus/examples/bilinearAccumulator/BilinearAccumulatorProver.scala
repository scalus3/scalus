package scalus.examples.bilinearAccumulator

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.{G1, G2, Scalar}
import scalus.uplc.builtin.Builtins.{bls12_381_G1_add, bls12_381_G1_scalarMul, bls12_381_G2_add, bls12_381_G2_scalarMul}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

/** Offchain prover for the BLS12-381 Bilinear Accumulator.
  *
  * Generates trusted setups, accumulators, and membership/non-membership proofs. After setup, tau
  * is no longer needed — all operations use the CRS points directly via multi-scalar
  * multiplication.
  */
object BilinearAccumulatorProver {

    /** BLS12-381 scalar field prime */
    val p: BigInt = Scalar.fieldPrime

    // -- Setup --

    /** Self-contained CRS with powers of tau on both G1 and G2.
      *
      * After `trustedSetup`, tau can be destroyed. All accumulator and proof operations use only
      * the CRS points.
      */
    case class Setup(g1Powers: Vector[G1Element], g2Powers: Vector[G2Element]) {
        def g2Generator: G2Element = g2Powers(0)
    }

    /** Generate a trusted setup with powers of tau on both G1 and G2:
      *   - G1: [G1, tau*G1, ..., tau^n*G1]
      *   - G2: [G2, tau*G2, ..., tau^n*G2]
      *
      * After calling this, tau should be securely destroyed ("toxic waste").
      */
    def trustedSetup(tau: BigInt, maxDegree: Int): Setup = {
        val g1Gen = G1.generator
        val g2Gen = G2.generator
        val g1Powers = (0 to maxDegree).map { i =>
            val scalar = tau.modPow(i, p)
            bls12_381_G1_scalarMul(scalar, g1Gen)
        }.toVector
        val g2Powers = (0 to maxDegree).map { i =>
            val scalar = tau.modPow(i, p)
            bls12_381_G2_scalarMul(scalar, g2Gen)
        }.toVector
        Setup(g1Powers, g2Powers)
    }

    // -- Accumulator and proof generation (tau-free) --

    /** Compute accumulator acc = P_U(tau)*G2 using multi-scalar multiplication on the CRS. */
    def accumulate(setup: Setup, elements: Vector[BigInt]): G2Element = {
        val poly = Poly.product(elements)
        g2Commitment(poly, setup.g2Powers)
    }

    /** Generate membership proof using the CRS: commit(Q, G2) where Q = P_fullSet / P_subset. */
    def membershipProof(
        setup: Setup,
        fullSet: Vector[BigInt],
        subset: Vector[BigInt]
    ): G2Element = {
        val polyFull = Poly.product(fullSet)
        val polySub = Poly.product(subset)
        val (quotient, remainder) = polyFull /% polySub
        require(remainder.isZero, "Subset is not a subset of the full set (non-zero remainder)")
        g2Commitment(quotient, setup.g2Powers)
    }

    /** Generate non-membership proof using extended GCD and the CRS. Returns (commit(S, G1),
      * commit(T, G2)) where S*P_U + T*P_D = 1.
      */
    def nonMembershipProof(
        setup: Setup,
        fullSet: Vector[BigInt],
        disjointSet: Vector[BigInt]
    ): (G1Element, G2Element) = {
        val polyU = Poly.product(fullSet)
        val polyD = Poly.product(disjointSet)
        val (gcd, s, t) = polyU.extGcd(polyD)
        require(gcd == Poly.one, s"Sets are not disjoint (GCD degree ${gcd.degree})")
        (g1Commitment(s, setup.g1Powers), g2Commitment(t, setup.g2Powers))
    }

    // -- Offchain verification wrappers --

    /** Verify membership proof by delegating to on-chain verifier. */
    def verifyMembership(
        setup: Setup,
        acc: G2Element,
        subset: Vector[BigInt],
        proof: G2Element
    ): Boolean = {
        val setupList = toScalusList(setup.g1Powers)
        val subsetList = toScalusList(subset)
        BilinearAccumulator.checkMembership(setupList, acc, subsetList, proof)
    }

    /** Verify non-membership proof by delegating to on-chain verifier. */
    def verifyNonMembership(
        setup: Setup,
        acc: G2Element,
        disjointSet: Vector[BigInt],
        proof: (G1Element, G2Element)
    ): Boolean = {
        val setupList = toScalusList(setup.g1Powers)
        val disjointList = toScalusList(disjointSet)
        BilinearAccumulator.checkNonMembership(
          setupList,
          setup.g2Generator,
          acc,
          disjointList,
          proof
        )
    }

    // -- Multi-scalar multiplication helpers --

    /** Compute polynomial commitment on G1: sum(coeff_i * g1Powers_i). */
    private def g1Commitment(poly: Poly, g1Powers: Vector[G1Element]): G1Element = {
        val coeffs = poly.coefficients
        require(coeffs.nonEmpty, "Cannot commit to zero polynomial")
        require(coeffs.length <= g1Powers.length, "Not enough G1 CRS points for polynomial degree")
        var acc = bls12_381_G1_scalarMul(coeffs(0), g1Powers(0))
        for i <- 1 until coeffs.length do
            acc = bls12_381_G1_add(acc, bls12_381_G1_scalarMul(coeffs(i), g1Powers(i)))
        acc
    }

    /** Compute polynomial commitment on G2: sum(coeff_i * g2Powers_i). */
    private def g2Commitment(poly: Poly, g2Powers: Vector[G2Element]): G2Element = {
        val coeffs = poly.coefficients
        require(coeffs.nonEmpty, "Cannot commit to zero polynomial")
        require(coeffs.length <= g2Powers.length, "Not enough G2 CRS points for polynomial degree")
        var acc = bls12_381_G2_scalarMul(coeffs(0), g2Powers(0))
        for i <- 1 until coeffs.length do
            acc = bls12_381_G2_add(acc, bls12_381_G2_scalarMul(coeffs(i), g2Powers(i)))
        acc
    }

    private[bilinearAccumulator] def toScalusList[A](vec: Vector[A]): PList[A] =
        vec.foldRight(PList.Nil: PList[A])((elem, acc) => PList.Cons(elem, acc))
}
