package scalus.crypto.accumulator

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.crypto.accumulator.{G1Accumulator, G2Accumulator}
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.{G1, G2, Scalar}
import scalus.uplc.builtin.Builtins.{bls12_381_G1_add, bls12_381_G1_scalarMul, bls12_381_G2_add, bls12_381_G2_scalarMul}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

/** Off-chain prover for the BLS12-381 Bilinear Accumulator.
  *
  * Supports both G1-accumulator (acc on G1, Ethereum ceremony compatible) and G2-accumulator (acc
  * on G2, cheaper on-chain verification). Generates trusted setups, accumulators, and
  * membership/non-membership proofs. After setup, tau is no longer needed — all operations use the
  * CRS points directly via multi-scalar multiplication.
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
        def g1Generator: G1Element = g1Powers(0)
        def g2Generator: G2Element = g2Powers(0)
    }

    object Setup {

        /** Create a Setup from pre-computed CRS points. */
        def fromPoints(g1Powers: Vector[G1Element], g2Powers: Vector[G2Element]): Setup =
            Setup(g1Powers, g2Powers)
    }

    /** Generate a trusted setup with powers of tau on both G1 and G2.
      *
      * WARNING: This is intended for testing only. In production, use a multi-party ceremony where
      * tau is never known to any single party. Anyone who knows tau can forge arbitrary proofs.
      *
      * @param tau
      *   the secret scalar (must be random and destroyed after setup)
      * @param maxDegree
      *   maximum polynomial degree to support
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

    // -- G2 Accumulator (acc on G2, setup on G1) --

    /** Compute G2 accumulator: acc = P_U(tau)*G2. */
    def accumulateG2(setup: Setup, elements: Vector[BigInt]): G2Element = {
        val poly = Poly.product(elements)
        g2Commitment(poly, setup.g2Powers)
    }

    /** Generate G2 membership proof: commit(Q, G2) where Q = P_fullSet / P_subset. */
    def membershipProofG2(
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

    /** Generate G2 non-membership proof using extended GCD. Returns (commit(S, G1), commit(T, G2))
      * where S*P_U + T*P_D = 1.
      */
    def nonMembershipProofG2(
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

    /** Verify G2 membership proof by delegating to on-chain verifier. */
    def verifyMembershipG2(
        setup: Setup,
        acc: G2Element,
        subset: Vector[BigInt],
        proof: G2Element
    ): Boolean = {
        val setupList = toScalusList(setup.g1Powers)
        val subsetList = toScalusList(subset)
        G2Accumulator.checkMembership(setupList, acc, subsetList, proof)
    }

    /** Verify G2 non-membership proof by delegating to on-chain verifier. */
    def verifyNonMembershipG2(
        setup: Setup,
        acc: G2Element,
        disjointSet: Vector[BigInt],
        proof: (G1Element, G2Element)
    ): Boolean = {
        val setupList = toScalusList(setup.g1Powers)
        val disjointList = toScalusList(disjointSet)
        G2Accumulator.checkNonMembership(
          setupList,
          setup.g2Generator,
          acc,
          disjointList,
          proof
        )
    }

    // -- G1 Accumulator (acc on G1, setup on G2) --

    /** Compute G1 accumulator: acc = P_U(tau)*G1. */
    def accumulateG1(setup: Setup, elements: Vector[BigInt]): G1Element = {
        val poly = Poly.product(elements)
        g1Commitment(poly, setup.g1Powers)
    }

    /** Generate G1 membership proof: commit(Q, G1) where Q = P_fullSet / P_subset. */
    def membershipProofG1(
        setup: Setup,
        fullSet: Vector[BigInt],
        subset: Vector[BigInt]
    ): G1Element = {
        val polyFull = Poly.product(fullSet)
        val polySub = Poly.product(subset)
        val (quotient, remainder) = polyFull /% polySub
        require(remainder.isZero, "Subset is not a subset of the full set (non-zero remainder)")
        g1Commitment(quotient, setup.g1Powers)
    }

    /** Generate G1 non-membership proof using extended GCD. Returns (commit(S, G2), commit(T, G1))
      * where S*P_U + T*P_D = 1.
      */
    def nonMembershipProofG1(
        setup: Setup,
        fullSet: Vector[BigInt],
        disjointSet: Vector[BigInt]
    ): (G2Element, G1Element) = {
        val polyU = Poly.product(fullSet)
        val polyD = Poly.product(disjointSet)
        val (gcd, s, t) = polyU.extGcd(polyD)
        require(gcd == Poly.one, s"Sets are not disjoint (GCD degree ${gcd.degree})")
        (g2Commitment(s, setup.g2Powers), g1Commitment(t, setup.g1Powers))
    }

    /** Verify G1 membership proof by delegating to on-chain verifier. */
    def verifyMembershipG1(
        setup: Setup,
        acc: G1Element,
        subset: Vector[BigInt],
        proof: G1Element
    ): Boolean = {
        val setupList = toScalusList(setup.g2Powers)
        val subsetList = toScalusList(subset)
        G1Accumulator.checkMembership(setupList, acc, subsetList, proof)
    }

    /** Verify G1 non-membership proof by delegating to on-chain verifier. */
    def verifyNonMembershipG1(
        setup: Setup,
        acc: G1Element,
        disjointSet: Vector[BigInt],
        proof: (G2Element, G1Element)
    ): Boolean = {
        val setupList = toScalusList(setup.g2Powers)
        val disjointList = toScalusList(disjointSet)
        G1Accumulator.checkNonMembership(
          setupList,
          setup.g1Generator,
          acc,
          disjointList,
          proof
        )
    }

    // -- Legacy API (G2 accumulator, for backwards compatibility) --

    /** Compute accumulator (G2 variant). */
    @deprecated("Use accumulateG2 instead", "0.15.1")
    def accumulate(setup: Setup, elements: Vector[BigInt]): G2Element =
        accumulateG2(setup, elements)

    /** Generate membership proof (G2 variant). */
    @deprecated("Use membershipProofG2 instead", "0.15.1")
    def membershipProof(
        setup: Setup,
        fullSet: Vector[BigInt],
        subset: Vector[BigInt]
    ): G2Element = membershipProofG2(setup, fullSet, subset)

    /** Generate non-membership proof (G2 variant). */
    @deprecated("Use nonMembershipProofG2 instead", "0.15.1")
    def nonMembershipProof(
        setup: Setup,
        fullSet: Vector[BigInt],
        disjointSet: Vector[BigInt]
    ): (G1Element, G2Element) = nonMembershipProofG2(setup, fullSet, disjointSet)

    /** Verify membership proof (G2 variant). */
    @deprecated("Use verifyMembershipG2 instead", "0.15.1")
    def verifyMembership(
        setup: Setup,
        acc: G2Element,
        subset: Vector[BigInt],
        proof: G2Element
    ): Boolean = verifyMembershipG2(setup, acc, subset, proof)

    /** Verify non-membership proof (G2 variant). */
    @deprecated("Use verifyNonMembershipG2 instead", "0.15.1")
    def verifyNonMembership(
        setup: Setup,
        acc: G2Element,
        disjointSet: Vector[BigInt],
        proof: (G1Element, G2Element)
    ): Boolean = verifyNonMembershipG2(setup, acc, disjointSet, proof)

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

    private[accumulator] def toScalusList[A](vec: Vector[A]): PList[A] =
        vec.foldRight(PList.Nil: PList[A])((elem, acc) => PList.Cons(elem, acc))
}
