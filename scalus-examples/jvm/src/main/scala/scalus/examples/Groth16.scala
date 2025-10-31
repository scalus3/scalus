package scalus.examples

import scalus.Compile
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.BLS12_381_MlResult
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.prelude.*
import scalus.prelude.crypto.bls12_381.{G1, G2}
import scalus.prelude.crypto.bls12_381.G1.*

/** Groth16 Zero-Knowledge Proof Verification Implementation
  *
  * This object provides functionality for verifying Groth16 zero-knowledge proofs using the
  * BLS12-381 curve. The implementation follows the Groth16 verification algorithm which verifies
  * that a prover knows a witness satisfying a given arithmetic circuit without revealing the
  * witness.
  *
  * This implementation is a transaction of [[https://github.com/Modulo-P/ak-381]].
  *
  * @see
  *   [[https://eprint.iacr.org/2016/260.pdf]] Groth16 paper
  */
@Compile
object Groth16:
    /** Verification key for the Groth16 proof system
      *
      * Contains the necessary elements for verifying a proof, including the encoded points on the
      * BLS12-381 curve
      *
      * @param nPublic
      *   Number of public inputs
      * @param vkAlpha
      *   Alpha parameter in G1
      * @param vkBeta
      *   Beta parameter in G2
      * @param vkGamma
      *   Gamma parameter in G2
      * @param vkDelta
      *   Delta parameter in G2
      * @param vkAlphaBeta
      *   Precomputed alpha*beta in G2
      * @param vkIC
      *   List of IC coefficients in G1 for linear combination of inputs
      */
    case class SnarkVerificationKey(
        nPublic: BigInt,
        vkAlpha: ByteString, // G1Element
        vkBeta: ByteString, // G2Element
        vkGamma: ByteString, // G2Element
        vkDelta: ByteString, // G2Element
        vkAlphaBeta: List[ByteString], // List<G2Element>
        vkIC: List[ByteString] // List<G1Element>
    )

    /** Proof structure for the Groth16 proof system
      *
      * Contains the three main elements of a Groth16 proof: πA, πB, and πC
      *
      * @param piA
      *   First proof element in G1
      * @param piB
      *   Second proof element in G2
      * @param piC
      *   Third proof element in G1
      */
    case class Proof(
        piA: ByteString, // G1Element
        piB: ByteString, // G2Element
        piC: ByteString // G1Element
    )

    given FromData[SnarkVerificationKey] = FromData.derived
    given FromData[Proof] = FromData.derived
    given ToData[SnarkVerificationKey] = ToData.derived
    given ToData[Proof] = ToData.derived

    /** Computes a pairing (Miller loop) between two curve points
      *
      * @param g1
      *   Compressed G1 point
      * @param g2
      *   Compressed G2 point
      * @return
      *   Result of Miller loop pairing
      */
    def pairing(g1: ByteString, g2: ByteString): BLS12_381_MlResult =
        bls12_381_millerLoop(G1.uncompress(g1), G2.uncompress(g2))

    /** Recursively derives the linear combination of IC elements with public inputs
      *
      * This function computes: result + Σ(vk_ic[i] * public[i])
      *
      * @param vk_ic
      *   List of IC coefficients in G1
      * @param public
      *   List of public inputs
      * @param result
      *   Accumulator for the result
      * @return
      *   Final G1 element representing the linear combination
      * @throws RuntimeException
      *   if lists have incompatible lengths
      */
    def derive(
        vk_ic: List[ByteString],
        public: List[BigInt],
        result: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = vk_ic match
        case List.Nil           => result
        case List.Cons(i, rest) =>
            public match
                case List.Cons(scalar, publicRest) =>
                    derive(rest, publicRest, result + G1.uncompress(i).scale(scalar))
                case _ => fail("Invalid input")

    /** Verifies a Groth16 proof
      *
      * Implements the verification equation: e(πA, πB) = e(vk_alpha, vk_beta) * e(vk_IC(public),
      * vk_gamma) * e(πC, vk_delta)
      *
      * Where e() is the optimal Ate pairing on BLS12-381
      *
      * @param vk
      *   Verification key
      * @param proof
      *   The proof to verify
      * @param public
      *   List of public inputs
      * @return
      *   true if the proof is valid, false otherwise
      * @throws RuntimeException
      *   if vkIC is empty
      */
    def grothVerify(
        vk: SnarkVerificationKey,
        proof: Proof,
        public: List[BigInt]
    ): Boolean =
        vk match
            case SnarkVerificationKey(_, vkAlpha, vkBeta, vkGamma, vkDelta, _, vkIC) =>
                proof match
                    case Proof(piA, piB, piC) =>
                        grothVerifyImpl(
                          public,
                          vkAlpha,
                          vkBeta,
                          vkGamma,
                          vkDelta,
                          vkIC,
                          piA,
                          piB,
                          piC
                        )

    private inline def grothVerifyImpl(
        public: List[BigInt],
        vkAlpha: ByteString,
        vkBeta: ByteString,
        vkGamma: ByteString,
        vkDelta: ByteString,
        vkIC: List[ByteString],
        piA: ByteString,
        piB: ByteString,
        piC: ByteString
    ): Boolean =
        // Compute left side of verification equation
        val eAB = pairing(piA, piB)
        // Compute right side components
        val eAlphaBeta = pairing(vkAlpha, vkBeta)

        // Compute linear combination of IC with public inputs
        val vkI = vkIC match
            case List.Cons(head, tail) => derive(tail, public, G1.uncompress(head))
            case _                     => fail("empty vkIC")

        // Compute remaining pairings for right side
        val eIGamma = bls12_381_millerLoop(vkI, G2.uncompress(vkGamma))
        val eCDelta = pairing(piC, vkDelta)

        // Combine all Miller loop results
        val mlr1 = bls12_381_mulMlResult(eAlphaBeta, eIGamma)
        val mlr2 = bls12_381_mulMlResult(mlr1, eCDelta)

        // Final pairing check
        bls12_381_finalVerify(eAB, mlr2)
