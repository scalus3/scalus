package scalus.examples.bilinearAccumulator

import scalus.Compile
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.bls12_381.G1Element
import scalus.uplc.builtin.bls12_381.G1Element.g1
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G2
import scalus.cardano.onchain.plutus.v3.{ParameterizedValidator, PubKeyHash, TxInfo, TxOutRef}

/** Hardcoded CRS (Common Reference String) for the allowlist validator.
  *
  * Uses tau = 12345678901234567890 with maxDegree = 1 (supports single-element membership proofs).
  *
  * WARNING: This is a test-only CRS with a known tau. In a real deployment, tau must be a
  * cryptographically random 256-bit value, securely destroyed after setup ("toxic waste" ceremony).
  * Anyone who knows tau can forge arbitrary membership proofs and bypass the validator.
  */
@Compile
object AllowlistCRS {
    val powersOfTau: List[G1Element] = List(
      g1"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
      g1"b9553070b412a376743b00acd69beb514826cdfa2b95350081853a8a3d7123a3828a487610078175eb7c3e75ca04e96c"
    )
}

/** Allowlist spend validator using bilinear accumulator membership proofs.
  *
  * UTXOs locked by this validator can only be spent by parties who prove membership in the
  * accumulated set. The parameter is the compressed accumulator (ByteString), applied as a UPLC
  * constant. Using ByteString instead of G2Element avoids the Data decoding overhead and is
  * compatible with the Plutus flat serialization (which doesn't support direct BLS element
  * encoding).
  *
  * The accumulated elements are public key hashes (as big-endian integers). The validator requires
  * the transaction to be signed by the corresponding key, preventing front-running attacks where an
  * observer replays a valid proof from the mempool.
  *
  * Redeemer: (ByteString, ByteString) — (member's PubKeyHash, compressed G2 proof)
  */
@Compile
object AllowlistValidator extends ParameterizedValidator[ByteString] {

    inline override def spend(
        compressedAcc: ByteString,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val acc = G2.uncompress(compressedAcc)
        val (memberPkh, proofBytes) = redeemer.to[(ByteString, ByteString)]
        val proof = G2.uncompress(proofBytes)
        val element = Builtins.byteStringToInteger(true, memberPkh)
        val subset = List.single(element)
        require(
          BilinearAccumulator.checkMembership(AllowlistCRS.powersOfTau, acc, subset, proof),
          "Membership proof verification failed"
        )
        require(tx.isSignedBy(PubKeyHash(memberPkh)), "Transaction must be signed by the member")
    }
}

private given Options = Options.release

lazy val AllowlistContract = PlutusV3.compile(AllowlistValidator.validate)
