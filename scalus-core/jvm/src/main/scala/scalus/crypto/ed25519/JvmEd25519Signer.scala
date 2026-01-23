package scalus.crypto.ed25519

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.params.{Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import org.bouncycastle.crypto.signers.Ed25519Signer as BCSigner
import scalus.uplc.builtin.ByteString

/** JVM implementation of Ed25519Signer using BouncyCastle. */
object JvmEd25519Signer extends Ed25519Signer:

    override def sign(signingKey: SigningKey, message: ByteString): Signature =
        val privateKeyParams = Ed25519PrivateKeyParameters(signingKey.bytes, 0)
        val signer = new BCSigner()
        signer.init(true, privateKeyParams)
        signer.update(message.bytes, 0, message.size)
        Signature.unsafeFromArray(signer.generateSignature())

    /** Sign using SLIP-001 extended key algorithm for Cardano HD wallets.
      *
      * Uses reflection to call BouncyCastle's internal `implSign` method because the SLIP-001
      * variant of Ed25519 is not exposed in the public API.
      */
    override def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature =
        val sig = signEd25519Extended(extendedKey.bytes, publicKey.bytes, message.bytes)
        Signature.unsafeFromArray(sig)

    override def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean =
        try
            val pubKeyParams = Ed25519PublicKeyParameters(verificationKey.bytes, 0)
            val verifier = new BCSigner()
            verifier.init(false, pubKeyParams)
            verifier.update(message.bytes, 0, message.size)
            verifier.verifySignature(signature.bytes)
        catch case _: Exception => false

    override def derivePublicKey(signingKey: SigningKey): VerificationKey =
        val privateKeyParams = Ed25519PrivateKeyParameters(signingKey.bytes, 0)
        val publicKeyParams = privateKeyParams.generatePublicKey()
        VerificationKey.unsafeFromArray(publicKeyParams.getEncoded)

    /** SLIP-001 Ed25519 signing for Cardano extended private keys.
      *
      * This uses reflection to access BouncyCastle's internal `implSign` method because Cardano
      * uses a non-standard Ed25519 variant for HD wallet key derivation.
      */
    private def signEd25519Extended(
        cardanoExtendedPrivKey: Array[Byte],
        publicKey: Array[Byte],
        data: Array[Byte]
    ): Array[Byte] =
        val ed25519Class = classOf[org.bouncycastle.math.ec.rfc8032.Ed25519]
        val method = ed25519Class.getDeclaredMethod(
          "implSign",
          classOf[org.bouncycastle.crypto.Digest],
          classOf[Array[Byte]],
          classOf[Array[Byte]],
          classOf[Array[Byte]],
          classOf[Int],
          classOf[Array[Byte]],
          classOf[Byte],
          classOf[Array[Byte]],
          classOf[Int],
          classOf[Int],
          classOf[Array[Byte]],
          classOf[Int]
        )
        method.setAccessible(true)

        val d = new SHA512Digest
        val h = new Array[Byte](64)
        Array.copy(cardanoExtendedPrivKey, 0, h, 0, 64)
        val s = new Array[Byte](32)
        Array.copy(cardanoExtendedPrivKey, 0, s, 0, 32)
        val pk = new Array[Byte](32)
        Array.copy(publicKey, 0, pk, 0, 32)
        val sig = new Array[Byte](64)

        method.invoke(null, d, h, s, pk, 0, null, 0.toByte, data, 0, data.length, sig, 0)
        sig

given Ed25519Signer = JvmEd25519Signer
