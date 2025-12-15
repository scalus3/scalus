package scalus.cardano.wallet

import scalus.builtin.{platform, ByteString}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.crypto.ed25519.{ExtendedSigningKey, Signature, SigningKey, VerificationKey}

/** A key pair for Ed25519 signing operations. */
trait KeyPair {
    type Underlying
    def underlying: Underlying

    /** The verification (public) key - 32 bytes. */
    def verificationKey: VerificationKey

    /** Sign a message and return the signature.
      * @param message
      *   the message to sign
      * @return
      *   64-byte Ed25519 signature
      */
    def sign(message: ByteString): Signature

    /** Verify a signature.
      * @param message
      *   the message that was signed
      * @param signature
      *   the signature to verify
      * @return
      *   true if the signature is valid
      */
    def verify(message: ByteString, signature: Signature): Boolean

    // Deprecated methods for backward compatibility

    @deprecated("Use verificationKey.bytes instead", "0.9.0")
    def publicKeyBytes: Array[Byte] = verificationKey.bytes

    @deprecated("Use the signing key directly", "0.9.0")
    def privateKeyBytes: Array[Byte]

    @deprecated("Use sign(ByteString) instead", "0.9.0")
    def sign(message: Array[Byte]): Array[Byte] =
        platform
            .signEd25519(
              ByteString.fromArray(privateKeyBytes),
              ByteString.fromArray(message)
            )
            .bytes

    @deprecated("Use verify(ByteString, Signature) instead", "0.9.0")
    def verify(message: Array[Byte], signature: Array[Byte]): Boolean =
        platform.verifyEd25519Signature(
          ByteString.fromArray(publicKeyBytes),
          ByteString.fromArray(message),
          ByteString.fromArray(signature)
        )

    @deprecated("Use verificationKey and signing key directly", "0.9.0")
    def toTuple: (Array[Byte], Array[Byte]) = (publicKeyBytes, privateKeyBytes)
}

/** A key pair backed by a standard 32-byte signing key. */
trait StandardKeyPair extends KeyPair {

    /** The signing (private) key - 32 bytes. */
    def signingKey: SigningKey

    @deprecated("Use signingKey.bytes instead", "0.9.0")
    override def privateKeyBytes: Array[Byte] = signingKey.bytes
}

/** A key pair backed by an extended 64-byte signing key (SLIP-001/HD wallets). */
trait ExtendedKeyPair extends KeyPair {

    /** The extended signing key - 64 bytes. */
    def extendedSigningKey: ExtendedSigningKey

    @deprecated("Use extendedSigningKey.bytes instead", "0.9.0")
    override def privateKeyBytes: Array[Byte] = extendedSigningKey.bytes
}

/** Account abstraction for Cardano wallets. */
trait Account {
    def paymentKeyPair: KeyPair
    def changeKeyPair: KeyPair
    def stakeKeyPair: KeyPair
    def drepKeyPair: KeyPair

    def signerForUtxos: TransactionSigner =
        new TransactionSigner(Set(paymentKeyPair))
}
