package scalus.crypto.ed25519

import scalus.builtin.ByteString

/** Cross-platform Ed25519 signing capability.
  *
  * Implementations are provided per-platform:
  *   - JVM: BouncyCastle
  *   - JS: @noble/curves/ed25519
  *   - Native: libsodium
  */
trait Ed25519Signer:
    /** Sign a message with a standard 32-byte signing key. */
    def sign(signingKey: SigningKey, message: ByteString): Signature

    /** Sign a message with an extended 64-byte signing key (SLIP-001 for HD wallets).
      * @param extendedKey
      *   64-byte extended private key
      * @param publicKey
      *   32-byte public key (needed for SLIP-001 signing)
      * @param message
      *   message to sign
      */
    def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature

    /** Verify an Ed25519 signature. */
    def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean

    /** Derive the public key from a standard signing key. */
    def derivePublicKey(signingKey: SigningKey): VerificationKey

object Ed25519Signer:
    /** Get the platform-specific signer instance. */
    inline def apply(using signer: Ed25519Signer): Ed25519Signer = signer
