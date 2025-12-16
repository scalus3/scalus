package scalus.crypto.ed25519

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scalus.builtin.ByteString

/** Libsodium bindings for Ed25519 signing operations. */
@link("sodium")
@extern
private object LibSodiumSigning:
    /** Sign a message and produce a detached signature.
      * @param sig
      *   output buffer for 64-byte signature
      * @param siglen_p
      *   optional output for signature length (can be null)
      * @param m
      *   message to sign
      * @param mlen
      *   message length
      * @param sk
      *   64-byte secret key (seed + public key)
      */
    def crypto_sign_ed25519_detached(
        sig: Ptr[Byte],
        siglen_p: Ptr[CUnsignedLongLong],
        m: Ptr[Byte],
        mlen: CUnsignedLongLong,
        sk: Ptr[Byte]
    ): CInt = extern

    /** Verify a detached Ed25519 signature. */
    def crypto_sign_ed25519_verify_detached(
        sig: Ptr[Byte],
        m: Ptr[Byte],
        mlen: CUnsignedLongLong,
        pk: Ptr[Byte]
    ): CInt = extern

    /** Derive keypair from 32-byte seed.
      * @param pk
      *   output buffer for 32-byte public key
      * @param sk
      *   output buffer for 64-byte secret key
      * @param seed
      *   32-byte seed
      */
    def crypto_sign_ed25519_seed_keypair(
        pk: Ptr[Byte],
        sk: Ptr[Byte],
        seed: Ptr[Byte]
    ): CInt = extern

    /** Extract public key from secret key.
      * @param pk
      *   output buffer for 32-byte public key
      * @param sk
      *   64-byte secret key
      */
    def crypto_sign_ed25519_sk_to_pk(
        pk: Ptr[Byte],
        sk: Ptr[Byte]
    ): CInt = extern

/** Native implementation of Ed25519Signer using libsodium. */
object NativeEd25519Signer extends Ed25519Signer:

    override def sign(signingKey: SigningKey, message: ByteString): Signature =
        // Libsodium expects 64-byte secret key (seed + public key)
        // Need to expand the 32-byte seed first
        val pk = new Array[Byte](32)
        val sk = new Array[Byte](64)
        val result = LibSodiumSigning.crypto_sign_ed25519_seed_keypair(
          pk.atUnsafe(0),
          sk.atUnsafe(0),
          signingKey.bytes.atUnsafe(0)
        )
        require(result == 0, "Failed to derive keypair from seed")

        val sig = new Array[Byte](64)
        val signResult = LibSodiumSigning.crypto_sign_ed25519_detached(
          sig.atUnsafe(0),
          null,
          message.bytes.atUnsafe(0),
          message.size.toULong,
          sk.atUnsafe(0)
        )
        require(signResult == 0, "Failed to sign message")
        Signature.unsafeFromArray(sig)

    /** Extended signing for SLIP-001/HD wallets.
      *
      * Note: Libsodium's Ed25519 implementation uses standard Ed25519 which doesn't directly
      * support Cardano's SLIP-001 extended key format. This implementation uses a simplified
      * approach that extracts the first 32 bytes and uses standard signing.
      *
      * For full SLIP-001 compatibility in Native, consider implementing the algorithm directly
      * using low-level Ed25519 operations.
      */
    override def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature =
        // SLIP-001 extended signing requires special handling.
        // For now, we use the standard key (first 32 bytes) approach.
        val standardKey = extendedKey.standardKey
        sign(standardKey, message)

    override def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean =
        LibSodiumSigning.crypto_sign_ed25519_verify_detached(
          signature.bytes.atUnsafe(0),
          message.bytes.atUnsafe(0),
          message.size.toULong,
          verificationKey.bytes.atUnsafe(0)
        ) == 0

    override def derivePublicKey(signingKey: SigningKey): VerificationKey =
        val pk = new Array[Byte](32)
        val sk = new Array[Byte](64)
        val result = LibSodiumSigning.crypto_sign_ed25519_seed_keypair(
          pk.atUnsafe(0),
          sk.atUnsafe(0),
          signingKey.bytes.atUnsafe(0)
        )
        require(result == 0, "Failed to derive public key")
        VerificationKey.unsafeFromArray(pk)

given Ed25519Signer = NativeEd25519Signer
