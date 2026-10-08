package scalus.crypto.ed25519

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scalus.uplc.builtin.ByteString

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

    /** Extended signing for BIP32-Ed25519/SLIP-001 HD wallets, with libsodium's scalar API.
      *
      * The 64-byte key is `kL ‖ kR`; kL is the scalar itself, not hashed. As `JsEd25519Signer`:
      *   1. r = SHA-512(kR ‖ message) mod L
      *   1. R = r·B
      *   1. k = SHA-512(R ‖ A ‖ message) mod L
      *   1. S = (r + k·kL) mod L
      *   1. signature = R ‖ S
      */
    override def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature =
        val xsk = extendedKey.bytes
        val msg = message.bytes
        val rInput = new Array[Byte](32 + msg.length)
        Array.copy(xsk, 32, rInput, 0, 32)
        Array.copy(msg, 0, rInput, 32, msg.length)
        val r = Ed25519MathPlatform.hashToScalar(rInput)
        Ed25519MathPlatform.memzero(rInput)
        val kLBytes = xsk.take(32)
        val kL = Ed25519MathPlatform.reduce32(kLBytes)
        Ed25519MathPlatform.memzero(kLBytes)
        try
            val rPoint = Ed25519MathPlatform.mulBase(r)
            val k = Ed25519MathPlatform.hashToScalar(rPoint ++ publicKey.bytes ++ msg)
            val s = Ed25519MathPlatform.mulAdd(k, kL, r)
            Signature.unsafeFromArray(rPoint ++ s)
        finally
            Ed25519MathPlatform.memzero(r)
            Ed25519MathPlatform.memzero(kL)

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
