package scalus.crypto.ed25519

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Libsodium bindings for Ed25519 scalar operations. */
@link("sodium")
@extern
private object LibSodiumEd25519Math:
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

/** Native implementation of Ed25519 mathematical operations using libsodium. */
object Ed25519MathPlatform {

    /** Multiply the Ed25519 base point by a scalar to derive the public key.
      *
      * @param scalar
      *   32-byte scalar in little-endian format (Ed25519 private key scalar)
      * @return
      *   32-byte compressed public key point
      */
    def scalarMultiplyBase(scalar: Array[Byte]): Array[Byte] = {
        val pk = new Array[Byte](32)
        val sk = new Array[Byte](64)
        val result = LibSodiumEd25519Math.crypto_sign_ed25519_seed_keypair(
          pk.atUnsafe(0),
          sk.atUnsafe(0),
          scalar.atUnsafe(0)
        )
        require(result == 0, "Failed to derive public key from scalar")
        pk
    }
}
