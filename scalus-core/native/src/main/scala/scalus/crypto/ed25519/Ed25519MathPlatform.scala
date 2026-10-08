package scalus.crypto.ed25519

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Libsodium bindings for Ed25519 scalar and point operations (libsodium 1.0.18 or later). */
@link("sodium")
@extern
private[ed25519] object LibSodiumEd25519Math:
    /** q = n·B, with no hashing and no clamping. libsodium clears the top bit of n, and returns -1
      * if n is zero or q is the identity.
      */
    def crypto_scalarmult_ed25519_base_noclamp(q: Ptr[Byte], n: Ptr[Byte]): CInt = extern

    /** r = s mod L, for a 64-byte s. */
    def crypto_core_ed25519_scalar_reduce(r: Ptr[Byte], s: Ptr[Byte]): Unit = extern

    /** z = x·y mod L. */
    def crypto_core_ed25519_scalar_mul(z: Ptr[Byte], x: Ptr[Byte], y: Ptr[Byte]): Unit = extern

    /** z = x + y mod L. */
    def crypto_core_ed25519_scalar_add(z: Ptr[Byte], x: Ptr[Byte], y: Ptr[Byte]): Unit = extern

    def crypto_hash_sha512(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    def sodium_memzero(pnt: Ptr[Byte], len: CSize): Unit = extern

/** Native implementation of Ed25519 mathematical operations using libsodium. */
object Ed25519MathPlatform {

    /** Multiply the Ed25519 base point by a scalar to derive the public key.
      *
      * For BIP32-Ed25519 the scalar is kL itself: no hashing and no clamping. The scalar is reduced
      * mod L first, because libsodium ignores its top bit; the point is the same.
      *
      * @param scalar
      *   32-byte scalar in little-endian format
      * @return
      *   32-byte compressed public key point
      */
    def scalarMultiplyBase(scalar: Array[Byte]): Array[Byte] = {
        require(scalar.length == 32, s"scalar must be 32 bytes, got ${scalar.length}")
        val reduced = reduce32(scalar)
        try mulBase(reduced)
        finally memzero(reduced)
    }

    /** SHA-512 of `input`, reduced mod L. */
    private[ed25519] def hashToScalar(input: Array[Byte]): Array[Byte] = {
        val hash = new Array[Byte](64)
        val rc = LibSodiumEd25519Math.crypto_hash_sha512(
          hash.atUnsafe(0),
          input.atUnsafe(0),
          input.length.toULong
        )
        require(rc == 0, "SHA-512 failed")
        try reduce64(hash)
        finally memzero(hash)
    }

    /** A 32-byte little-endian scalar, reduced mod L. */
    private[ed25519] def reduce32(scalar: Array[Byte]): Array[Byte] = {
        val wide = new Array[Byte](64)
        Array.copy(scalar, 0, wide, 0, 32)
        try reduce64(wide)
        finally memzero(wide)
    }

    /** n·B for a scalar n already reduced mod L. */
    private[ed25519] def mulBase(n: Array[Byte]): Array[Byte] = {
        val q = new Array[Byte](32)
        val rc = LibSodiumEd25519Math.crypto_scalarmult_ed25519_base_noclamp(
          q.atUnsafe(0),
          n.atUnsafe(0)
        )
        require(rc == 0, "scalar is zero mod L")
        q
    }

    /** x·y + z mod L. */
    private[ed25519] def mulAdd(x: Array[Byte], y: Array[Byte], z: Array[Byte]): Array[Byte] = {
        val xy = new Array[Byte](32)
        LibSodiumEd25519Math.crypto_core_ed25519_scalar_mul(
          xy.atUnsafe(0),
          x.atUnsafe(0),
          y.atUnsafe(0)
        )
        val out = new Array[Byte](32)
        LibSodiumEd25519Math.crypto_core_ed25519_scalar_add(
          out.atUnsafe(0),
          xy.atUnsafe(0),
          z.atUnsafe(0)
        )
        memzero(xy)
        out
    }

    private[ed25519] def memzero(buf: Array[Byte]): Unit =
        if buf.nonEmpty then
            LibSodiumEd25519Math.sodium_memzero(buf.atUnsafe(0), buf.length.toCSize)

    private def reduce64(wide: Array[Byte]): Array[Byte] = {
        val out = new Array[Byte](32)
        LibSodiumEd25519Math.crypto_core_ed25519_scalar_reduce(out.atUnsafe(0), wide.atUnsafe(0))
        out
    }
}
