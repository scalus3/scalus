package scalus.crypto.ed25519

import org.bouncycastle.math.ec.rfc8032.Ed25519

/** JVM implementation of Ed25519 mathematical operations using BouncyCastle.
  *
  * WARNING: This implementation uses reflection to access BouncyCastle's internal
  * `scalarMultBaseEncoded` method because the public API only exposes key derivation that hashes
  * the seed first (standard Ed25519), whereas BIP32-Ed25519 requires direct scalar multiplication
  * with an already-clamped scalar.
  *
  * This approach is fragile and may break with future BouncyCastle updates. If BouncyCastle exposes
  * a public scalar multiplication API in the future, this should be updated to use it instead.
  */
object Ed25519MathPlatform {

    // Cache the reflection method for performance.
    // Uses setAccessible(true) to access package-private method.
    private lazy val scalarMultBaseEncodedMethod = {
        val method = classOf[Ed25519].getDeclaredMethod(
          "scalarMultBaseEncoded",
          classOf[Array[Byte]],
          classOf[Array[Byte]],
          classOf[Int]
        )
        method.setAccessible(true)
        method
    }

    /** Multiply the Ed25519 base point by a scalar to derive the public key.
      *
      * For BIP32-Ed25519, the scalar is already clamped and we need direct scalar*base
      * multiplication, not the standard Ed25519 key derivation which hashes the seed first.
      *
      * Uses reflection to call BouncyCastle's internal `scalarMultBaseEncoded` method.
      *
      * @param scalar
      *   32-byte clamped scalar in little-endian format
      * @return
      *   32-byte compressed public key point
      */
    def scalarMultiplyBase(scalar: Array[Byte]): Array[Byte] = {
        val publicKey = new Array[Byte](32)
        scalarMultBaseEncodedMethod.invoke(null, scalar, publicKey, Integer.valueOf(0))
        publicKey
    }
}
