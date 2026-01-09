package scalus.crypto.ed25519

/** Platform-independent interface for Ed25519 mathematical operations.
  *
  * These operations are needed for BIP32-Ed25519 key derivation which requires scalar
  * multiplication with the base point for non-hardened derivation.
  */
object Ed25519Math {

    /** Multiply the Ed25519 base point by a scalar to derive the public key.
      *
      * Given a 32-byte scalar k (little-endian), compute A = k * G where G is the Ed25519 base
      * point.
      *
      * @param scalar
      *   32-byte scalar in little-endian format
      * @return
      *   32-byte compressed public key point
      */
    def scalarMultiplyBase(scalar: Array[Byte]): Array[Byte] =
        Ed25519MathPlatform.scalarMultiplyBase(scalar)
}
