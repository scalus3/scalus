package scalus.crypto.ed25519

import scalus.utils.scalajs.internal.*

/** JS implementation of Ed25519 mathematical operations using @noble/curves.
  *
  * Note: Unlike the JVM implementation which uses BouncyCastle's internal scalar multiplication
  * directly, this implementation must reduce the scalar modulo L before calling @noble/curves'
  * `multiply` method. This is because @noble/curves validates that 1 <= scalar < L.
  *
  * For BIP32-Ed25519 derived keys, the scalar is already clamped (bits 0-2 cleared, bit 254 set,
  * bit 255 cleared), which guarantees it's less than L. The explicit reduction is defensive and
  * ensures compatibility with the @noble/curves validation requirements.
  *
  * Both implementations produce identical public keys for the same input scalar.
  */
object Ed25519MathPlatform {

    /** Multiply the Ed25519 base point by a scalar to derive the public key.
      *
      * For BIP32-Ed25519, the scalar is already clamped and we need direct scalar*base
      * multiplication, not the standard Ed25519 key derivation which hashes the seed first.
      *
      * @param scalar
      *   32-byte clamped scalar in little-endian format
      * @return
      *   32-byte compressed public key point
      */
    def scalarMultiplyBase(scalar: Array[Byte]): Array[Byte] = {
        // Use Point.BASE.multiply for direct scalar multiplication
        // The scalar must be reduced mod L for @noble/curves which requires 1 <= n < L
        val reducedScalar = JsEd25519Signer.bytesToBigInt(scalar) % JsEd25519Signer.L
        val point = NobleEd25519.ed25519.Point.BASE.multiply(reducedScalar)
        point.toBytes().toByteArray
    }
}
