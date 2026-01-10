package scalus.crypto.ed25519

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.scalajs.js.JSConverters.*

@JSImport("@noble/curves/ed25519", JSImport.Namespace)
@js.native
private object NobleEd25519Math extends js.Object:
    val ed25519: NobleEd25519Ops = js.native

@js.native
private trait NobleEd25519Ops extends js.Object:
    def getPublicKey(privateKey: Uint8Array): Uint8Array = js.native
    val ExtendedPoint: ExtendedPointCompanion = js.native

@js.native
private trait ExtendedPointCompanion extends js.Object:
    val BASE: ExtendedPoint = js.native

@js.native
private trait ExtendedPoint extends js.Object:
    def multiply(scalar: js.BigInt): ExtendedPoint = js.native
    def toRawBytes(): Uint8Array = js.native

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

    /** Ed25519 curve order L = 2^252 + 27742317777372353535851937790883648493 */
    private val L: js.BigInt =
        js.BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")

    private def toUint8Array(bytes: Array[Byte]): Uint8Array =
        val int8Array = new Int8Array(bytes.toJSArray)
        new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    private def fromUint8Array(arr: Uint8Array): Array[Byte] =
        new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray

    /** Convert little-endian bytes to BigInt for JS. */
    private def bytesToBigInt(bytes: Array[Byte]): js.BigInt = {
        if bytes.isEmpty then js.BigInt(0)
        else
            // Convert little-endian bytes to hex string (big-endian)
            val hex = bytes.reverse.map(b => f"${b & 0xff}%02x").mkString
            js.BigInt("0x" + hex)
    }

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
        // Use ExtendedPoint.BASE.multiply for direct scalar multiplication
        // The scalar must be reduced mod L for @noble/curves which requires 1 <= n < L
        val scalarBigInt = bytesToBigInt(scalar)
        val reducedScalar = scalarBigInt % L
        val point = NobleEd25519Math.ed25519.ExtendedPoint.BASE.multiply(reducedScalar)
        fromUint8Array(point.toRawBytes())
    }
}
