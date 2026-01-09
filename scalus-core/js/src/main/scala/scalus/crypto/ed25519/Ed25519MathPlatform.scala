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

/** JS implementation of Ed25519 mathematical operations using @noble/curves. */
object Ed25519MathPlatform {

    private def toUint8Array(bytes: Array[Byte]): Uint8Array =
        val int8Array = new Int8Array(bytes.toJSArray)
        new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    private def fromUint8Array(arr: Uint8Array): Array[Byte] =
        new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray

    /** Convert little-endian bytes to BigInt for JS. */
    private def bytesToBigInt(bytes: Array[Byte]): js.BigInt = {
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
        val scalarBigInt = bytesToBigInt(scalar)
        val point = NobleEd25519Math.ed25519.ExtendedPoint.BASE.multiply(scalarBigInt)
        fromUint8Array(point.toRawBytes())
    }
}
