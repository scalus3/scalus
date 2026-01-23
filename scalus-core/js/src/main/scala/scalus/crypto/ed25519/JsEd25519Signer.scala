package scalus.crypto.ed25519

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.scalajs.js.JSConverters.*
import scalus.uplc.builtin.ByteString

@JSImport("@noble/curves/ed25519", JSImport.Namespace)
@js.native
private object NobleEd25519 extends js.Object:
    val ed25519: NobleEd25519Trait = js.native

@js.native
private trait NobleEd25519Trait extends js.Object:
    def sign(message: Uint8Array, privateKey: Uint8Array): Uint8Array = js.native
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
    def getPublicKey(privateKey: Uint8Array): Uint8Array = js.native
    val ExtendedPoint: NobleExtendedPointCompanion = js.native
    val CURVE: NobleCurveParams = js.native

@js.native
private trait NobleExtendedPointCompanion extends js.Object:
    val BASE: NobleExtendedPoint = js.native

@js.native
private trait NobleExtendedPoint extends js.Object:
    def multiply(scalar: js.BigInt): NobleExtendedPoint = js.native
    def toRawBytes(): Uint8Array = js.native

@js.native
private trait NobleCurveParams extends js.Object:
    val n: js.BigInt = js.native // curve order L

@JSImport("@noble/hashes/sha512", JSImport.Namespace)
@js.native
private object NobleSha512 extends js.Object:
    def sha512(data: Uint8Array): Uint8Array = js.native

@JSImport("@noble/hashes/utils", JSImport.Namespace)
@js.native
private object NobleHashUtils extends js.Object:
    def concatBytes(arrays: Uint8Array*): Uint8Array = js.native

/** JS implementation of Ed25519Signer using @noble/curves. */
object JsEd25519Signer extends Ed25519Signer:

    /** Ed25519 curve order L */
    private lazy val L: js.BigInt = NobleEd25519.ed25519.CURVE.n

    private def toUint8Array(bs: ByteString): Uint8Array =
        val int8Array = new Int8Array(bs.bytes.toJSArray)
        new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    private def toUint8ArrayFromArray(arr: Array[Byte]): Uint8Array =
        val int8Array = new Int8Array(arr.toJSArray)
        new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    private def fromUint8Array(arr: Uint8Array): ByteString =
        ByteString.unsafeFromArray(new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray)

    private def uint8ArrayToArray(arr: Uint8Array): Array[Byte] =
        new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray

    /** Convert little-endian bytes to BigInt. */
    private def bytesToBigInt(bytes: Array[Byte]): js.BigInt =
        val hex = bytes.reverse.map(b => f"${b & 0xff}%02x").mkString
        if hex.isEmpty then js.BigInt(0) else js.BigInt("0x" + hex)

    /** Convert BigInt to 32-byte little-endian array. */
    private def bigIntToBytes32(n: js.BigInt): Array[Byte] =
        // Convert to hex string, pad to 64 chars, convert to bytes (big-endian)
        val hex = n.toString(16).reverse.padTo(64, '0').reverse.mkString
        val bytes = hex.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toArray.reverse
        // takeRight ensures we keep the least significant bytes after little-endian reversal
        bytes.takeRight(32)

    override def sign(signingKey: SigningKey, message: ByteString): Signature =
        val sig = NobleEd25519.ed25519.sign(toUint8Array(message), toUint8Array(signingKey))
        Signature.unsafeFromByteString(fromUint8Array(sig))

    /** Extended signing for BIP32-Ed25519/SLIP-001 HD wallets.
      *
      * Implements the Ed25519 signing algorithm with pre-computed extended keys, as used by Cardano
      * HD wallets (BIP32-Ed25519, Icarus-style). This variant uses the extended key directly
      * instead of hashing a seed first.
      *
      * Algorithm: 1. r = SHA-512(kR || message) mod L 2. R = r * G 3. k = SHA-512(R || A ||
      * message) mod L 4. S = (r + k * kL) mod L 5. Signature = R || S
      */
    override def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature =
        // Extract kL (scalar) and kR (nonce prefix) from the 64-byte extended key
        val kL = extendedKey.bytes.take(32)
        val kR = extendedKey.bytes.drop(32)
        val pk = publicKey.bytes
        val msgBytes = message.bytes

        // Step 1: r = SHA-512(kR || message) mod L
        val rInput = kR ++ msgBytes
        val rHash = uint8ArrayToArray(NobleSha512.sha512(toUint8ArrayFromArray(rInput)))
        val r = bytesToBigInt(rHash) % L

        // Step 2: R = r * G (r is already < L, safe for multiply)
        val RPoint = NobleEd25519.ed25519.ExtendedPoint.BASE.multiply(r)
        val RBytes = uint8ArrayToArray(RPoint.toRawBytes())

        // Step 3: k = SHA-512(R || A || message) mod L
        val kInput = RBytes ++ pk ++ msgBytes
        val kHash = uint8ArrayToArray(NobleSha512.sha512(toUint8ArrayFromArray(kInput)))
        val k = bytesToBigInt(kHash) % L

        // Step 4: S = (r + k * kL) mod L
        val kLScalar = bytesToBigInt(kL)
        val S = (r + k * kLScalar) % L

        // Step 5: Signature = R (32 bytes) || S (32 bytes little-endian)
        val SBytes = bigIntToBytes32(S)
        val sigBytes = RBytes ++ SBytes
        Signature.unsafeFromArray(sigBytes)

    override def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean =
        try
            NobleEd25519.ed25519.verify(
              toUint8Array(signature),
              toUint8Array(message),
              toUint8Array(verificationKey)
            )
        catch case _: Exception => false

    override def derivePublicKey(signingKey: SigningKey): VerificationKey =
        val pubKey = NobleEd25519.ed25519.getPublicKey(toUint8Array(signingKey))
        VerificationKey.unsafeFromByteString(fromUint8Array(pubKey))

given Ed25519Signer = JsEd25519Signer
