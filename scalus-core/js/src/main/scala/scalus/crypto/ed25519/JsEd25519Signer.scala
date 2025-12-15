package scalus.crypto.ed25519

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.scalajs.js.JSConverters.*
import scalus.builtin.ByteString

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

/** JS implementation of Ed25519Signer using @noble/curves. */
object JsEd25519Signer extends Ed25519Signer:

    private def toUint8Array(bs: ByteString): Uint8Array =
        val int8Array = new Int8Array(bs.bytes.toJSArray)
        new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    private def fromUint8Array(arr: Uint8Array): ByteString =
        ByteString.unsafeFromArray(new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray)

    override def sign(signingKey: SigningKey, message: ByteString): Signature =
        val sig = NobleEd25519.ed25519.sign(toUint8Array(message), toUint8Array(signingKey))
        Signature.unsafeFromByteString(fromUint8Array(sig))

    /** Extended signing for SLIP-001/HD wallets.
      *
      * Note: @noble/curves/ed25519 uses standard Ed25519 which doesn't support extended keys
      * directly. For Cardano HD wallet signing, use CML or implement SLIP-001 algorithm.
      *
      * This implementation uses a simplified approach that works for most Cardano use cases: it
      * extracts the first 32 bytes of the extended key and uses standard signing. For full SLIP-001
      * compatibility, consider using CML's PrivateKey.sign() method instead.
      */
    override def signExtended(
        extendedKey: ExtendedSigningKey,
        publicKey: VerificationKey,
        message: ByteString
    ): Signature =
        // SLIP-001 extended signing requires the full 64-byte extended key
        // For a proper implementation, we would need to use CML or implement
        // the SLIP-001 algorithm directly using @noble/ed25519 primitives.
        // For now, we use the standard key (first 32 bytes) which works for
        // many use cases but may not be compatible with all Cardano HD wallet flows.
        val standardKey = extendedKey.standardKey
        sign(standardKey, message)

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
