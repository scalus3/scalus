package scalus.testing.kit

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scalus.uplc.builtin.ByteString

/** Platform-specific Ed25519 key pair generation for tests (JS implementation using @noble/curves).
  */
object KeyPairGenerator {
    def generateKeyPair(): (ByteString, ByteString) = {
        val privateKeyArray = Ed25519.utils.randomPrivateKey()
        val publicKeyArray = Ed25519.getPublicKey(privateKeyArray)
        val privateKey = ByteString.fromArray(privateKeyArray.toArray.map(_.toByte))
        val publicKey = ByteString.fromArray(publicKeyArray.toArray.map(_.toByte))
        (privateKey, publicKey)
    }

    @js.native
    @JSImport("@noble/curves/ed25519", "ed25519")
    private object Ed25519 extends js.Object {
        def getPublicKey(privateKey: Uint8Array): Uint8Array = js.native
        val utils: Ed25519Utils = js.native
    }

    @js.native
    private trait Ed25519Utils extends js.Object {
        def randomPrivateKey(): Uint8Array = js.native
    }
}
