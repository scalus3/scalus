package scalus.crypto.ed25519

import scala.scalajs.js
import scalus.utils.scalajs.internal.*

/** Ed25519 verification that gives libsodium 1.0.18's verdicts, as the Cardano node does.
  *
  * noble's `ed25519.verify` uses the cofactored equation even with `zip215: false`, so this checks
  * the cofactorless one: `encode([S]B - [h]A)` must equal R byte for byte.
  */
private[scalus] object JsEd25519Verifier {
    import JsEd25519Signer.{bytesToBigInt, L}

    def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean = {
        Ed25519LibsodiumRules.passesPreChecks(pk, sig) && {
            try
                val r = sig.take(32)
                val s = bytesToBigInt(sig.drop(32))
                // zip215 = false: reject y >= p and points off the curve
                val a = NobleEd25519.ed25519.Point.fromBytes(pk.toUint8Array, false)
                // h over the received bytes of R and A, not re-encoded ones
                val rAM =
                    NobleHashUtils.concatBytes(r.toUint8Array, pk.toUint8Array, msg.toUint8Array)
                val h = bytesToBigInt(NobleSha512.sha512(rAM).toByteArray) % L
                // multiplyUnsafe: multiply rejects a zero scalar
                val rCheck = NobleEd25519.ed25519.Point.BASE
                    .multiplyUnsafe(s)
                    .subtract(a.multiplyUnsafe(h))
                rCheck.toBytes().toByteArray.sameElements(r)
            catch case _: js.JavaScriptException => false
        }
    }
}
