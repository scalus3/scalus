package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.{Ed25519Signer, JsEd25519Signer}

class JsSignerTest extends AnyFunSuite {
    // Use JS Ed25519 signer for key derivation
    private given Ed25519Signer = JsEd25519Signer

    private val mnemonic = "test " * 23 + "sauce"
    // An arbitrary known tx hash that, using the above mnemonic signs to a deterministic signature
    private val message =
        ByteString.fromHex("81cefa46b4cd141baf32d9d450439c6ca5aa2c2c54cba30157fc229c8810a64b")

    test("produces deterministic signature for test mnemonic") {
        val account = HdAccount.fromMnemonic(mnemonic)
        val signature1 = account.paymentKeyPair.sign(message)
        val signature2 = account.paymentKeyPair.sign(message)

        assert(
          signature1 == signature2,
          s"Signatures should be deterministic: ${signature1.toHex} != ${signature2.toHex}"
        )
    }

    test("verifies own signature") {
        val account = HdAccount.fromMnemonic(mnemonic)

        val signature = account.paymentKeyPair.sign(message)
        val isValid = account.paymentKeyPair.verify(message, signature)

        assert(isValid, "Signature should be valid")
    }
}
