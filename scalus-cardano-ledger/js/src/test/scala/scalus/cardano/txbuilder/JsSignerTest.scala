package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.wallet.LucidAccount
import scalus.builtin.ByteString

class JsSignerTest extends AnyFunSuite {

    private val mnemonic = "test " * 23 + "sauce"
    private val derivationPath = "m/1852'/1815'/0'/0/0"
    // An arbitrary known tx hash that, using the above mnemonic + der path signs to the known signature
    private val message =
        ByteString.fromHex("81cefa46b4cd141baf32d9d450439c6ca5aa2c2c54cba30157fc229c8810a64b")
    private val expectedSignature =
        "e12a182f987cff1bc7e051d16efd52943c03e060c5614e208c2335feac798c1e9da570c1fae47678f55893324e4ddafcebdee2a8d648f4d6aadeb94e47619b01"
    private val expectedPublicKey =
        "6ea31d27d585439ea8fd9cd8e6664ed83e605c06aec24d32dfaba488e49287d9"

    test("produces expected signature for test mnemonic") {
        val account = LucidAccount(Network.Mainnet, mnemonic, derivationPath)
        val signature = account.paymentKeyPair.sign(message.bytes)

        assert(
          ByteString.fromArray(signature).toHex == expectedSignature,
          s"Signature mismatch: expected $expectedSignature, got ${ByteString.fromArray(signature).toHex}"
        )
    }

    test("verifies own signature") {
        val account = LucidAccount(Network.Mainnet, mnemonic, derivationPath)

        val signature = account.paymentKeyPair.sign(message.bytes)
        val isValid = account.paymentKeyPair.verify(message.bytes, signature)

        assert(isValid, "Signature should be valid")
    }
}
