package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.crypto.ed25519.{Ed25519Signer, JvmEd25519Signer}
import scalus.utils.Hex.toHex

/** HdKeyPair tests using JVM Ed25519 signer. */
class HdKeyPairTest extends AnyFunSuite {

    // Use JVM signer for tests
    given Ed25519Signer = JvmEd25519Signer

    val testMnemonic =
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

    test("fromMnemonic creates valid key pair") {
        val keyPair = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")

        assert(keyPair.verificationKey.size == 32)
        assert(keyPair.extendedSigningKey.size == 64)
        assert(keyPair.signingKey.size == 32)
        assert(keyPair.chainCode.length == 32)
    }

    test("fromMnemonic with passphrase creates valid key pair") {
        val keyPair = HdKeyPair.fromMnemonic(testMnemonic, "secret", "m/1852'/1815'/0'/0/0")

        assert(keyPair.verificationKey.size == 32)
    }

    test("masterFromMnemonic creates master key") {
        val master = HdKeyPair.masterFromMnemonic(testMnemonic)

        assert(master.verificationKey.size == 32)
    }

    test("deriveHardened produces deterministic keys") {
        val master = HdKeyPair.masterFromMnemonic(testMnemonic)

        val child1 = master.deriveHardened(0)
        val child2 = master.deriveHardened(0)

        assert(child1.verificationKey == child2.verificationKey)
        assert(child1.extendedKey.kL.toHex == child2.extendedKey.kL.toHex)
    }

    test("different indices produce different keys") {
        val master = HdKeyPair.masterFromMnemonic(testMnemonic)

        val child0 = master.deriveHardened(0)
        val child1 = master.deriveHardened(1)

        assert(child0.verificationKey != child1.verificationKey)
    }

    test("sign and verify") {
        val keyPair = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")
        val message = ByteString.fromString("Hello, Cardano!")

        val signature = keyPair.sign(message)

        assert(signature.size == 64)
        assert(keyPair.verify(message, signature))
    }

    test("signature verification fails for wrong message") {
        val keyPair = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")
        val message1 = ByteString.fromString("Hello, Cardano!")
        val message2 = ByteString.fromString("Hello, World!")

        val signature = keyPair.sign(message1)

        assert(!keyPair.verify(message2, signature))
    }

    test("CIP-1852 payment key derivation") {
        val paymentPath = Cip1852.paymentPath(0, 0)
        val paymentKey = HdKeyPair.fromMnemonic(testMnemonic, paymentPath)

        assert(paymentKey.verificationKey.size == 32)

        // Sign and verify with payment key
        val message = ByteString.fromString("test")
        val sig = paymentKey.sign(message)
        assert(paymentKey.verify(message, sig))
    }

    test("CIP-1852 staking key derivation") {
        val stakingPath = Cip1852.stakingPath(0)
        val stakingKey = HdKeyPair.fromMnemonic(testMnemonic, stakingPath)

        assert(stakingKey.verificationKey.size == 32)
    }

    test("chain derivation matches manual derivation") {
        val master = HdKeyPair.masterFromMnemonic(testMnemonic)

        // Derive using chain: m/1852'/1815'/0'
        val account = master.deriveHardened(1852).deriveHardened(1815).deriveHardened(0)

        // Derive using path
        val accountDirect = HdKeyPair.fromMnemonic(testMnemonic, Cip1852.accountPath(0))

        assert(account.verificationKey == accountDirect.verificationKey)
        assert(account.extendedKey.kL.toHex == accountDirect.extendedKey.kL.toHex)
    }

    test("equality") {
        val keyPair1 = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")
        val keyPair2 = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")

        assert(keyPair1 == keyPair2)
        assert(keyPair1.hashCode() == keyPair2.hashCode())
    }

    test("inequality for different paths") {
        val keyPair1 = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/0")
        val keyPair2 = HdKeyPair.fromMnemonic(testMnemonic, "m/1852'/1815'/0'/0/1")

        assert(keyPair1 != keyPair2)
    }

    test("passphrase affects derived keys") {
        val keyPairNoPass = HdKeyPair.fromMnemonic(testMnemonic, "", "m/1852'/1815'/0'/0/0")
        val keyPairWithPass = HdKeyPair.fromMnemonic(testMnemonic, "secret", "m/1852'/1815'/0'/0/0")

        assert(keyPairNoPass.verificationKey != keyPairWithPass.verificationKey)
    }

    test("non-hardened derivation works") {
        val master = HdKeyPair.masterFromMnemonic(testMnemonic)
        val account = master.deriveHardened(1852).deriveHardened(1815).deriveHardened(0)

        // Non-hardened derivation for role and index (CIP-1852)
        val paymentKey = account.deriveNormal(0).deriveNormal(0)

        assert(paymentKey.verificationKey.size == 32)
    }
}
