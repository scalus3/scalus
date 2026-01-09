package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.crypto.ed25519.{Ed25519Signer, JvmEd25519Signer}

/** HdAccount tests using JVM Ed25519 signer. */
class HdAccountTest extends AnyFunSuite {

    given Ed25519Signer = JvmEd25519Signer

    val testMnemonic =
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

    test("fromMnemonic creates valid account") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        assert(account.accountIndex == 0)
        assert(account.paymentKeyPair.verificationKey.size == 32)
        assert(account.changeKeyPair.verificationKey.size == 32)
        assert(account.stakeKeyPair.verificationKey.size == 32)
        assert(account.drepKeyPair.verificationKey.size == 32)
    }

    test("fromMnemonic with passphrase creates valid account") {
        val account = HdAccount.fromMnemonic(testMnemonic, "testpass")

        assert(account.accountIndex == 0)
        assert(account.paymentKeyPair.verificationKey.size == 32)
    }

    test("different account indices produce different keys") {
        val account0 = HdAccount.fromMnemonic(testMnemonic, "", 0)
        val account1 = HdAccount.fromMnemonic(testMnemonic, "", 1)

        assert(account0.paymentKeyPair.verificationKey != account1.paymentKeyPair.verificationKey)
        assert(account0.stakeKeyPair.verificationKey != account1.stakeKeyPair.verificationKey)
    }

    test("payment, change, and staking keys are different") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        assert(account.paymentKeyPair.verificationKey != account.changeKeyPair.verificationKey)
        assert(account.paymentKeyPair.verificationKey != account.stakeKeyPair.verificationKey)
        assert(account.changeKeyPair.verificationKey != account.stakeKeyPair.verificationKey)
    }

    test("derivePaymentKey produces deterministic keys") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        val key1 = account.derivePaymentKey(0)
        val key2 = account.derivePaymentKey(0)

        assert(key1.verificationKey == key2.verificationKey)
    }

    test("different payment indices produce different keys") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        val key0 = account.derivePaymentKey(0)
        val key1 = account.derivePaymentKey(1)

        assert(key0.verificationKey != key1.verificationKey)
    }

    test("sign and verify with payment key") {
        val account = HdAccount.fromMnemonic(testMnemonic)
        val message = ByteString.fromString("Hello, Cardano!")

        val signature = account.paymentKeyPair.sign(message)

        assert(signature.size == 64)
        assert(account.paymentKeyPair.verify(message, signature))
    }

    test("sign and verify with staking key") {
        val account = HdAccount.fromMnemonic(testMnemonic)
        val message = ByteString.fromString("Stake delegation")

        val signature = account.stakeKeyPair.sign(message)

        assert(signature.size == 64)
        assert(account.stakeKeyPair.verify(message, signature))
    }

    test("withPaymentIndex creates new account with different payment key") {
        val account = HdAccount.fromMnemonic(testMnemonic)
        val accountNewPayment = account.withPaymentIndex(5)

        // Same staking key
        assert(
          account.stakeKeyPair.verificationKey == accountNewPayment.stakeKeyPair.verificationKey
        )

        // Different payment key
        assert(
          account.paymentKeyPair.verificationKey != accountNewPayment.paymentKeyPair.verificationKey
        )

        // New payment key matches derivePaymentKey(5)
        assert(
          accountNewPayment.paymentKeyPair.verificationKey == account
              .derivePaymentKey(5)
              .verificationKey
        )
    }

    test("multipleFromMnemonic creates multiple accounts") {
        val accounts = HdAccount.multipleFromMnemonic(testMnemonic, "", 3)

        assert(accounts.length == 3)
        assert(accounts(0).accountIndex == 0)
        assert(accounts(1).accountIndex == 1)
        assert(accounts(2).accountIndex == 2)

        // All have different payment keys
        assert(
          accounts(0).paymentKeyPair.verificationKey != accounts(1).paymentKeyPair.verificationKey
        )
        assert(
          accounts(1).paymentKeyPair.verificationKey != accounts(2).paymentKeyPair.verificationKey
        )
    }

    test("passphrase affects all derived keys") {
        val accountNoPass = HdAccount.fromMnemonic(testMnemonic, "")
        val accountWithPass = HdAccount.fromMnemonic(testMnemonic, "secret")

        assert(
          accountNoPass.paymentKeyPair.verificationKey != accountWithPass.paymentKeyPair.verificationKey
        )
        assert(
          accountNoPass.stakeKeyPair.verificationKey != accountWithPass.stakeKeyPair.verificationKey
        )
    }

    test("accountKeyPair is accessible") {
        val account = HdAccount.fromMnemonic(testMnemonic)
        val accountKey = account.accountKeyPair

        assert(accountKey.verificationKey.size == 32)
        assert(accountKey.chainCode.length == 32)
    }

    test("drepKeyPair equals stakeKeyPair") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        assert(account.drepKeyPair.verificationKey == account.stakeKeyPair.verificationKey)
    }

    test("negative index validation") {
        val account = HdAccount.fromMnemonic(testMnemonic)

        intercept[IllegalArgumentException] {
            account.derivePaymentKey(-1)
        }

        intercept[IllegalArgumentException] {
            account.deriveChangeKey(-1)
        }

        intercept[IllegalArgumentException] {
            account.deriveStakingKey(-1)
        }
    }

    test("negative account index validation") {
        intercept[IllegalArgumentException] {
            HdAccount.fromMnemonic(testMnemonic, "", -1)
        }
    }
}
