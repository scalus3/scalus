package scalus.cardano.wallet.hd

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Networks
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.crypto.ed25519.{Ed25519Signer, JvmEd25519Signer, Signature}

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

    // Bloxbean compatibility tests

    test("payment key matches bloxbean Account") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), testMnemonic)

        val hdPaymentKey = hdAccount.paymentKeyPair.verificationKey
        val bbPaymentKey = ByteString.fromArray(bbAccount.hdKeyPair().getPublicKey.getKeyData)

        assert(
          hdPaymentKey == bbPaymentKey,
          s"Payment keys differ:\n  HdAccount: ${hdPaymentKey.toHex}\n  Bloxbean:  ${bbPaymentKey.toHex}"
        )
    }

    test("staking key matches bloxbean Account") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), testMnemonic)

        val hdStakeKey = hdAccount.stakeKeyPair.verificationKey
        val bbStakeKey = ByteString.fromArray(bbAccount.stakeHdKeyPair().getPublicKey.getKeyData)

        assert(
          hdStakeKey == bbStakeKey,
          s"Staking keys differ:\n  HdAccount: ${hdStakeKey.toHex}\n  Bloxbean:  ${bbStakeKey.toHex}"
        )
    }

    test("base address matches bloxbean Account on mainnet") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), testMnemonic)

        val hdBaseAddress = hdAccount.baseAddress(Network.Mainnet).toBech32.get
        val bbBaseAddress = bbAccount.baseAddress()

        assert(
          hdBaseAddress == bbBaseAddress,
          s"Base addresses differ:\n  HdAccount: $hdBaseAddress\n  Bloxbean:  $bbBaseAddress"
        )
    }

    test("base address matches bloxbean Account on testnet") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.testnet(), testMnemonic)

        val hdBaseAddress = hdAccount.baseAddress(Network.Testnet).toBech32.get
        val bbBaseAddress = bbAccount.baseAddress()

        assert(
          hdBaseAddress == bbBaseAddress,
          s"Base addresses differ:\n  HdAccount: $hdBaseAddress\n  Bloxbean:  $bbBaseAddress"
        )
    }

    test("signature from HdAccount is verified by bloxbean") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), testMnemonic)

        val message = ByteString.fromString("Hello, Cardano!")
        val hdSignature = hdAccount.paymentKeyPair.sign(message)

        // Verify using bloxbean's signing provider
        val bbPublicKey = bbAccount.hdKeyPair().getPublicKey.getKeyData
        val isValid = scalus.builtin.platform.verifyEd25519Signature(
          ByteString.unsafeFromArray(bbPublicKey),
          message,
          hdSignature
        )

        assert(isValid, "Signature from HdAccount should be verifiable")
    }

    test("signature from bloxbean is verified by HdAccount") {
        val hdAccount = HdAccount.fromMnemonic(testMnemonic)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), testMnemonic)

        val message = ByteString.fromString("Hello, Cardano!")

        // Sign using bloxbean
        import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val bbSignature = signingProvider.signExtended(
          message.bytes,
          bbAccount.hdKeyPair().getPrivateKey.getKeyData
        )

        // Verify using HdAccount
        val isValid =
            hdAccount.paymentKeyPair.verify(message, Signature.unsafeFromArray(bbSignature))

        assert(isValid, "Signature from bloxbean should be verifiable by HdAccount")
    }

    test("different account indices match bloxbean") {
        for accountIndex <- Seq(0, 1, 2, 5) do {
            val hdAccount = HdAccount.fromMnemonic(testMnemonic, "", accountIndex)
            // createFromMnemonic(Network, String, int account, int index) derives at m/1852'/1815'/account/0/index
            val bbAccount =
                Account.createFromMnemonic(Networks.mainnet(), testMnemonic, accountIndex, 0)

            val hdPaymentKey = hdAccount.paymentKeyPair.verificationKey
            val bbPaymentKey = ByteString.fromArray(bbAccount.hdKeyPair().getPublicKey.getKeyData)

            assert(
              hdPaymentKey == bbPaymentKey,
              s"Payment keys differ for account $accountIndex:\n  HdAccount: ${hdPaymentKey.toHex}\n  Bloxbean:  ${bbPaymentKey.toHex}"
            )

            val hdStakeKey = hdAccount.stakeKeyPair.verificationKey
            val bbStakeKey =
                ByteString.fromArray(bbAccount.stakeHdKeyPair().getPublicKey.getKeyData)

            assert(
              hdStakeKey == bbStakeKey,
              s"Staking keys differ for account $accountIndex:\n  HdAccount: ${hdStakeKey.toHex}\n  Bloxbean:  ${bbStakeKey.toHex}"
            )
        }
    }

    test("24-word mnemonic produces same keys as bloxbean") {
        val mnemonic24 =
            "abandon abandon abandon abandon abandon abandon abandon abandon " +
                "abandon abandon abandon abandon abandon abandon abandon abandon " +
                "abandon abandon abandon abandon abandon abandon abandon art"

        val hdAccount = HdAccount.fromMnemonic(mnemonic24)
        val bbAccount = Account.createFromMnemonic(Networks.mainnet(), mnemonic24)

        val hdPaymentKey = hdAccount.paymentKeyPair.verificationKey
        val bbPaymentKey = ByteString.fromArray(bbAccount.hdKeyPair().getPublicKey.getKeyData)

        assert(
          hdPaymentKey == bbPaymentKey,
          s"Payment keys differ for 24-word mnemonic:\n  HdAccount: ${hdPaymentKey.toHex}\n  Bloxbean:  ${bbPaymentKey.toHex}"
        )

        val hdBaseAddress = hdAccount.baseAddress(Network.Mainnet).toBech32.get
        val bbBaseAddress = bbAccount.baseAddress()

        assert(
          hdBaseAddress == bbBaseAddress,
          s"Base addresses differ for 24-word mnemonic:\n  HdAccount: $hdBaseAddress\n  Bloxbean:  $bbBaseAddress"
        )
    }
}
