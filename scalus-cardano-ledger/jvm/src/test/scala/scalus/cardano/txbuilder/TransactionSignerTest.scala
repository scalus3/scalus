package scalus.cardano.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.platform
import scalus.cardano.ledger.*
import scalus.cardano.wallet.{BloxbeanKeyPair, KeyPair}

class TransactionSignerTest extends AnyFunSuite with ArbitraryInstances {

    private val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    private var accountIndex = 0

    private def generateKeyPair(): BloxbeanKeyPair = {
        val account = Account.createFromMnemonic(
          BBNetwork(0, 42),
          mnemonic,
          createExternalAddressDerivationPathForAccount(accountIndex)
        )
        accountIndex += 1
        BloxbeanKeyPair(account.hdKeyPair())
    }

    private def createUnsignedTransaction: Transaction = {
        val tx = Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)
        tx.withWitness(_.copy(vkeyWitnesses = TaggedSortedSet.empty))
    }

    test("sign with single key adds VKeyWitness to transaction") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witnesses = signedTx.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 1, "Should have exactly one witness")

        val witness = witnesses.head
        val expectedVkey: ByteString = keyPair.verificationKey
        assert(witness.vkey == expectedVkey, "VKey should match public key")
    }

    test("sign with multiple keys adds multiple VKeyWitnesses") {
        val keyPair1 = generateKeyPair()
        val keyPair2 = generateKeyPair()
        val keyPair3 = generateKeyPair()

        val keyPairs = Set[KeyPair](keyPair1, keyPair2, keyPair3)
        val signer = new TransactionSigner(keyPairs)

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witnesses = signedTx.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 3, "Should have three witnesses")

        val witnessVkeys = witnesses.map(_.vkey).toSet
        val expectedVkeys: Set[ByteString] = keyPairs.map(kp => kp.verificationKey)
        assert(witnessVkeys == expectedVkeys, "All public keys should be present in witnesses")
    }

    test("signatures are valid Ed25519 signatures") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witness = signedTx.witnessSet.vkeyWitnesses.toSeq.head
        val txHash: ByteString = unsignedTx.id
        val publicKey: ByteString = keyPair.verificationKey

        val isValid = platform.verifyEd25519Signature(publicKey, txHash, witness.signature)
        assert(isValid, "Signature should be valid")
    }

    test("TransactionSigner companion object creates signer from ByteString pairs") {
        val keyPair1 = generateKeyPair()
        val keyPair2 = generateKeyPair()

        // The companion object's KeyPair uses platform.signEd25519 which expects 32-byte private keys
        val priv1: ByteString = keyPair1.extendedSigningKey.standardKey
        val pub1: ByteString = keyPair1.verificationKey
        val priv2: ByteString = keyPair2.extendedSigningKey.standardKey
        val pub2: ByteString = keyPair2.verificationKey

        val signer = TransactionSigner(Set((priv1, pub1), (priv2, pub2)))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witnesses = signedTx.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 2, "Should have two witnesses")
    }

    test("signing with empty key set produces transaction with empty witnesses") {
        val signer = new TransactionSigner(Set.empty)

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.isEmpty, "Should have no witnesses")
    }

    test("multiple signatures from same keys on different transactions are different") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val tx1 = createUnsignedTransaction
        val tx2 = createUnsignedTransaction

        val signedTx1 = signer.sign(tx1)
        val signedTx2 = signer.sign(tx2)

        val sig1 = signedTx1.witnessSet.vkeyWitnesses.toSeq.head.signature
        val sig2 = signedTx2.witnessSet.vkeyWitnesses.toSeq.head.signature

        assert(sig1 != sig2, "Signatures for different transactions should differ")
    }

    test("VKeyWitness has correct field sizes") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witness = signedTx.witnessSet.vkeyWitnesses.toSeq.head
        val expectedVkey: ByteString = keyPair.verificationKey

        assert(witness.vkey.length == 32, "VKey should be 32 bytes")
        assert(witness.vkey == expectedVkey, "VKey should match verification key")
        assert(witness.signature.length == 64, "Ed25519 signature should be 64 bytes")
    }

    test("signing preserves existing witnesses and adds new ones") {
        val keyPair1 = generateKeyPair()
        val keyPair2 = generateKeyPair()

        val signer1 = new TransactionSigner(Set(keyPair1))
        val signer2 = new TransactionSigner(Set(keyPair2))

        val unsignedTx = createUnsignedTransaction
        val signedOnce = signer1.sign(unsignedTx)
        val signedTwice = signer2.sign(signedOnce)

        val witnesses = signedTwice.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 2, "Should have two witnesses after signing twice")

        val witnessVkeys = witnesses.map(_.vkey).toSet
        val expectedVkey1: ByteString = keyPair1.verificationKey
        val expectedVkey2: ByteString = keyPair2.verificationKey

        assert(witnessVkeys.contains(expectedVkey1), "First signer's witness should be preserved")
        assert(witnessVkeys.contains(expectedVkey2), "Second signer's witness should be present")
    }
}
