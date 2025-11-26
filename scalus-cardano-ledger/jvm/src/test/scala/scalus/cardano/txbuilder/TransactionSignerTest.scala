package scalus.cardano.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.platform
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

    private def generateKeyPair(): KeyPair = {
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
        tx.copy(witnessSet = tx.witnessSet.copy(vkeyWitnesses = TaggedSortedSet.empty))
    }

    test("sign with single key adds VKeyWitness to transaction") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witnesses = signedTx.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 1, "Should have exactly one witness")

        val witness = witnesses.head
        val expectedVkey = ByteString.fromArray(keyPair.publicKeyBytes.take(32))
        assert(witness.vkey == expectedVkey, "VKey should match public key")
    }

    test("sign with multiple keys adds multiple VKeyWitnesses") {
        val keyPair1 = generateKeyPair()
        val keyPair2 = generateKeyPair()
        val keyPair3 = generateKeyPair()

        val keyPairs = Set(keyPair1, keyPair2, keyPair3)
        val signer = new TransactionSigner(keyPairs)

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witnesses = signedTx.witnessSet.vkeyWitnesses.toSeq
        assert(witnesses.size == 3, "Should have three witnesses")

        val witnessVkeys = witnesses.map(_.vkey).toSet
        val expectedVkeys = keyPairs.map(kp => ByteString.fromArray(kp.publicKeyBytes.take(32)))
        assert(witnessVkeys == expectedVkeys, "All public keys should be present in witnesses")
    }

    test("signatures are valid Ed25519 signatures") {
        val keyPair = generateKeyPair()
        val signer = new TransactionSigner(Set(keyPair))

        val unsignedTx = createUnsignedTransaction
        val signedTx = signer.sign(unsignedTx)

        val witness = signedTx.witnessSet.vkeyWitnesses.toSeq.head
        val txHash: ByteString = unsignedTx.id
        val publicKey = ByteString.fromArray(keyPair.publicKeyBytes)

        val isValid = platform.verifyEd25519Signature(publicKey, txHash, witness.signature)
        assert(isValid, "Signature should be valid")
    }

    test("TransactionSigner companion object creates signer from ByteString pairs") {
        val keyPair1 = generateKeyPair()
        val keyPair2 = generateKeyPair()

        // The companion object's KeyPair uses platform.signEd25519 which expects 32-byte private keys
        val priv1 = ByteString.fromArray(keyPair1.privateKeyBytes.take(32))
        val pub1 = ByteString.fromArray(keyPair1.publicKeyBytes)
        val priv2 = ByteString.fromArray(keyPair2.privateKeyBytes.take(32))
        val pub2 = ByteString.fromArray(keyPair2.publicKeyBytes)

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
        val expectedVkey = ByteString.fromArray(keyPair.publicKeyBytes.take(32))

        assert(witness.vkey.length == 32, "VKey should be 32 bytes")
        assert(witness.vkey == expectedVkey, "VKey should be first 32 bytes of public key")
        assert(witness.signature.length == 64, "Ed25519 signature should be 64 bytes")
    }
}
