package scalus.cardano.txbuilder

import scalus.builtin.ByteString
import scalus.cardano.ledger.{TaggedSortedSet, Transaction, VKeyWitness}
import scalus.cardano.wallet.{KeyPair, StandardKeyPair}
import scalus.crypto.ed25519.{JvmEd25519Signer, Signature, SigningKey, VerificationKey}

/** JVM TransactionSigner that uses JvmEd25519Signer directly. */
class TransactionSigner(keys: Set[KeyPair]) {

    def sign(unsignedTransaction: Transaction): Transaction = {
        val newWitnesses = keys.view.map(createWitness(_, unsignedTransaction.id))
        val existingWitnesses = unsignedTransaction.witnessSet.vkeyWitnesses.toSet
        val vkeyWitnesses = TaggedSortedSet(existingWitnesses ++ newWitnesses)
        unsignedTransaction.withWitness(_.copy(vkeyWitnesses = vkeyWitnesses))
    }

    private def createWitness(keyPair: KeyPair, transactionId: ByteString): VKeyWitness = {
        val signature = keyPair.sign(transactionId)
        VKeyWitness(keyPair.verificationKey, signature)
    }
}

object TransactionSigner {
    def apply(keyPairs: Set[(ByteString, ByteString)]): TransactionSigner = {
        new TransactionSigner(
          keyPairs.map { case (priv, pub) =>
              new StandardKeyPair {
                  override type Underlying = (ByteString, ByteString)
                  override def underlying: (ByteString, ByteString) = (priv, pub)
                  override def verificationKey: VerificationKey =
                      VerificationKey.unsafeFromByteString(pub)
                  override def signingKey: SigningKey =
                      SigningKey.unsafeFromByteString(priv)
                  override def sign(message: ByteString): Signature =
                      JvmEd25519Signer.sign(signingKey, message)
                  override def verify(message: ByteString, signature: Signature): Boolean =
                      JvmEd25519Signer.verify(verificationKey, message, signature)
              }
          }
        )
    }
}
