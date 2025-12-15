package scalus.cardano.txbuilder

import scalus.builtin.ByteString
import scalus.cardano.ledger.{TaggedSortedSet, Transaction, VKeyWitness}
import scalus.cardano.wallet.{KeyPair, StandardKeyPair}
import scalus.crypto.ed25519.{JsEd25519Signer, Signature, SigningKey, VerificationKey}

/** JS TransactionSigner that uses JsEd25519Signer directly. */
class TransactionSigner(keys: Set[KeyPair]) {

    def sign(unsignedTransaction: Transaction): Transaction = {
        val transactionIdBS = ByteString.unsafeFromArray(unsignedTransaction.id.bytes)
        val newWitnesses = keys.view.map(createWitness(_, transactionIdBS))
        val existingWitnesses = unsignedTransaction.witnessSet.vkeyWitnesses.toSeq
        val vkeyWitnesses = TaggedSortedSet.from(existingWitnesses ++ newWitnesses)
        unsignedTransaction.copy(
          witnessSet = unsignedTransaction.witnessSet.copy(vkeyWitnesses = vkeyWitnesses)
        )
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
                      JsEd25519Signer.sign(signingKey, message)
                  override def verify(message: ByteString, signature: Signature): Boolean =
                      JsEd25519Signer.verify(verificationKey, message, signature)
              }
          }
        )
    }
}
