package scalus.cardano.txbuilder

import scalus.builtin.ByteString
import scalus.cardano.ledger.{TaggedSortedSet, Transaction, TransactionHash, VKeyWitness}
import scalus.cardano.wallet.KeyPair
class TransactionSigner(keys: Set[KeyPair]) {

    def sign(unsignedTransaction: Transaction): Transaction = {
        val ws = keys.view.map(signEd25519(_, unsignedTransaction.id))
        val vkeyWitnesses = TaggedSortedSet.from(ws)
        unsignedTransaction.copy(
          witnessSet = unsignedTransaction.witnessSet.copy(vkeyWitnesses = vkeyWitnesses)
        )
    }

    protected def signEd25519(
        keyPair: KeyPair,
        transactionId: TransactionHash
    ): VKeyWitness = {
        val signature = keyPair.sign(transactionId.bytes)
        VKeyWitness(ByteString.fromArray(keyPair.publicKeyBytes), ByteString.fromArray(signature))
    }
}

object TransactionSigner {
    def apply(keyPairs: Set[(ByteString, ByteString)]): TransactionSigner =
        new TransactionSigner(
          keyPairs.map { case (priv, pub) =>
              new KeyPair {
                  override type Underlying = (ByteString, ByteString)
                  override def underlying: (ByteString, ByteString) = (priv, pub)
                  override def publicKeyBytes: Array[Byte] = pub.bytes
                  override def privateKeyBytes: Array[Byte] = priv.bytes
              }
          }
        )
}
