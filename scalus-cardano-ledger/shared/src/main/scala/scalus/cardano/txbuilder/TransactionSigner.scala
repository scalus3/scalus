package scalus.cardano.txbuilder

import scalus.builtin.{platform, ByteString, given}
import scalus.cardano.ledger.{AddrKeyHash, TaggedSortedSet, Transaction, TransactionException, TransactionHash, Utxos, VKeyWitness}
import scalus.cardano.ledger.utils.{AllNeededKeyHashes, MissingKeyHashes}

trait TransactionSigner {
    def publicKeyHashes: Set[AddrKeyHash]

    final def sign(unsignedTransaction: Transaction, utxos: Utxos): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException |
          TransactionException.MissingKeyHashesException,
      Transaction
    ] = {
        for
            _ <- MissingKeyHashes.validateAllMissingKeyHashes(
              unsignedTransaction,
              publicKeyHashes,
              utxos
            )
            allNeededKeyHashesView <- AllNeededKeyHashes.allNeededKeyHashesView(
              unsignedTransaction,
              utxos
            )
        yield
            val vkeyWitnesses = TaggedSortedSet.from(
              allNeededKeyHashesView.map { hash =>
                  signEd25519(hash.asInstanceOf[AddrKeyHash], unsignedTransaction.id)
              }
            )

            unsignedTransaction.copy(
              witnessSet = unsignedTransaction.witnessSet.copy(vkeyWitnesses = vkeyWitnesses)
            )
    }

    protected def signEd25519(
        addrKeyHash: AddrKeyHash,
        transactionId: TransactionHash
    ): VKeyWitness
}

object TransactionSigner {
    def apply(keyPairs: Set[(ByteString, ByteString)]): TransactionSigner =
        new Impl(keyPairs)

    private final class Impl(keyPairs: Set[(ByteString, ByteString)]) extends TransactionSigner {
        private val keys: Map[AddrKeyHash, (ByteString, ByteString)] = keyPairs.view.map {
            case keyPair @ (_, publicKey) =>
                AddrKeyHash(platform.blake2b_224(publicKey)) -> keyPair
        }.toMap

        override val publicKeyHashes: Set[AddrKeyHash] = keys.keySet

        override protected def signEd25519(
            addrKeyHash: AddrKeyHash,
            transactionId: TransactionHash
        ): VKeyWitness = {
            val (privateKey, publicKey) = keys(addrKeyHash)
            val signature = platform.signEd25519(privateKey, transactionId)
            VKeyWitness(publicKey, signature)
        }
    }
}
