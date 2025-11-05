package scalus.cardano.txbuilder

//import scalus.builtin.{platform, ByteString, given}
//import scalus.cardano.ledger.{AddrKeyHash, Hash, Transaction, Utxos}
//import scalus.cardano.ledger.TransactionException.*
//
//trait TransactionSigner {
//    def publicKeyHashes: Set[AddrKeyHash]
//
//    def sign(transaction: Transaction, utxos: Utxos): Either[
//      BadInputsUTxOException | BadCollateralInputsUTxOException | MissingKeyHashesException,
//      Transaction
//    ]
//}

//object TransactionSigner {
//    class Impl(keyPairs: Set[(ByteString, ByteString)]) extends TransactionSigner {
//        private val keys: Map[AddrKeyHash, (ByteString, ByteString)] = keyPairs.view
//            .map(keyPair =>
//                AddrKeyHash(
//                  platform.blake2b_224(keyPair._2)
//                ) -> keyPair
//            )
//            .toMap
//
//        override val publicKeyHashes: Set[AddrKeyHash] = keys.keySet
//
//        override def sign(
//            transaction: Transaction,
//            utxos: Utxos
//        ): Either[
//          BadInputsUTxOException | BadCollateralInputsUTxOException | MissingKeyHashesException,
//          Transaction
//        ] = {
//
//        }
//    }
//}
