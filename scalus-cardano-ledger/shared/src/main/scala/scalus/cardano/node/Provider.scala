package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

trait Provider {

    def submit(transaction: Transaction): Either[SubmitError, TransactionHash]

    def findUtxo(input: TransactionInput): Either[RuntimeException, Utxo]

    def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos]

    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxo]

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxos]
}

enum SubmitError:
    case NetworkError(message: String, exception: Option[Throwable] = None)
    case NodeError(message: String, exception: Option[Throwable] = None)
