package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

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

extension (provider: Provider)
    def toAsync: AsyncProvider = new AsyncProvider {
        def submit(transaction: Transaction)(using
            ExecutionContext
        ): Future[Either[SubmitError, TransactionHash]] =
            Future.successful(provider.submit(transaction))

        def findUtxo(input: TransactionInput)(using
            ExecutionContext
        ): Future[Either[RuntimeException, Utxo]] =
            Future.successful(provider.findUtxo(input))

        def findUtxos(inputs: Set[TransactionInput])(using
            ExecutionContext
        ): Future[Either[RuntimeException, Utxos]] =
            Future.successful(provider.findUtxos(inputs))

        def findUtxo(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin]
        )(using ExecutionContext): Future[Either[RuntimeException, Utxo]] =
            Future.successful(provider.findUtxo(address, transactionId, datum, minAmount))

        def findUtxos(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin],
            minRequiredTotalAmount: Option[Coin]
        )(using ExecutionContext): Future[Either[RuntimeException, Utxos]] =
            Future.successful(
              provider.findUtxos(address, transactionId, datum, minAmount, minRequiredTotalAmount)
            )
    }
