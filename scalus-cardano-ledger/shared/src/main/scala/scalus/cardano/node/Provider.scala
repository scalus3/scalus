package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

/** Provider for Cardano blockchain operations.
  *
  * Provider is the cross-platform interface for interacting with Cardano nodes. All methods return
  * `Future` values and work on both JVM and JavaScript platforms.
  *
  * Use `scalus.utils.await` extension for blocking operations on JVM when needed.
  */
trait Provider {

    def submit(transaction: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]]

    def findUtxo(input: TransactionInput)(using
        ExecutionContext
    ): Future[Either[RuntimeException, Utxo]]

    def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[RuntimeException, Utxos]]

    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[RuntimeException, Utxo]]

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[RuntimeException, Utxos]]
}

enum SubmitError:
    case NetworkError(message: String, exception: Option[Throwable] = None)
    case NodeError(message: String, exception: Option[Throwable] = None)
