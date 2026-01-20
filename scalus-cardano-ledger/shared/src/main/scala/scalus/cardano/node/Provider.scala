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

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams]

    def submit(transaction: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]]

    /** Find a single UTxO by its transaction input.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxo(input: TransactionInput)(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxo]]

    /** Find UTxOs by a set of transaction inputs.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]]

    /** Find a single UTxO by address and optional filters.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxo]]

    /** Find UTxOs by address and optional filters.
      *
      * @deprecated
      *   Use findUtxos(UtxoQuery) instead
      */
    @deprecated("Use findUtxos(UtxoQuery) instead", "0.14.2")
    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]]

    /** Find UTxOs using a type-safe query.
      *
      * @param query
      *   The query specifying source, filters, and pagination
      * @return
      *   Either a UtxoQueryError or the matching UTxOs
      */
    def findUtxos(query: UtxoQuery)(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]]
}

enum SubmitError:
    case NetworkError(message: String, exception: Option[Throwable] = None)
    case NodeError(message: String, exception: Option[Throwable] = None)
