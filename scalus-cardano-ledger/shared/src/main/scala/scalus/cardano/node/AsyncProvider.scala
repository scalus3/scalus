package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

/** Asynchronous provider for Cardano blockchain operations.
  *
  * AsyncProvider is the cross-platform interface for interacting with Cardano nodes. All methods
  * return `Future` values and work on both JVM and JavaScript platforms.
  *
  * ## Platform Support
  *
  *   - **JVM**: Provides additional blocking methods via `AsyncProviderPlatformOps`:
  *     - `toSync` - converts to synchronous `Provider`
  *     - `awaitConfirmation` - polls for transaction confirmation with `Thread.sleep`
  *     - `submitAndWait` - submits and waits for confirmation
  *   - **JavaScript**: Only async methods are available. Use `Future` composition for workflows.
  *
  * @see
  *   [[Provider]] for synchronous operations (JVM only)
  */
trait AsyncProvider extends AsyncProviderPlatformOps {

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
