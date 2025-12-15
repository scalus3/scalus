package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scalus.utils.await

import scala.concurrent.duration.DurationInt
import scala.concurrent.{blocking, ExecutionContext, Future}

/** JVM platform-specific operations for AsyncProvider.
  *
  * These operations use blocking calls like `await` and `Thread.sleep` which are only available on
  * the JVM platform.
  */
private trait AsyncProviderPlatformOps { self: AsyncProvider =>

    /** Converts this AsyncProvider to a synchronous Provider.
      *
      * This blocks the calling thread while waiting for results. Not available on JavaScript
      * platform - use the async methods directly instead.
      */
    def toSync(using executionContext: ExecutionContext): Provider = new Provider {
        def submit(transaction: Transaction): Either[SubmitError, TransactionHash] =
            self.submit(transaction).await(30.seconds)

        def findUtxo(input: TransactionInput): Either[RuntimeException, Utxo] =
            self.findUtxo(input).await(30.seconds)

        def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos] =
            self.findUtxos(inputs).await(30.seconds)

        def findUtxo(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin]
        ): Either[RuntimeException, Utxo] =
            self.findUtxo(address, transactionId, datum, minAmount).await(30.seconds)

        def findUtxos(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin],
            minRequiredTotalAmount: Option[Coin]
        ): Either[RuntimeException, Utxos] =
            self
                .findUtxos(address, transactionId, datum, minAmount, minRequiredTotalAmount)
                .await(30.seconds)
    }

    /** Waits for a transaction to be confirmed on-chain by polling for its first output.
      *
      * This method uses `Thread.sleep` for delays between polling attempts, which is only available
      * on the JVM platform. Use async alternatives on JavaScript.
      */
    def awaitConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    )(using ec: ExecutionContext): Future[Either[SubmitError, Unit]] = {
        val txOutRef = TransactionInput(txHash, 0)

        def poll(attempt: Int): Future[Either[SubmitError, Unit]] = {
            if attempt >= maxAttempts then
                Future.successful(
                  Left(
                    SubmitError.NetworkError(
                      s"Transaction not confirmed after ${maxAttempts} attempts (${maxAttempts * delayMs}ms)"
                    )
                  )
                )
            else
                self.findUtxo(txOutRef).flatMap {
                    case Right(_) => Future.successful(Right(()))
                    case Left(_) =>
                        Future {
                            blocking {
                                Thread.sleep(delayMs)
                            }
                        }.flatMap(_ => poll(attempt + 1))
                }
        }

        poll(0)
    }

    /** Submits and asynchronously waits for the transaction to be confirmed on the blockchain.
      *
      * This method uses `Thread.sleep` for delays between polling attempts, which is only available
      * on the JVM platform. Use async alternatives on JavaScript.
      */
    def submitAndWait(
        tx: Transaction,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    )(using ec: ExecutionContext): Future[Either[SubmitError, Unit]] = {
        self.submit(tx).flatMap {
            case Left(error)   => Future.successful(Left(error))
            case Right(txHash) => awaitConfirmation(txHash, maxAttempts, delayMs)
        }
    }
}
