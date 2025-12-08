package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.cardano.address.Address

import scala.annotation.tailrec
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}

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

    /** Waits for a transaction to be confirmed on-chain by polling for its first output. */
    def awaitConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    )(using ec: ExecutionContext): Future[Either[SubmitError, Unit]] = Future {
        val txOutRef = TransactionInput(txHash, 0)

        @tailrec
        def poll(attempt: Int): Either[SubmitError, Unit] = {
            if attempt >= maxAttempts then
                Left(
                  SubmitError.NetworkError(
                    s"Transaction not confirmed after ${maxAttempts} attempts (${maxAttempts * delayMs}ms)"
                  )
                )
            else
                findUtxo(txOutRef) match {
                    case Right(_) => Right(())
                    case Left(_) =>
                        blocking {
                            Thread.sleep(delayMs)
                        }
                        poll(attempt + 1)
                }
        }

        poll(0)
    }

    /** Submits and synchronously waits for the transaction to be submitted to the blockchain. */
    def submitAndWait(
        tx: Transaction,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    ): Either[SubmitError, Unit] = {
        submit(tx) match {
            case Left(error) => Left(error)
            case Right(txHash) =>
                given ExecutionContext = ExecutionContext.global
                val timeout = (maxAttempts * delayMs + 1000).millis
                try {
                    Await.result(awaitConfirmation(txHash, maxAttempts, delayMs), timeout)
                } catch {
                    case _: java.util.concurrent.TimeoutException =>
                        Left(
                          SubmitError.NetworkError(
                            s"Transaction confirmation timed out after $timeout"
                          )
                        )
                }
        }
    }

    /** Submits and asynchronously waits for the transaction to be submitted to the blockchain. */
    def submitAsync(
        tx: Transaction,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    )(using ec: ExecutionContext): Future[Either[SubmitError, Unit]] = {
        submit(tx) match {
            case Left(error)   => Future.successful(Left(error))
            case Right(txHash) => awaitConfirmation(txHash, maxAttempts, delayMs)
        }
    }

//    def setSlot(slot: SlotNo): Unit
}

enum SubmitError:
    case NetworkError(message: String, exception: Option[Throwable] = None)
    case NodeError(message: String, exception: Option[Throwable] = None)
