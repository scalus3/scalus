package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.concurrent.duration.DurationInt
import scala.concurrent.{blocking, Await, ExecutionContext, Future}

trait AsyncProvider {

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

    /** Waits for a transaction to be confirmed on-chain by polling for its first output. */
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
                findUtxo(txOutRef).flatMap {
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

    /** Submits and asynchronously waits for the transaction to be submitted to the blockchain. */
    def submitAndWait(
        tx: Transaction,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    )(using ec: ExecutionContext): Future[Either[SubmitError, Unit]] = {
        submit(tx).flatMap {
            case Left(error)   => Future.successful(Left(error))
            case Right(txHash) => awaitConfirmation(txHash, maxAttempts, delayMs)
        }
    }
}

extension (provider: AsyncProvider)
    def toSync(using executionContext: ExecutionContext): Provider = new Provider {
        def submit(transaction: Transaction): Either[SubmitError, TransactionHash] =
            Await.result(provider.submit(transaction), 30.seconds)

        def findUtxo(input: TransactionInput): Either[RuntimeException, Utxo] =
            Await.result(provider.findUtxo(input), 30.seconds)

        def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos] =
            Await.result(provider.findUtxos(inputs), 30.seconds)

        def findUtxo(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin]
        ): Either[RuntimeException, Utxo] =
            Await.result(provider.findUtxo(address, transactionId, datum, minAmount), 30.seconds)

        def findUtxos(
            address: Address,
            transactionId: Option[TransactionHash],
            datum: Option[DatumOption],
            minAmount: Option[Coin],
            minRequiredTotalAmount: Option[Coin]
        ): Either[RuntimeException, Utxos] =
            Await.result(
              provider.findUtxos(address, transactionId, datum, minAmount, minRequiredTotalAmount),
              30.seconds
            )
    }
