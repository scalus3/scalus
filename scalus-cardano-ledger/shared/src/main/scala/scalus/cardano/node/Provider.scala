package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.cardano.address.Address

trait Provider {

    def submit(transaction: Transaction): Either[SubmitError, Unit]

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

    /** Submit transaction and wait for confirmation by polling for the first output.
      *
      * Returns when the transaction appears on the ledger, or when the [[maxAttempts]] are made or
      * [[delayMs]] elapses, whatever happens first.
      */
    def submitAndWait(
        tx: Transaction,
        maxAttempts: Int = 10,
        delayMs: Int = 500
    ): Either[SubmitError, Unit] = {
        submit(tx) match {
            case Left(error) => return Left(error)
            case Right(_)    => ()
        }

        val txOutRef = TransactionInput(tx.id, 0)
        var attempts = 0

        while attempts < maxAttempts do {
            findUtxo(txOutRef) match {
                case Right(_) => return Right(())
                case Left(_) =>
                    attempts += 1
                    if attempts < maxAttempts then Thread.sleep(delayMs)
            }
        }

        Left(
          SubmitError.NetworkError(
            s"Transaction not confirmed after ${maxAttempts} attempts (${maxAttempts * delayMs}ms)"
          )
        )
    }

//    def setSlot(slot: SlotNo): Unit
}

enum SubmitError:
    case NetworkError(message: String, exception: Option[Throwable] = None)
    case NodeError(message: String, exception: Option[Throwable] = None)
