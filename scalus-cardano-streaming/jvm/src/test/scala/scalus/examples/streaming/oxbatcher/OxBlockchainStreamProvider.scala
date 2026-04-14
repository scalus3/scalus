package scalus.examples.streaming.oxbatcher

import ox.flow.Flow
import scalus.cardano.ledger.{
    CardanoInfo,
    ProtocolParams,
    SlotNo,
    Transaction,
    TransactionHash,
    Utxos
}
import scalus.cardano.node.{SubmitError, TransactionStatus, UtxoQuery, UtxoQueryError}
import scalus.cardano.node.stream.BaseStreamProvider

import OxScalusAsyncStream.{Id, given}

/** ox specialisation of [[BaseStreamProvider]].
  *
  * Inherits the per-subscription registry and channel allocation from
  * the base class; the streaming side delivers events as `ox.flow.Flow`
  * to consumers. Snapshot-side methods are stubbed pending composition
  * with an existing `BlockchainProvider`.
  */
class OxBlockchainStreamProvider(val cardanoInfo: CardanoInfo)
    extends BaseStreamProvider[Id, Flow] {

    def close(): Id[Unit] = closeAllSinks()

    private def unimplemented[T]: T =
        throw new NotImplementedError("snapshot side not wired")

    def fetchLatestParams: Id[ProtocolParams] = unimplemented
    def findUtxos(query: UtxoQuery): Id[Either[UtxoQueryError, Utxos]] = unimplemented
    def currentSlot: Id[SlotNo] = unimplemented
    def checkTransaction(txHash: TransactionHash): Id[TransactionStatus] = unimplemented
    def submit(transaction: Transaction): Id[Either[SubmitError, TransactionHash]] =
        unimplemented
    def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): Id[TransactionStatus] = unimplemented
    def submitAndPoll(
        transaction: Transaction,
        maxAttempts: Int,
        delayMs: Long
    ): Id[Either[SubmitError, TransactionHash]] = unimplemented
}
