package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
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

import Fs2ScalusAsyncStream.{IOStream, given}

/** fs2 / cats-effect specialisation of [[BaseStreamProvider]].
  *
  * Inherits the per-subscription registry and channel allocation from
  * the base class. Snapshot-side `BlockchainProviderTF[IO]` methods
  * are stubbed pending composition with an existing
  * `BlockchainProvider` (Blockfrost or user-supplied).
  */
class Fs2BlockchainStreamProvider(
    val cardanoInfo: CardanoInfo
)(using Dispatcher[IO], scala.concurrent.ExecutionContext)
    extends BaseStreamProvider[IO, IOStream] {

    def close(): IO[Unit] = IO(closeAllSinks())

    private def unimplemented[T]: IO[T] =
        IO.raiseError(new NotImplementedError("snapshot side not wired"))

    def fetchLatestParams: IO[ProtocolParams] = unimplemented
    def findUtxos(query: UtxoQuery): IO[Either[UtxoQueryError, Utxos]] = unimplemented
    def currentSlot: IO[SlotNo] = unimplemented
    def checkTransaction(txHash: TransactionHash): IO[TransactionStatus] = unimplemented
    def submit(transaction: Transaction): IO[Either[SubmitError, TransactionHash]] =
        unimplemented
    def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): IO[TransactionStatus] = unimplemented
    def submitAndPoll(
        transaction: Transaction,
        maxAttempts: Int,
        delayMs: Long
    ): IO[Either[SubmitError, TransactionHash]] = unimplemented
}
