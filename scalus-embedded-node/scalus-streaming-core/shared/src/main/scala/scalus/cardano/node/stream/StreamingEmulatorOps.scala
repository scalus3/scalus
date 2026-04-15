package scalus.cardano.node.stream

import scalus.cardano.ledger.{BlockHash, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.node.stream.engine.{AppliedBlock, AppliedTransaction}
import scalus.uplc.builtin.ByteString

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.{ExecutionContext, Future}

/** Mix-in for a [[BaseStreamProvider]] that is backed by a [[scalus.cardano.node.Emulator]].
  *
  * Turns emulator activity into [[AppliedBlock]] events fed into the streaming engine:
  *   - Overrides `submit` to run [[Emulator.submitSync]] (ledger-validated: the emulator's
  *     `validators` + `mutators` pipeline runs before state is committed), and on success
  *     signals the engine via `notifySubmit(hash) → onRollForward(block)`.
  *   - Exposes `newEmptyBlock()` so tests can advance the tip without a transaction.
  *
  * **Correctness invariant.** An [[AppliedBlock]] is emitted only on
  * `Right(hash)` from `submitSync`, i.e. only after all validators + mutators pass and the
  * emulator commits the new state. Rejected transactions never surface as events.
  *
  * Block / slot numbering is streaming-local: the wrapper maintains its own monotonic counter
  * so subscribers always see a strictly increasing tip. It does not try to mirror the
  * emulator's internal slot — the emulator remains authoritative for rule evaluation, but
  * chain-position identity here is an implementation detail of the stream view.
  */
trait StreamingEmulatorOps[F[_], C[_]] extends BaseStreamProvider[F, C] {

    protected def emulator: Emulator

    private val blockNoCounter = new AtomicLong(0L)

    override def submit(
        transaction: Transaction
    ): F[Either[SubmitError, TransactionHash]] = liftFuture {
        emulator.submitSync(transaction) match
            case Left(e) => Future.successful(Left(e))
            case Right(hash) =>
                given ExecutionContext = ExecutionContext.parasitic
                val block = StreamingEmulatorOps.buildAppliedBlock(
                  nextBlockNo(),
                  Seq(transaction -> hash)
                )
                for
                    _ <- engine.notifySubmit(hash)
                    _ <- engine.onRollForward(block)
                yield Right(hash)
    }

    /** Produce an empty block — advances the tip without submitting a transaction. */
    def newEmptyBlock(): F[Unit] = liftFuture {
        engine.onRollForward(
          StreamingEmulatorOps.buildAppliedBlock(nextBlockNo(), Seq.empty)
        )
    }

    private def nextBlockNo(): Long = blockNoCounter.incrementAndGet()
}

object StreamingEmulatorOps {

    /** Synthetic [[BlockHash]] derived from the block number. Good enough for the rollback
      * buffer's identity checks; has no on-chain meaning in simulation.
      */
    private def syntheticBlockHash(blockNo: Long): BlockHash = {
        val arr = new Array[Byte](32)
        var i = 0
        while i < 8 do
            arr(24 + i) = ((blockNo >>> ((7 - i) * 8)) & 0xffL).toByte
            i += 1
        BlockHash.fromByteString(ByteString.fromArray(arr))
    }

    private[stream] def buildAppliedBlock(
        blockNo: Long,
        txs: Seq[(Transaction, TransactionHash)]
    ): AppliedBlock = {
        val slot: SlotNo = blockNo
        val point = ChainPoint(slot, syntheticBlockHash(blockNo))
        val tip = ChainTip(point, blockNo)
        val applied = txs.map { (tx, hash) =>
            AppliedTransaction(
              id = hash,
              inputs = tx.body.value.inputs.toSet,
              outputs = tx.body.value.outputs.map(_.value).toIndexedSeq
            )
        }
        AppliedBlock(tip, applied)
    }
}
