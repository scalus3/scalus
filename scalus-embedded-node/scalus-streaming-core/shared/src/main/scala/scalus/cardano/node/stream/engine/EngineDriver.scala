package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{BlockHash, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.uplc.builtin.ByteString

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.{ExecutionContext, Future}

/** Shared helper for driving an [[Engine]] from simulation-style event sources:
  *   - the mutable `StreamingEmulator` (M3, live driver over [[scalus.cardano.node.Emulator]]);
  *   - the (deferred) `ImmutableStreamingEmulator`, which threads
  *     [[scalus.testing.ImmutableEmulator]] through a scenario and drives the same engine with
  *     replayed blocks.
  *
  * Owns the streaming-local monotonic blockNo counter and synthesises deterministic
  * [[AppliedBlock]]s. Ordering contract per successful submit is the single call
  * [[applySubmit]] — subscribers see `Pending → Confirmed` in that order because both
  * `notifySubmit` and `onRollForward` land on the engine's single worker thread, in the
  * order enqueued.
  *
  * Correctness invariant (enforced by callers): an [[AppliedBlock]] is emitted only for
  * transactions that the upstream emulator has ledger-validated and committed. `EngineDriver`
  * does not re-validate; it trusts the caller. See `StreamingEmulatorOps` for the mutable
  * enforcement path.
  */
final class EngineDriver(engine: Engine) {

    private val blockNoCounter = new AtomicLong(0L)

    /** On a successful submit: notify Pending, then drive an `AppliedBlock` carrying the
      * single transaction. Returns when both have been processed on the worker.
      */
    def applySubmit(
        transaction: Transaction,
        hash: TransactionHash
    ): Future[Unit] = {
        given ExecutionContext = ExecutionContext.parasitic
        val block = EngineDriver.buildAppliedBlock(
          nextBlockNo(),
          Seq(transaction -> hash)
        )
        for
            _ <- engine.notifySubmit(hash)
            _ <- engine.onRollForward(block)
        yield ()
    }

    /** Produce an empty block — tip advance, no transaction events. */
    def emptyBlock(): Future[Unit] =
        engine.onRollForward(EngineDriver.buildAppliedBlock(nextBlockNo(), Seq.empty))

    private def nextBlockNo(): Long = blockNoCounter.incrementAndGet()
}

object EngineDriver {

    /** Synthetic [[BlockHash]] derived from the block number. Unique per blockNo so the
      * rollback buffer's identity checks work; no on-chain meaning in simulation.
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
