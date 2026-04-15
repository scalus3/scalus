package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.{BlockHash, SlotNo, Transaction, TransactionHash}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.uplc.builtin.ByteString

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Future

/** Shared helper for driving an [[Engine]] from simulation-style event sources:
  *   - the mutable `StreamingEmulator` (M3, live driver over [[scalus.cardano.node.Emulator]]);
  *   - the (deferred) `ImmutableStreamingEmulator`, which threads
  *     [[scalus.testing.ImmutableEmulator]] through a scenario and drives the same engine with
  *     replayed blocks.
  *
  * Owns the streaming-local monotonic blockNo counter and synthesises deterministic
  * [[AppliedBlock]]s. Ordering contract per successful submit is the single call [[applySubmit]] —
  * subscribers see `Pending → Confirmed` in that order because both `notifySubmit` and
  * `onRollForward` land on the engine's single worker thread, in the order enqueued.
  *
  * Correctness invariant (enforced by callers): an [[AppliedBlock]] is emitted only for
  * transactions that the upstream emulator has ledger-validated and committed. `EngineDriver` does
  * not re-validate; it trusts the caller. See `StreamingEmulatorOps` for the mutable enforcement
  * path.
  */
final class EngineDriver(engine: Engine) {

    private val blockNoCounter = new AtomicLong(0L)

    /** On a successful submit: notify Pending, then drive an `AppliedBlock` carrying the single
      * transaction. Returns when the block has been processed on the worker.
      *
      * Both engine calls are enqueued synchronously on the caller thread in order, so the worker
      * processes them FIFO: Pending → block. We return the `onRollForward` future rather than
      * chaining with `flatMap` — that way a failure inside `notifySubmit`'s task body (which would
      * complete its Promise with `Failure`) does not short-circuit the block's delivery.
      * Consequence: if `notifySubmit` fails, `Pending` status is lost but `Confirmed` still fires
      * via the block path; the engine and emulator do not diverge.
      */
    def applySubmit(
        transaction: Transaction,
        hash: TransactionHash
    ): Future[Unit] = {
        val block = EngineDriver.buildAppliedBlock(
          nextBlockNo(),
          Seq(transaction -> hash)
        )
        val _ = engine.notifySubmit(hash)
        engine.onRollForward(block)
    }

    /** Produce an empty block — tip advance, no transaction events. */
    def emptyBlock(): Future[Unit] =
        engine.onRollForward(EngineDriver.buildAppliedBlock(nextBlockNo(), Seq.empty))

    private def nextBlockNo(): Long = blockNoCounter.incrementAndGet()
}

object EngineDriver {

    /** Synthetic [[BlockHash]] derived from the block number. Unique per blockNo within a single
      * [[EngineDriver]] instance so the rollback buffer's identity checks work; no on-chain meaning
      * in simulation.
      */
    private def syntheticBlockHash(blockNo: Long): BlockHash = {
        val buf = ByteBuffer.allocate(32)
        buf.position(24)
        buf.putLong(blockNo)
        BlockHash.fromByteString(ByteString.fromArray(buf.array()))
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
