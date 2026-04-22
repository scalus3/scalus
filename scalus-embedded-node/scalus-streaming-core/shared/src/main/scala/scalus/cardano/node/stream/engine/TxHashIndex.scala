package scalus.cardano.node.stream.engine

import scalus.cardano.ledger.TransactionHash
import scalus.cardano.node.TransactionStatus
import scalus.cardano.node.stream.ChainPoint

import scala.collection.mutable

/** Rolling index of transaction hashes observed in the rollback buffer, plus the set of hashes we
  * have locally submitted but not yet confirmed.
  *
  * Lookup precedence matches the doc's fallback chain:
  *
  *   1. confirmed in the volatile tail → `Confirmed`
  *   2. our own pending submission → `Pending`
  *   3. (caller's job) delegate to backup
  *
  * Not thread-safe.
  */
final class TxHashIndex {

    private val confirmedAt = mutable.Map.empty[TransactionHash, ChainPoint]

    /** Hashes we submitted ourselves. Lifetime-stable: NOT cleared on confirmation, so that a
      * rollback of a confirmation correctly reverts to `Pending` rather than `NotFound`. Trims are
      * caller-driven (see [[clearOwnSubmission]]) — in practice an app that wants bounded memory
      * can drop the hash once it's "deep enough" (past `securityParam`).
      */
    private val ownSubmissions = mutable.Set.empty[TransactionHash]

    /** Per-block tx-id lists, aligned with the rollback buffer. We keep a queue of `(point, ids)`
      * so we can evict/undo per block rather than scanning the whole map.
      */
    private val perBlock = mutable.ArrayDeque.empty[(ChainPoint, Set[TransactionHash])]

    def applyForward(block: AppliedBlock): Unit = {
        val ids = block.transactionIds
        ids.foreach(h => confirmedAt.update(h, block.point))
        perBlock += (block.point -> ids)
    }

    /** Undo the most recently applied block's confirmations.
      *
      * Returns the set of tx-ids whose status just changed so the engine can fire subscriber events
      * for the new (`Pending` if in [[ownSubmissions]], else `NotFound`) status.
      */
    def applyBackward(): Set[TransactionHash] = {
        if perBlock.isEmpty then Set.empty
        else {
            val (_, ids) = perBlock.removeLast()
            ids.foreach(confirmedAt.remove)
            ids
        }
    }

    /** Drop the per-block journal entry for a block that just aged out of the rollback window.
      * Confirmed-at entries stay — a tx older than the rollback horizon is still `Confirmed` from
      * the app's point of view.
      */
    def forgetBlock(point: ChainPoint): Unit = {
        if perBlock.nonEmpty && perBlock.head._1 == point then perBlock.removeHead()
    }

    def recordOwnSubmission(hash: TransactionHash): Unit =
        ownSubmissions += hash

    /** Explicit cleanup hook. The engine doesn't call this; apps that care about memory can after
      * they're sure a submission has passed the rollback horizon.
      */
    def clearOwnSubmission(hash: TransactionHash): Unit =
        ownSubmissions -= hash

    def statusOf(hash: TransactionHash): Option[TransactionStatus] =
        if confirmedAt.contains(hash) then Some(TransactionStatus.Confirmed)
        else if ownSubmissions.contains(hash) then Some(TransactionStatus.Pending)
        else None

    def confirmationPoint(hash: TransactionHash): Option[ChainPoint] =
        confirmedAt.get(hash)

    /** Snapshot of the own-submission set for persistence. Immutable copy. */
    def ownSubmissionsSnapshot: Set[TransactionHash] = ownSubmissions.toSet
}
