package scalus.cardano.node.emulator

import scalus.cardano.ledger.{DataHash, KeepRaw, SlotNo, Transaction}
import scalus.cardano.ledger.epoch.internal.{EpochState, EpochTransition}
import scalus.cardano.ledger.rules.{Context, State}
import scalus.cardano.node.EmulatorBase
import scalus.uplc.builtin.Data

/** The ledger as it stands after a block or after a mempool tx, spec section 2.
  *
  * Snapshots share their `Context` with later snapshots, so no transition writes `Context.fee`,
  * spec [NL-3].
  *
  * The last epoch boundary its ledger ran is `epoch.lastEpoch`, ledger-rules spec D66. It is mostly
  * the epoch of `context.env.slot`, but not after the `Instant` preset moved the slot back below
  * the tip.
  *
  * @param ledger
  *   the ledger state: the UTxO set, the certificate state, the fees and the donations
  * @param context
  *   the validation context, carrying the slot, the protocol parameters and the treasury
  * @param binaryDatums
  *   every datum seen so far, by hash, with its original bytes
  * @param epoch
  *   the state only epoch boundaries read and write, ledger-rules spec [DM-8]. Its treasury is
  *   `context.env.treasury`, spec [DM-13].
  */
private[node] case class LedgerSnapshot(
    ledger: State,
    context: Context,
    binaryDatums: Map[DataHash, KeepRaw[Data]],
    epoch: EpochState
) {

    /** The snapshot ticked to `slot`, the tick of spec [NL-2]: one epoch transition for each
      * boundary from the one after `epoch.lastEpoch` to the epoch of `slot`, in order, ledger-rules
      * spec [EB-1], [EB-1a]; none when the move crosses no boundary, spec [EB-2], or goes
      * backwards. The env takes `slot` and the treasury of the account state, spec [DM-13]. The
      * same snapshot when it is at `slot` already.
      */
    private[node] def atSlot(slot: SlotNo): LedgerSnapshot =
        if context.env.slot == slot then this
        else
            val boundaries = epoch.lastEpoch + 1 to context.slotConfig.epochOf(slot)
            val (newLedger, newEpoch) = boundaries.foldLeft((ledger, epoch)) {
                case ((l, e), boundary) => EpochTransition.transition(l, e, boundary)
            }
            val env = context.env.copy(slot = slot, treasury = newEpoch.accountState.treasury)
            LedgerSnapshot(newLedger, context.copy(env = env), binaryDatums, newEpoch)

    /** The snapshot after `tx` moved the ledger to `newLedger`: the tx's datums join the store. */
    private[node] def after(newLedger: State, tx: Transaction): LedgerSnapshot =
        copy(ledger = newLedger, binaryDatums = binaryDatums ++ EmulatorBase.binaryDatumsOf(tx))
}
