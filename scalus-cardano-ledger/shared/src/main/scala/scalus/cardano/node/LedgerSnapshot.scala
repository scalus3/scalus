package scalus.cardano.node

import scalus.cardano.ledger.{DataHash, KeepRaw, SlotNo, Transaction}
import scalus.cardano.ledger.rules.{Context, State}
import scalus.uplc.builtin.Data

/** The ledger as it stands after a block or after a mempool tx, spec section 2.
  *
  * Snapshots share their `Context` with later snapshots, so no transition writes `Context.fee`,
  * spec [NL-3].
  *
  * @param ledger
  *   the ledger state: the UTxO set, the certificate state, the fees and the donations
  * @param context
  *   the validation context, carrying the slot, the protocol parameters and the treasury
  * @param binaryDatums
  *   every datum seen so far, by hash, with its original bytes
  */
private[node] case class LedgerSnapshot(
    ledger: State,
    context: Context,
    binaryDatums: Map[DataHash, KeepRaw[Data]]
) {

    /** The snapshot ticked to `slot`, the tick of spec [NL-2]: [[EmulatorBase.atSlot]], and nothing
      * more. The same snapshot when it is at `slot` already.
      */
    private[node] def atSlot(slot: SlotNo): LedgerSnapshot =
        if context.env.slot == slot then this
        else
            val (newLedger, env) =
                EmulatorBase.atSlot(ledger, context.env, context.slotConfig, slot)
            copy(ledger = newLedger, context = context.copy(env = env))

    /** The snapshot after `tx` moved the ledger to `newLedger`: the tx's datums join the store. */
    private[node] def after(newLedger: State, tx: Transaction): LedgerSnapshot =
        LedgerSnapshot(newLedger, context, binaryDatums ++ EmulatorBase.binaryDatumsOf(tx))
}
