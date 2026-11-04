package scalus.cardano.node.emulator

import scalus.cardano.ledger.{DataHash, KeepRaw, LedgerState, SlotNo, Transaction, UTxOState}
import scalus.cardano.ledger.epoch.{EpochState, FuturePParams, NewEpochState, NonMyopic, PoolDistr}
import scalus.cardano.ledger.epoch.internal.{EpochBookkeeping, EpochEnv, EpochTransition}
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
  *   the ledger state: the UTxO set, the certificate state, the gov state, the deposits owed, the
  *   fees and the donations
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
    epoch: EpochBookkeeping
) {

    /** The snapshot ticked to `slot`, the tick of spec [NL-2]: one epoch transition for each
      * boundary from the one after `epoch.lastEpoch` to the epoch of `slot`, in order, ledger-rules
      * spec [EB-1], [EB-1a]; none when the move crosses no boundary, spec [EB-2], or goes
      * backwards. The env takes `slot`, the treasury of the account state, spec [DM-13], and the
      * current pparams of the last transition: each transition takes the pparams the one before it
      * left, spec [EB-25], D88. The same snapshot when it is at `slot` already. The transitions
      * read the reward settings of `config`, ledger-rules spec D84.
      */
    private[node] def atSlot(slot: SlotNo, config: ChainConfig): LedgerSnapshot =
        if context.env.slot == slot then this
        else
            // ceiling: the cost is linear in the boundaries crossed, one full transition each; add
            // a cap or a lazy tick if a caller jumps millions of epochs
            val boundaries = epoch.lastEpoch + 1 to context.slotConfig.epochOf(slot)
            val (newLedger, newEpoch, params) =
                boundaries.foldLeft((ledger, epoch, context.env.params)) {
                    case ((l, e, params), boundary) =>
                        val env = EpochEnv(
                          params,
                          context.slotConfig.epochLength,
                          config.poolPerformance,
                          config.activeSlotsCoeff
                        )
                        EpochTransition.transition(l, e, boundary, env)
                }
            val env = context.env.copy(
              slot = slot,
              params = params,
              treasury = newEpoch.accountState.treasury
            )
            LedgerSnapshot(newLedger, context.copy(env = env), binaryDatums, newEpoch)

    /** This ledger in the shape of the node's `NewEpochState`, ledger-rules spec D122. The pool
      * distribution is that of `set`: the node takes it from `mark` before SNAP moves `mark` to
      * `set` (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/NewEpoch.hs:182), D123.
      */
    private[node] def newEpochState: NewEpochState = {
        val utxoState = UTxOState(
          ledger.utxos,
          ledger.deposited,
          ledger.fees,
          ledger.govState,
          ledger.stakeDistribution,
          ledger.donation
        )
        val epochState = EpochState(
          epoch.accountState,
          LedgerState(utxoState, ledger.certState),
          epoch.snapshots,
          NonMyopic.empty,
          context.env.params,
          epoch.prevParams,
          FuturePParams.of(epoch.pendingRatify, ledger.govState),
          epoch.pendingRatify
        )
        NewEpochState(
          epoch.lastEpoch,
          epoch.blocksMadePrev,
          epoch.blocksMadeCur,
          epochState,
          epoch.pendingRewards,
          PoolDistr.of(epoch.snapshots.set)
        )
    }

    /** The snapshot after `tx` moved the ledger to `newLedger`: the tx's datums join the store. */
    private[node] def after(newLedger: State, tx: Transaction): LedgerSnapshot =
        copy(ledger = newLedger, binaryDatums = binaryDatums ++ EmulatorBase.binaryDatumsOf(tx))
}
