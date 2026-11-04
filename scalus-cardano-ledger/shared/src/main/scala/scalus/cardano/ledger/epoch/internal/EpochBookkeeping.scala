package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{EpochNo, PoolKeyHash, ProtocolParams}
import scalus.cardano.ledger.epoch.{AccountState, RatifyState, RewardUpdate, SnapShots}

/** The ledger state that only epoch boundaries read and write, not the tx rules: the part of the
  * ledger's `EpochState` and `NewEpochState` outside `rules.State`, ledger-rules spec [DM-9],
  * [DM-9a], [DM-9b]. The current pparams stay in `Context.env.params`, spec [DM-7].
  *
  * @param accountState
  *   the treasury and the reserves, `esChainAccountState`
  * @param lastEpoch
  *   the epoch of the last boundary the state ran, `nesEL`, D66: not always the epoch of the
  *   snapshot's slot, since the `Instant` preset lets the slot move back below the tip
  * @param snapshots
  *   the stake snapshots, `esSnapshots`, spec [EB-6]
  * @param blocksMadePrev
  *   the blocks each pool made in the epoch before the last boundary, `nesBprev`
  * @param blocksMadeCur
  *   the blocks each pool made since the last boundary, `nesBcur`, spec [ES-4]
  * @param pendingRewards
  *   the reward update the last boundary computed and the next one applies, `nesRu`, spec [EB-22],
  *   [RW-21]; `None` at genesis, spec [ES-8a]
  * @param prevParams
  *   the pparams before the last boundary, `cgsPrevPParams`, which the reward calculation reads,
  *   spec [EB-22a], D68; the genesis pparams at genesis, spec [DM-9c]
  * @param pendingRatify
  *   the RATIFY result the last boundary computed and the next one enacts, the result of
  *   `cgsDRepPulsingState`, spec [EB-10a], [EB-20]; at genesis the enact state of the genesis with
  *   nothing enacted or expired, spec [DM-9c], D85
  */
case class EpochBookkeeping(
    accountState: AccountState,
    lastEpoch: EpochNo,
    snapshots: SnapShots,
    blocksMadePrev: Map[PoolKeyHash, Long],
    blocksMadeCur: Map[PoolKeyHash, Long],
    pendingRewards: Option[RewardUpdate],
    prevParams: ProtocolParams,
    pendingRatify: RatifyState
)
