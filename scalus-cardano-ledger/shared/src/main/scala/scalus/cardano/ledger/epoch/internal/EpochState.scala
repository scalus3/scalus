package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{EpochNo, PoolKeyHash}
import scalus.cardano.ledger.epoch.{AccountState, RewardUpdate, SnapShots}

/** The ledger state that only epoch boundaries read and write, not the tx rules: the part of the
  * ledger's `EpochState` and `NewEpochState` outside `rules.State`, ledger-rules spec [DM-9],
  * [DM-9a].
  *
  * The pending ratify state and previous pparams of phase 5 join it later.
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
  */
case class EpochState(
    accountState: AccountState,
    lastEpoch: EpochNo,
    snapshots: SnapShots,
    blocksMadePrev: Map[PoolKeyHash, Long],
    blocksMadeCur: Map[PoolKeyHash, Long],
    pendingRewards: Option[RewardUpdate]
)
