package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.EpochNo
import scalus.cardano.ledger.epoch.AccountState

/** The ledger state that only epoch boundaries read and write, not the tx rules: the part of the
  * ledger's `EpochState` and `NewEpochState` outside `rules.State`, ledger-rules spec [DM-9].
  *
  * Phase 3 holds the account state and the last epoch. The snapshots, the blocks made, the pending
  * reward update and the pending ratify state of phases 4 and 5 join it later.
  *
  * @param accountState
  *   the treasury and the reserves, `esChainAccountState`
  * @param lastEpoch
  *   the epoch of the last boundary the state ran, `nesEL`, D66: not always the epoch of the
  *   snapshot's slot, since the `Instant` preset lets the slot move back below the tip
  */
case class EpochState(accountState: AccountState, lastEpoch: EpochNo)
