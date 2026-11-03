package scalus.cardano.ledger.epoch

import scalus.cardano.ledger.{Coin, Credential}

/** The reward update one epoch boundary computes and the next applies, the ledger's `RewardUpdate`
  * without `nonMyopic` (shelley RewardUpdate.hs:111-131), ledger-rules spec [DM-11a].
  *
  * The four parts sum to 0, spec [RW-9]: `deltaT + deltaR + Σ rewards + deltaF = 0`.
  *
  * @param deltaT
  *   the change to the treasury, `floor(tau · rPot)`
  * @param deltaR
  *   the change to the reserves, with its sign: what was not paid out minus what was taken
  * @param rewards
  *   the reward of each credential, the leader and member rewards of one credential summed, spec
  *   [RW-7]. A member reward of 0 is not an entry; the leader reward of every pool that earns is,
  *   also when it is 0, spec [RW-8a]
  * @param deltaF
  *   the change to the fees, minus the fee snapshot
  */
case class RewardUpdate(
    deltaT: Coin,
    deltaR: Coin,
    rewards: Map[Credential, Coin],
    deltaF: Coin
)
