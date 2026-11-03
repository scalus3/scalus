package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{ProtocolParams, UnitInterval}
import scalus.cardano.ledger.epoch.PoolPerformance

/** What an epoch transition reads besides the state: the configuration of the chain, ledger-rules
  * spec D84. The Emulator builds it from its `Context` and `ChainConfig`.
  *
  * @param params
  *   the pparams the reward calculation reads, spec [RW-13], D68: the current ones until phase 5
  *   stores the previous ones
  * @param epochLength
  *   the slots per epoch, `slotConfig.epochLength`
  * @param poolPerformance
  *   how the reward calculation rates the pools, spec [ES-1]
  * @param activeSlotsCoeff
  *   `f`, the share of slots with a block, spec [ES-2a]
  */
case class EpochEnv(
    params: ProtocolParams,
    epochLength: Long,
    poolPerformance: PoolPerformance,
    activeSlotsCoeff: UnitInterval
)
