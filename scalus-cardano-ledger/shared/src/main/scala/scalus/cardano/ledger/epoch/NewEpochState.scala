package scalus.cardano.ledger.epoch

import scalus.cardano.ledger.{Coin, EpochNo, GovAction, GovState, LedgerState, NonNegativeInterval, PoolKeyHash, ProtocolParams, UnitInterval, VrfKeyHash}

/** The whole ledger state of the Emulator in the shape of the node's `NewEpochState`
  * (eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState/Types.hs:308-333), ledger-rules spec
  * D122. A read result: the Emulator builds it, a caller reads it; a new field later adds an
  * accessor only. `stashedAVVMAddresses` is left out: it is empty after the Allegra era. Java reads
  * the maps and the reward update through the getters of [[NewEpochStatePlatform]], D128.
  *
  * @param lastEpoch
  *   the epoch of the last boundary the ledger ran, `nesEL`, D66
  * @param blocksMadePrev
  *   the blocks each pool made in the epoch before the last boundary, `nesBprev`
  * @param blocksMadeCur
  *   the blocks each pool made since the last boundary, `nesBcur`
  * @param epochState
  *   the epoch state, `nesEs`
  * @param pendingRewards
  *   the reward update the last boundary computed and the next one applies, `nesRu`; the Emulator
  *   computes it whole at the boundary, so it is never a pulser, D3
  * @param poolDistr
  *   the stake distribution of the pools for this epoch, `nesPd`, D123
  */
final case class NewEpochState private[scalus] (
    lastEpoch: EpochNo,
    blocksMadePrev: Map[PoolKeyHash, Long],
    blocksMadeCur: Map[PoolKeyHash, Long],
    epochState: EpochState,
    pendingRewards: Option[RewardUpdate],
    poolDistr: PoolDistr
) extends NewEpochStatePlatform

/** The node's `EpochState`
  * (eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState/Types.hs:66-75) with the parts of the
  * node's `ConwayGovState` (eras/conway/impl/src/Cardano/Ledger/Conway/Governance.hs:245-252) that
  * only epoch boundaries write, D122.
  *
  * In the node, `params`, `prevParams`, `futureParams` and the ratify pulser live in the gov state
  * of `ledgerState`. Here they sit beside it: the Scalus `GovState` holds only the part the tx
  * rules read and write, D27, and the current pparams stay in the validation context, spec [DM-7].
  *
  * @param accountState
  *   the treasury and the reserves, `esChainAccountState`
  * @param ledgerState
  *   the ledger state of the tx rules, `esLState`
  * @param snapshots
  *   the stake snapshots and the fee snapshot, `esSnapshots`
  * @param nonMyopic
  *   the pool-ranking data, `esNonMyopic`; always [[NonMyopic.empty]], D124
  * @param params
  *   the current pparams, `cgsCurPParams`
  * @param prevParams
  *   the pparams before the last boundary, `cgsPrevPParams`
  * @param futureParams
  *   the pparams the next boundary adopts, `cgsFuturePParams`, D125
  * @param pendingRatify
  *   the ratify result the next boundary enacts, the result of `cgsDRepPulsingState`
  */
final case class EpochState private[scalus] (
    accountState: AccountState,
    ledgerState: LedgerState,
    snapshots: SnapShots,
    nonMyopic: NonMyopic,
    params: ProtocolParams,
    prevParams: ProtocolParams,
    futureParams: FuturePParams,
    pendingRatify: RatifyState
)

/** The pool-ranking data of the node, `NonMyopic`
  * (eras/shelley/impl/src/Cardano/Ledger/Shelley/PoolRank.hs:231-235). Not part of the protocol:
  * only the wallet ranking `getNonMyopicMemberRewards` reads it. The Emulator does not compute it,
  * so it is always [[NonMyopic.empty]], D124.
  *
  * @param likelihoods
  *   the log-weights of each pool's block counts, `likelihoodsNM` (a `Likelihood` is a sequence of
  *   `Float` log-weights, PoolRank.hs:88-101)
  * @param rewardPot
  *   the reward pot of the last reward update, `rewardPotNM`
  */
final case class NonMyopic private[scalus] (
    likelihoods: Map[PoolKeyHash, IndexedSeq[Float]],
    rewardPot: Coin
) extends NonMyopicPlatform

object NonMyopic {

    /** No likelihoods and no reward pot, the node's `def`. */
    val empty: NonMyopic = NonMyopic(Map.empty, Coin.zero)
}

/** The stake distribution of the pools, the node's `PoolDistr`
  * (libs/cardano-ledger-core/src/Cardano/Ledger/State/PoolDistr.hs:98-106), D123.
  *
  * @param pools
  *   the stake of each pool with at least one delegator, `unPoolDistr`
  * @param totalActiveStake
  *   the total active stake of the snapshot, at least 1 lovelace, `pdTotalActiveStake`
  */
final case class PoolDistr private[scalus] (
    pools: Map[PoolKeyHash, IndividualPoolStake],
    totalActiveStake: Coin
) extends PoolDistrPlatform

object PoolDistr {

    /** The pool distribution of `snapshot`, `calculatePoolDistr`
      * (libs/cardano-ledger-core/src/Cardano/Ledger/State/SnapShots.hs:446-463): every pool with a
      * delegator, its share of the total active stake in lowest terms, its stake and its VRF key.
      */
    private[scalus] def of(snapshot: StakeSnapshot): PoolDistr = {
        val total = snapshot.totalActiveStake
        val pools = snapshot.pools.collect {
            case (id, pool) if pool.numDelegators > 0 =>
                val share =
                    NonNegativeInterval.reduceBigInt(BigInt(pool.stake.value), BigInt(total.value))
                id -> IndividualPoolStake(
                  UnitInterval(share.numerator, share.denominator),
                  pool.stake,
                  pool.vrfKeyHash
                )
        }
        PoolDistr(pools, total)
    }
}

/** The stake of one pool, the node's `IndividualPoolStake`
  * (libs/cardano-ledger-core/src/Cardano/Ledger/State/PoolDistr.hs:54-62).
  *
  * @param stakeShare
  *   the pool's stake over the total active stake, in lowest terms, `individualPoolStake`
  * @param totalStake
  *   the stake delegated to the pool, `individualTotalPoolStake`
  * @param vrfKeyHash
  *   the VRF key of the pool params, `individualPoolStakeVrf`
  */
final case class IndividualPoolStake private[scalus] (
    stakeShare: UnitInterval,
    totalStake: Coin,
    vrfKeyHash: VrfKeyHash
)

/** The pparams the next epoch boundary adopts, the node's `FuturePParams`
  * (libs/cardano-ledger-core/src/Cardano/Ledger/State/Governance.hs:90-101), D125.
  *
  * The node has a third case, `PotentialPParamsUpdate`, for the time before the ratify pulser is
  * done. The Emulator computes the ratify result whole at the boundary, D3, so the update is always
  * known: it is never potential. Java reads the case with `instanceof
  * FuturePParams.DefinitePParamsUpdate` and its `params()`, and builds one with the static
  * forwarders `FuturePParams.noPParamsUpdate()` and `definitePParamsUpdate(params)`, interop rule
  * 9.
  */
enum FuturePParams {

    /** The next boundary does not change the pparams. */
    case NoPParamsUpdate

    /** The next boundary adopts `params`. */
    case DefinitePParamsUpdate(params: ProtocolParams)
}

object FuturePParams {

    /** [[NoPParamsUpdate]], for Java, interop rule 9. */
    def noPParamsUpdate: FuturePParams = NoPParamsUpdate

    /** [[DefinitePParamsUpdate]], for Java, interop rule 9. */
    def definitePParamsUpdate(params: ProtocolParams): FuturePParams =
        DefinitePParamsUpdate(params)

    /** The update of `ratify`, `predictFuturePParams`
      * (eras/conway/impl/src/Cardano/Ledger/Conway/Governance.hs:301-325): definite when it enacts
      * a ParameterChange or a HardForkInitiation, whose proposals `govState` still holds.
      */
    private[scalus] def of(ratify: RatifyState, govState: GovState): FuturePParams = {
        val changesParams = ratify.enacted.exists { id =>
            govState
                .proposal(id)
                .exists(_.procedure.govAction match
                    case _: GovAction.ParameterChange | _: GovAction.HardForkInitiation => true
                    case _                                                              => false)
        }
        if changesParams then DefinitePParamsUpdate(ratify.enactState.params) else NoPParamsUpdate
    }
}
