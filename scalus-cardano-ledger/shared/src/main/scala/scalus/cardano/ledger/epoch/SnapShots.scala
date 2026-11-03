package scalus.cardano.ledger.epoch

import scalus.cardano.ledger.{AddrKeyHash, Coin, Credential, PoolKeyHash, UnitInterval, VrfKeyHash}

/** The three stake snapshots and the fee snapshot of an epoch boundary, the ledger's `SnapShots`
  * (cardano-ledger-core State/SnapShots.hs), ledger-rules spec [DM-9a].
  *
  * SNAP makes a new `mark` at every boundary and moves the others down one: `go := set`,
  * `set := mark`, spec [EB-6]. Rewards read `go`, the stake of two boundaries ago.
  *
  * @param mark
  *   the stake at the last boundary, `ssStakeMark`
  * @param set
  *   the stake at the boundary before, `ssStakeSet`
  * @param go
  *   the stake at the boundary before that, `ssStakeGo`
  * @param fee
  *   the fees of the ledger at the last boundary, `ssFee`
  */
case class SnapShots(mark: StakeSnapshot, set: StakeSnapshot, go: StakeSnapshot, fee: Coin)

object SnapShots {

    /** Three empty snapshots and no fee, `emptySnapShots`. */
    val empty: SnapShots =
        SnapShots(StakeSnapshot.empty, StakeSnapshot.empty, StakeSnapshot.empty, Coin.zero)
}

/** The stake of one boundary, the ledger's `SnapShot`, ledger-rules spec [DM-10a].
  *
  * @param activeStake
  *   the stake of each registered credential that delegates to a pool and has stake above 0: the
  *   coin of its base-address outputs plus its reward balance, spec [SN-2] to [SN-5]
  * @param delegations
  *   the pool each credential of `activeStake` delegates to, with the same keys
  * @param pools
  *   one entry per registered pool at the boundary, also a pool with no stake, spec [SN-7]
  */
case class StakeSnapshot(
    activeStake: Map[Credential, Coin],
    delegations: Map[Credential, PoolKeyHash],
    pools: Map[PoolKeyHash, PoolSnapshot]
) {

    /** The sum of `activeStake`, or 1 lovelace when it is 0, `sumAllActiveStake`
      * (cardano-ledger-core State/Stake.hs:157-160), spec [SN-6], [DM-10b].
      */
    def totalActiveStake: Coin = {
        val total = activeStake.valuesIterator.foldLeft(Coin.zero)(_ + _)
        if total == Coin.zero then Coin(1) else total
    }
}

object StakeSnapshot {

    /** No stake and no pools, `emptySnapShot`. */
    val empty: StakeSnapshot = StakeSnapshot(Map.empty, Map.empty, Map.empty)
}

/** One pool in a [[StakeSnapshot]], the ledger's `StakePoolSnapShot` without the derived
  * `spssStakeRatio` (cardano-ledger-core State/SnapShots.hs:160-185), ledger-rules spec [DM-10b],
  * [SN-8].
  *
  * @param stake
  *   the sum of the active stake of the credentials that delegate to the pool
  * @param selfDelegatedOwners
  *   the owners whose key credential is a registered account that delegates to the pool
  * @param selfDelegatedOwnersStake
  *   the sum of the active stake of `selfDelegatedOwners`
  * @param vrfKeyHash
  *   the VRF key of the pool params
  * @param pledge
  *   the pledge of the pool params
  * @param cost
  *   the fixed cost of the pool params
  * @param margin
  *   the margin of the pool params
  * @param numDelegators
  *   the number of registered accounts that delegate to the pool, whatever their stake
  * @param rewardAccount
  *   the credential of the reward account of the pool params
  */
case class PoolSnapshot(
    stake: Coin,
    selfDelegatedOwners: Set[AddrKeyHash],
    selfDelegatedOwnersStake: Coin,
    vrfKeyHash: VrfKeyHash,
    pledge: Coin,
    cost: Coin,
    margin: UnitInterval,
    numDelegators: Int,
    rewardAccount: Credential
)
