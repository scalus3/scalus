package scalus.cardano.ledger

import scalus.cardano.address.*

/** Stake distribution computation for an epoch
  *
  * Implements the SNAP rule from the Cardano ledger specification. Computes stake distribution from
  * the current ledger state.
  */
object StakeDistribution:

    /** Result of stake distribution computation */
    case class StakeDistributionSnapshot(
        activeStake: Coin, // Total active stake
        poolStakes: Map[PoolKeyHash, Coin], // Stake delegated to each pool
        stakeCredentials: Map[Credential, Coin] // Stake per credential
    )

    /** Compute stake distribution from UTxO and delegation state
      *
      * @param utxos
      *   Current UTxO set
      * @param delegations
      *   Stake pool delegations (credential -> pool)
      * @param drepDelegations
      *   DRep delegations (credential -> drep)
      * @return
      *   Snapshot of stake distribution
      */
    def computeSnapshot(
        utxos: Utxos,
        delegations: Map[Credential, PoolKeyHash],
        drepDelegations: Map[Credential, DRep]
    ): StakeDistributionSnapshot =
        // Aggregate stake by credential from UTxOs
        val stakeByCredential = aggregateUtxoStake(utxos)

        // Compute stake delegated to each pool
        val poolStakes = computePoolStakes(stakeByCredential, delegations)

        // Total active stake (sum of all pool stakes)
        val totalActiveStake = poolStakes.values.foldLeft(Coin.zero)(_ + _)

        StakeDistributionSnapshot(
          activeStake = totalActiveStake,
          poolStakes = poolStakes,
          stakeCredentials = stakeByCredential
        )

    /** Aggregate stake from UTxOs by credential
      *
      * Extracts stake credentials from UTxO addresses and sums up the coins.
      */
    private def aggregateUtxoStake(utxos: Utxos): Map[Credential, Coin] =
        utxos.values
            .flatMap { output =>
                extractStakeCredential(output.address).map(cred => cred -> output.value.coin)
            }
            .groupMapReduce(_._1)(_._2)(_ + _)

    /** Extract stake credential from an address
      *
      * Returns None for Enterprise addresses (no delegation)
      */
    private def extractStakeCredential(address: Address): Option[Credential] =
        address match
            case addr: ShelleyAddress =>
                addr.delegation match
                    case ShelleyDelegationPart.Key(hash) =>
                        Some(Credential.KeyHash(AddrKeyHash(hash)))
                    case ShelleyDelegationPart.Script(hash) =>
                        Some(Credential.ScriptHash(hash))
                    case ShelleyDelegationPart.Pointer(_) | ShelleyDelegationPart.Null =>
                        None
            case _: StakeAddress =>
                None // Stake addresses themselves don't hold value
            case _ =>
                None // Byron addresses

    /** Compute stake delegated to each pool */
    private def computePoolStakes(
        stakeByCredential: Map[Credential, Coin],
        delegations: Map[Credential, PoolKeyHash]
    ): Map[PoolKeyHash, Coin] =
        delegations
            .flatMap { case (credential, poolId) =>
                stakeByCredential.get(credential).map(stake => poolId -> stake)
            }
            .groupMapReduce(_._1)(_._2)(_ + _)

    /** Compute voting stake for DReps */
    def computeDRepVotingStake(
        stakeByCredential: Map[Credential, Coin],
        drepDelegations: Map[Credential, DRep]
    ): Map[DRep, Coin] =
        drepDelegations
            .flatMap { case (credential, drep) =>
                stakeByCredential.get(credential).map(stake => drep -> stake)
            }
            .groupMapReduce(_._1)(_._2)(_ + _)

    /** Build complete stake distribution from cert state and UTxO
      *
      * @param utxos
      *   Current UTxO set
      * @param certState
      *   Certificate state with delegations
      * @return
      *   Complete snapshot result
      */
    def fromCertState(utxos: Utxos, certState: CertState): StakeDistributionSnapshot =
        computeSnapshot(
          utxos = utxos,
          delegations = certState.dstate.stakePools,
          drepDelegations = certState.dstate.dreps
        )
