package scalus.cardano.ledger

/** Rewards calculation for an epoch
  *
  * Implements rewards computation according to the Cardano ledger specification. Based on the
  * POOLREAP and REWARD rules.
  */
object RewardsCalculation:

    /** Configuration parameters for rewards calculation */
    case class RewardsConfig(
        monetaryExpansionRate: Double, // rho (ρ)
        treasuryExpansionRate: Double, // tau (τ)
        minPoolCost: Coin, // Minimum pool cost
        optimalPoolCount: Int, // n_opt
        poolPledgeInfluence: Double // a0
    )

    /** Per-pool rewards breakdown */
    case class PoolRewardSummary(
        totalRewards: Coin, // Total rewards for this pool
        leaderReward: Coin, // Pool operator/leader reward
        memberRewards: Map[Credential, Coin] // Individual delegator rewards
    )

    /** Complete rewards summary for an epoch */
    case class EpochRewardsSummary(
        epoch: Long,
        totalRewards: Coin,
        treasuryTax: Coin,
        availableRewards: Coin,
        poolRewards: Map[PoolKeyHash, PoolRewardSummary],
        accountRewards: Map[Credential, Coin]
    )

    /** Compute total rewards for an epoch
      *
      * @param reserves
      *   Current reserve pot
      * @param fees
      *   Transaction fees collected
      * @param config
      *   Rewards configuration parameters
      * @return
      *   (total rewards, treasury portion)
      */
    def computeTotalRewards(
        reserves: Coin,
        fees: Coin,
        config: RewardsConfig
    ): (Coin, Coin) =
        // Calculate monetary expansion: reserves * rho
        val monetaryExpansion = multiplyByDouble(reserves, config.monetaryExpansionRate)

        // Total rewards = fees + monetary expansion
        val totalRewards = fees + monetaryExpansion

        // Treasury tax = total rewards * tau
        val treasuryTax = multiplyByDouble(totalRewards, config.treasuryExpansionRate)

        (totalRewards, treasuryTax)

    /** Compute rewards distribution among pools
      *
      * Simplified rewards calculation that distributes available rewards proportionally to pool
      * stakes.
      *
      * @param availableRewards
      *   Rewards available after treasury tax
      * @param poolStakes
      *   Stake delegated to each pool
      * @param poolParams
      *   Pool parameters (cost, margin, pledge, etc.)
      * @param totalActiveStake
      *   Total active stake in the system
      * @param config
      *   Rewards configuration
      * @return
      *   Rewards for each pool
      */
    def distributePoolRewards(
        availableRewards: Coin,
        poolStakes: Map[PoolKeyHash, Coin],
        poolParams: Map[PoolKeyHash, PoolParameters],
        totalActiveStake: Coin,
        config: RewardsConfig
    ): Map[PoolKeyHash, Coin] =
        if totalActiveStake.value == 0 then Map.empty
        else
            poolStakes.map { case (poolId, stake) =>
                // Proportional rewards based on stake
                val proportionalReward = divideProportionally(
                  availableRewards,
                  stake,
                  totalActiveStake
                )

                poolId -> proportionalReward
            }

    /** Split pool rewards between leader and members
      *
      * @param poolReward
      *   Total reward for the pool
      * @param poolParams
      *   Pool parameters (margin, cost)
      * @param poolStake
      *   Total stake delegated to pool
      * @param memberStakes
      *   Individual delegator stakes
      * @return
      *   Breakdown of rewards
      */
    def splitPoolRewards(
        poolReward: Coin,
        poolParams: PoolParameters,
        poolStake: Coin,
        memberStakes: Map[Credential, Coin]
    ): PoolRewardSummary =
        // Leader takes: cost + margin * (rewards - cost)
        val cost = poolParams.cost
        val remainingAfterCost = if poolReward > cost then poolReward - cost else Coin.zero

        val marginAmount = multiplyByDouble(
          remainingAfterCost,
          poolParams.marginNumerator.toDouble / poolParams.marginDenominator.toDouble
        )
        val leaderReward = cost + marginAmount

        // Remaining rewards distributed to members proportionally
        val memberRewardsPool =
            if poolReward > leaderReward then poolReward - leaderReward else Coin.zero

        val memberRewards =
            if poolStake.value > 0 then
                memberStakes.map { case (credential, stake) =>
                    val reward = divideProportionally(memberRewardsPool, stake, poolStake)
                    credential -> reward
                }
            else Map.empty[Credential, Coin]

        PoolRewardSummary(
          totalRewards = poolReward,
          leaderReward = leaderReward,
          memberRewards = memberRewards
        )

    /** Aggregate all member rewards into account rewards */
    def aggregateAccountRewards(
        poolRewards: Map[PoolKeyHash, PoolRewardSummary]
    ): Map[Credential, Coin] =
        poolRewards.values
            .flatMap(_.memberRewards)
            .groupMapReduce(_._1)(_._2)(_ + _)

    /** Helper: Multiply coin by a double (for percentages) */
    private def multiplyByDouble(coin: Coin, factor: Double): Coin =
        Coin((coin.value.toDouble * factor).toLong)

    /** Helper: Divide coin proportionally */
    private def divideProportionally(total: Coin, part: Coin, whole: Coin): Coin =
        if whole.value == 0 then Coin.zero
        else
            // Use BigInt to avoid overflow when multiplying large values
            val result = (BigInt(total.value) * BigInt(part.value)) / BigInt(whole.value)
            Coin(result.toLong)

    /** Pool parameters needed for rewards calculation */
    case class PoolParameters(
        cost: Coin,
        marginNumerator: Long,
        marginDenominator: Long,
        pledge: Coin
    )
