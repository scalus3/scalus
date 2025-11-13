package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.ledger.*

class RewardsCalculationTest extends AnyFunSuite with Matchers:

    test("compute total rewards with simple mock data") {
        val config = RewardsCalculation.RewardsConfig(
          monetaryExpansionRate = 0.003, // 0.3% rho
          treasuryExpansionRate = 0.2, // 20% tau
          minPoolCost = Coin(340000000),
          optimalPoolCount = 500,
          poolPledgeInfluence = 0.3
        )

        val reserves = Coin(14000000000000000L) // 14M ADA
        val fees = Coin(1000000000L) // 1K ADA

        val (totalRewards, treasuryTax) = RewardsCalculation.computeTotalRewards(
          reserves,
          fees,
          config
        )

        val availableRewards = Coin(totalRewards.value - treasuryTax.value)

        println(s"Total rewards computation:")
        println(s"  Reserves: $reserves")
        println(s"  Fees: $fees")
        println(s"  Total rewards: $totalRewards")
        println(s"  Treasury tax (20%): $treasuryTax")
        println(s"  Available for pools: $availableRewards")

        totalRewards.value should be > fees.value
        treasuryTax.value should be > 0L
        availableRewards.value should be > 0L
    }

    test("distribute pool rewards proportionally with simple mock data") {
        val pool1 = PoolKeyHash.fromHex("1" * 56)
        val pool2 = PoolKeyHash.fromHex("2" * 56)

        val poolStakes = Map(
          pool1 -> Coin(6000000000L), // 60% of total
          pool2 -> Coin(4000000000L) // 40% of total
        )

        val totalActiveStake = Coin(10000000000L)
        val availableRewards = Coin(1000000000L) // 1K ADA to distribute

        val config = RewardsCalculation.RewardsConfig(
          monetaryExpansionRate = 0.003,
          treasuryExpansionRate = 0.2,
          minPoolCost = Coin(340000000),
          optimalPoolCount = 500,
          poolPledgeInfluence = 0.3
        )

        val poolRewards = RewardsCalculation.distributePoolRewards(
          availableRewards,
          poolStakes,
          Map.empty,
          totalActiveStake,
          config
        )

        val totalDistributed = poolRewards.values.map(_.value).sum

        println(s"\nSimple pool rewards distribution:")
        println(s"  Pool 1 (60% stake): ${poolRewards.get(pool1)}")
        println(s"  Pool 2 (40% stake): ${poolRewards.get(pool2)}")
        println(s"  Total distributed: $totalDistributed")
        println(s"  Available rewards: ${availableRewards.value}")

        poolRewards should have size 2
        // Pool 1 should get ~60% of rewards
        poolRewards(pool1).value should be > 500000000L
        // Pool 2 should get ~40% of rewards
        poolRewards(pool2).value should be > 300000000L
        // Validate rewards conservation
        totalDistributed shouldBe availableRewards.value
    }

    test("split pool rewards between leader and members") {
        val poolReward = Coin(1000000000) // 1000 ADA total reward

        val poolParams = RewardsCalculation.PoolParameters(
          cost = Coin(340000000), // 340 ADA fixed cost
          marginNumerator = 3, // 3% margin
          marginDenominator = 100,
          pledge = Coin(500000000) // 500 ADA pledge
        )

        val poolStake = Coin(10000000000L) // 10K ADA total stake

        val member1 = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
        val member2 = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))

        val memberStakes = Map(
          member1 -> Coin(6000000000L), // 6K ADA (60%)
          member2 -> Coin(4000000000L) // 4K ADA (40%)
        )

        val summary = RewardsCalculation.splitPoolRewards(
          poolReward,
          poolParams,
          poolStake,
          memberStakes
        )

        val totalMemberRewards = summary.memberRewards.values.map(_.value).sum

        println(s"Pool reward split:")
        println(s"  Total: ${summary.totalRewards}")
        println(s"  Leader: ${summary.leaderReward} (cost + margin)")
        println(s"  Member 1 (60%): ${summary.memberRewards.get(member1)}")
        println(s"  Member 2 (40%): ${summary.memberRewards.get(member2)}")
        println(s"  Total member rewards: $totalMemberRewards")

        summary.totalRewards shouldBe poolReward
        // Leader should get at least the fixed cost
        summary.leaderReward.value should be >= poolParams.cost.value
        // Members should get proportional rewards
        summary.memberRewards should have size 2
        // Validate reward conservation: leader + members = total
        (summary.leaderReward.value + totalMemberRewards) shouldBe poolReward.value
    }

    test("aggregate account rewards from multiple pools") {
        val member1 = Credential.KeyHash(AddrKeyHash.fromHex("a" * 56))
        val member2 = Credential.KeyHash(AddrKeyHash.fromHex("b" * 56))

        val pool1Summary = RewardsCalculation.PoolRewardSummary(
          totalRewards = Coin(1000000000),
          leaderReward = Coin(400000000),
          memberRewards = Map(
            member1 -> Coin(300000000), // 300 ADA from pool 1
            member2 -> Coin(300000000) // 300 ADA from pool 1
          )
        )

        val pool2Summary = RewardsCalculation.PoolRewardSummary(
          totalRewards = Coin(500000000),
          leaderReward = Coin(200000000),
          memberRewards = Map(
            member1 -> Coin(300000000) // 300 ADA from pool 2
          )
        )

        val poolRewards = Map(
          PoolKeyHash.fromHex("1" * 56) -> pool1Summary,
          PoolKeyHash.fromHex("2" * 56) -> pool2Summary
        )

        val accountRewards = RewardsCalculation.aggregateAccountRewards(poolRewards)

        println(s"Account rewards aggregation:")
        println(s"  Member 1 total: ${accountRewards.get(member1)} (from 2 pools)")
        println(s"  Member 2 total: ${accountRewards.get(member2)} (from 1 pool)")

        accountRewards should have size 2
        accountRewards(member1).value shouldBe 600000000L // 300 + 300
        accountRewards(member2).value shouldBe 300000000L // 300
    }

    test("handle zero stake gracefully") {
        val poolStakes = Map.empty[PoolKeyHash, Coin]
        val totalActiveStake = Coin(0)
        val availableRewards = Coin(1000000000L)

        val config = RewardsCalculation.RewardsConfig(
          monetaryExpansionRate = 0.003,
          treasuryExpansionRate = 0.2,
          minPoolCost = Coin(340000000),
          optimalPoolCount = 500,
          poolPledgeInfluence = 0.3
        )

        val poolRewards = RewardsCalculation.distributePoolRewards(
          availableRewards,
          poolStakes,
          Map.empty,
          totalActiveStake,
          config
        )

        // Should return empty map when no stake
        poolRewards shouldBe empty
    }
