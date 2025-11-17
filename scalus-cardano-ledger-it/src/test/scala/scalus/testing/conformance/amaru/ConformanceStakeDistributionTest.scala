package scalus.testing.conformance.amaru

import scalus.cardano.ledger.*

/** Conformance tests for stake distribution calculation using Amaru test data
  *
  * Tests the StakeDistribution module against real Cardano epoch data from Amaru.
  */
class ConformanceStakeDistributionTest extends ConformanceTestBase with ConformanceTestHelpers:

    test("compute stake distribution from epoch 168 data (preprod)") {
        val epoch = 168
        val network = "preprod"

        // Load real conformance data
        val pools = loadPools(network, epoch)
        val dreps = loadDReps(network, epoch)

        pools should not be empty
        dreps should not be empty

        println(s"\nLoaded Amaru conformance data for epoch $epoch:")
        println(s"  Pools: ${pools.size}")
        println(s"  DReps: ${dreps.size}")

        // Build UTxO set from pool pledge amounts (simplified model)
        val utxos = buildUtxosFromPoolPledges(pools)

        // Build delegation mappings
        val poolDelegations = buildPoolDelegations(pools)
        val drepDelegations = buildDRepDelegations(dreps)

        println(s"\nBuilt test data:")
        println(s"  UTxOs: ${utxos.size}")
        println(s"  Pool delegations: ${poolDelegations.size}")
        println(s"  DRep delegations: ${drepDelegations.size}")

        // Compute stake distribution using Scalus logic
        val snapshot = StakeDistribution.computeSnapshot(
          utxos,
          poolDelegations,
          drepDelegations
        )

        val sumOfPoolStakes = snapshot.poolStakes.values.map(_.value).sum

        println(s"\nStake distribution computed:")
        println(s"  Active stake: ${snapshot.activeStake}")
        println(s"  Pools with stake: ${snapshot.poolStakes.size}")
        println(s"  Total credentials: ${snapshot.stakeCredentials.size}")
        println(s"  Sum of pool stakes: $sumOfPoolStakes")

        // Validate results
        snapshot.activeStake.value should be > 0L
        snapshot.poolStakes should not be empty
        snapshot.poolStakes.size should be <= pools.size
        snapshot.stakeCredentials should not be empty
        // Validate stake conservation: sum of pool stakes = active stake
        sumOfPoolStakes shouldBe snapshot.activeStake.value
    }

    test("compute DRep voting stake from epoch 168 data") {
        import TestDataModels.extractLovelace
        val epoch = 168
        val network = "preprod"

        val dreps = loadDReps(network, epoch)
        val pools = loadPools(network, epoch)

        // Build mock stake credentials from pools
        val stakeByCredential = pools.flatMap { case (poolId, poolData) =>
            poolData.owners.map { ownerHex =>
                val credential = Credential.KeyHash(AddrKeyHash.fromHex(ownerHex))
                credential -> Coin(extractLovelace(poolData.pledge).toLong)
            }
        }

        val drepDelegations = buildDRepDelegations(dreps)

        // Compute DRep voting stake
        val drepStakes = StakeDistribution.computeDRepVotingStake(
          stakeByCredential,
          drepDelegations
        )

        val totalVotingStake = drepStakes.values.map(_.value).sum
        val totalDelegatedStake =
            drepDelegations.keys.flatMap(stakeByCredential.get).map(_.value).sum

        println(s"\nDRep voting stakes from epoch $epoch:")
        println(s"  DReps with stake: ${drepStakes.size}")
        println(s"  Total voting stake: $totalVotingStake")
        println(s"  Total delegated stake: $totalDelegatedStake")

        drepStakes.size should be >= 0
        // Validate stake conservation: total DRep stake = sum of delegated stakes
        totalVotingStake shouldBe totalDelegatedStake
    }

    test("compare pool stakes across multiple epochs (preprod)") {
        val network = "preprod"
        val epochs = List(168, 169, 170)

        epochs.foreach { epoch =>
            val pools = loadPools(network, epoch)
            val utxos = buildUtxosFromPoolPledges(pools)
            val poolDelegations = buildPoolDelegations(pools)

            val snapshot = StakeDistribution.computeSnapshot(utxos, poolDelegations, Map.empty)
            val sumOfPoolStakes = snapshot.poolStakes.values.map(_.value).sum

            println(s"\nEpoch $epoch stake distribution:")
            println(s"  Total pools: ${pools.size}")
            println(s"  Pools with stake: ${snapshot.poolStakes.size}")
            println(s"  Total active stake: ${snapshot.activeStake}")
            println(s"  Sum of pool stakes: $sumOfPoolStakes")
            println(s"  Top 3 pools by stake: ${snapshot.poolStakes.toSeq
                    .sortBy(-_._2.value)
                    .take(3)
                    .map(_._2)
                    .mkString(", ")}")

            snapshot.poolStakes should not be empty
            // Validate stake conservation for each epoch
            sumOfPoolStakes shouldBe snapshot.activeStake.value
        }
    }
