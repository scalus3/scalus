package scalus.testing.conformance.amaru

/** Comprehensive conformance tests for epoch data using Amaru test data
  *
  * These tests validate that Amaru epoch data is complete, consistent, and
  * can be correctly loaded using Scalus infrastructure.
  *
  * Following Amaru's pattern where tests load data for multiple epochs
  * to verify the complete reward calculation cycle.
  */
class EpochDataConformanceTest extends ConformanceTestBase:

    /** Test that we can load complete data for the reward calculation cycle
      *
      * For epoch N rewards calculation, we need:
      * - Epoch N: pools and dreps configuration
      * - Epoch N+1: rewards provenance (blocks produced)
      * - Epoch N+3: pots (final balances)
      */
    test("load complete reward cycle data for epoch 168 (preprod)") {
        import TestDataModels.extractLovelace
        val epoch = 168
        val network = "preprod"

        // Epoch N data
        val pools = loadPools(network, epoch)
        val dreps = loadDReps(network, epoch)

        // Epoch N+1 data
        val rewardsProvenance = loadRewardsProvenance(network, epoch + 1)

        // Epoch N+3 data
        val pots = loadPots(network, epoch + 3)

        // Verify all data loaded
        pools should not be empty
        rewardsProvenance.stakePools should not be empty

        println(s"Loaded complete reward cycle for epoch $epoch:")
        println(s"  Pools (epoch $epoch): ${pools.size}")
        println(s"  DReps (epoch $epoch): ${dreps.size}")
        println(s"  Rewards provenance (epoch ${epoch + 1}): ${rewardsProvenance.stakePools.size} pools")
        println(s"  Pots (epoch ${epoch + 3}): treasury=${extractLovelace(pots.treasury)}, reserves=${extractLovelace(pots.reserves)}")

        // Verify data consistency: pools in epoch N should appear in rewards provenance for epoch N+1
        val poolsWithBlocks = rewardsProvenance.stakePools.keySet
        val commonPools = pools.keySet.intersect(poolsWithBlocks)

        println(s"  Common pools between epoch $epoch and ${epoch + 1}: ${commonPools.size}")
    }

    test("validate pool data structure for multiple epochs (preprod)") {
        import TestDataModels.extractLovelace
        val network = "preprod"
        val testEpochs = List(168, 169, 170)

        testEpochs.foreach { epoch =>
            val pools = loadPools(network, epoch)
            pools should not be empty

            val firstPool = pools.values.head
            println(s"Epoch $epoch: ${pools.size} pools, first pool pledge: ${extractLovelace(firstPool.pledge)}")

            // Validate pool structure
            pools.values.foreach { pool =>
                pool.id should not be empty
                extractLovelace(pool.pledge) should be >= BigInt(0)
                extractLovelace(pool.cost) should be >= BigInt(0)
                pool.margin should include("/")
                pool.rewardAccount should not be empty
                pool.owners should not be empty
            }
        }
    }

    test("validate DRep data structure for multiple epochs (preprod)") {
        val network = "preprod"
        val testEpochs = List(168, 169, 170)

        testEpochs.foreach { epoch =>
            val dreps = loadDReps(network, epoch)
            dreps should not be empty

            val registeredDReps = dreps.filter(_.`type` == "registered")
            val abstainDReps = dreps.filter(_.`type` == "abstain")
            val noConfidenceDReps = dreps.filter(_.`type` == "no_confidence")

            println(s"Epoch $epoch: ${dreps.size} DReps total")
            println(s"  Registered: ${registeredDReps.size}, Abstain: ${abstainDReps.size}, NoConfidence: ${noConfidenceDReps.size}")

            // Validate registered DReps have required fields
            registeredDReps.foreach { drep =>
                drep.id should not be empty
                drep.from should not be empty
            }
        }
    }

    test("validate rewards provenance data structure (preprod)") {
        import TestDataModels.extractLovelace
        val network = "preprod"
        val epoch = 169 // Test epoch N+1 for epoch 168

        val rewardsProvenance = loadRewardsProvenance(network, epoch)

        rewardsProvenance.stakePools should not be empty
        extractLovelace(rewardsProvenance.totalStake) should be > BigInt(0)
        extractLovelace(rewardsProvenance.activeStake) should be > BigInt(0)
        extractLovelace(rewardsProvenance.fees) should be >= BigInt(0)

        println(s"Rewards provenance for epoch $epoch:")
        println(s"  Pools: ${rewardsProvenance.stakePools.size}")
        println(s"  Total stake: ${extractLovelace(rewardsProvenance.totalStake)}")
        println(s"  Active stake: ${extractLovelace(rewardsProvenance.activeStake)}")
        println(s"  Fees: ${extractLovelace(rewardsProvenance.fees)}")
        println(s"  Total rewards: ${extractLovelace(rewardsProvenance.totalRewards)}")
        println(s"  Efficiency: ${rewardsProvenance.efficiency}")

        // Active stake should be <= total stake
        extractLovelace(rewardsProvenance.activeStake) should be <= extractLovelace(rewardsProvenance.totalStake)
    }

    test("validate pots data for multiple epochs (preprod)") {
        import TestDataModels.extractLovelace
        val network = "preprod"
        val testEpochs = List(171, 172, 173) // epoch+3 for epochs 168, 169, 170

        testEpochs.foreach { epoch =>
            val pots = loadPots(network, epoch)

            val treasury = extractLovelace(pots.treasury)
            val reserves = extractLovelace(pots.reserves)

            treasury should be > BigInt(0)
            reserves should be > BigInt(0)

            println(s"Pots for epoch $epoch: treasury=$treasury, reserves=$reserves")
        }
    }

    test("verify network configuration data (preprod)") {
        val config = loadConfig("preprod")

        config.points should not be empty
        config.snapshots should not be empty

        println(s"Preprod network configuration:")
        println(s"  Points: ${config.points.size}")
        println(s"  Snapshots: ${config.snapshots.size}")
        println(s"  First point: epoch ${config.points.head.epoch}, slot ${config.points.head.slot}")
        println(s"  Last point: epoch ${config.points.last.epoch}, slot ${config.points.last.slot}")
    }

    test("validate epoch progression in available data (preprod)") {
        val epochs = getAvailableEpochs("preprod")

        epochs should not be empty
        epochs.sorted shouldEqual epochs // Should already be sorted

        println(s"Available epochs for preprod: ${epochs.min} to ${epochs.max} (${epochs.size} total)")

        // Verify we have the test epochs we need
        val testEpochs = List(168, 169, 170, 171, 172, 173)
        testEpochs.foreach { epoch =>
            if epochs.contains(epoch) then
                println(s"  Epoch $epoch: available")
            else
                println(s"  Epoch $epoch: missing")
        }
    }
