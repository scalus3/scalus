package scalus.testing.conformance

/** Test to verify conformance test data can be loaded correctly
  *
  * This test validates:
  *   1. Test data symlink is correctly set up
  *   2. JSON parsing works for all data types
  *   3. Snapshot comparison infrastructure works
  */
class ConformanceDataTest extends ConformanceTestBase:

    test("load preprod network configuration") {
        val config = loadConfig("preprod")
        config.points should not be empty
        config.snapshots should not be empty
        println(
          s"Loaded config with ${config.points.size} points and ${config.snapshots.size} snapshot epochs"
        )
    }

    test("load preview network configuration") {
        val config = loadConfig("preview")
        config.points should not be empty
        println(s"Loaded config with ${config.points.size} points")
    }

    test("get available epochs for preprod") {
        val epochs = getAvailableEpochs("preprod")
        epochs should not be empty
        println(s"Available epochs for preprod: ${epochs.take(10).mkString(", ")}...")
    }

    test("load pool data for epoch 168 (preprod)") {
        import TestDataModels.extractLovelace
        val pools = loadPools("preprod", 168)
        pools should not be empty
        val firstPool = pools.values.head
        println(s"Loaded ${pools.size} pools for epoch 168")
        println(
          s"First pool: ${firstPool.id}, pledge: ${extractLovelace(firstPool.pledge)}, cost: ${extractLovelace(firstPool.cost)}"
        )
    }

    test("load rewards provenance for epoch 168 (preprod)") {
        val rewards = loadRewardsProvenance("preprod", 168)
        rewards.stakePools should not be empty
        println(s"Loaded rewards provenance for epoch 168 with ${rewards.stakePools.size} pools")
    }

    test("load DRep data for epoch 168 (preprod)") {
        import TestDataModels.extractLovelace
        val dreps = loadDReps("preprod", 168)
        dreps should not be empty
        println(s"Loaded ${dreps.size} DReps for epoch 168")
        if dreps.nonEmpty then
            val firstDRep = dreps.head
            val depositStr = firstDRep.deposit.map(extractLovelace).map(_.toString).getOrElse("N/A")
            println(
              s"First DRep: type=${firstDRep.`type`}, id=${firstDRep.id.getOrElse("N/A")}, deposit=$depositStr"
            )
    }

    test("load pots data for epoch 171 (preprod, epoch+3)") {
        import TestDataModels.extractLovelace
        val pots = loadPots("preprod", 171)
        extractLovelace(pots.treasury) should be > BigInt(0)
        extractLovelace(pots.reserves) should be > BigInt(0)
        println(
          s"Pots for epoch 171: treasury=${extractLovelace(pots.treasury)}, reserves=${extractLovelace(pots.reserves)}"
        )
    }

    test("create and compare snapshot") {
        import com.github.plokhotnyuk.jsoniter_scala.core.*
        import com.github.plokhotnyuk.jsoniter_scala.macros.*

        case class TestData(epoch: Int, test: String, value: Int)
        given JsonValueCodec[TestData] = JsonCodecMaker.make

        val testData = TestData(168, "snapshot test", 12345)
        val snapshot = writeToString(testData, WriterConfig.withIndentionStep(2))

        // This will create a snapshot file if it doesn't exist
        // or compare with existing snapshot
        compareWithSnapshot("test_snapshot_168", snapshot, "preprod")
    }

    test("validate data relationship for epoch 168") {
        import TestDataModels.extractLovelace
        // For epoch n, we need data from epochs n, n+1, and n+3
        val epoch = 168

        // Epoch n data
        val pools = loadPools("preprod", epoch)
        val dreps = loadDReps("preprod", epoch)

        // Epoch n+1 data
        val rewardsProvenance = loadRewardsProvenance("preprod", epoch + 1)

        // Epoch n+3 data
        val pots = loadPots("preprod", epoch + 3)

        pools should not be empty
        extractLovelace(pots.treasury) should be > BigInt(0)

        println(s"Successfully loaded related data for epoch $epoch:")
        println(s"  Pools (epoch $epoch): ${pools.size}")
        println(s"  DReps (epoch $epoch): ${dreps.size}")
        println(s"  Rewards (epoch ${epoch + 1}): ${rewardsProvenance.stakePools.size} pools")
        println(s"  Pots (epoch ${epoch + 3}): treasury=${extractLovelace(pots.treasury)}")
    }
