package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.testing.kit.TestUtil

/** Usage examples for the UTxO query DSL with Emulator.
  *
  * These tests demonstrate how to use `provider.queryUtxos` with lambda expressions.
  */
class UtxoQueryDslExamplesSpec extends AnyFunSuite {

    // Create test addresses
    private val aliceKeyHash =
        AddrKeyHash.fromHex("c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f")
    private val bobKeyHash =
        AddrKeyHash.fromHex("337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251")

    private val aliceAddress = ShelleyAddress(
      network = Network.Mainnet,
      payment = ShelleyPaymentPart.Key(aliceKeyHash),
      delegation = ShelleyDelegationPart.Null
    )

    private val bobAddress = ShelleyAddress(
      network = Network.Mainnet,
      payment = ShelleyPaymentPart.Key(bobKeyHash),
      delegation = ShelleyDelegationPart.Null
    )

    // Test policy and asset (28 bytes = 56 hex chars for ScriptHash)
    private val testPolicyId: PolicyId =
        ScriptHash.fromHex("abcdef0123456789abcdef0123456789abcdef0123456789abcdef01")
    private val testAssetName = AssetName(ByteString.fromString("TestToken"))

    test("queryUtxos: find UTxOs by address") {
        // Setup: Create emulator with initial UTxOs
        val genesisHash = TestUtil.genesisHash
        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, Value.ada(50)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(bobAddress, Value.ada(200))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find all UTxOs at Alice's address
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress
            }
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(utxos.toOption.get.size == 2)
        assert(utxos.toOption.get.values.forall(_.address == aliceAddress))
    }

    test("queryUtxos: find UTxOs with minimum lovelace") {
        val genesisHash = TestUtil.genesisHash
        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, Value.ada(50)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(aliceAddress, Value.ada(10))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find UTxOs with at least 50 ADA
        val minAmount = Coin.ada(50)
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress && u.output.value.coin >= minAmount
            }
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(utxos.toOption.get.size == 2) // 100 ADA and 50 ADA UTxOs
        assert(utxos.toOption.get.values.forall(_.value.coin >= minAmount))
    }

    test("queryUtxos: find UTxOs containing specific asset") {
        val genesisHash = TestUtil.genesisHash
        val valueWithAsset = Value.ada(10) + Value.asset(testPolicyId, testAssetName, 1000)

        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, valueWithAsset),
          TransactionInput(genesisHash, 2) -> TransactionOutput(bobAddress, Value.ada(200))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find UTxOs containing the test token
        val result = provider
            .queryUtxos { u =>
                u.output.value.hasAsset(testPolicyId, testAssetName)
            }
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(utxos.toOption.get.size == 1)
        assert(utxos.toOption.get.values.head.value.hasAsset(testPolicyId, testAssetName))
    }

    test("queryUtxos: combine address and asset filter") {
        val genesisHash = TestUtil.genesisHash
        val valueWithAsset = Value.ada(10) + Value.asset(testPolicyId, testAssetName, 1000)

        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, valueWithAsset),
          TransactionInput(genesisHash, 2) -> TransactionOutput(bobAddress, valueWithAsset)
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find Alice's UTxOs that contain the test token
        // This lambda translates to:
        //   UtxoQuery.Simple(
        //     source = UtxoSource.FromAddress(aliceAddress),
        //     filter = Some(UtxoFilter.HasAsset(testPolicyId, testAssetName))
        //   )
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress && u.output.value.hasAsset(
                  testPolicyId,
                  testAssetName
                )
            }
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(utxos.toOption.get.size == 1)
        val utxo = utxos.toOption.get.head
        assert(utxo._2.address == aliceAddress)
        assert(utxo._2.value.hasAsset(testPolicyId, testAssetName))
    }

    test("queryUtxos: OR query for multiple addresses") {
        val genesisHash = TestUtil.genesisHash
        val charlieKeyHash =
            AddrKeyHash.fromHex("1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val charlieAddress = ShelleyAddress(
          network = Network.Mainnet,
          payment = ShelleyPaymentPart.Key(charlieKeyHash),
          delegation = ShelleyDelegationPart.Null
        )

        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(100)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(bobAddress, Value.ada(50)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(charlieAddress, Value.ada(200))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find UTxOs at Alice's OR Bob's address
        // This lambda translates to:
        //   UtxoQuery.Or(
        //     UtxoQuery.Simple(source = UtxoSource.FromAddress(aliceAddress)),
        //     UtxoQuery.Simple(source = UtxoSource.FromAddress(bobAddress))
        //   )
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress || u.output.address == bobAddress
            }
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(utxos.toOption.get.size == 2)
    }

    test("queryUtxos: minTotal for coin selection") {
        val genesisHash = TestUtil.genesisHash
        // Create many small UTxOs
        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(10)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, Value.ada(20)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(aliceAddress, Value.ada(30)),
          TransactionInput(genesisHash, 3) -> TransactionOutput(aliceAddress, Value.ada(40)),
          TransactionInput(genesisHash, 4) -> TransactionOutput(aliceAddress, Value.ada(50))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find UTxOs until we have at least 75 ADA
        // This is useful for coin selection - stop fetching once we have enough
        val requiredAmount = Coin.ada(75)
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress
            }
            .minTotal(requiredAmount)
            .execute()

        // Verify: Should have enough UTxOs to cover 75 ADA
        val utxos = result.futureValue
        assert(utxos.isRight)
        val totalLovelace = utxos.toOption.get.values.map(_.value.coin.value).sum
        assert(
          totalLovelace >= requiredAmount.value,
          s"Total $totalLovelace should be >= ${requiredAmount.value}"
        )
        // Early termination means we might not get all 5 UTxOs
        println(
          s"minTotal test: got ${utxos.toOption.get.size} UTxOs with total ${totalLovelace / 1_000_000} ADA"
        )
    }

    test("queryUtxos: minTotal with OR propagates to branches") {
        val genesisHash = TestUtil.genesisHash
        // Alice has small UTxOs, Bob has larger ones
        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(10)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, Value.ada(20)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(bobAddress, Value.ada(100)),
          TransactionInput(genesisHash, 3) -> TransactionOutput(bobAddress, Value.ada(200))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find UTxOs from Alice OR Bob until we have 150 ADA
        val requiredAmount = Coin.ada(150)
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress || u.output.address == bobAddress
            }
            .minTotal(requiredAmount)
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        val totalLovelace = utxos.toOption.get.values.map(_.value.coin.value).sum
        assert(totalLovelace >= requiredAmount.value)
        println(
          s"minTotal OR test: got ${utxos.toOption.get.size} UTxOs with total ${totalLovelace / 1_000_000} ADA"
        )
    }

    test("queryUtxos: limit restricts number of results") {
        val genesisHash = TestUtil.genesisHash
        val initialUtxos: Utxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(aliceAddress, Value.ada(10)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(aliceAddress, Value.ada(20)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(aliceAddress, Value.ada(30)),
          TransactionInput(genesisHash, 3) -> TransactionOutput(aliceAddress, Value.ada(40)),
          TransactionInput(genesisHash, 4) -> TransactionOutput(aliceAddress, Value.ada(50))
        )
        val provider: BlockchainProvider = new Emulator(initialUtxos)

        // Query: Find at most 2 UTxOs
        val result = provider
            .queryUtxos { u =>
                u.output.address == aliceAddress
            }
            .limit(2)
            .execute()

        // Verify
        val utxos = result.futureValue
        assert(utxos.isRight)
        assert(
          utxos.toOption.get.size == 2,
          s"Should have exactly 2 UTxOs, got ${utxos.toOption.get.size}"
        )
    }

    // Helper for async test results
    implicit class FutureOps[T](f: scala.concurrent.Future[T]) {
        def futureValue: T =
            scala.concurrent.Await.result(f, scala.concurrent.duration.Duration(5, "seconds"))
    }
}
