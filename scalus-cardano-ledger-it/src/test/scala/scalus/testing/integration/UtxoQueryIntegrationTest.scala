package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.node.*
import scalus.cardano.txbuilder.*
import scalus.compiler.compile
import scalus.testing.kit.Party
import scalus.testing.yaci.YaciDevKit
import scalus.toUplc
import scalus.utils.await

import scala.concurrent.duration.*

/** Integration tests for UtxoQuery with BlockfrostProvider using Yaci DevKit
  *
  * These tests verify that the UtxoQuery API works correctly with a real
  * Blockfrost-compatible backend (Yaci Store).
  */
class UtxoQueryIntegrationTest extends AnyFunSuite with YaciDevKit {

    private lazy val ctx = createYaciContext()

    // =========================================================================
    // Test: Simple address query
    // =========================================================================

    test("UtxoQuery.Simple with FromAddress returns UTxOs at address") {
        val query = UtxoQuery(UtxoSource.FromAddress(ctx.alice.address))
        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs at address")
                assert(utxos.nonEmpty, "Should have UTxOs from Yaci genesis")
                // Verify all UTxOs are at the expected address
                utxos.foreach { case (_, output) =>
                    assert(output.address == ctx.alice.address, "UTxO should be at queried address")
                }
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: Query with MinLovelace filter
    // =========================================================================

    test("UtxoQuery with MinLovelace filter") {
        val minAmount = Coin.ada(100)
        val query = UtxoQuery(UtxoSource.FromAddress(ctx.alice.address)) &&
            UtxoFilter.MinLovelace(minAmount)

        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs with >= ${minAmount.value} lovelace")
                utxos.foreach { case (_, output) =>
                    assert(
                      output.value.coin >= minAmount,
                      s"UTxO should have >= ${minAmount.value} lovelace"
                    )
                }
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: Query with limit
    // =========================================================================

    test("UtxoQuery with limit") {
        val limit = 2
        val query = UtxoQuery(UtxoSource.FromAddress(ctx.alice.address)).limit(limit)

        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs (limit: $limit)")
                assert(utxos.size <= limit, s"Should return at most $limit UTxOs")
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: Query with minRequiredTotalAmount (early termination)
    // =========================================================================

    test("UtxoQuery with minRequiredTotalAmount") {
        val minTotal = Coin.ada(50)
        val query = UtxoQuery(UtxoSource.FromAddress(ctx.alice.address)).minTotal(minTotal)

        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                val totalLovelace = utxos.values.map(_.value.coin.value).sum
                println(s"Found ${utxos.size} UTxOs with total ${totalLovelace} lovelace")
                assert(
                  totalLovelace >= minTotal.value,
                  s"Total should be >= ${minTotal.value} lovelace"
                )
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: Query by transaction (FromTransaction)
    // =========================================================================

    test("UtxoQuery.FromTransaction returns outputs from a transaction") {
        // First, submit a transaction to get a known txId
        val recipientVkey = Party.Alice.account.changeKeyPair.verificationKey
        val recipientKeyHash: AddrKeyHash =
            Hash(platform.blake2b_224(ByteString.fromArray(recipientVkey.bytes)))
        val recipientAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(recipientKeyHash),
          ShelleyDelegationPart.Null
        )

        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(recipientAddress, Value.ada(10))
            .complete(ctx.provider, ctx.alice.address)
            .await(30.seconds)
            .sign(ctx.alice.signer)
            .transaction

        val txId = tx.id
        println(s"Submitted tx: ${txId.toHex}")

        ctx.provider.submit(tx).await(30.seconds) match {
            case Right(_)    => ctx.waitForBlock()
            case Left(error) => fail(s"Submit failed: $error")
        }

        // Now query by transaction
        val query = UtxoQuery(UtxoSource.FromTransaction(txId))
        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs from tx ${txId.toHex.take(16)}...")
                assert(utxos.nonEmpty, "Should have UTxOs from the transaction")
                // Verify all inputs are from this transaction
                utxos.foreach { case (input, _) =>
                    assert(input.transactionId == txId, "UTxO should be from queried transaction")
                }
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: Combining queries with OR
    // =========================================================================

    test("UtxoQuery.Or combines results from two queries") {
        // Create two addresses
        val addr1 = ctx.alice.address
        val changeVkey = Party.Alice.account.changeKeyPair.verificationKey
        val changeKeyHash: AddrKeyHash =
            Hash(platform.blake2b_224(ByteString.fromArray(changeVkey.bytes)))
        val addr2 = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(changeKeyHash),
          ShelleyDelegationPart.Null
        )

        // First send some ADA to addr2 so it has UTxOs
        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(addr2, Value.ada(5))
            .complete(ctx.provider, ctx.alice.address)
            .await(30.seconds)
            .sign(ctx.alice.signer)
            .transaction

        ctx.provider.submit(tx).await(30.seconds) match {
            case Right(_)    => ctx.waitForBlock()
            case Left(error) => fail(s"Submit failed: $error")
        }

        // Query both addresses
        val query1 = UtxoQuery(UtxoSource.FromAddress(addr1))
        val query2 = UtxoQuery(UtxoSource.FromAddress(addr2))
        val combinedQuery = query1 || query2

        val result = ctx.provider.findUtxos(combinedQuery).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs from combined query")
                // Should have UTxOs from both addresses
                val addresses = utxos.values.map(_.address).toSet
                assert(addresses.contains(addr1), "Should have UTxOs from addr1")
                assert(addresses.contains(addr2), "Should have UTxOs from addr2")
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: HasAsset filter with optimized endpoint
    // =========================================================================

    test("UtxoQuery with HasAsset filter uses optimized endpoint") {
        // First mint some tokens
        val alwaysSucceedsMinting = compile { (_: Data, _: Data) => () }
        val mintingPolicyScript = Script.PlutusV2(
          alwaysSucceedsMinting.toUplc().plutusV2.cborByteString
        )
        val policyId = mintingPolicyScript.scriptHash
        val assetName = AssetName.fromString("QueryTestToken")
        val mintAmount = 100L

        val mintedValue = Value.asset(policyId, assetName, mintAmount, Coin.ada(2))

        val tx = TxBuilder(ctx.cardanoInfo)
            .mint(mintingPolicyScript, Map(assetName -> mintAmount), ())
            .payTo(ctx.alice.address, mintedValue)
            .complete(ctx.provider, ctx.alice.address)
            .await(30.seconds)
            .sign(ctx.alice.signer)
            .transaction

        ctx.provider.submit(tx).await(30.seconds) match {
            case Right(_)    => ctx.waitForBlock()
            case Left(error) => fail(s"Submit failed: $error")
        }

        // Query for UTxOs with this asset
        val query = UtxoQuery(UtxoSource.FromAddress(ctx.alice.address)) &&
            UtxoFilter.HasAsset(policyId, assetName)

        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs with asset ${policyId.toHex.take(8)}...")
                assert(utxos.nonEmpty, "Should find UTxOs with the minted asset")
                // Verify all UTxOs have the asset
                utxos.foreach { case (_, output) =>
                    val hasAsset = output.value.assets.assets
                        .get(policyId)
                        .exists(_.contains(assetName))
                    assert(hasAsset, "UTxO should contain the queried asset")
                }
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }

    // =========================================================================
    // Test: And combinator (FromAddress && FromTransaction)
    // =========================================================================

    test("UtxoQuery with And combinator filters by intersection") {
        // Submit a transaction first
        val tx = TxBuilder(ctx.cardanoInfo)
            .payTo(ctx.alice.address, Value.ada(3))
            .complete(ctx.provider, ctx.alice.address)
            .await(30.seconds)
            .sign(ctx.alice.signer)
            .transaction

        val txId = tx.id

        ctx.provider.submit(tx).await(30.seconds) match {
            case Right(_)    => ctx.waitForBlock()
            case Left(error) => fail(s"Submit failed: $error")
        }

        // Query for UTxOs at address AND from this transaction
        val query = UtxoQuery(
          UtxoSource.FromAddress(ctx.alice.address) && UtxoSource.FromTransaction(txId)
        )

        val result = ctx.provider.findUtxos(query).await(30.seconds)

        result match {
            case Right(utxos) =>
                println(s"Found ${utxos.size} UTxOs at address from tx ${txId.toHex.take(16)}...")
                // Should only have UTxOs that are both at the address AND from the transaction
                utxos.foreach { case (input, output) =>
                    assert(output.address == ctx.alice.address, "UTxO should be at queried address")
                    assert(
                      input.transactionId == txId,
                      "UTxO should be from queried transaction"
                    )
                }
            case Left(error) =>
                fail(s"Query failed: $error")
        }
    }
}
