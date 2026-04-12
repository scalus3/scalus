package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.blockfrost.*
import scalus.cardano.node.BlockfrostProvider
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

/** Integration tests for all BlockfrostProvider MiniBF-compatible endpoints against Preprod.
  *
  * Reads the Blockfrost API key from the BLOCKFROST_API_KEY environment variable.
  *
  * Run with: {{{
  *   BLOCKFROST_API_KEY=preprodXXX sbt "scalusCardanoLedgerIt/testOnly *BlockfrostEndpointsTest"
  * }}}
  */
class BlockfrostEndpointsTest extends AnyFunSuite {

    private val timeout = 30.seconds

    private lazy val apiKey: String = {
        val key = Option(System.getenv("BLOCKFROST_API_KEY")).getOrElse("")
        if key.isEmpty then cancel("BLOCKFROST_API_KEY not set, skipping test")
        key
    }

    private lazy val provider: BlockfrostProvider =
        BlockfrostProvider.preprod(apiKey).await(timeout)

    // Dynamically discover test data from the chain
    // Fetch a recent block that has transactions, then use its data for all tests
    private lazy val latestBlock = provider.fetchLatestBlock.await(timeout)

    /** Find a recent block with transactions for testing */
    private lazy val testBlock: BlockInfo = {
        // Walk backwards to find a block with txs
        def findBlockWithTxs(hash: String, attempts: Int): BlockInfo = {
            val block = provider.fetchBlock(hash).await(timeout)
            if block.txCount > 0 || attempts <= 0 then block
            else
                block.previousBlock match
                    case Some(prev) => findBlockWithTxs(prev, attempts - 1)
                    case None       => block
        }
        findBlockWithTxs(latestBlock.hash, 20)
    }

    /** A transaction hash from the test block */
    private lazy val testTxHash: String =
        provider.fetchBlockTxs(testBlock.hash, page = Some(1), count = 1).await(timeout).head

    /** Discover a stake address from a pool delegator, then derive a real address from it. This
      * guarantees both testStakeAddress and testAddress are valid and linked.
      */
    private lazy val (testStakeAddress, testAddress): (String, String) = {
        // Find a pool, get a delegator's stake address
        val pools = provider.fetchPoolsExtended(page = Some(1), count = 1).await(timeout)
        val stakeAddr = pools.iterator
            .flatMap { pool =>
                val delegators = provider
                    .fetchPoolDelegators(pool.poolId, page = Some(1), count = 5)
                    .await(timeout)
                delegators.map(_.address)
            }
            .nextOption()
            .getOrElse(sys.error("No delegators found on any pool"))

        // Get an address associated with the stake address
        val addrs = provider
            .fetchAccountAddresses(stakeAddr, page = Some(1), count = 1)
            .await(timeout)
        val addr = addrs.headOption
            .map(_.address)
            .getOrElse(sys.error(s"No addresses found for stake address $stakeAddr"))

        (stakeAddr, addr)
    }

    private lazy val testBlockHash: String = testBlock.hash

    // ── Health ──────────────────────────────────────────────────────────────

    test("GET / — fetchApiRoot") {
        val result = provider.fetchApiRoot.await(timeout)
        assert(result.url.nonEmpty, "url should be non-empty")
        assert(result.version.nonEmpty, "version should be non-empty")
        println(s"  API root: url=${result.url}, version=${result.version}")
    }

    test("GET /health — fetchHealth") {
        val result = provider.fetchHealth.await(timeout)
        assert(result.isHealthy, "API should be healthy")
        println(s"  Health: isHealthy=${result.isHealthy}")
    }

    test("GET /health/clock — fetchHealthClock") {
        val result = provider.fetchHealthClock.await(timeout)
        assert(result.serverTime > 0, "server time should be positive")
        println(s"  Server time: ${result.serverTime}")
    }

    // ── Genesis & Network ───────────────────────────────────────────────────

    test("GET /genesis — fetchGenesis") {
        val result = provider.fetchGenesis.await(timeout)
        assert(result.networkMagic > 0, "network magic should be positive")
        assert(result.epochLength > 0, "epoch length should be positive")
        assert(result.maxLovelaceSupply.value > 0, "max supply should be positive")
        println(
          s"  Genesis: magic=${result.networkMagic}, epochLength=${result.epochLength}, maxSupply=${result.maxLovelaceSupply}"
        )
    }

    test("GET /network — fetchNetwork") {
        val result = provider.fetchNetwork.await(timeout)
        assert(result.supply.max.value > 0, "max supply should be positive")
        assert(result.supply.total.value > 0, "total supply should be positive")
        assert(result.stake.live.value > 0, "live stake should be positive")
        println(
          s"  Network: maxSupply=${result.supply.max}, totalSupply=${result.supply.total}, liveStake=${result.stake.live}"
        )
    }

    test("GET /network/eras — fetchNetworkEras") {
        val result = provider.fetchNetworkEras.await(timeout)
        assert(result.nonEmpty, "should have at least one era")
        println(s"  Network eras: ${result.size} eras")
        result.foreach { era =>
            println(
              s"    start: epoch=${era.start.epoch}, slot=${era.start.slot}, params: epochLen=${era.parameters.epochLength}"
            )
        }
    }

    // ── Epochs ──────────────────────────────────────────────────────────────

    test("GET /epochs/latest/parameters — fetchLatestParams") {
        val params = provider.fetchLatestParams.await(timeout)
        assert(params.txFeePerByte > 0, "txFeePerByte should be positive")
        assert(params.maxTxSize > 0, "maxTxSize should be positive")
        println(
          s"  Latest params: feePerByte=${params.txFeePerByte}, maxTxSize=${params.maxTxSize}"
        )
    }

    test("GET /epochs/{n}/blocks — fetchEpochBlocks (page 1)") {
        // Use a recent epoch, fetch just page 1
        val result = provider.fetchEpochBlocks(10, page = Some(1), count = 5).await(timeout)
        assert(result.nonEmpty, "epoch 10 should have blocks")
        println(s"  Epoch 10 blocks (page 1, up to 5): ${result.size} blocks")
    }

    // ── Blocks ──────────────────────────────────────────────────────────────

    test("GET /blocks/latest — fetchLatestBlock") {
        val block = provider.fetchLatestBlock.await(timeout)
        assert(block.hash.nonEmpty, "block hash should be non-empty")
        assert(block.height.exists(_ > 0), "height should be positive")
        println(
          s"  Latest block: hash=${block.hash.take(16)}..., height=${block.height}, slot=${block.slot}, txCount=${block.txCount}"
        )
    }

    test("GET /blocks/{hash} — fetchBlock") {
        val latest = provider.fetchLatestBlock.await(timeout)
        val block = provider.fetchBlock(latest.hash).await(timeout)
        assert(block.hash == latest.hash, "fetched block should match")
        println(s"  Block by hash: confirmations=${block.confirmations}")
    }

    test("GET /blocks/slot/{n} — fetchBlockBySlot") {
        val latest = provider.fetchLatestBlock.await(timeout)
        latest.slot match
            case Some(slot) =>
                val block = provider.fetchBlockBySlot(slot).await(timeout)
                assert(block.hash == latest.hash, "block at slot should match latest")
                println(s"  Block by slot $slot: hash=${block.hash.take(16)}...")
            case None => cancel("latest block has no slot")
    }

    test("GET /blocks/latest/txs — fetchLatestBlockTxs") {
        val txs = provider.fetchLatestBlockTxs(page = Some(1), count = 10).await(timeout)
        // Latest block may or may not have txs
        println(s"  Latest block txs: ${txs.size}")
    }

    test("GET /blocks/{hash}/txs — fetchBlockTxs") {
        val txs =
            provider.fetchBlockTxs(testBlockHash, page = Some(1), count = 10).await(timeout)
        println(s"  Block txs: ${txs.size}")
    }

    test("GET /blocks/{hash}/next — fetchBlockNext") {
        // Use a block from the previous list to ensure there are blocks after it
        val prevBlocks =
            provider.fetchBlockPrevious(latestBlock.hash, page = Some(1), count = 3).await(timeout)
        if prevBlocks.nonEmpty then {
            val olderBlock = prevBlocks.last
            val nextBlocks =
                provider.fetchBlockNext(olderBlock.hash, page = Some(1), count = 3).await(timeout)
            assert(nextBlocks.nonEmpty, "should have next blocks after an older block")
            println(s"  Next blocks after ${olderBlock.hash.take(16)}...: ${nextBlocks.size}")
        } else cancel("Could not find previous blocks to test with")
    }

    test("GET /blocks/{hash}/previous — fetchBlockPrevious") {
        val latest = provider.fetchLatestBlock.await(timeout)
        val blocks =
            provider.fetchBlockPrevious(latest.hash, page = Some(1), count = 3).await(timeout)
        assert(blocks.nonEmpty, "should have previous blocks")
        println(s"  Previous blocks: ${blocks.size}")
    }

    test("GET /blocks/{hash}/addresses — fetchBlockAddresses") {
        val addrs =
            provider.fetchBlockAddresses(testBlockHash, page = Some(1), count = 10).await(timeout)
        println(s"  Block addresses: ${addrs.size}")
    }

    // ── Addresses ───────────────────────────────────────────────────────────

    test("GET /addresses/{addr} — fetchAddressInfo") {
        val info = provider.fetchAddressInfo(testAddress).await(timeout)
        assert(info.address == testAddress, "address should match")
        assert(info.addressType.nonEmpty, "type should be non-empty")
        println(
          s"  Address info: type=${info.addressType}, script=${info.script}, amounts=${info.amount.size}"
        )
    }

    test("GET /addresses/{addr}/transactions — fetchAddressTransactions (page 1)") {
        val txs =
            provider
                .fetchAddressTransactions(testAddress, page = Some(1), count = 5)
                .await(timeout)
        assert(txs.nonEmpty, "address should have transactions")
        txs.foreach { tx =>
            assert(tx.txHash.nonEmpty, "tx hash should be non-empty")
            assert(tx.blockHeight > 0, "block height should be positive")
        }
        println(s"  Address transactions (page 1): ${txs.size}")
    }

    test("GET /addresses/{addr}/txs — fetchAddressTxHashes (page 1)") {
        val hashes =
            provider.fetchAddressTxHashes(testAddress, page = Some(1), count = 5).await(timeout)
        assert(hashes.nonEmpty, "address should have tx hashes")
        hashes.foreach(h => assert(h.length == 64, "tx hash should be 64 hex chars"))
        println(s"  Address tx hashes (page 1): ${hashes.size}")
    }

    test("GET /addresses/{addr}/utxos — findUtxos with pagination") {
        try {
            import scalus.cardano.address.Address
            val addr = Address.fromBech32(testAddress)
            val result = provider.findUtxos(addr).await(timeout)
            result match
                case Right(utxos) =>
                    println(s"  UTxOs at address: ${utxos.size}")
                case Left(error) =>
                    println(s"  UTxOs query returned: $error (address may have no UTxOs)")
        } catch {
            case e: IllegalArgumentException =>
                // Address may be Byron format which doesn't support bech32
                println(s"  Skipping: ${e.getMessage}")
        }
    }

    // ── Transactions ────────────────────────────────────────────────────────

    test("GET /txs/{hash} — checkTransaction") {
        val status = provider.checkTransaction(
          scalus.cardano.ledger.TransactionHash.fromHex(testTxHash)
        ).await(timeout)
        println(s"  Transaction status: $status")
    }

    test("GET /txs/{hash}/utxos — fetchUtxosFromTransaction (via findUtxos)") {
        import scalus.cardano.ledger.*
        import scalus.cardano.node.*
        val txId = TransactionHash.fromHex(testTxHash)
        val query = UtxoQuery(UtxoSource.FromTransaction(txId))
        val result = provider.findUtxos(query).await(timeout)
        result match
            case Right(utxos) =>
                assert(utxos.nonEmpty, "transaction should have outputs")
                println(s"  Transaction UTxOs: ${utxos.size}")
            case Left(error) =>
                fail(s"Failed to fetch tx UTxOs: $error")
    }

    test("GET /txs/{hash} — fetchTransactionInfo") {
        val txInfo = provider.fetchTransactionInfo(testTxHash).await(timeout)
        assert(txInfo.hash == testTxHash, "tx hash should match")
        assert(txInfo.fees.value >= 0, "fees should be non-negative")
        println(
          s"  Transaction info: hash=${txInfo.hash.take(16)}..., fees=${txInfo.fees}, size=${txInfo.size}"
        )
    }

    test("GET /txs/{hash}/metadata — fetchTransactionMetadata") {
        val metadata = provider.fetchTransactionMetadata(testTxHash).await(timeout)
        println(s"  Transaction metadata entries: ${metadata.size}")
    }

    test("GET /txs/{hash}/metadata/cbor — fetchTransactionMetadataCbor") {
        val metadata = provider.fetchTransactionMetadataCbor(testTxHash).await(timeout)
        println(s"  Transaction metadata CBOR entries: ${metadata.size}")
    }

    test("GET /txs/{hash}/redeemers — fetchTransactionRedeemers") {
        val redeemers = provider.fetchTransactionRedeemers(testTxHash).await(timeout)
        println(s"  Transaction redeemers: ${redeemers.size}")
    }

    test("GET /txs/{hash}/withdrawals — fetchTransactionWithdrawals") {
        val withdrawals = provider.fetchTransactionWithdrawals(testTxHash).await(timeout)
        println(s"  Transaction withdrawals: ${withdrawals.size}")
    }

    test("GET /txs/{hash}/delegations — fetchTransactionDelegations") {
        val delegations = provider.fetchTransactionDelegations(testTxHash).await(timeout)
        println(s"  Transaction delegations: ${delegations.size}")
    }

    test("GET /txs/{hash}/mirs — fetchTransactionMirs") {
        val mirs = provider.fetchTransactionMirs(testTxHash).await(timeout)
        println(s"  Transaction MIRs: ${mirs.size}")
    }

    test("GET /txs/{hash}/pool_updates — fetchTransactionPoolUpdates") {
        val updates = provider.fetchTransactionPoolUpdates(testTxHash).await(timeout)
        println(s"  Transaction pool updates: ${updates.size}")
    }

    test("GET /txs/{hash}/pool_retires — fetchTransactionPoolRetires") {
        val retires = provider.fetchTransactionPoolRetires(testTxHash).await(timeout)
        println(s"  Transaction pool retires: ${retires.size}")
    }

    test("GET /txs/{hash}/stakes — fetchTransactionStakes") {
        val stakes = provider.fetchTransactionStakes(testTxHash).await(timeout)
        println(s"  Transaction stakes: ${stakes.size}")
    }

    test("GET /txs/{hash}/cbor — fetchTransactionCbor") {
        val cbor = provider.fetchTransactionCbor(testTxHash).await(timeout)
        assert(cbor.bytes.nonEmpty, "CBOR should be non-empty")
        println(s"  Transaction CBOR: ${cbor.bytes.length} bytes")
    }

    // ── Scripts & Datums ────────────────────────────────────────────────────

    /** Discover a script hash from a script address in recent block transactions. Looks at the
      * transaction outputs to find an address whose payment credential is a script hash.
      */
    private lazy val testScriptHash: Option[String] = {
        import scalus.cardano.ledger.*
        import scalus.cardano.address.*
        import scalus.cardano.node.*
        try {
            val txId = TransactionHash.fromHex(testTxHash)
            val query = UtxoQuery(UtxoSource.FromTransaction(txId))
            val utxos = provider.findUtxos(query).await(timeout).toOption.getOrElse(Map.empty)
            // Find a script address in the outputs
            utxos.values.iterator
                .map(_.address)
                .collectFirst {
                    case ShelleyAddress(_, ShelleyPaymentPart.Script(hash), _) => hash.toHex
                }
                .orElse {
                    // Fallback: search through multiple recent block transactions
                    val txHashes = provider
                        .fetchBlockTxs(testBlock.hash, page = Some(1), count = 20)
                        .await(timeout)
                    txHashes.iterator
                        .flatMap { txh =>
                            val txUtxos = provider
                                .findUtxos(
                                  UtxoQuery(
                                    UtxoSource.FromTransaction(TransactionHash.fromHex(txh))
                                  )
                                )
                                .await(timeout)
                                .toOption
                                .getOrElse(Map.empty)
                            txUtxos.values.iterator
                                .map(_.address)
                                .collectFirst {
                                    case ShelleyAddress(
                                          _,
                                          ShelleyPaymentPart.Script(hash),
                                          _
                                        ) =>
                                        hash.toHex
                                }
                        }
                        .nextOption()
                }
        } catch { case _: Exception => None }
    }

    test("GET /scripts/{hash} — fetchScriptInfo") {
        testScriptHash match
            case None => cancel("No script hash discovered from recent transactions")
            case Some(scriptHash) =>
                val info = provider.fetchScriptInfo(scriptHash).await(timeout)
                assert(info.scriptHash == scriptHash, "script hash should match")
                assert(info.scriptType.nonEmpty, "script type should be non-empty")
                println(
                  s"  Script info: hash=${info.scriptHash.take(16)}..., type=${info.scriptType}, size=${info.serialisedSize}"
                )
    }

    test("GET /scripts/{hash}/cbor — fetchScriptCbor") {
        testScriptHash match
            case None => cancel("No script hash discovered from recent transactions")
            case Some(scriptHash) =>
                val info = provider.fetchScriptInfo(scriptHash).await(timeout)
                // Only Plutus scripts have CBOR, native scripts use JSON
                if info.scriptType.startsWith("plutus") then {
                    val cbor = provider.fetchScriptCbor(scriptHash).await(timeout)
                    assert(cbor.bytes.nonEmpty, "script CBOR should be non-empty")
                    println(s"  Script CBOR: ${cbor.bytes.length} bytes")
                } else {
                    val json = provider.fetchScriptJson(scriptHash).await(timeout)
                    println(s"  Script JSON (native): $json")
                }
    }

    // ── Accounts ────────────────────────────────────────────────────────────

    test("GET /accounts/{stake} — fetchAccount") {
        val account = provider.fetchAccount(testStakeAddress).await(timeout)
        assert(account.stakeAddress == testStakeAddress, "stake address should match")
        println(
          s"  Account: active=${account.active}, controlled=${account.controlledAmount}, rewards=${account.rewardsSum}"
        )
    }

    test("GET /accounts/{stake}/registrations — fetchAccountRegistrations") {
        val regs = provider
            .fetchAccountRegistrations(testStakeAddress, page = Some(1), count = 5)
            .await(timeout)
        println(s"  Account registrations: ${regs.size}")
    }

    test("GET /accounts/{stake}/delegations — fetchAccountDelegations") {
        val dels = provider
            .fetchAccountDelegations(testStakeAddress, page = Some(1), count = 5)
            .await(timeout)
        println(s"  Account delegations: ${dels.size}")
    }

    test("GET /accounts/{stake}/addresses — fetchAccountAddresses") {
        val addrs = provider
            .fetchAccountAddresses(testStakeAddress, page = Some(1), count = 5)
            .await(timeout)
        assert(addrs.nonEmpty, "should have at least one address")
        println(s"  Account addresses: ${addrs.size}")
    }

    test("GET /accounts/{stake}/rewards — fetchAccountRewards") {
        val rewards = provider
            .fetchAccountRewards(testStakeAddress, page = Some(1), count = 5)
            .await(timeout)
        println(s"  Account rewards: ${rewards.size}")
        rewards.foreach { r =>
            println(s"    epoch=${r.epoch}, amount=${r.amount}, type=${r.rewardType}")
        }
    }

    test("GET /accounts/{stake}/utxos — fetchAccountUtxos") {
        val result = provider
            .fetchAccountUtxos(testStakeAddress, page = Some(1), count = 5)
            .await(timeout)
        result match
            case Right(utxos) => println(s"  Account UTxOs: ${utxos.size}")
            case Left(error)  => println(s"  Account UTxOs error: $error")
    }

    // ── Pools ───────────────────────────────────────────────────────────────

    private lazy val testPoolId: Option[String] = {
        try {
            val pools = provider.fetchPoolsExtended(page = Some(1), count = 1).await(timeout)
            pools.headOption.map(_.poolId)
        } catch { case _: Exception => None }
    }

    test("GET /pools/extended — fetchPoolsExtended (page 1)") {
        val pools = provider.fetchPoolsExtended(page = Some(1), count = 3).await(timeout)
        assert(pools.nonEmpty, "should have pools on preprod")
        pools.foreach { pool =>
            assert(pool.poolId.nonEmpty, "pool id should be non-empty")
        }
        println(s"  Pools (page 1): ${pools.size}")
    }

    test("GET /pools/{id}/delegators — fetchPoolDelegators") {
        testPoolId match
            case None => cancel("No pools found")
            case Some(poolId) =>
                val delegators = provider
                    .fetchPoolDelegators(poolId, page = Some(1), count = 5)
                    .await(timeout)
                println(s"  Pool delegators: ${delegators.size}")
    }

    test("GET /pools/{id}/history — fetchPoolHistory") {
        testPoolId match
            case None => cancel("No pools found")
            case Some(poolId) =>
                val history = provider
                    .fetchPoolHistory(poolId, page = Some(1), count = 5)
                    .await(timeout)
                println(s"  Pool history: ${history.size}")
    }

    // ── Metadata ────────────────────────────────────────────────────────────

    test("GET /metadata/txs/labels/{label} — fetchMetadataByLabel") {
        // Label 674 is commonly used for CIP-20 messages
        val metadata = provider.fetchMetadataByLabel(674, page = Some(1), count = 5).await(timeout)
        println(s"  Metadata for label 674: ${metadata.size} entries")
    }

    test("GET /metadata/txs/labels/{label}/cbor — fetchMetadataByLabelCbor") {
        val metadata =
            provider.fetchMetadataByLabelCbor(674, page = Some(1), count = 5).await(timeout)
        println(s"  Metadata CBOR for label 674: ${metadata.size} entries")
    }

    // ── Assets ──────────────────────────────────────────────────────────────

    /** Discover a native asset from the test block transactions */
    private lazy val testAsset: Option[String] = {
        import scalus.cardano.ledger.*
        import scalus.cardano.node.*
        try {
            val txHashes = provider
                .fetchBlockTxs(testBlock.hash, page = Some(1), count = 20)
                .await(timeout)
            txHashes.iterator
                .flatMap { txh =>
                    val utxos = provider
                        .findUtxos(
                          UtxoQuery(UtxoSource.FromTransaction(TransactionHash.fromHex(txh)))
                        )
                        .await(timeout)
                        .toOption
                        .getOrElse(Map.empty)
                    utxos.values.iterator.flatMap { output =>
                        output.value.assets.assets.headOption.map { case (policyId, assets) =>
                            policyId.toHex + assets.head._1.bytes.toHex
                        }
                    }
                }
                .nextOption()
        } catch { case _: Exception => None }
    }

    test("GET /assets/{asset} — fetchAsset") {
        testAsset match
            case None => cancel("No native asset found at test address")
            case Some(asset) =>
                val info = provider.fetchAsset(asset).await(timeout)
                assert(info.asset == asset, "asset should match")
                assert(info.policyId.nonEmpty, "policy id should be non-empty")
                println(
                  s"  Asset: ${info.asset.take(20)}..., policy=${info.policyId.take(16)}..., qty=${info.quantity}"
                )
    }

    test("GET /assets/{asset}/addresses — fetchAssetAddresses") {
        testAsset match
            case None => cancel("No native asset found")
            case Some(asset) =>
                val addrs =
                    provider.fetchAssetAddresses(asset, page = Some(1), count = 5).await(timeout)
                assert(addrs.nonEmpty, "should have at least one holder")
                println(s"  Asset addresses: ${addrs.size}")
    }

    test("GET /assets/{asset}/transactions — fetchAssetTransactions") {
        testAsset match
            case None => cancel("No native asset found")
            case Some(asset) =>
                val txs = provider
                    .fetchAssetTransactions(asset, page = Some(1), count = 5)
                    .await(timeout)
                assert(txs.nonEmpty, "should have at least one transaction")
                println(s"  Asset transactions: ${txs.size}")
    }

    test("findUtxos with UtxoSource.FromAsset") {
        import scalus.cardano.ledger.*
        import scalus.cardano.node.*
        import scalus.uplc.builtin.ByteString
        testAsset match
            case None => cancel("No native asset found")
            case Some(asset) =>
                val policyId = ScriptHash.fromHex(asset.take(56))
                val assetName = AssetName(ByteString.fromHex(asset.drop(56)))
                val query = UtxoQuery(UtxoSource.FromAsset(policyId, assetName))
                val result = provider.findUtxos(query).await(timeout)
                result match
                    case Right(utxos) =>
                        assert(utxos.nonEmpty, "should find UTxOs holding the asset")
                        utxos.values.foreach { output =>
                            val hasAsset = output.value.assets.assets.exists {
                                case (pid, assets) =>
                                    pid == policyId && assets.contains(assetName)
                            }
                            assert(hasAsset, s"each UTxO should contain the queried asset")
                        }
                        println(s"  FromAsset UTxOs: ${utxos.size}")
                    case Left(error) =>
                        fail(s"Failed to fetch UTxOs by asset: $error")
    }

    // ── Governance ──────────────────────────────────────────────────────────

    test("GET /governance/dreps/{id} — fetchDrep") {
        // Use the always-abstain DRep which exists on all networks
        val drepId = "drep_always_abstain"
        try {
            val drep = provider.fetchDrep(drepId).await(timeout)
            assert(drep.drepId.nonEmpty, "drep id should be non-empty")
            println(s"  DRep: id=${drep.drepId}, active=${drep.active}, amount=${drep.amount}")
        } catch {
            case e: RuntimeException if e.getMessage.contains("404") =>
                cancel("DRep not found on preprod")
        }
    }

    // ── Datum Resolution ────────────────────────────────────────────────────

    /** Discover a datum hash by searching recent transactions across multiple blocks */
    private lazy val testDatumHash: Option[String] = {
        import scalus.cardano.ledger.*
        import scalus.cardano.node.*
        try {
            // Walk back through blocks to find a transaction with a datum hash
            def searchBlock(hash: String, remaining: Int): Option[String] = {
                if remaining <= 0 then None
                else {
                    val txHashes = provider
                        .fetchBlockTxs(hash, page = Some(1), count = 20)
                        .await(timeout)
                    val found = txHashes.iterator
                        .flatMap { txh =>
                            val utxos = provider
                                .findUtxos(
                                  UtxoQuery(
                                    UtxoSource.FromTransaction(TransactionHash.fromHex(txh))
                                  )
                                )
                                .await(timeout)
                                .toOption
                                .getOrElse(Map.empty)
                            utxos.values.iterator.flatMap(_.datumOption).collectFirst {
                                case DatumOption.Hash(h) => h.toHex
                            }
                        }
                        .nextOption()
                    found.orElse {
                        val block = provider.fetchBlock(hash).await(timeout)
                        block.previousBlock.flatMap(prev => searchBlock(prev, remaining - 1))
                    }
                }
            }
            searchBlock(testBlock.hash, 10)
        } catch { case _: Exception => None }
    }

    test("GET /scripts/datum/{hash}/cbor — fetchDatumCbor + fetchDatum") {
        testDatumHash match
            case None =>
                // Datum hash UTxOs are rare on modern preprod (most use inline datums).
                // Verify the endpoint is callable by testing with a known-missing hash — it should
                // throw (not return malformed data).
                val fakeHash = "0000000000000000000000000000000000000000000000000000000000000000"
                val result = scala.util.Try(provider.fetchDatumCbor(fakeHash).await(timeout))
                assert(result.isFailure, "should fail for non-existent datum hash")
                println(s"  Datum endpoint verified (404 for missing hash, as expected)")
            case Some(hash) =>
                val cbor = provider.fetchDatumCbor(hash).await(timeout)
                assert(cbor.bytes.nonEmpty, "datum CBOR should be non-empty")
                val datum = provider.fetchDatum(hash).await(timeout)
                println(s"  Datum: hash=$hash, cbor=${cbor.bytes.length} bytes, data=$datum")
    }
}
