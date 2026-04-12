package scalus.cardano.node

import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.utils.Hex.hexToBytes
import sttp.client4.*
import sttp.model.StatusCode

import scalus.cardano.blockfrost.*
import upickle.default.read

import scala.annotation.nowarn
import scala.collection.immutable.SortedMap
import scala.concurrent.{ExecutionContext, Future}

/** Blockfrost-based Provider for Cardano blockchain operations.
  *
  * Use the async factory methods in the companion object to create instances:
  * `BlockfrostProvider.preview(apiKey)`, `BlockfrostProvider.mainnet(apiKey)`, etc. These fetch
  * protocol parameters during construction so that `cardanoInfo` is immediately available.
  *
  * @param apiKey
  *   Blockfrost API key
  * @param baseUrl
  *   Blockfrost API base URL
  * @param maxConcurrentRequests
  *   Maximum concurrent HTTP requests
  * @param initialCardanoInfo
  *   CardanoInfo with protocol parameters (fetched during async construction)
  */
class BlockfrostProvider(
    apiKey: String,
    baseUrl: String,
    maxConcurrentRequests: Int,
    initialCardanoInfo: CardanoInfo
)(using
    backend: Backend[Future],
    ec: ExecutionContext
) extends BlockchainProvider {

    private val log = scribe.Logger[BlockfrostProvider]

    override def executionContext: ExecutionContext = ec

    @volatile private var _cardanoInfo: CardanoInfo = initialCardanoInfo

    override def cardanoInfo: CardanoInfo = _cardanoInfo

    def network: Network = _cardanoInfo.network
    def slotConfig: SlotConfig = _cardanoInfo.slotConfig

    private val limiter = new ConcurrencyLimiter(maxConcurrentRequests)

    private def headers = Map("project_id" -> apiKey)

    /** Force refresh of cached CardanoInfo from the network.
      *
      * Fetches latest protocol parameters and updates the internal cache. Use this for long-running
      * applications that need to stay current with network changes.
      */
    def refreshCardanoInfo: Future[CardanoInfo] =
        fetchLatestParams.map { params =>
            val info = CardanoInfo(params, _cardanoInfo.network, _cardanoInfo.slotConfig)
            _cardanoInfo = info
            info
        }

    /** Wrap an HTTP request with rate limiting */
    private def rateLimited[T](request: => Future[T]): Future[T] = limiter(request)

    // ── Generic fetch helpers ───────────────────────────────────────────────

    /** Fetch a single JSON value from a path. Throws on non-success. */
    private def fetchJson(path: String): Future[ujson.Value] = {
        val url = s"$baseUrl$path"
        rateLimited(basicRequest.get(uri"$url").headers(headers).send(backend)).map { response =>
            if response.code.isSuccess then
                response.body match
                    case Right(body) => ujson.read(body, trace = false)
                    case Left(error) =>
                        throw RuntimeException(s"Failed to fetch $path: $error")
            else
                throw RuntimeException(
                  s"HTTP ${response.code} for $path: ${response.body}"
                )
        }
    }

    /** Fetch a single JSON value, returning None on 404. */
    private def fetchJsonOpt(path: String): Future[Option[ujson.Value]] = {
        val url = s"$baseUrl$path"
        rateLimited(basicRequest.get(uri"$url").headers(headers).send(backend)).map { response =>
            if response.code.isSuccess then
                response.body match
                    case Right(body) => Some(ujson.read(body, trace = false))
                    case Left(_)     => None
            else if response.code == StatusCode.NotFound then None
            else
                throw RuntimeException(
                  s"HTTP ${response.code} for $path: ${response.body}"
                )
        }
    }

    /** Fetch a single page of results from a paginated endpoint. */
    private def fetchPage(
        path: String,
        page: Int = 1,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[ujson.Value]] = {
        val separator = if path.contains('?') then '&' else '?'
        val url = s"$baseUrl$path${separator}page=$page&count=$count&order=$order"
        rateLimited(basicRequest.get(uri"$url").headers(headers).send(backend)).map { response =>
            if response.code.isSuccess then
                response.body match
                    case Right(body) => ujson.read(body, trace = false).arr.toSeq
                    case Left(_)     => Seq.empty
            else if response.code == StatusCode.NotFound then Seq.empty
            else
                throw RuntimeException(
                  s"HTTP ${response.code} for $path (page=$page): ${response.body}"
                )
        }
    }

    /** Fetch all pages from a paginated endpoint, accumulating results.
      *
      * Blockfrost returns max 100 items per page. Fetches pages sequentially until a page returns
      * fewer than `count` items or `maxPages` is reached.
      */
    private def fetchAllPages(
        path: String,
        count: Int = 100,
        order: String = "asc",
        maxPages: Int = 100
    ): Future[Seq[ujson.Value]] = {
        def go(page: Int, acc: Vector[ujson.Value]): Future[Seq[ujson.Value]] =
            if page > maxPages then Future.successful(acc)
            else
                fetchPage(path, page, count, order).flatMap { items =>
                    val updated = acc ++ items
                    if items.size < count then Future.successful(updated)
                    else go(page + 1, updated)
                }
        go(1, Vector.empty)
    }

    /** Fetch paginated results — either a single page or all pages.
      *
      * @param page
      *   None = fetch all pages, Some(n) = fetch page n only (1-based)
      */
    private def fetchPaginated(
        path: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[ujson.Value]] = page match
        case Some(p) => fetchPage(path, p, count, order)
        case None    => fetchAllPages(path, count, order)

    // ── Scripts & Datum Resolution ──────────────────────────────────────────

    /** Fetch script info by hash. `GET /scripts/{hash}` */
    def fetchScriptInfo(scriptHash: String): Future[ScriptInfo] =
        fetchJson(s"/scripts/$scriptHash").map(read[ScriptInfo](_))

    /** Fetch script as JSON (for native/timelock scripts). `GET /scripts/{hash}/json` */
    def fetchScriptJson(scriptHash: String): Future[ujson.Value] =
        fetchJson(s"/scripts/$scriptHash/json")

    /** Fetch script CBOR by hash. `GET /scripts/{hash}/cbor` */
    def fetchScriptCbor(scriptHash: String): Future[ByteString] =
        fetchJson(s"/scripts/$scriptHash/cbor").map(json => ByteString.fromHex(json("cbor").str))

    /** Fetch datum as JSON by datum hash. `GET /scripts/datum/{hash}` */
    def fetchDatumJson(datumHash: String): Future[ujson.Value] =
        fetchJson(s"/scripts/datum/$datumHash")

    /** Fetch datum CBOR by datum hash. `GET /scripts/datum/{hash}/cbor` */
    def fetchDatumCbor(datumHash: String): Future[ByteString] =
        fetchJson(s"/scripts/datum/$datumHash/cbor").map(json =>
            ByteString.fromHex(json("cbor").str)
        )

    /** Fetch and parse datum by hash, returning a Data value. */
    def fetchDatum(datumHash: String): Future[Data] =
        fetchDatumCbor(datumHash).map(cbor => Data.fromCbor(cbor.bytes))

    // ── Addresses ───────────────────────────────────────────────────────────

    /** Fetch address info. `GET /addresses/{address}` */
    def fetchAddressInfo(address: String): Future[AddressInfo] =
        fetchJson(s"/addresses/$address").map(read[AddressInfo](_))

    /** Fetch transactions for an address. `GET /addresses/{address}/transactions` */
    def fetchAddressTransactions(
        address: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AddressTransaction]] =
        fetchPaginated(s"/addresses/$address/transactions", page, count, order)
            .map(_.map(read[AddressTransaction](_)))

    /** Fetch transaction hashes for an address. `GET /addresses/{address}/txs` */
    def fetchAddressTxHashes(
        address: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[String]] =
        fetchPaginated(s"/addresses/$address/txs", page, count, order).map(_.map(_.str))

    // ── Blocks ──────────────────────────────────────────────────────────────

    /** Fetch the latest block. `GET /blocks/latest` */
    def fetchLatestBlock: Future[BlockInfo] =
        fetchJson("/blocks/latest").map(read[BlockInfo](_))

    /** Fetch transaction hashes in the latest block. `GET /blocks/latest/txs` */
    def fetchLatestBlockTxs(
        page: Option[Int] = None,
        count: Int = 100
    ): Future[Seq[String]] =
        fetchPaginated("/blocks/latest/txs", page, count).map(_.map(_.str))

    /** Fetch a block by hash or number. `GET /blocks/{hash_or_number}` */
    def fetchBlock(hashOrNumber: String): Future[BlockInfo] =
        fetchJson(s"/blocks/$hashOrNumber").map(read[BlockInfo](_))

    /** Fetch transaction hashes in a block. `GET /blocks/{hash_or_number}/txs` */
    def fetchBlockTxs(
        hashOrNumber: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[String]] =
        fetchPaginated(s"/blocks/$hashOrNumber/txs", page, count, order).map(_.map(_.str))

    /** Fetch subsequent blocks. `GET /blocks/{hash_or_number}/next` */
    def fetchBlockNext(
        hashOrNumber: String,
        page: Option[Int] = None,
        count: Int = 100
    ): Future[Seq[BlockInfo]] =
        fetchPaginated(s"/blocks/$hashOrNumber/next", page, count).map(_.map(read[BlockInfo](_)))

    /** Fetch preceding blocks. `GET /blocks/{hash_or_number}/previous` */
    def fetchBlockPrevious(
        hashOrNumber: String,
        page: Option[Int] = None,
        count: Int = 100
    ): Future[Seq[BlockInfo]] =
        fetchPaginated(s"/blocks/$hashOrNumber/previous", page, count)
            .map(_.map(read[BlockInfo](_)))

    /** Fetch addresses affected in a block. `GET /blocks/{hash_or_number}/addresses` */
    def fetchBlockAddresses(
        hashOrNumber: String,
        page: Option[Int] = None,
        count: Int = 100
    ): Future[Seq[BlockAddress]] =
        fetchPaginated(s"/blocks/$hashOrNumber/addresses", page, count)
            .map(_.map(read[BlockAddress](_)))

    /** Fetch a block by slot number. `GET /blocks/slot/{slot_number}` */
    def fetchBlockBySlot(slot: Long): Future[BlockInfo] =
        fetchJson(s"/blocks/slot/$slot").map(read[BlockInfo](_))

    // ── Transactions ────────────────────────────────────────────────────────

    /** Fetch transaction details. `GET /txs/{hash}` */
    def fetchTransactionInfo(txHash: String): Future[TransactionInfo] =
        fetchJson(s"/txs/$txHash").map(read[TransactionInfo](_))

    /** Fetch transaction CBOR. `GET /txs/{hash}/cbor` */
    def fetchTransactionCbor(txHash: String): Future[ByteString] =
        fetchJson(s"/txs/$txHash/cbor").map(json => ByteString.fromHex(json("cbor").str))

    /** Fetch transaction metadata as JSON. `GET /txs/{hash}/metadata`
      *
      * Returns empty sequence if the transaction has no metadata (404).
      */
    def fetchTransactionMetadata(txHash: String): Future[Seq[TransactionMetadataEntry]] =
        fetchJsonOpt(s"/txs/$txHash/metadata").map {
            case Some(json) => json.arr.map(read[TransactionMetadataEntry](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction metadata as CBOR. `GET /txs/{hash}/metadata/cbor`
      *
      * Returns empty sequence if the transaction has no metadata (404).
      */
    def fetchTransactionMetadataCbor(txHash: String): Future[Seq[TransactionMetadataCborEntry]] =
        fetchJsonOpt(s"/txs/$txHash/metadata/cbor").map {
            case Some(json) => json.arr.map(read[TransactionMetadataCborEntry](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction redeemers. `GET /txs/{hash}/redeemers`
      *
      * Returns empty sequence if the transaction has no redeemers (404).
      */
    def fetchTransactionRedeemers(txHash: String): Future[Seq[TransactionRedeemer]] =
        fetchJsonOpt(s"/txs/$txHash/redeemers").map {
            case Some(json) => json.arr.map(read[TransactionRedeemer](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction withdrawals. `GET /txs/{hash}/withdrawals`
      *
      * Returns empty sequence if the transaction has no withdrawals (404).
      */
    def fetchTransactionWithdrawals(txHash: String): Future[Seq[TransactionWithdrawal]] =
        fetchJsonOpt(s"/txs/$txHash/withdrawals").map {
            case Some(json) => json.arr.map(read[TransactionWithdrawal](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction delegation certificates. `GET /txs/{hash}/delegations`
      *
      * Returns empty sequence if the transaction has no delegations (404).
      */
    def fetchTransactionDelegations(txHash: String): Future[Seq[TransactionDelegation]] =
        fetchJsonOpt(s"/txs/$txHash/delegations").map {
            case Some(json) => json.arr.map(read[TransactionDelegation](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction MIR certificates. `GET /txs/{hash}/mirs`
      *
      * Returns empty sequence if the transaction has no MIRs (404).
      */
    def fetchTransactionMirs(txHash: String): Future[Seq[TransactionMir]] =
        fetchJsonOpt(s"/txs/$txHash/mirs").map {
            case Some(json) => json.arr.map(read[TransactionMir](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction pool update certificates. `GET /txs/{hash}/pool_updates`
      *
      * Returns empty sequence if the transaction has no pool updates (404).
      */
    def fetchTransactionPoolUpdates(txHash: String): Future[Seq[TransactionPoolUpdate]] =
        fetchJsonOpt(s"/txs/$txHash/pool_updates").map {
            case Some(json) => json.arr.map(read[TransactionPoolUpdate](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction pool retirement certificates. `GET /txs/{hash}/pool_retires`
      *
      * Returns empty sequence if the transaction has no pool retirements (404).
      */
    def fetchTransactionPoolRetires(txHash: String): Future[Seq[TransactionPoolRetire]] =
        fetchJsonOpt(s"/txs/$txHash/pool_retires").map {
            case Some(json) => json.arr.map(read[TransactionPoolRetire](_)).toSeq
            case None       => Seq.empty
        }

    /** Fetch transaction stake address certificates. `GET /txs/{hash}/stakes`
      *
      * Returns empty sequence if the transaction has no stake certificates (404).
      */
    def fetchTransactionStakes(txHash: String): Future[Seq[TransactionStake]] =
        fetchJsonOpt(s"/txs/$txHash/stakes").map {
            case Some(json) => json.arr.map(read[TransactionStake](_)).toSeq
            case None       => Seq.empty
        }

    // ── Accounts ────────────────────────────────────────────────────────────

    /** Fetch account info by stake address. `GET /accounts/{stake_address}` */
    def fetchAccount(stakeAddress: String): Future[AccountInfo] =
        fetchJson(s"/accounts/$stakeAddress").map(read[AccountInfo](_))

    /** Fetch account registration history. `GET /accounts/{stake_address}/registrations` */
    def fetchAccountRegistrations(
        stakeAddress: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AccountRegistration]] =
        fetchPaginated(s"/accounts/$stakeAddress/registrations", page, count, order)
            .map(_.map(read[AccountRegistration](_)))

    /** Fetch account delegation history. `GET /accounts/{stake_address}/delegations` */
    def fetchAccountDelegations(
        stakeAddress: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AccountDelegation]] =
        fetchPaginated(s"/accounts/$stakeAddress/delegations", page, count, order)
            .map(_.map(read[AccountDelegation](_)))

    /** Fetch addresses associated with account. `GET /accounts/{stake_address}/addresses` */
    def fetchAccountAddresses(
        stakeAddress: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AccountAddress]] =
        fetchPaginated(s"/accounts/$stakeAddress/addresses", page, count, order)
            .map(_.map(read[AccountAddress](_)))

    /** Fetch UTxOs for a stake address. `GET /accounts/{stake_address}/utxos` */
    def fetchAccountUtxos(
        stakeAddress: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Either[UtxoQueryError, Utxos]] =
        fetchPaginated(s"/accounts/$stakeAddress/utxos", page, count, order)
            .map { items =>
                if items.isEmpty then Right(Map.empty)
                else Right(BlockfrostProvider.parseUtxoItems(items))
            }
            .recover { case e: Throwable =>
                Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
            }

    /** Fetch account reward history. `GET /accounts/{stake_address}/rewards` */
    def fetchAccountRewards(
        stakeAddress: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AccountReward]] =
        fetchPaginated(s"/accounts/$stakeAddress/rewards", page, count, order)
            .map(_.map(read[AccountReward](_)))

    // ── Assets ──────────────────────────────────────────────────────────────

    /** Fetch asset info. `GET /assets/{asset}` */
    def fetchAsset(asset: String): Future[AssetInfo] =
        fetchJson(s"/assets/$asset").map(read[AssetInfo](_))

    /** Fetch addresses holding an asset. `GET /assets/{asset}/addresses` */
    def fetchAssetAddresses(
        asset: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AssetAddress]] =
        fetchPaginated(s"/assets/$asset/addresses", page, count, order)
            .map(_.map(read[AssetAddress](_)))

    /** Fetch transactions involving an asset. `GET /assets/{asset}/transactions` */
    def fetchAssetTransactions(
        asset: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[AssetTransaction]] =
        fetchPaginated(s"/assets/$asset/transactions", page, count, order)
            .map(_.map(read[AssetTransaction](_)))

    // ── Metadata ────────────────────────────────────────────────────────────

    /** Fetch transaction metadata by label as JSON. `GET /metadata/txs/labels/{label}` */
    def fetchMetadataByLabel(
        label: Long,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[TxMetadataJson]] =
        fetchPaginated(s"/metadata/txs/labels/$label", page, count, order)
            .map(_.map(read[TxMetadataJson](_)))

    /** Fetch transaction metadata by label as CBOR. `GET /metadata/txs/labels/{label}/cbor` */
    def fetchMetadataByLabelCbor(
        label: Long,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[TxMetadataCbor]] =
        fetchPaginated(s"/metadata/txs/labels/$label/cbor", page, count, order)
            .map(_.map(read[TxMetadataCbor](_)))

    // ── Genesis & Network ───────────────────────────────────────────────────

    /** Fetch blockchain genesis parameters. `GET /genesis` */
    def fetchGenesis: Future[GenesisInfo] =
        fetchJson("/genesis").map(read[GenesisInfo](_))

    /** Fetch network info (supply, stake). `GET /network` */
    def fetchNetwork: Future[NetworkInfo] =
        fetchJson("/network").map(read[NetworkInfo](_))

    /** Fetch network era summaries. `GET /network/eras` */
    def fetchNetworkEras: Future[Seq[EraInfo]] =
        fetchJson("/network/eras").map(_.arr.map(read[EraInfo](_)).toSeq)

    // ── Epochs ──────────────────────────────────────────────────────────��───

    /** Fetch block hashes in an epoch. `GET /epochs/{number}/blocks` */
    def fetchEpochBlocks(
        epoch: Long,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[String]] =
        fetchPaginated(s"/epochs/$epoch/blocks", page, count, order).map(_.map(_.str))

    // ── Pools ──────────────────────────────────���────────────────────────────

    /** Fetch extended pool list. `GET /pools/extended` */
    def fetchPoolsExtended(
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[PoolExtended]] =
        fetchPaginated("/pools/extended", page, count, order).map(_.map(read[PoolExtended](_)))

    /** Fetch pool delegators. `GET /pools/{pool_id}/delegators` */
    def fetchPoolDelegators(
        poolId: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[PoolDelegator]] =
        fetchPaginated(s"/pools/$poolId/delegators", page, count, order)
            .map(_.map(read[PoolDelegator](_)))

    /** Fetch pool history. `GET /pools/{pool_id}/history` */
    def fetchPoolHistory(
        poolId: String,
        page: Option[Int] = None,
        count: Int = 100,
        order: String = "asc"
    ): Future[Seq[PoolHistoryEntry]] =
        fetchPaginated(s"/pools/$poolId/history", page, count, order)
            .map(_.map(read[PoolHistoryEntry](_)))

    // ── Governance ──────────────────────────────────────────────────────────

    /** Fetch DRep info by ID. `GET /governance/dreps/{drep_id}` */
    def fetchDrep(drepId: String): Future[DrepInfo] =
        fetchJson(s"/governance/dreps/$drepId").map(read[DrepInfo](_))

    // ── Health ──────────────────────────────────────────────────────────────

    /** Fetch API root info. `GET /` */
    def fetchApiRoot: Future[ApiRootInfo] =
        fetchJson("/").map(read[ApiRootInfo](_))

    /** Fetch API health status. `GET /health` */
    def fetchHealth: Future[HealthStatus] =
        fetchJson("/health").map(read[HealthStatus](_))

    /** Fetch API server clock. `GET /health/clock` */
    def fetchHealthClock: Future[HealthClock] =
        fetchJson("/health/clock").map(read[HealthClock](_))

    def currentSlot: Future[SlotNo] =
        Future.successful(
          cardanoInfo.slotConfig.instantToSlot(java.time.Instant.now()).toLong
        )

    def fetchLatestParams: Future[ProtocolParams] =
        fetchProtocolParamsFromUrl(s"$baseUrl/epochs/latest/parameters")

    def fetchParamsOfEpoch(epoch: Long): Future[ProtocolParams] =
        fetchProtocolParamsFromUrl(s"$baseUrl/epochs/$epoch/parameters")

    private def fetchProtocolParamsFromUrl(url: String): Future[ProtocolParams] =
        rateLimited(basicRequest.get(uri"$url").headers(headers).send(backend))
            .map(BlockfrostProvider.parseProtocolParamsResponse)

    override def submit(
        tx: Transaction
    ): Future[Either[SubmitError, TransactionHash]] = {
        val url = s"$baseUrl/tx/submit"
        val txCbor = tx.toCbor
        log.info(s"Submitting tx ${tx.id.toHex} (${txCbor.length} bytes CBOR)")

        rateLimited(
          basicRequest
              .post(uri"$url")
              .headers(headers ++ Map("Content-Type" -> "application/cbor"))
              .body(txCbor)
              .send(backend)
        ).map { response =>
            response.code.code match {
                case c if c >= 200 && c < 300 =>
                    log.info(s"Tx ${tx.id.toHex} submitted successfully")
                    Right(tx.id)
                case c =>
                    val errorMsg = response.body.left.getOrElse(response.body.toString)
                    // Try to extract the message from Blockfrost JSON error response
                    val message =
                        try {
                            val json = ujson.read(errorMsg, trace = false)
                            json.obj.get("message").map(_.str).getOrElse(errorMsg)
                        } catch {
                            case _: Exception => errorMsg
                        }
                    val error = SubmitError.fromHttpResponse(c, message)
                    log.warn(s"Tx ${tx.id.toHex} rejected (HTTP $c): $message")
                    Left(error)
            }
        }.recover { case exception =>
            log.warn(s"Tx ${tx.id.toHex} submit exception: ${exception.getMessage}")
            Left(SubmitError.ConnectionError(s"Blockfrost submit exception", Some(exception)))
        }
    }

    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        checkTransactionHex(txHash.toHex)

    /** Internal implementation with precomputed hex hash to avoid repeated conversions during
      * polling.
      */
    private def checkTransactionHex(hexHash: String): Future[TransactionStatus] = {
        val txUrl = s"$baseUrl/txs/$hexHash"
        rateLimited(
          basicRequest.get(uri"$txUrl").headers(headers).send(backend)
        ).flatMap { response =>
            if response.code.isSuccess then {
                log.debug(s"checkTransaction($hexHash): Confirmed")
                Future.successful(TransactionStatus.Confirmed)
            } else if response.code == StatusCode.NotFound then {
                // Not confirmed yet — check mempool
                val mempoolUrl = s"$baseUrl/mempool/$hexHash"
                rateLimited(
                  basicRequest.get(uri"$mempoolUrl").headers(headers).send(backend)
                ).map { mempoolResponse =>
                    if mempoolResponse.code.isSuccess then {
                        log.debug(s"checkTransaction($hexHash): Pending (in mempool)")
                        TransactionStatus.Pending
                    } else {
                        log.debug(s"checkTransaction($hexHash): NotFound")
                        TransactionStatus.NotFound
                    }
                }
            } else {
                log.debug(s"checkTransaction($hexHash): NotFound (HTTP ${response.code})")
                Future.successful(TransactionStatus.NotFound)
            }
        }.recover { case scala.util.control.NonFatal(e) =>
            log.debug(s"checkTransaction($hexHash): NotFound (error: ${e.getMessage})")
            TransactionStatus.NotFound
        }
    }

    override def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int = 60,
        delayMs: Long = 1000
    ): Future[TransactionStatus] = {
        val hexHash = txHash.toHex

        def poll(attempt: Int, lastStatus: TransactionStatus): Future[TransactionStatus] =
            if attempt >= maxAttempts then {
                log.warn(
                  s"pollForConfirmation($hexHash): not confirmed after $maxAttempts attempts, last status: $lastStatus"
                )
                Future.successful(lastStatus)
            } else {
                checkTransactionHex(hexHash).flatMap { status =>
                    status match
                        case TransactionStatus.Confirmed =>
                            log.info(
                              s"pollForConfirmation($hexHash): Confirmed (attempt ${attempt + 1}/$maxAttempts)"
                            )
                            Future.successful(TransactionStatus.Confirmed)
                        case other =>
                            if (attempt + 1) % 10 == 0 then
                                log.debug(
                                  s"pollForConfirmation($hexHash): $other (attempt ${attempt + 1}/$maxAttempts)"
                                )
                            BlockfrostProviderPlatform.delayFuture(delayMs).flatMap { _ =>
                                poll(attempt + 1, other)
                            }
                }
            }

        poll(0, TransactionStatus.NotFound)
    }

    override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] = {
        // Evaluate source to get candidate UTxOs
        def evalSource(source: UtxoSource): Future[Either[UtxoQueryError, Utxos]] = source match
            case UtxoSource.FromAddress(addr) =>
                fetchUtxosFromAddress(addr)
            case UtxoSource.FromAsset(policyId, assetName) =>
                val asset = policyId.toHex + assetName.bytes.toHex
                fetchAssetAddresses(asset)
                    .flatMap { assetAddresses =>
                        val futures = assetAddresses.map { aa =>
                            fetchUtxosFromBech32Address(aa.address, Some(asset))
                        }
                        Future.sequence(futures).map { results =>
                            val errors = results.collect { case Left(e) => e }
                            val combined = results
                                .collect { case Right(utxos) => utxos }
                                .foldLeft(Map.empty: Utxos)(_ ++ _)
                            if combined.isEmpty && errors.nonEmpty then Left(errors.head)
                            else Right(combined)
                        }
                    }
                    .recover { case e: Throwable =>
                        Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
                    }
            case UtxoSource.FromInputs(inputs) =>
                // Fetch each input individually and combine
                val futures = inputs.toSeq.map(fetchUtxoFromInput)
                Future.sequence(futures).map { results =>
                    val (errors, successes) = results.partition(_.isLeft)
                    if errors.nonEmpty then Left(UtxoQueryError.NotFound(source))
                    else
                        Right(successes.collect { case Right(utxo) =>
                            utxo.input -> utxo.output
                        }.toMap)
                }
            case UtxoSource.FromTransaction(txId) =>
                fetchUtxosFromTransaction(txId)
            case UtxoSource.Or(left, right) =>
                // Execute both sources in parallel
                val leftFuture = evalSource(left)
                val rightFuture = evalSource(right)
                leftFuture.zip(rightFuture).map { case (leftResult, rightResult) =>
                    (leftResult, rightResult) match
                        case (Right(l), Right(r)) => Right(l ++ r)
                        case (Right(l), _)        => Right(l)
                        case (_, Right(r))        => Right(r)
                        case (Left(e), _)         => Left(e)
                }
            case UtxoSource.And(left, right) =>
                // Execute both sources in parallel (both must be fetched for intersection)
                val leftFuture = evalSource(left)
                val rightFuture = evalSource(right)
                leftFuture.zip(rightFuture).map { case (leftResult, rightResult) =>
                    (leftResult, rightResult) match
                        case (Right(l), Right(r)) =>
                            Right(l.filter { case (input, _) => r.contains(input) })
                        case (Left(e), _) => Left(e)
                        case (_, Left(e)) => Left(e)
                }

        // Extract HasAsset filter from a filter tree (returns first found and remaining filter)
        def extractHasAsset(
            filter: UtxoFilter
        ): (Option[(PolicyId, AssetName)], Option[UtxoFilter]) = filter match
            case UtxoFilter.HasAsset(policyId, assetName) =>
                (Some((policyId, assetName)), None)
            case UtxoFilter.And(left, right) =>
                extractHasAsset(left) match
                    case (Some(asset), None) => (Some(asset), Some(right))
                    case (Some(asset), Some(remaining)) =>
                        (Some(asset), Some(UtxoFilter.And(remaining, right)))
                    case (None, _) =>
                        extractHasAsset(right) match
                            case (Some(asset), None) => (Some(asset), Some(left))
                            case (Some(asset), Some(remaining)) =>
                                (Some(asset), Some(UtxoFilter.And(left, remaining)))
                            case (None, _) => (None, Some(filter))
            case _ => (None, Some(filter))

        // Evaluate a simple query with optimization for FromAddress + HasAsset
        def evalSimple(q: UtxoQuery.Simple): Future[Either[UtxoQueryError, Utxos]] = {
            // Optimization: use /addresses/{addr}/utxos/{asset} when possible
            val (sourceFuture, remainingFilter) = q.source match
                case UtxoSource.FromAddress(addr) =>
                    q.filter match
                        case Some(f) =>
                            extractHasAsset(f) match
                                case (Some((policyId, assetName)), remaining) =>
                                    (
                                      fetchUtxosFromAddressWithAsset(addr, policyId, assetName),
                                      remaining
                                    )
                                case (None, _) =>
                                    (evalSource(q.source), q.filter)
                        case None =>
                            (evalSource(q.source), None)
                case _ =>
                    (evalSource(q.source), q.filter)

            sourceFuture.map {
                case Left(e) => Left(e)
                case Right(candidates) =>
                    val filtered = remainingFilter match
                        case Some(f) => candidates.filter(UtxoQuery.evalFilter(f, _))
                        case None    => candidates
                    Right(
                      UtxoQuery.applyPagination(
                        filtered,
                        q.limit,
                        q.offset,
                        q.minRequiredTotalAmount
                      )
                    )
            }
        }

        // Evaluate query recursively
        def evalQuery(q: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] = q match
            case simple: UtxoQuery.Simple                           => evalSimple(simple)
            case UtxoQuery.Or(left, right, limit, offset, minTotal) =>
                // Execute both queries in parallel
                val leftFuture = evalQuery(UtxoQuery.propagate(left, limit, minTotal))
                val rightFuture = evalQuery(UtxoQuery.propagate(right, limit, minTotal))
                leftFuture.zip(rightFuture).map { case (leftResult, rightResult) =>
                    (leftResult, rightResult) match
                        case (Right(l), Right(r)) =>
                            Right(UtxoQuery.applyPagination(l ++ r, limit, offset, minTotal))
                        case (Right(l), _) =>
                            Right(UtxoQuery.applyPagination(l, limit, offset, minTotal))
                        case (_, Right(r)) =>
                            Right(UtxoQuery.applyPagination(r, limit, offset, minTotal))
                        case (Left(e), _) => Left(e)
                }

        evalQuery(query)
    }

    /** Convert address to Bech32 for API requests, returning an error for unsupported addresses */
    private def addressToBech32(address: Address): Either[UtxoQueryError, String] = {
        import scala.util.{Success, Failure}
        address match {
            case sh @ ShelleyAddress(_, _, _) =>
                sh.toBech32 match
                    case Success(b) => Right(b)
                    case Failure(_) =>
                        Left(
                          UtxoQueryError.NotSupported(
                            UtxoQuery(UtxoSource.FromAddress(address)),
                            "Invalid address"
                          )
                        )
            case _ =>
                Left(
                  UtxoQueryError.NotSupported(
                    UtxoQuery(UtxoSource.FromAddress(address)),
                    "Shelley addresses only"
                  )
                )
        }
    }

    /** Fetch UTxOs from an address using Blockfrost API */
    private def fetchUtxosFromAddress(address: Address): Future[Either[UtxoQueryError, Utxos]] = {
        addressToBech32(address) match
            case Left(error)   => Future.successful(Left(error))
            case Right(bech32) => fetchUtxosFromBech32Address(bech32)
    }

    /** Fetch UTxOs from an address filtered by asset using Blockfrost API. Uses the optimized
      * /addresses/{addr}/utxos/{asset} endpoint.
      */
    private def fetchUtxosFromAddressWithAsset(
        address: Address,
        policyId: PolicyId,
        assetName: AssetName
    ): Future[Either[UtxoQueryError, Utxos]] = {
        addressToBech32(address) match
            case Left(error)   => Future.successful(Left(error))
            case Right(bech32) =>
                // Asset is policyId + assetName hex concatenated
                val asset = policyId.toHex + assetName.bytes.toHex
                fetchUtxosFromBech32Address(bech32, Some(asset))
    }

    /** Internal helper to fetch UTxOs from a Bech32 address, optionally filtered by asset.
      *
      * Automatically paginates to fetch all UTxOs (Blockfrost returns max 100 per page).
      */
    private def fetchUtxosFromBech32Address(
        bech32: String,
        asset: Option[String] = None
    ): Future[Either[UtxoQueryError, Utxos]] = {
        val path = asset match
            case Some(a) => s"/addresses/$bech32/utxos/$a"
            case None    => s"/addresses/$bech32/utxos"
        fetchAllPages(path)
            .map { items =>
                if items.isEmpty then Right(Map.empty)
                else Right(BlockfrostProvider.parseUtxoItems(items))
            }
            .recover { case e: Throwable =>
                Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
            }
    }

    /** Fetch UTxOs from a transaction using Blockfrost API */
    private def fetchUtxosFromTransaction(
        txId: TransactionHash
    ): Future[Either[UtxoQueryError, Utxos]] = {
        val url = s"$baseUrl/txs/${txId.toHex}/utxos"
        rateLimited(
          basicRequest
              .get(uri"$url")
              .headers(headers)
              .send(backend)
        ).map { response =>
            if response.code.isSuccess then {
                response.body match {
                    case Right(body) =>
                        val json = ujson.read(body, trace = false)
                        val outputs = json("outputs").arr
                        val utxos = outputs.zipWithIndex.map { case (outputJson, index) =>
                            val input = TransactionInput(txId, index)
                            BlockfrostProvider.parseUtxoOutput(input, outputJson)
                        }.toMap
                        Right(utxos)
                    case Left(error) =>
                        Left(UtxoQueryError.NetworkError(s"Failed to fetch tx UTxOs: $error"))
                }
            } else if response.code == StatusCode.NotFound then {
                Left(UtxoQueryError.NotFound(UtxoSource.FromTransaction(txId)))
            } else {
                Left(UtxoQueryError.NetworkError(s"Failed to fetch tx UTxOs: ${response.body}"))
            }
        }.recover { case e: Throwable =>
            Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
        }
    }

    /** Fetch a single UTxO by its input */
    private def fetchUtxoFromInput(
        input: TransactionInput
    ): Future[Either[UtxoQueryError, Utxo]] = {
        fetchUtxosFromTransaction(input.transactionId).map {
            case Left(e) => Left(e)
            case Right(utxos) =>
                utxos.get(input) match
                    case Some(output) => Right(Utxo(input, output))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(Set(input))))
        }
    }
}

/** Companion object for BlockfrostProvider with factory methods and utilities. */
object BlockfrostProvider {

    /** Blockfrost API URL for Cardano mainnet */
    val mainnetUrl = "https://cardano-mainnet.blockfrost.io/api/v0"

    /** Parse a Blockfrost/Yaci `amount[]` array into a Value.
      *
      * Handles both Blockfrost (string quantity) and Yaci (number quantity) formats.
      */
    private def parseValue(amountArray: ujson.Arr): Value = {
        var lovelace = 0L
        val multiAssetBuilder =
            scala.collection.mutable
                .Map[ScriptHash, scala.collection.mutable.Map[AssetName, Long]]()

        amountArray.value.foreach { item =>
            val unit = item("unit").str
            // Handle both string (Blockfrost) and number (Yaci) formats
            val quantity = item("quantity") match
                case s: ujson.Str => s.str.toLong
                case n: ujson.Num => n.num.toLong
                case other        => other.toString.toLong

            if unit == "lovelace" then {
                lovelace = quantity
            } else {
                // Parse multi-asset: first 56 chars = policy ID (28 bytes hex), rest = asset name
                val policyId = ScriptHash.fromHex(unit.take(56))
                val assetNameHex = unit.drop(56)
                val assetName = AssetName(ByteString.fromHex(assetNameHex))

                multiAssetBuilder
                    .getOrElseUpdate(policyId, scala.collection.mutable.Map())
                    .update(assetName, quantity)
            }
        }

        if multiAssetBuilder.isEmpty then Value.lovelace(lovelace)
        else {
            val immutableAssets: SortedMap[ScriptHash, SortedMap[AssetName, Long]] =
                SortedMap.from(multiAssetBuilder.view.mapValues(m => SortedMap.from(m)))
            Value(Coin(lovelace), MultiAsset(immutableAssets))
        }
    }

    /** Parse datum fields from a Blockfrost UTxO JSON object.
      *
      * Handles `data_hash`, `inline_datum`, and `inline_datum_cbor` fields. Supports both
      * Blockfrost format (inline_datum as JSON object, inline_datum_cbor as hex) and Yaci format
      * (inline_datum as CBOR hex string).
      */
    private def parseDatumOption(json: ujson.Obj): Option[DatumOption] = {
        val dataHash = json.value.get("data_hash")
        val inlineDatum = json.value.get("inline_datum")
        val inlineDatumCbor = json.value.get("inline_datum_cbor")

        (dataHash, inlineDatum, inlineDatumCbor) match {
            // Prefer inline_datum_cbor if present (hex CBOR, available in MiniBF/recent Blockfrost)
            case (_, _, Some(cborJson)) if !cborJson.isNull =>
                Some(DatumOption.Inline(Data.fromCbor(hexToBytes(cborJson.str))))
            // inline_datum as hex CBOR string (Yaci format)
            case (_, Some(s: ujson.Str), _) =>
                Some(DatumOption.Inline(Data.fromCbor(hexToBytes(s.str))))
            // inline_datum as JSON object (Blockfrost format) — fall back to data_hash
            case (_, Some(_: ujson.Obj), _) =>
                dataHash match
                    case Some(dh) if !dh.isNull =>
                        Some(DatumOption.Hash(Hash(ByteString.fromHex(dh.str))))
                    case _ => None
            case (Some(dataHashJson), _, _) if !dataHashJson.isNull =>
                Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHashJson.str))))
            case _ => None
        }
    }

    /** Parse protocol parameters from HTTP response, throwing on error. */
    private[node] def parseProtocolParamsResponse(
        response: Response[Either[String, String]]
    ): ProtocolParams = {
        if response.code == StatusCode.Ok then {
            response.body match {
                case Right(body) => ProtocolParams.fromBlockfrostJson(body)
                case Left(error) =>
                    throw RuntimeException(
                      s"Failed to fetch protocol parameters. Status: ${response.code}, Body: $error"
                    )
            }
        } else {
            throw RuntimeException(
              s"Failed to fetch protocol parameters. Status: ${response.code}, Body: ${response.body}"
            )
        }
    }

    /** Blockfrost API URL for Cardano preview testnet */
    val previewUrl = "https://cardano-preview.blockfrost.io/api/v0"

    /** Blockfrost API URL for Cardano preprod testnet */
    val preprodUrl = "https://cardano-preprod.blockfrost.io/api/v0"

    /** Local Yaci DevKit API URL */
    val localUrl = "http://localhost:8080/api/v1"

    /** Local Yaci DevKit admin API URL */
    val localAdminUrl = "http://localhost:10000/local-cluster/api"

    @deprecated("Use mainnetUrl instead", "0.14.1")
    val MainnetUrl: String = mainnetUrl

    @deprecated("Use previewUrl instead", "0.14.1")
    val PreviewUrl: String = previewUrl

    @deprecated("Use preprodUrl instead", "0.14.1")
    val PreprodUrl: String = preprodUrl

    @deprecated("Use localUrl instead", "0.14.1")
    val LocalUrl: String = localUrl

    def parseUtxos(json: String): Utxos = {
        val utxosArray = ujson.read(json, trace = false).arr.toSeq
        parseUtxoItems(utxosArray)
    }

    /** Parse a sequence of UTxO JSON values into a Utxos map. */
    private[node] def parseUtxoItems(items: Seq[ujson.Value]): Utxos = {
        items.map { utxoJson =>
            val txInput = TransactionInput(
              TransactionHash.fromHex(utxoJson("tx_hash").str),
              utxoJson("output_index").num.toInt
            )
            parseUtxoOutput(txInput, utxoJson)
        }.toMap
    }

    /** Parse a single UTxO JSON into a (TransactionInput, TransactionOutput) pair. */
    private def parseUtxoOutput(
        input: TransactionInput,
        json: ujson.Value
    ): (TransactionInput, TransactionOutput) = {
        val address = Address.fromBech32(json("address").str)
        val value = parseValue(json("amount").arr)
        val datumOption = parseDatumOption(json.obj)
        // reference_script_hash is present in the JSON but we cannot reconstruct
        // the full ScriptRef without fetching the script bytes via /scripts/{hash}/cbor.
        // Use BlockfrostProvider.resolveScriptRefs to enrich UTxOs with script references.
        val txOutput = TransactionOutput(
          address = address,
          value = value,
          datumOption = datumOption,
          scriptRef = None
        )
        input -> txOutput
    }

    /** Create a BlockfrostProvider for Cardano mainnet.
      *
      * Fetches protocol parameters during construction.
      *
      * @param apiKey
      *   Blockfrost API key
      * @param maxConcurrentRequests
      *   Maximum concurrent HTTP requests (default 5)
      * @return
      *   Future containing the configured BlockfrostProvider
      */
    def mainnet(apiKey: String, maxConcurrentRequests: Int = 5)(using
        ec: ExecutionContext
    ): Future[BlockfrostProvider] =
        create(apiKey, mainnetUrl, Network.Mainnet, SlotConfig.mainnet, maxConcurrentRequests)

    /** Create a BlockfrostProvider for Cardano preview testnet.
      *
      * Fetches protocol parameters during construction.
      *
      * @param apiKey
      *   Blockfrost API key
      * @param maxConcurrentRequests
      *   Maximum concurrent HTTP requests (default 5)
      * @return
      *   Future containing the configured BlockfrostProvider
      */
    def preview(apiKey: String, maxConcurrentRequests: Int = 5)(using
        ec: ExecutionContext
    ): Future[BlockfrostProvider] =
        create(apiKey, previewUrl, Network.Testnet, SlotConfig.preview, maxConcurrentRequests)

    /** Create a BlockfrostProvider for Cardano preprod testnet.
      *
      * Fetches protocol parameters during construction.
      *
      * @param apiKey
      *   Blockfrost API key
      * @param maxConcurrentRequests
      *   Maximum concurrent HTTP requests (default 5)
      * @return
      *   Future containing the configured BlockfrostProvider
      */
    def preprod(apiKey: String, maxConcurrentRequests: Int = 5)(using
        ec: ExecutionContext
    ): Future[BlockfrostProvider] =
        create(apiKey, preprodUrl, Network.Testnet, SlotConfig.preprod, maxConcurrentRequests)

    /** Create a BlockfrostProvider for local Yaci DevKit.
      *
      * Fetches protocol parameters and slot configuration during construction. The slot config is
      * fetched from the Yaci admin API (`/admin/devnet`) to get the correct `startTime`.
      *
      * @param baseUrl
      *   API base URL (default: http://localhost:8080/api/v1)
      * @param adminUrl
      *   Yaci admin API URL (default: http://localhost:10000/local-cluster/api)
      * @param maxConcurrentRequests
      *   Maximum concurrent HTTP requests (default 5)
      * @return
      *   Future containing the configured BlockfrostProvider
      */
    def localYaci(
        baseUrl: String = localUrl,
        adminUrl: String = localAdminUrl,
        maxConcurrentRequests: Int = 5
    )(using ec: ExecutionContext): Future[BlockfrostProvider] = {
        given backend: Backend[Future] = BlockfrostProviderPlatform.defaultBackend
        val paramsFuture = fetchProtocolParams("", baseUrl)
        val slotConfigFuture = fetchYaciSlotConfig(adminUrl)
        paramsFuture.zip(slotConfigFuture).map { case (params, slotConfig) =>
            new BlockfrostProvider(
              "",
              baseUrl,
              maxConcurrentRequests,
              CardanoInfo(params, Network.Testnet, slotConfig)
            )
        }
    }

    /** Fetch slot configuration from a Yaci DevKit admin API.
      *
      * Calls `GET {adminUrl}/admin/devnet` and extracts `startTime` (epoch seconds) and
      * `slotLength` (seconds as double) from the response JSON.
      *
      * @param adminUrl
      *   Yaci admin API base URL (e.g. http://localhost:10000/local-cluster/api)
      * @return
      *   Future containing the SlotConfig
      */
    @nowarn("msg=long2double")
    def fetchYaciSlotConfig(adminUrl: String)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ): Future[SlotConfig] = {
        val url = s"${adminUrl.stripSuffix("/")}/admin/devnet"
        basicRequest.get(uri"$url").send(backend).map { response =>
            if response.code == StatusCode.Ok then
                response.body match
                    case Right(body) =>
                        val json = ujson.read(body, trace = false)
                        val startTime = json("startTime").num.toLong
                        val slotLength = json("slotLength").num
                        SlotConfig(startTime * 1000, 0L, (slotLength * 1000).toLong)
                    case Left(error) =>
                        throw RuntimeException(
                          s"Failed to fetch Yaci slot config. Status: ${response.code}, Body: $error"
                        )
            else
                throw RuntimeException(
                  s"Failed to fetch Yaci slot config. Status: ${response.code}, Body: ${response.body}"
                )
        }
    }

    /** Create a BlockfrostProvider with custom configuration.
      *
      * Fetches protocol parameters during construction.
      *
      * @param apiKey
      *   Blockfrost API key
      * @param baseUrl
      *   Blockfrost-compatible API base URL
      * @param network
      *   Cardano network (Mainnet or Testnet)
      * @param slotConfig
      *   Slot configuration for the network
      * @param maxConcurrentRequests
      *   Maximum concurrent HTTP requests (default 5)
      * @return
      *   Future containing the configured BlockfrostProvider
      */
    def create(
        apiKey: String,
        baseUrl: String,
        network: Network,
        slotConfig: SlotConfig,
        maxConcurrentRequests: Int = 5
    )(using ec: ExecutionContext): Future[BlockfrostProvider] = {
        given backend: Backend[Future] = BlockfrostProviderPlatform.defaultBackend
        fetchProtocolParams(apiKey, baseUrl).map { params =>
            new BlockfrostProvider(
              apiKey,
              baseUrl,
              maxConcurrentRequests,
              CardanoInfo(params, network, slotConfig)
            )
        }
    }

    /** Fetch protocol parameters from Blockfrost-compatible API. */
    private def fetchProtocolParams(apiKey: String, baseUrl: String)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ): Future[ProtocolParams] = {
        val url = s"$baseUrl/epochs/latest/parameters"
        val headers = Map("project_id" -> apiKey)
        basicRequest.get(uri"$url").headers(headers).send(backend).map(parseProtocolParamsResponse)
    }
}
