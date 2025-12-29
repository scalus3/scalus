package scalus.cardano.node

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.utils.Hex.hexToBytes
import sttp.client4.*
import sttp.model.StatusCode

import scala.collection.immutable.SortedMap
import scala.concurrent.{ExecutionContext, Future}

class BlockfrostProvider(apiKey: String, baseUrl: String = BlockfrostProvider.previewUrl)(using
    backend: Backend[Future],
    ec: ExecutionContext
) extends Provider {

    private def headers = Map("project_id" -> apiKey)

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams] = {
        val url = s"$baseUrl/epochs/latest/parameters"
        basicRequest
            .get(uri"$url")
            .headers(headers)
            .send(backend)
            .map { response =>
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
    }

    def fetchParamsOfEpoch(epoch: Long): Future[ProtocolParams] = {
        val url = s"$baseUrl/epochs/$epoch/parameters"
        basicRequest
            .get(uri"$url")
            .headers(headers)
            .send(backend)
            .map { response =>
                if response.code == StatusCode.Ok then {
                    response.body match {
                        case Right(body) => ProtocolParams.fromBlockfrostJson(body)
                        case Left(error) =>
                            throw RuntimeException(
                              s"Failed to fetch protocol parameters for epoch $epoch. Status: ${response.code}, Body: $error"
                            )
                    }
                } else {
                    throw RuntimeException(
                      s"Failed to fetch protocol parameters for epoch $epoch. Status: ${response.code}, Body: ${response.body}"
                    )
                }
            }
    }

    override def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[RuntimeException, Utxos]] = {
        val bech32 = address match {
            case sh @ ShelleyAddress(network, payment, delegation) => sh.toBech32.get
            case _ => return Future.successful(Left(new RuntimeException("Shelley addresses only")))
        }
        val url = s"$baseUrl/addresses/$bech32/utxos"
        basicRequest
            .get(uri"$url")
            .headers(headers)
            .send(backend)
            .map { response =>
                if response.code.isSuccess then {
                    response.body match {
                        case Right(body) =>
                            val utxos = BlockfrostProvider.parseUtxos(body)
                            // Apply filters
                            val filtered = utxos.filter { case (input, output) =>
                                val txIdMatch =
                                    transactionId.forall(txId => input.transactionId == txId)
                                val minAmountMatch =
                                    minAmount.forall(min => output.value.coin.value >= min.value)
                                txIdMatch && minAmountMatch
                            }
                            Right(filtered)
                        case Left(error) =>
                            Left(
                              RuntimeException(
                                s"Failed to fetch UTXOs for address $address. Status: ${response.code}, Body: $error"
                              )
                            )
                    }
                } else {
                    Left(
                      RuntimeException(
                        s"Failed to fetch UTXOs for address $address. Status: ${response.code}, Body: ${response.body}"
                      )
                    )
                }
            }
    }

    override def submit(
        tx: Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        val url = s"$baseUrl/tx/submit"
        val txCbor = tx.toCbor

        basicRequest
            .post(uri"$url")
            .headers(headers ++ Map("Content-Type" -> "application/cbor"))
            .body(txCbor)
            .send(backend)
            .map { response =>
                response.code.code match {
                    case c if c >= 400 && c < 500 =>
                        val errorMsg = response.body.left.getOrElse(response.body.toString)
                        Left(SubmitError.NodeError(errorMsg))
                    case c if c >= 500 =>
                        val errorMsg = response.body.left.getOrElse(response.body.toString)
                        Left(SubmitError.NodeError(s"Blockfrost submit error: $errorMsg"))
                    case _ => Right(tx.id)
                }
            }
            .recover { case exception =>
                Left(SubmitError.NetworkError(s"Blockfrost submit exception", Some(exception)))
            }
    }

    override def findUtxo(
        input: TransactionInput
    )(using ExecutionContext): Future[Either[RuntimeException, Utxo]] = {
        // Query Blockfrost for the specific transaction output
        val txHash = input.transactionId.toHex
        val outputIndex = input.index

        val url = s"$baseUrl/txs/$txHash/utxos"
        basicRequest
            .get(uri"$url")
            .headers(headers)
            .send(backend)
            .map { response =>
                if response.code.isSuccess then {
                    response.body match {
                        case Right(body) =>
                            val json = ujson.read(body, trace = false)
                            val outputs = json("outputs").arr

                            if outputIndex >= outputs.size then {
                                throw new RuntimeException(
                                  s"Output index $outputIndex out of bounds (tx has ${outputs.size} outputs)"
                                )
                            }

                            val outputJson = outputs(outputIndex)
                            val address = Address.fromBech32(outputJson("address").str)

                            // Parse value
                            val amountArray = outputJson("amount").arr
                            var lovelace = 0L
                            val multiAssetBuilder = scala.collection.mutable
                                .Map[ScriptHash, scala.collection.mutable.Map[AssetName, Long]]()

                            amountArray.foreach { item =>
                                val unit = item("unit").str
                                val quantity = item("quantity").str.toLong

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

                            val value = if multiAssetBuilder.isEmpty then {
                                Value.lovelace(lovelace)
                            } else {
                                // Convert mutable maps to immutable SortedMaps
                                val immutableAssets
                                    : SortedMap[ScriptHash, SortedMap[AssetName, Long]] =
                                    SortedMap.from(
                                      multiAssetBuilder.view.mapValues(m => SortedMap.from(m))
                                    )
                                Value(Coin(lovelace), MultiAsset(immutableAssets))
                            }

                            // Parse datum if present
                            val datumOption: Option[DatumOption] = {
                                val dataHash = outputJson.obj.get("data_hash")
                                val inlineDatum = outputJson.obj.get("inline_datum")

                                (dataHash, inlineDatum) match {
                                    case (_, Some(inlineDatumJson)) if !inlineDatumJson.isNull =>
                                        Some(
                                          DatumOption.Inline(
                                            Data.fromCbor(hexToBytes(inlineDatumJson.str))
                                          )
                                        )
                                    case (Some(dataHashJson), _) if !dataHashJson.isNull =>
                                        Some(
                                          DatumOption.Hash(
                                            Hash(ByteString.fromHex(dataHashJson.str))
                                          )
                                        )
                                    case _ => None
                                }
                            }

                            val output = TransactionOutput(
                              address = address,
                              value = value,
                              datumOption = datumOption,
                              scriptRef = None
                            )

                            Right(Utxo(input, output))
                        case Left(error) =>
                            Left(new RuntimeException(s"Failed to fetch UTxO: $error"))
                    }
                } else if response.code == StatusCode.NotFound then {
                    Left(new RuntimeException(s"Transaction ${txHash.take(16)}... not found"))
                } else {
                    Left(new RuntimeException(s"Failed to fetch UTxO: ${response.body}"))
                }
            }
            .recover {
                case e: RuntimeException => Left(e)
                case e: Throwable =>
                    Left(new RuntimeException(s"Failed to find UTxO: ${e.getMessage}", e))
            }
    }

    override def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[RuntimeException, Utxos]] =
        Future.successful(Left(new RuntimeException("Unimplemented, use `findUtxos(address)`")))

    override def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash],
        datum: Option[DatumOption],
        minAmount: Option[Coin]
    )(using ExecutionContext): Future[Either[RuntimeException, Utxo]] = {
        findUtxos(address, transactionId, datum, minAmount, None).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match {
                    case Some((input, output)) => Right(Utxo(input, output))
                    case None => Left(new RuntimeException(s"No UTxO found at address $address"))
                }
            }
        }
    }

    /** Check if a transaction has been confirmed on-chain */
    def isTransactionConfirmed(txHash: String): Future[Either[RuntimeException, Boolean]] = {
        val url = s"$baseUrl/txs/$txHash"
        val response = basicRequest
            .get(uri"$url")
            .headers(headers)
            .send(backend)
            .map { response =>
                if response.code.isSuccess then {
                    response.body match {
                        case Right(body) =>
                            val json = ujson.read(body, trace = false)
                            val blockHeight = json.obj.get("block_height")
                            Right(blockHeight.exists(_.num > 0))
                        case Left(_) => Right(false)
                    }
                } else if response.code == StatusCode.NotFound then {
                    Right(false)
                } else {
                    Left(
                      new RuntimeException(
                        s"Failed to check transaction status: ${response.body}"
                      )
                    )
                }
            }
        response
    }
}

object BlockfrostProvider {

    /** Blockfrost API URL for Cardano mainnet */
    val mainnetUrl = "https://cardano-mainnet.blockfrost.io/api/v0"

    /** Blockfrost API URL for Cardano preview testnet */
    val previewUrl = "https://cardano-preview.blockfrost.io/api/v0"

    /** Blockfrost API URL for Cardano preprod testnet */
    val preprodUrl = "https://cardano-preprod.blockfrost.io/api/v0"

    /** Local Yaci DevKit API URL */
    val localUrl = "http://localhost:8080/api/v1"

    @deprecated("Use mainnetUrl instead", "0.14.1")
    val MainnetUrl: String = mainnetUrl

    @deprecated("Use previewUrl instead", "0.14.1")
    val PreviewUrl: String = previewUrl

    @deprecated("Use preprodUrl instead", "0.14.1")
    val PreprodUrl: String = preprodUrl

    @deprecated("Use localUrl instead", "0.14.1")
    val LocalUrl: String = localUrl

    def localYaci(using backend: Backend[Future], ec: ExecutionContext) =
        BlockfrostProvider("", localUrl)

    /** Create a Blockfrost client for mainnet */
    def mainnet(apiKey: String)(using backend: Backend[Future], ec: ExecutionContext) =
        new BlockfrostProvider(apiKey, mainnetUrl)

    /** Create a Blockfrost client for preview testnet */
    def preview(apiKey: String)(using backend: Backend[Future], ec: ExecutionContext) =
        new BlockfrostProvider(apiKey, previewUrl)

    /** Create a Blockfrost client for preprod testnet */
    def preprod(apiKey: String)(using backend: Backend[Future], ec: ExecutionContext) =
        new BlockfrostProvider(apiKey, preprodUrl)

    enum BlockfrostError:
        case NetworkError(underlying: Throwable)
        case LedgerError(description: String)

    def parseUtxos(json: String): Utxos = {
        val utxosArray = ujson.read(json, trace = false).arr
        utxosArray.map { utxoJson =>
            val txInput = TransactionInput(
              TransactionHash.fromHex(utxoJson("tx_hash").str),
              utxoJson("output_index").num.toInt
            )
            val address = Address.fromBech32(utxoJson("address").str)

            // Parse value
            val amountArray = utxoJson("amount").arr
            var lovelace = 0L
            val multiAssetBuilder = scala.collection.mutable
                .Map[ScriptHash, scala.collection.mutable.Map[AssetName, Long]]()

            amountArray.foreach { item =>
                val unit = item("unit").str
                val quantity = item("quantity").str.toLong

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

            val value = if multiAssetBuilder.isEmpty then {
                Value.lovelace(lovelace)
            } else {
                // Convert mutable maps to immutable SortedMaps
                val immutableAssets: SortedMap[ScriptHash, SortedMap[AssetName, Long]] =
                    SortedMap.from(
                      multiAssetBuilder.view.mapValues(m => SortedMap.from(m))
                    )
                Value(Coin(lovelace), MultiAsset(immutableAssets))
            }

            // Parse datum if present
            val datumOption: Option[DatumOption] = {
                val dataHash = utxoJson.obj.get("data_hash")
                val inlineDatum = utxoJson.obj.get("inline_datum")

                (dataHash, inlineDatum) match {
                    case (_, Some(inlineDatumJson)) if !inlineDatumJson.isNull =>
                        Some(DatumOption.Inline(Data.fromCbor(hexToBytes(inlineDatumJson.str))))
                    case (Some(dataHashJson), _) if !dataHashJson.isNull =>
                        Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHashJson.str))))
                    case _ => None
                }
            }

            val txOutput = TransactionOutput(
              address = address,
              value = value,
              datumOption = datumOption,
              scriptRef = None
            )

            txInput -> txOutput
        }.toMap
    }
}
