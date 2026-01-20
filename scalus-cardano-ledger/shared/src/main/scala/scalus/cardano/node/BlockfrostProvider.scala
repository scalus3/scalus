package scalus.cardano.node

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.utils.Hex.hexToBytes
import sttp.client4.*
import sttp.model.StatusCode

import scala.collection.immutable.SortedMap
import scala.concurrent.{ExecutionContext, Future}

class BlockfrostProvider(
    apiKey: String,
    baseUrl: String = BlockfrostProvider.previewUrl,
    maxConcurrentRequests: Int = 5
)(using
    backend: Backend[Future],
    ec: ExecutionContext
) extends Provider {

    private val limiter = new ConcurrencyLimiter(maxConcurrentRequests)

    private def headers = Map("project_id" -> apiKey)

    /** Wrap an HTTP request with rate limiting */
    private def rateLimited[T](request: => Future[T]): Future[T] = limiter(request)

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams] = {
        val url = s"$baseUrl/epochs/latest/parameters"
        rateLimited(
          basicRequest
              .get(uri"$url")
              .headers(headers)
              .send(backend)
        ).map { response =>
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
        rateLimited(
          basicRequest
              .get(uri"$url")
              .headers(headers)
              .send(backend)
        ).map { response =>
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
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]] = {
        if minRequiredTotalAmount.exists(_ <= Coin(0)) then
            return Future.successful(Right(Map.empty))

        // Build source using And combinator when transactionId is provided
        val source: UtxoSource = transactionId match
            case Some(txId) => UtxoSource.FromAddress(address) && UtxoSource.FromTransaction(txId)
            case None       => UtxoSource.FromAddress(address)

        // Build the query
        var query: UtxoQuery = UtxoQuery(source)

        // Add minRequiredTotalAmount
        query = minRequiredTotalAmount.fold(query)(amt => query.minTotal(amt))

        // Add datum filter
        query = datum.fold(query)(d => query && UtxoFilter.HasDatum(d))

        // Add minAmount filter
        query = minAmount.fold(query)(amt => query && UtxoFilter.MinLovelace(amt))

        findUtxos(query)
    }

    override def submit(
        tx: Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        val url = s"$baseUrl/tx/submit"
        val txCbor = tx.toCbor

        rateLimited(
          basicRequest
              .post(uri"$url")
              .headers(headers ++ Map("Content-Type" -> "application/cbor"))
              .body(txCbor)
              .send(backend)
        ).map { response =>
            response.code.code match {
                case c if c >= 200 && c < 300 => Right(tx.id)
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
                    Left(SubmitError.fromHttpResponse(c, message))
            }
        }.recover { case exception =>
            Left(SubmitError.NetworkError(s"Blockfrost submit exception", Some(exception)))
        }
    }

    override def findUtxo(
        input: TransactionInput
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxo]] = {
        fetchUtxoFromInput(input)
    }

    override def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]] = {
        findUtxos(UtxoQuery(UtxoSource.FromInputs(inputs))).map { result =>
            result.flatMap { foundUtxos =>
                if foundUtxos.size == inputs.size then Right(foundUtxos)
                else Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(inputs)))
            }
        }
    }

    override def findUtxos(query: UtxoQuery)(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]] = {
        // Evaluate source to get candidate UTxOs
        def evalSource(source: UtxoSource): Future[Either[UtxoQueryError, Utxos]] = source match
            case UtxoSource.FromAddress(addr) =>
                fetchUtxosFromAddress(addr)
            case UtxoSource.FromAsset(policyId, assetName) =>
                // Not implemented - would require /assets/{asset}/addresses endpoint
                Future.successful(
                  Left(UtxoQueryError.NotSupported(query, "FromAsset not yet implemented"))
                )
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

        // Evaluate filter
        def evalFilter(filter: UtxoFilter, utxo: (TransactionInput, TransactionOutput)): Boolean = {
            val (_, output) = utxo
            filter match
                case UtxoFilter.HasAsset(policyId, assetName) =>
                    output.value.assets.assets
                        .get(policyId)
                        .exists(_.contains(assetName))
                case UtxoFilter.HasDatum(datum) =>
                    (datum, output.datumOption) match
                        case (d1, Some(d2)) => d1.contentEquals(d2)
                        case _              => false
                case UtxoFilter.HasDatumHash(hash) =>
                    output.datumOption match
                        case Some(d) => d.dataHash == hash
                        case None    => false
                case UtxoFilter.MinLovelace(amount) =>
                    output.value.coin >= amount
                case UtxoFilter.AtInputs(inputs) =>
                    inputs.contains(utxo._1)
                case UtxoFilter.And(left, right) =>
                    evalFilter(left, utxo) && evalFilter(right, utxo)
                case UtxoFilter.Or(left, right) =>
                    evalFilter(left, utxo) || evalFilter(right, utxo)
                case UtxoFilter.Not(f) =>
                    !evalFilter(f, utxo)
        }

        // Apply pagination and minRequiredTotalAmount to a result set
        def applyPagination(
            candidates: Utxos,
            limit: Option[Int],
            offset: Option[Int],
            minRequiredTotalAmount: Option[Coin]
        ): Utxos = {
            val offsetValue = offset.getOrElse(0)
            val paginated = candidates.drop(offsetValue)

            minRequiredTotalAmount match
                case Some(minTotal) if minTotal.value > 0 =>
                    val (collected, _) = paginated.foldLeft(
                      (Map.empty[TransactionInput, TransactionOutput], Coin(0))
                    ) { case (acc @ (accUtxos, accAmount), (input, output)) =>
                        if accAmount >= minTotal then acc
                        else
                            val newAmount = Coin(accAmount.value + output.value.coin.value)
                            (accUtxos + (input -> output), newAmount)
                    }
                    limit.fold(collected)(n => collected.take(n))
                case _ =>
                    limit.fold(paginated.toMap)(n => paginated.take(n).toMap)
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
                        case Some(f) => candidates.filter(evalFilter(f, _))
                        case None    => candidates
                    Right(applyPagination(filtered, q.limit, q.offset, q.minRequiredTotalAmount))
            }
        }

        // Evaluate query recursively
        def evalQuery(q: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] = q match
            case simple: UtxoQuery.Simple                           => evalSimple(simple)
            case UtxoQuery.Or(left, right, limit, offset, minTotal) =>
                // Propagate limit and minTotal to branches for early termination
                // Methods take minimum, so safe to always propagate
                def propagate(q: UtxoQuery): UtxoQuery =
                    val withLimit = limit.fold(q)(q.limit)
                    minTotal.fold(withLimit)(withLimit.minTotal)
                // Execute both queries in parallel
                val leftFuture = evalQuery(propagate(left))
                val rightFuture = evalQuery(propagate(right))
                leftFuture.zip(rightFuture).map { case (leftResult, rightResult) =>
                    (leftResult, rightResult) match
                        case (Right(l), Right(r)) =>
                            // Apply again to combined result
                            Right(applyPagination(l ++ r, limit, offset, minTotal))
                        case (Right(l), _) =>
                            Right(applyPagination(l, limit, offset, minTotal))
                        case (_, Right(r)) =>
                            Right(applyPagination(r, limit, offset, minTotal))
                        case (Left(e), _) => Left(e)
                }

        evalQuery(query)
    }

    /** Fetch UTxOs from an address using Blockfrost API */
    private def fetchUtxosFromAddress(address: Address)(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]] = {
        import scala.util.{Success, Failure}
        val bech32 = address match {
            case sh @ ShelleyAddress(_, _, _) =>
                sh.toBech32 match
                    case Success(b) => b
                    case Failure(_) =>
                        return Future.successful(
                          Left(
                            UtxoQueryError.NotSupported(
                              UtxoQuery(UtxoSource.FromAddress(address)),
                              "Invalid address"
                            )
                          )
                        )
            case _ =>
                return Future.successful(
                  Left(
                    UtxoQueryError.NotSupported(
                      UtxoQuery(UtxoSource.FromAddress(address)),
                      "Shelley addresses only"
                    )
                  )
                )
        }
        val url = s"$baseUrl/addresses/$bech32/utxos"
        rateLimited(
          basicRequest
              .get(uri"$url")
              .headers(headers)
              .send(backend)
        ).map { response =>
            if response.code.isSuccess then {
                response.body match {
                    case Right(body) => Right(BlockfrostProvider.parseUtxos(body))
                    case Left(error) =>
                        Left(UtxoQueryError.NetworkError(s"Failed to fetch UTxOs: $error"))
                }
            } else if response.code == StatusCode.NotFound then {
                Right(Map.empty) // No UTxOs at this address
            } else {
                Left(UtxoQueryError.NetworkError(s"Failed to fetch UTxOs: ${response.body}"))
            }
        }.recover { case e: Throwable =>
            Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
        }
    }

    /** Fetch UTxOs from an address filtered by asset using Blockfrost API. Uses the optimized
      * /addresses/{addr}/utxos/{asset} endpoint.
      */
    private def fetchUtxosFromAddressWithAsset(
        address: Address,
        policyId: PolicyId,
        assetName: AssetName
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]] = {
        import scala.util.{Success, Failure}
        val bech32 = address match {
            case sh @ ShelleyAddress(_, _, _) =>
                sh.toBech32 match
                    case Success(b) => b
                    case Failure(_) =>
                        return Future.successful(
                          Left(
                            UtxoQueryError.NotSupported(
                              UtxoQuery(UtxoSource.FromAddress(address)),
                              "Invalid address"
                            )
                          )
                        )
            case _ =>
                return Future.successful(
                  Left(
                    UtxoQueryError.NotSupported(
                      UtxoQuery(UtxoSource.FromAddress(address)),
                      "Shelley addresses only"
                    )
                  )
                )
        }
        // Asset is policyId + assetName hex concatenated
        val asset = policyId.toHex + assetName.bytes.toHex
        val url = s"$baseUrl/addresses/$bech32/utxos/$asset"
        rateLimited(
          basicRequest
              .get(uri"$url")
              .headers(headers)
              .send(backend)
        ).map { response =>
            if response.code.isSuccess then {
                response.body match {
                    case Right(body) => Right(BlockfrostProvider.parseUtxos(body))
                    case Left(error) =>
                        Left(UtxoQueryError.NetworkError(s"Failed to fetch UTxOs: $error"))
                }
            } else if response.code == StatusCode.NotFound then {
                Right(Map.empty) // No UTxOs with this asset at this address
            } else {
                Left(UtxoQueryError.NetworkError(s"Failed to fetch UTxOs: ${response.body}"))
            }
        }.recover { case e: Throwable =>
            Left(UtxoQueryError.NetworkError(e.getMessage, Some(e)))
        }
    }

    /** Fetch UTxOs from a transaction using Blockfrost API */
    private def fetchUtxosFromTransaction(txId: TransactionHash)(using
        ExecutionContext
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
                            val address = Address.fromBech32(outputJson("address").str)

                            // Parse value
                            val amountArray = outputJson("amount").arr
                            var lovelace = 0L
                            val multiAssetBuilder = scala.collection.mutable
                                .Map[ScriptHash, scala.collection.mutable.Map[
                                  AssetName,
                                  Long
                                ]]()

                            amountArray.foreach { item =>
                                val unit = item("unit").str
                                // Handle both string (Blockfrost) and number (Yaci) formats
                                val quantity = item("quantity") match
                                    case s: ujson.Str => s.str.toLong
                                    case n: ujson.Num => n.num.toLong
                                    case other        => other.toString.toLong

                                if unit == "lovelace" then {
                                    lovelace = quantity
                                } else {
                                    val policyId = ScriptHash.fromHex(unit.take(56))
                                    val assetNameHex = unit.drop(56)
                                    val assetName = AssetName(ByteString.fromHex(assetNameHex))

                                    multiAssetBuilder
                                        .getOrElseUpdate(
                                          policyId,
                                          scala.collection.mutable.Map()
                                        )
                                        .update(assetName, quantity)
                                }
                            }

                            val value = if multiAssetBuilder.isEmpty then {
                                Value.lovelace(lovelace)
                            } else {
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

                            val txOutput = TransactionOutput(
                              address = address,
                              value = value,
                              datumOption = datumOption,
                              scriptRef = None
                            )

                            input -> txOutput
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
    private def fetchUtxoFromInput(input: TransactionInput)(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxo]] = {
        fetchUtxosFromTransaction(input.transactionId).map {
            case Left(e) => Left(e)
            case Right(utxos) =>
                utxos.get(input) match
                    case Some(output) => Right(Utxo(input, output))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(Set(input))))
        }
    }

    override def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash],
        datum: Option[DatumOption],
        minAmount: Option[Coin]
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxo]] = {
        findUtxos(address, transactionId, datum, minAmount, None).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match {
                    case Some((input, output)) => Right(Utxo(input, output))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromAddress(address)))
                }
            }
        }
    }

    /** Fetch a script by its hash from Blockfrost
      * @param scriptHash
      *   the hex-encoded script hash
      * @return
      *   the Script or an error
      */
    def fetchScript(scriptHash: String): Future[Either[RuntimeException, Script]] = {
        // First, get the script type
        val typeUrl = s"$baseUrl/scripts/$scriptHash"
        rateLimited(
          basicRequest
              .get(uri"$typeUrl")
              .headers(headers)
              .send(backend)
        ).flatMap { response =>
            if response.code.isSuccess then {
                response.body match {
                    case Right(body) =>
                        val json = ujson.read(body, trace = false)
                        val scriptType = json("type").str
                        if scriptType == "timelock" then {
                            // Fetch JSON for timelock scripts
                            val jsonUrl = s"$baseUrl/scripts/$scriptHash/json"
                            rateLimited(
                              basicRequest
                                  .get(uri"$jsonUrl")
                                  .headers(headers)
                                  .send(backend)
                            ).map { jsonResponse =>
                                if jsonResponse.code.isSuccess then {
                                    jsonResponse.body match {
                                        case Right(jsonBody) =>
                                            import Timelock.blockfrostReadWriter
                                            val jsonData =
                                                ujson.read(jsonBody, trace = false)
                                            val timelock =
                                                upickle.default.read[Timelock](
                                                  jsonData("json")
                                                )
                                            Right(Script.Native(timelock))
                                        case Left(error) =>
                                            Left(
                                              new RuntimeException(
                                                s"Failed to fetch script JSON: $error"
                                              )
                                            )
                                    }
                                } else {
                                    Left(
                                      new RuntimeException(
                                        s"Failed to fetch script JSON: ${jsonResponse.body}"
                                      )
                                    )
                                }
                            }
                        } else {
                            // Fetch CBOR for Plutus scripts
                            val cborUrl = s"$baseUrl/scripts/$scriptHash/cbor"
                            rateLimited(
                              basicRequest
                                  .get(uri"$cborUrl")
                                  .headers(headers)
                                  .send(backend)
                            ).map { cborResponse =>
                                if cborResponse.code.isSuccess then {
                                    cborResponse.body match {
                                        case Right(cborBody) =>
                                            val cborJson =
                                                ujson.read(cborBody, trace = false)
                                            val cborHex = cborJson("cbor").str
                                            val scriptBytes =
                                                ByteString.fromHex(cborHex)
                                            val script = scriptType match {
                                                case "plutusV1" =>
                                                    Script.PlutusV1(scriptBytes)
                                                case "plutusV2" =>
                                                    Script.PlutusV2(scriptBytes)
                                                case "plutusV3" =>
                                                    Script.PlutusV3(scriptBytes)
                                                case other =>
                                                    throw new RuntimeException(
                                                      s"Unknown Plutus script type: $other"
                                                    )
                                            }
                                            Right(script)
                                        case Left(error) =>
                                            Left(
                                              new RuntimeException(
                                                s"Failed to fetch script CBOR: $error"
                                              )
                                            )
                                    }
                                } else {
                                    Left(
                                      new RuntimeException(
                                        s"Failed to fetch script CBOR: ${cborResponse.body}"
                                      )
                                    )
                                }
                            }
                        }
                    case Left(error) =>
                        Future.successful(
                          Left(new RuntimeException(s"Failed to fetch script type: $error"))
                        )
                }
            } else if response.code == StatusCode.NotFound then {
                Future.successful(
                  Left(new RuntimeException(s"Script $scriptHash not found"))
                )
            } else {
                Future.successful(
                  Left(new RuntimeException(s"Failed to fetch script: ${response.body}"))
                )
            }
        }.recover { case e: Throwable =>
            Left(new RuntimeException(s"Failed to fetch script: ${e.getMessage}", e))
        }
    }

    /** Check if a transaction has been confirmed on-chain */
    def isTransactionConfirmed(txHash: String): Future[Either[RuntimeException, Boolean]] = {
        val url = s"$baseUrl/txs/$txHash"
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

    def localYaci(maxConcurrentRequests: Int = 5)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ) =
        BlockfrostProvider("", localUrl, maxConcurrentRequests)

    /** Create a Blockfrost client for mainnet */
    def mainnet(apiKey: String, maxConcurrentRequests: Int = 5)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ) =
        new BlockfrostProvider(apiKey, mainnetUrl, maxConcurrentRequests)

    /** Create a Blockfrost client for preview testnet */
    def preview(apiKey: String, maxConcurrentRequests: Int = 5)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ) =
        new BlockfrostProvider(apiKey, previewUrl, maxConcurrentRequests)

    /** Create a Blockfrost client for preprod testnet */
    def preprod(apiKey: String, maxConcurrentRequests: Int = 5)(using
        backend: Backend[Future],
        ec: ExecutionContext
    ) =
        new BlockfrostProvider(apiKey, preprodUrl, maxConcurrentRequests)

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
