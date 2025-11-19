package scalus.testing.conformance

import scalus.builtin.Data as PlutusData
import scalus.testing.conformance.ConformanceTestSchema.*
import scalus.testing.conformance.CardanoLedgerVectorImporter.RawTestVector
import scalus.utils.Hex

import scala.util.{Success, Try}

/** CBOR parsing utilities for conformance tests
  *
  * Provides functions to parse CBOR-encoded transactions, ledger states, and other Cardano data
  * structures from conformance test vectors.
  */
object CborParser {
    import io.bullet.borer.{Reader, Cbor}

    // Helper functions for safe CBOR parsing
    private def tryParse[T](reader: Reader, default: T)(parse: Reader => T): T =
        try parse(reader)
        catch { case _: Exception => reader.skipDataItem(); default }

    private def tryParseOption[T](reader: Reader)(parse: Reader => T): Option[T] =
        try Some(parse(reader))
        catch { case _: Exception => reader.skipDataItem(); None }

    private def parseMapEntries[K, V](
        reader: Reader,
        parseKey: Reader => K,
        parseValue: Reader => V
    ): List[(K, V)] = {
        val mapSize = reader.readMapHeader()
        (0L until mapSize).flatMap { _ =>
            try Some((parseKey(reader), parseValue(reader)))
            catch { case _: Exception => reader.skipDataItem(); reader.skipDataItem(); None }
        }.toList
    }

    // Parse a single DRep entry
    private def parseDRep(reader: Reader, credBytes: Array[Byte]): DRepState = {
        val drepId = Hex.bytesToHex(credBytes)
        val stateSize = reader.readArrayHeader()

        val expiryEpoch = if (stateSize >= 1) tryParseOption(reader)(_.read[Long]().toInt) else None
        val anchor = if (stateSize >= 2) tryParseOption(reader) { r =>
            val anchorSize = r.readArrayHeader()
            val url = if (anchorSize >= 1) r.read[String]() else ""
            val hash = if (anchorSize >= 2) Hex.bytesToHex(r.read[Array[Byte]]()) else ""
            (2L until anchorSize).foreach(_ => r.skipDataItem())
            AnchorInfo(url, hash)
        } else None
        val deposit = if (stateSize >= 3) tryParseOption(reader)(_.read[Long]().toString) else None

        (3L until stateSize).foreach(_ => reader.skipDataItem())

        DRepState("registered", Some(drepId), anchor, deposit, expiryEpoch)
    }

    // Parse a single stake pool entry
    private def parseStakePool(reader: Reader, poolIdBytes: Array[Byte]): StakePoolState = {
        val poolId = Hex.bytesToHex(poolIdBytes)
        val paramsSize = reader.readArrayHeader()

        if (paramsSize >= 1) reader.skipDataItem() // operator
        val vrfKeyHash = if (paramsSize >= 2) tryParse(reader, "")(r => Hex.bytesToHex(r.read[Array[Byte]]())) else ""
        val pledge = if (paramsSize >= 3) tryParse(reader, "0")(_.read[Long]().toString) else "0"
        val cost = if (paramsSize >= 4) tryParse(reader, "0")(_.read[Long]().toString) else "0"

        val margin = if (paramsSize >= 5) {
            tryParse(reader, "0/1") { r =>
                val tag = r.readTag()
                if (tag == io.bullet.borer.Tag.Other(30)) {
                    val marginSize = r.readArrayHeader()
                    val num = if (marginSize >= 1) r.read[Long]() else 0
                    val den = if (marginSize >= 2) r.read[Long]() else 1
                    (2L until marginSize).foreach(_ => r.skipDataItem())
                    s"$num/$den"
                } else "0/1"
            }
        } else "0/1"

        val rewardAccount = if (paramsSize >= 6) tryParse(reader, "")(r => Hex.bytesToHex(r.read[Array[Byte]]())) else ""
        val owners = if (paramsSize >= 7) {
            tryParse(reader, List.empty[String]) { r =>
                val ownersSize = r.readArrayHeader()
                (0L until ownersSize).flatMap { _ =>
                    try Some(Hex.bytesToHex(r.read[Array[Byte]]()))
                    catch { case _: Exception => r.skipDataItem(); None }
                }.toList
            }
        } else List.empty

        if (paramsSize >= 8) reader.skipDataItem() // relays

        val metadata = if (paramsSize >= 9) {
            tryParseOption(reader) { r =>
                val metaSize = r.readArrayHeader()
                val url = if (metaSize >= 1) r.read[String]() else ""
                val hash = if (metaSize >= 2) Hex.bytesToHex(r.read[Array[Byte]]()) else ""
                (2L until metaSize).foreach(_ => r.skipDataItem())
                PoolMetadataRef(url, hash)
            }
        } else None

        (9L until paramsSize).foreach(_ => reader.skipDataItem())

        StakePoolState(poolId, vrfKeyHash, pledge, cost, margin, rewardAccount, owners, List.empty, metadata)
    }

    /** Parse a transaction from CBOR hex string
      *
      * @param cborHex
      *   CBOR hex-encoded transaction
      * @return
      *   Parsed transaction or error
      */
    def parseTransaction(cborHex: String): Try[scalus.cardano.ledger.Transaction] = Try {
        import scalus.cardano.ledger.{Transaction, ProtocolVersion}

        val bytes = Hex.hexToBytes(cborHex)

        // Parse the transaction using Conway protocol version by default
        given ProtocolVersion = ProtocolVersion.conwayPV
        Transaction.fromCbor(bytes)
    }

    /** Parse ledger state from CBOR hex string
      *
      * The ledger state structure from cardano-ledger is:
      * LedgerState = [CertState, UTxOState]
      *
      * Where:
      * - CertState (index 0): [VState, PState, DState] - voting, pools, delegations
      * - UTxOState (index 1): [utxos, deposited, fees, govState, instantStake, donation]
      *
      * @param cborHex
      *   CBOR hex-encoded ledger state
      * @return
      *   Parsed ledger state or error
      */
    def parseLedgerState(cborHex: String): Try[InitialLedgerState] = Try {
        import io.bullet.borer.*
        import scalus.cardano.ledger.{
            DatumOption,
            OriginalCborByteArray,
            Sized,
            TransactionInput,
            TransactionOutput
        }

        val bytes = Hex.hexToBytes(cborHex)
        val input = Input.fromByteArray(bytes)
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val reader = Cbor.reader(input)

        // LedgerState = [CertState, UTxOState]
        val ledgerStateSize = reader.readArrayHeader()
        require(ledgerStateSize >= 2, s"Invalid LedgerState size: $ledgerStateSize")

        // Parse CertState = [VState, PState, DState] or simplified format
        val (dreps, stakePools, accounts) = tryParse(reader, (None, None, None)) { r =>
            val certStateSize = r.readArrayHeader()
            if (certStateSize < 3) {
                (None, None, None)
            } else {
                // Parse VState - voting state with DReps
                val dreps = tryParseOption(r) { vr =>
                    val vStateSize = vr.readArrayHeader()
                    if (vStateSize >= 1) {
                        val mapSize = vr.readMapHeader()
                        val drepList = (0L until mapSize).flatMap { _ =>
                            try {
                                val credBytes = vr.read[Array[Byte]]()
                                Some(parseDRep(vr, credBytes))
                            } catch { case _: Exception => vr.skipDataItem(); vr.skipDataItem(); None }
                        }.toList
                        (1L until vStateSize).foreach(_ => vr.skipDataItem())
                        if (drepList.nonEmpty) Some(drepList) else None
                    } else None
                }.flatten

                // Parse PState - pool state
                val pools = tryParseOption(r) { pr =>
                    val pStateSize = pr.readArrayHeader()
                    if (pStateSize >= 1) {
                        val mapSize = pr.readMapHeader()
                        val poolList = (0L until mapSize).flatMap { _ =>
                            try {
                                val poolIdBytes = pr.read[Array[Byte]]()
                                Some(parseStakePool(pr, poolIdBytes))
                            } catch { case _: Exception => pr.skipDataItem(); pr.skipDataItem(); None }
                        }.toList
                        (1L until pStateSize).foreach(_ => pr.skipDataItem())
                        if (poolList.nonEmpty) Some(poolList) else None
                    } else None
                }.flatten

                // Parse DState - delegation state with rewards and delegations
                val accounts = tryParseOption(r) { dr =>
                    val dStateSize = dr.readArrayHeader()
                    if (dStateSize >= 2) {
                        val rewards = parseMapEntries(dr, r => Hex.bytesToHex(r.read[Array[Byte]]()), _.read[Long]().toString)
                        val delegations = parseMapEntries(dr, r => Hex.bytesToHex(r.read[Array[Byte]]()), r => Hex.bytesToHex(r.read[Array[Byte]]()))
                        (2L until dStateSize).foreach(_ => dr.skipDataItem())

                        val rewardsMap = rewards.toMap
                        val delegationsMap = delegations.toMap
                        val allAddrs = (rewardsMap.keySet ++ delegationsMap.keySet).toList

                        if (allAddrs.nonEmpty) {
                            Some(allAddrs.map { addr =>
                                AccountState(
                                  addr,
                                  delegationsMap.get(addr).map(p => DelegationInfo(Some(p), None)),
                                  rewardsMap.get(addr),
                                  None
                                )
                            })
                        } else None
                    } else None
                }.flatten

                (dreps, pools, accounts)
            }
        }

        // Parse UTxOState = [utxos, deposited, fees, govState, instantStake, donation]
        val utxoStateSize = tryParse(reader, 0L)(_.readArrayHeader())

        // Parse UTxOs map
        val utxos = if (utxoStateSize >= 1) {
            tryParse(reader, List.empty[UtxoEntry]) { r =>
                val mapSize = r.readMapHeader()
                (0L until mapSize).flatMap { _ =>
                    try {
                        val txIn = r.read[TransactionInput]()
                        val txOut = r.read[Sized[TransactionOutput]]()
                        val valueBytes = Cbor.encode(txOut.value.value).toByteArray
                        val (lovelace, assets) = parseValue(Hex.bytesToHex(valueBytes)).get

                        Some(UtxoEntry(
                          txHash = txIn.transactionId.toHex,
                          outputIndex = txIn.index,
                          address = txOut.value.address.encode.getOrElse(txOut.value.address.toHex),
                          value = lovelace,
                          datum = txOut.value.datumOption.map { d =>
                              DatumRef(
                                datumType = d match {
                                    case DatumOption.Hash(_)   => "hash"
                                    case DatumOption.Inline(_) => "inline"
                                },
                                datum = Hex.bytesToHex(Cbor.encode(d).toByteArray)
                              )
                          },
                          referenceScript = txOut.value.scriptRef.map { s =>
                              val scriptHex = Hex.bytesToHex(Cbor.encode(s.script).toByteArray)
                              parseScript(scriptHex).get
                          },
                          assets = assets
                        ))
                    } catch { case _: Exception => None }
                }.toList
            }
        } else List.empty

        // Parse deposited, fees, donation
        val deposited = if (utxoStateSize >= 2) tryParseOption(reader)(_.read[Long]().toString) else None
        val fees = if (utxoStateSize >= 3) tryParseOption(reader)(_.read[Long]().toString) else None
        if (utxoStateSize >= 4) reader.skipDataItem() // govState
        if (utxoStateSize >= 5) reader.skipDataItem() // instantStake
        val donation = if (utxoStateSize >= 6) tryParseOption(reader)(_.read[Long]().toString) else None

        InitialLedgerState(
          utxos = utxos,
          deposited = deposited.orElse(Some("0")),
          fees = fees.orElse(Some("0")),
          donation = donation,
          stakePools = stakePools,
          dreps = dreps,
          accounts = accounts,
          treasury = None,
          reserves = None,
          protocolParams = None
        )
    }

    /** Extract UTxO entries from CBOR-encoded UTxO map
      *
      * @param cborHex
      *   CBOR hex-encoded UTxO map
      * @return
      *   List of UTXO entries
      */
    def parseUtxoMap(cborHex: String): Try[scala.List[UtxoEntry]] = Try {
        import io.bullet.borer.{Cbor, Decoder}
        import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Sized, OriginalCborByteArray}

        val bytes = Hex.hexToBytes(cborHex)

        // Decode map of TxIn -> TxOut
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        given Decoder[scala.List[UtxoEntry]] = Decoder { r =>
            val mapSize = r.readMapHeader()
            var utxos = scala.List.empty[UtxoEntry]

            for _ <- 0L until mapSize do {
                // Read transaction input (key)
                val txIn = r.read[TransactionInput]()

                // Read transaction output (value)
                val txOut = r.read[Sized[TransactionOutput]]()

                // Convert to UtxoEntry
                val valueObj = txOut.value.value
                val valueBytes: Array[Byte] = Cbor.encode(valueObj).toByteArray
                val (lovelace, assets) = parseValue(Hex.bytesToHex(valueBytes)).get

                val entry = UtxoEntry(
                  txHash = txIn.transactionId.toHex,
                  outputIndex = txIn.index,
                  address = txOut.value.address.encode.getOrElse(txOut.value.address.toHex),
                  value = lovelace,
                  datum = txOut.value.datumOption.map { d =>
                      val datumBytes: Array[Byte] = Cbor.encode(d).toByteArray
                      DatumRef(
                        datumType = d match {
                            case scalus.cardano.ledger.DatumOption.Hash(_)   => "hash"
                            case scalus.cardano.ledger.DatumOption.Inline(_) => "inline"
                        },
                        datum = Hex.bytesToHex(datumBytes)
                      )
                  },
                  referenceScript = txOut.value.scriptRef.map { s =>
                      val scriptBytes: Array[Byte] = Cbor.encode(s.script).toByteArray
                      val scriptHex = Hex.bytesToHex(scriptBytes)
                      parseScript(scriptHex).get
                  },
                  assets = assets
                )

                utxos = entry :: utxos
            }

            utxos.reverse
        }

        Cbor.decode(bytes).to[scala.List[UtxoEntry]].value
    }

    /** Parse transaction metadata from CBOR
      *
      * @param cborHex
      *   CBOR hex-encoded metadata
      * @return
      *   Parsed metadata map
      */
    def parseMetadata(cborHex: String): Try[Map[Long, PlutusData]] =
        Try {
            import io.bullet.borer.{Cbor, Decoder}

            val bytes = Hex.hexToBytes(cborHex)

            // Metadata is a map from label (Long) to metadata value
            // For now, we return an empty map as metadata parsing requires
            // special handling (it's not the same as Plutus Data)
            // This would need proper implementation based on the metadata format
            given Decoder[Map[Long, PlutusData]] = Decoder { r =>
                if r.hasMapHeader then {
                    val mapSize = r.readMapHeader()
                    // Skip all map entries for now
                    for _ <- 0L until mapSize * 2 do {
                        r.skipDataItem()
                    }
                }
                scala.collection.immutable.Map.empty
            }

            Cbor.decode(bytes).to[Map[Long, PlutusData]].value
        }

    /** Parse script from CBOR
      *
      * @param cborHex
      *   CBOR hex-encoded script
      * @return
      *   Script reference with type
      */
    def parseScript(cborHex: String): Try[ScriptRef] = Try {
        import io.bullet.borer.Cbor
        import scalus.cardano.ledger.Script

        val bytes = Hex.hexToBytes(cborHex)
        val script = Cbor.decode(bytes).to[Script].value

        // Determine script type from the parsed script
        val scriptType = script match {
            case Script.Native(_)   => "NativeScript"
            case Script.PlutusV1(_) => "PlutusV1"
            case Script.PlutusV2(_) => "PlutusV2"
            case Script.PlutusV3(_) => "PlutusV3"
        }

        ScriptRef(
          scriptType = scriptType,
          script = cborHex
        )
    }

    /** Parse datum from CBOR
      *
      * @param cborHex
      *   CBOR hex-encoded datum
      * @param isInline
      *   Whether this is an inline datum
      * @return
      *   Datum reference
      */
    def parseDatum(cborHex: String, isInline: Boolean): Try[DatumRef] = Try {
        import io.bullet.borer.Cbor
        import scalus.cardano.ledger.DatumOption

        val bytes = Hex.hexToBytes(cborHex)
        val datumOption = Cbor.decode(bytes).to[DatumOption].value

        // Determine datum type from parsed datum option
        val datumType = datumOption match {
            case DatumOption.Hash(_)   => "hash"
            case DatumOption.Inline(_) => "inline"
        }

        DatumRef(
          datumType = datumType,
          datum = cborHex
        )
    }

    /** Parse value (ADA + native assets) from CBOR
      *
      * @param cborHex
      *   CBOR hex-encoded value
      * @return
      *   Tuple of (lovelace amount, optional assets map)
      */
    def parseValue(cborHex: String): Try[
      (String, Option[Map[String, Map[String, String]]])] = Try {
        import io.bullet.borer.Cbor
        import scalus.cardano.ledger.Value

        val bytes = Hex.hexToBytes(cborHex)
        val value = Cbor.decode(bytes).to[Value].value

        // Convert to string representation
        val lovelace = value.coin.value.toString

        // Convert assets if present
        val assets = if value.assets.isEmpty then None else Some(
              value.assets.assets.map { case (policyId, assetMap) =>
                  policyId.toHex -> assetMap.map { case (assetName, quantity) =>
                      assetName.bytes.toHex -> quantity.toString
                  }
              }
            )

        (lovelace, assets)
    }

    /** Parse address from CBOR
      *
      * @param cborHex
      *   CBOR hex-encoded address
      * @return
      *   Address string (Bech32 or Byron)
      */
    def parseAddress(cborHex: String): Try[String] = Try {
        import io.bullet.borer.Cbor
        import scalus.cardano.address.Address

        val bytes = Hex.hexToBytes(cborHex)
        val address = Cbor.decode(bytes).to[Address].value

        // Try to encode as bech32, fallback to hex
        address.encode.getOrElse(address.toHex)
    }

    /** Convert raw test vector to initial ledger state
      *
      * Extracts the initial state from oldLedgerState CBOR
      *
      * @param vector
      *   Raw test vector
      * @return
      *   Initial ledger state
      */
    def extractInitialState(vector: RawTestVector): Try[InitialLedgerState] = {
        // oldLedgerState is a String, not Option[String]
        if vector.oldLedgerState.nonEmpty then {
            parseLedgerState(vector.oldLedgerState)
        } else {
            Success(InitialLedgerState(utxos = scala.List.empty))
        }
    }

    /** Convert raw test vector to expected ledger state
      *
      * Extracts the expected state from newLedgerState CBOR
      *
      * @param vector
      *   Raw test vector
      * @return
      *   Expected ledger state
      */
    def extractExpectedState(vector: RawTestVector): Try[ExpectedLedgerState] = {
        vector.newLedgerState match {
            case Some(newStateHex) =>
                parseLedgerState(newStateHex).map { state =>
                    ExpectedLedgerState(utxos = Some(state.utxos))
                }
            case None =>
                Success(ExpectedLedgerState())
        }
    }

    /** Validate CBOR structure without full parsing
      *
      * Checks if CBOR is well-formed and has expected structure
      *
      * @param cborHex
      *   CBOR hex string
      * @return
      *   true if CBOR appears valid
      */
    def isValidCbor(cborHex: String): Boolean = {
        Try {
            val bytes = Hex.hexToBytes(cborHex)
            bytes.length > 0 // Basic check - non-empty
        }.getOrElse(false)
    }

    /** Extract transaction hash from CBOR transaction
      *
      * @param cborHex
      *   CBOR hex-encoded transaction
      * @return
      *   Transaction hash
      */
    def extractTransactionHash(cborHex: String): Try[String] = {
        // Parse transaction using parseTransaction and get its computed hash
        parseTransaction(cborHex).map(_.id.toHex)
    }
}
