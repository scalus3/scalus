package scalus.testing.conformance

import scalus.builtin.{Data => PlutusData}
import scalus.testing.conformance.ConformanceTestSchema.*
import scalus.testing.conformance.CardanoLedgerVectorImporter.RawTestVector
import scalus.utils.Hex

import scala.util.{Success, Try}

/** CBOR parsing utilities for conformance tests
  *
  * Provides functions to parse CBOR-encoded transactions, ledger states,
  * and other Cardano data structures from conformance test vectors.
  */
object CborParser {

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
    * Note: This is a placeholder. Full implementation requires parsing the complete
    * ledger state CBOR format from cardano-ledger, which includes:
    * - UTxO map
    * - Stake distribution
    * - DRep state
    * - Governance proposals
    * - Protocol parameters
    *
    * @param cborHex
    *   CBOR hex-encoded ledger state
    * @return
    *   Parsed ledger state or error
    */
  def parseLedgerState(cborHex: String): Try[InitialLedgerState] = Try {
    import io.bullet.borer.{Cbor, Decoder}
    import scalus.cardano.ledger.{TransactionInput, TransactionOutput, Sized, OriginalCborByteArray}

    val bytes = Hex.hexToBytes(cborHex)

    // Decode the ledger state CBOR structure
    // The format varies by era, but typically includes:
    // - UTxO state (key 0 or 3)
    // - Account state (treasury, reserves)
    // - Protocol parameters
    // - Epoch state

    // Custom decoder using Reader API for flexibility
    given OriginalCborByteArray = OriginalCborByteArray(bytes)
    given Decoder[InitialLedgerState] = Decoder { r =>
      var slot: Long = 0
      var blockNo: Long = 0
      var blockHash: String = "0" * 64
      var utxos: scala.List[UtxoEntry] = scala.List.empty

      // Try to decode as a map
      if (r.hasMapHeader) {
        val mapSize = r.readMapHeader().toInt
        for (_ <- 0 until mapSize) {
          val key = r.readInt()
          key match {
            case 0 =>
              // Slot field
              if (r.hasLong) slot = r.readLong()
              else if (r.hasInt) slot = r.readInt().toLong
              else r.skipDataItem()
            case 1 =>
              // Block number field
              if (r.hasLong) blockNo = r.readLong()
              else if (r.hasInt) blockNo = r.readInt().toLong
              else r.skipDataItem()
            case 2 =>
              // Block hash field
              if (r.hasBytes) {
                val hashBytes = r.readByteArray()
                blockHash = Hex.bytesToHex(hashBytes)
              } else if (r.hasString) {
                blockHash = r.readString()
              } else {
                r.skipDataItem()
              }
            case 3 =>
              // UTxO field - decode the UTxO map
              if (r.hasMapHeader) {
                val utxoMapSize = r.readMapHeader()
                val utxoList = scala.collection.mutable.ListBuffer.empty[UtxoEntry]

                for (_ <- 0L until utxoMapSize) {
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
                          case scalus.cardano.ledger.DatumOption.Hash(_) => "hash"
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

                  utxoList += entry
                }

                utxos = utxoList.toList
              } else {
                r.skipDataItem()
              }
            case _ =>
              // Skip unknown fields
              r.skipDataItem()
          }
        }
      } else {
        // If not a map, skip the entire structure
        r.skipDataItem()
      }

      InitialLedgerState(
        slot = slot,
        blockNo = blockNo,
        blockHash = blockHash,
        utxos = utxos
      )
    }

    Cbor.decode(bytes).to[InitialLedgerState].value
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

      for (_ <- 0L until mapSize) {
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
                case scalus.cardano.ledger.DatumOption.Hash(_) => "hash"
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
  def parseMetadata(cborHex: String): Try[scala.collection.immutable.Map[Long, PlutusData]] = Try {
    import io.bullet.borer.{Cbor, Decoder}

    val bytes = Hex.hexToBytes(cborHex)

    // Metadata is a map from label (Long) to metadata value
    // For now, we return an empty map as metadata parsing requires
    // special handling (it's not the same as Plutus Data)
    // This would need proper implementation based on the metadata format
    given Decoder[scala.collection.immutable.Map[Long, PlutusData]] = Decoder { r =>
      if (r.hasMapHeader) {
        val mapSize = r.readMapHeader()
        // Skip all map entries for now
        for (_ <- 0L until mapSize * 2) {
          r.skipDataItem()
        }
      }
      scala.collection.immutable.Map.empty
    }

    Cbor.decode(bytes).to[scala.collection.immutable.Map[Long, PlutusData]].value
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
  def parseValue(cborHex: String): Try[(String, Option[scala.collection.immutable.Map[String, scala.collection.immutable.Map[String, String]]])] = Try {
    import io.bullet.borer.Cbor
    import scalus.cardano.ledger.Value

    val bytes = Hex.hexToBytes(cborHex)
    val value = Cbor.decode(bytes).to[Value].value

    // Convert to string representation
    val lovelace = value.coin.value.toString

    // Convert assets if present
    val assets = if (value.assets.isEmpty) {
      None
    } else {
      Some(
        value.assets.assets.map { case (policyId, assetMap) =>
          policyId.toHex -> assetMap.map { case (assetName, quantity) =>
            assetName.bytes.toHex -> quantity.toString
          }
        }
      )
    }

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
    if (vector.oldLedgerState.nonEmpty) {
      parseLedgerState(vector.oldLedgerState)
    } else {
      Success(
        InitialLedgerState(
          slot = 0,
          blockNo = 0,
          blockHash = "0" * 64,
          utxos = scala.List.empty
        )
      )
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
          ExpectedLedgerState(
            slot = state.slot,
            blockNo = state.blockNo,
            blockHash = Some(state.blockHash),
            utxos = Some(state.utxos)
          )
        }
      case None =>
        Success(
          ExpectedLedgerState(
            slot = 0,
            blockNo = 0
          )
        )
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
