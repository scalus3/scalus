package scalus.testing.conformance

import scalus.testing.conformance.ConformanceTestSchema.*
import scalus.utils.Hex

import scala.util.Try

/** Validator for reference scripts and inline datums
  *
  * Validates proper handling of:
  * - Reference scripts (PlutusV1, PlutusV2, PlutusV3, NativeScript)
  * - Inline datums
  * - Datum hashes
  * - Script references in UTXOs
  */
object ScriptDatumValidator {

  /** Validate reference script in a UTXO entry
    *
    * @param utxo
    *   UTXO entry with optional reference script
    * @return
    *   Validation errors, empty if valid
    */
  def validateReferenceScript(utxo: UtxoEntry): List[String] = {
    utxo.referenceScript match {
      case None => List.empty
      case Some(scriptRef) =>
        validateScriptRef(scriptRef)
    }
  }

  /** Validate script reference structure and content
    *
    * @param scriptRef
    *   Script reference to validate
    * @return
    *   Validation errors, empty if valid
    */
  def validateScriptRef(scriptRef: ScriptRef): List[String] = {
    val errors = List.newBuilder[String]

    // Validate script type
    scriptRef.scriptType match {
      case "PlutusV1" | "PlutusV2" | "PlutusV3" | "NativeScript" =>
      // Valid script types
      case other =>
        errors += s"Invalid script type: $other (expected PlutusV1, PlutusV2, PlutusV3, or NativeScript)"
    }

    // Validate script CBOR is well-formed
    if (!CborParser.isValidCbor(scriptRef.script)) {
      errors += "Script CBOR is malformed"
    }

    // Validate script size
    Try {
      val scriptBytes = Hex.hexToBytes(scriptRef.script)
      if (scriptBytes.length > 16384) { // 16KB max script size
        errors += s"Script size ${scriptBytes.length} exceeds maximum 16384 bytes"
      }
    }.failed.foreach { e =>
      errors += s"Failed to parse script: ${e.getMessage}"
    }

    errors.result()
  }

  /** Validate inline datum in a UTXO entry
    *
    * @param utxo
    *   UTXO entry with optional datum
    * @return
    *   Validation errors, empty if valid
    */
  def validateInlineDatum(utxo: UtxoEntry): List[String] = {
    utxo.datum match {
      case None => List.empty
      case Some(datumRef) =>
        validateDatumRef(datumRef)
    }
  }

  /** Validate datum reference structure
    *
    * @param datumRef
    *   Datum reference to validate
    * @return
    *   Validation errors, empty if valid
    */
  def validateDatumRef(datumRef: DatumRef): List[String] = {
    val errors = List.newBuilder[String]

    // Validate datum type
    datumRef.datumType match {
      case "inline" =>
        // Inline datums must have valid CBOR data
        if (!CborParser.isValidCbor(datumRef.datum)) {
          errors += "Inline datum CBOR is malformed"
        }
      case "hash" =>
        // Datum hashes must be 32 bytes (64 hex chars)
        if (datumRef.datum.length != 64) {
          errors += s"Datum hash must be 64 hex characters, got ${datumRef.datum.length}"
        }
      case other =>
        errors += s"Invalid datum type: $other (expected 'inline' or 'hash')"
    }

    errors.result()
  }

  /** Validate that reference inputs are used correctly
    *
    * Reference inputs provide scripts and datums without being spent
    *
    * @param transaction
    *   Transaction context with reference inputs
    * @param utxos
    *   Available UTXOs
    * @return
    *   Validation errors, empty if valid
    */
  def validateReferenceInputs(
      transaction: TransactionContext,
      utxos: List[UtxoEntry]
  ): List[String] = {
    val errors = List.newBuilder[String]

    transaction.referenceInputs.foreach { refInputs =>
      val utxoMap = utxos.map(u => (u.txHash, u.outputIndex) -> u).toMap

      refInputs.foreach { refInput =>
        utxoMap.get((refInput.txHash, refInput.outputIndex)) match {
          case None =>
            errors += s"Reference input ${refInput.txHash}#${refInput.outputIndex} not found in UTXO set"
          case Some(utxo) =>
            // Reference input must have either a reference script or inline datum
            if (utxo.referenceScript.isEmpty && utxo.datum.isEmpty) {
              errors += s"Reference input ${refInput.txHash}#${refInput.outputIndex} has no reference script or datum"
            }
        }
      }
    }

    errors.result()
  }

  /** Validate that supplemental datums are well-formed
    *
    * @param transaction
    *   Transaction context with supplemental datums
    * @return
    *   Validation errors, empty if valid
    */
  def validateSupplementalDatums(transaction: TransactionContext): List[String] = {
    val errors = List.newBuilder[String]

    transaction.supplementalDatums.foreach { datums =>
      datums.zipWithIndex.foreach { case (datumCbor, idx) =>
        if (!CborParser.isValidCbor(datumCbor)) {
          errors += s"Supplemental datum $idx is malformed CBOR"
        }
      }
    }

    errors.result()
  }

  /** Extract script hash from script reference
    *
    * @param scriptRef
    *   Script reference
    * @return
    *   Script hash (Blake2b-224)
    */
  def computeScriptHash(scriptRef: ScriptRef): Try[String] = Try {
    val scriptBytes = Hex.hexToBytes(scriptRef.script)

    // TODO: Implement proper script hashing
    // Script hash is Blake2b-224 of:
    // - For Plutus scripts: tag byte || script CBOR
    // - For native scripts: script CBOR

    "0" * 56 // 28 bytes = 56 hex chars
  }

  /** Extract datum hash from inline datum
    *
    * @param datumCbor
    *   CBOR-encoded datum
    * @return
    *   Datum hash (Blake2b-256)
    */
  def computeDatumHash(datumCbor: String): Try[String] = Try {
    val datumBytes = Hex.hexToBytes(datumCbor)

    // TODO: Implement proper datum hashing
    // Datum hash is Blake2b-256 of the CBOR-encoded datum

    "0" * 64 // 32 bytes = 64 hex chars
  }

  /** Validate script purpose and redeemer pairing
    *
    * Ensures each script execution has appropriate redeemer
    *
    * @param scriptHash
    *   Script being executed
    * @param purpose
    *   Script purpose (Spending, Minting, Rewarding, Certifying, Voting, Proposing)
    * @param redeemer
    *   Redeemer CBOR
    * @return
    *   Validation errors, empty if valid
    */
  def validateScriptPurpose(
      scriptHash: String,
      purpose: String,
      redeemer: String
  ): List[String] = {
    val errors = List.newBuilder[String]

    // Validate purpose
    purpose match {
      case "Spending" | "Minting" | "Rewarding" | "Certifying" | "Voting" | "Proposing" =>
      // Valid purposes
      case other =>
        errors += s"Invalid script purpose: $other"
    }

    // Validate redeemer
    if (!CborParser.isValidCbor(redeemer)) {
      errors += "Redeemer CBOR is malformed"
    }

    errors.result()
  }

  /** Validate that scripts are available for all script-locked inputs
    *
    * Scripts can come from:
    * - Transaction witnesses
    * - Reference inputs (reference scripts)
    *
    * @param scriptHashes
    *   Required script hashes
    * @param witnessScripts
    *   Scripts provided in witnesses
    * @param referenceScripts
    *   Scripts available via reference inputs
    * @return
    *   Validation errors, empty if valid
    */
  def validateScriptAvailability(
      scriptHashes: Set[String],
      witnessScripts: List[ScriptRef],
      referenceScripts: List[ScriptRef]
  ): List[String] = {
    val errors = List.newBuilder[String]

    // Compute hashes of available scripts
    val availableHashes = (witnessScripts ++ referenceScripts).flatMap { script =>
      computeScriptHash(script).toOption
    }.toSet

    // Check all required scripts are available
    val missingScripts = scriptHashes -- availableHashes
    if (missingScripts.nonEmpty) {
      errors += s"Missing required scripts: ${missingScripts.mkString(", ")}"
    }

    errors.result()
  }

  /** Validate that datums are available for all datum-required inputs
    *
    * Datums can come from:
    * - Inline datums in UTXOs
    * - Transaction witness datums
    * - Reference inputs (inline datums)
    *
    * @param datumHashes
    *   Required datum hashes
    * @param witnessDatums
    *   Datums provided in witnesses
    * @param inlineDatums
    *   Inline datums from UTXOs and reference inputs
    * @return
    *   Validation errors, empty if valid
    */
  def validateDatumAvailability(
      datumHashes: Set[String],
      witnessDatums: List[String],
      inlineDatums: List[String]
  ): List[String] = {
    val errors = List.newBuilder[String]

    // Compute hashes of available datums
    val availableHashes = (witnessDatums ++ inlineDatums).flatMap { datum =>
      computeDatumHash(datum).toOption
    }.toSet

    // Check all required datums are available
    val missingDatums = datumHashes -- availableHashes
    if (missingDatums.nonEmpty) {
      errors += s"Missing required datums: ${missingDatums.mkString(", ")}"
    }

    errors.result()
  }

  /** Validate complete script and datum availability for a transaction
    *
    * @param transaction
    *   Transaction context
    * @param utxos
    *   Available UTXOs
    * @return
    *   All validation errors
    */
  def validateScriptDatumContext(
      transaction: TransactionContext,
      utxos: List[UtxoEntry]
  ): List[String] = {
    val errors = List.newBuilder[String]

    // Validate reference inputs
    errors ++= validateReferenceInputs(transaction, utxos)

    // Validate supplemental datums
    errors ++= validateSupplementalDatums(transaction)

    // Validate reference scripts in UTXOs
    utxos.foreach { utxo =>
      errors ++= validateReferenceScript(utxo)
      errors ++= validateInlineDatum(utxo)
    }

    errors.result()
  }
}
