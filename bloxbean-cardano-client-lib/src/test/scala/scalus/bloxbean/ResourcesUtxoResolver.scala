package scalus.bloxbean

import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.plutus.spec.{PlutusScript, PlutusV1Script, PlutusV2Script, PlutusV3Script}
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import com.fasterxml.jackson.databind.{ObjectMapper, SerializationFeature}
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.*
import scalus.utils.{Hex, Utils}
import scalus.utils.Hex.hexToBytes

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Resolves UTXOs for transactions using scalus.cardano.ledger domain classes */
private[scalus] class ResourcesUtxoResolver {
    private val mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT)

    /** Resolve UTXOs for a transaction with no additional input UTXOs */
    def resolveUtxos(transaction: Transaction): Map[TransactionInput, TransactionOutput] =
        resolveUtxos(transaction, Map.empty)

    /** Resolve UTXOs for a transaction with additional input UTXOs
      *
      * @param transaction
      *   The transaction to resolve UTXOs for
      * @param inputUtxos
      *   Additional UTXOs to include in resolution
      * @return
      *   Map from transaction inputs to their corresponding outputs
      */
    def resolveUtxos(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput]
    ): Map[TransactionInput, TransactionOutput] = {
        def decodeToSingleCbor(script: PlutusScript) =
            // unwrap the outer CBOR encoding
            ByteString.unsafeFromArray(
              Cbor.decode(script.getCborHex.hexToBytes).to[Array[Byte]].value
            )
        // Initialize UTXOs with provided input UTXOs
        val utxos = mutable.HashMap[TransactionInput, TransactionOutput]()
        val allInputs =
            transaction.body.value.inputs.toSet.view ++ transaction.body.value.referenceInputs.toSet.view
        for input <- allInputs do
            if !utxos.contains(input) then {
                val stream = getClass.getResourceAsStream(
                  s"/utxos/${input.transactionId.toHex}-${input.index}"
                )
                if stream == null then
                    throw new IllegalStateException(
                      s"UTXO not found for input $input in resources or file system"
                    )

                val output = mapper.readValue(stream, classOf[Utxo])
                val scriptRef = Option(output.getReferenceScriptHash).map { scriptHash =>
                    // FIXME: add support for native reference scripts
                    val stream = getClass.getResourceAsStream(s"/scripts/${scriptHash}")
                    if stream == null then {
                        throw new IllegalStateException(
                          s"Reference script not found for hash $scriptHash in resources"
                        )
                    }

                    val scriptBytes = stream.readAllBytes()
                    val script =
                        PlutusUtil.getPlutusScript(scriptHash, Utils.bytesToHex(scriptBytes)).get
                    script match {
                        case s: PlutusV1Script =>
                            ScriptRef(Script.PlutusV1(decodeToSingleCbor(s)))
                        case s: PlutusV2Script =>
                            ScriptRef(Script.PlutusV2(decodeToSingleCbor(s)))
                        case s: PlutusV3Script =>
                            ScriptRef(Script.PlutusV3(decodeToSingleCbor(s)))
                    }
                }
                val out = getTransactionOutput(output, scriptRef)
                utxos.put(input, out)
            }

        // Return resolved UTXOs for all inputs (regular and reference)
        allInputs.map { input =>
            val output = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"UTXO not found for input $input")
            )
            input -> output
        }.toMap
    }

    def copyToResources(transaction: Transaction): Unit = {
        val allInputs =
            transaction.body.value.inputs.toSet.view ++ transaction.body.value.referenceInputs.toSet.view
        for input <- allInputs do
            val file = Path.of(s"utxos/${input.transactionId.toHex}-${input.index}")
            if !file.toFile.exists() then
                throw new IllegalStateException(
                  s"UTXO not found for input $input in resources or file system"
                )
            println(s"Copying UTXO for input $input to resources")
            Files.copy(
              file,
              Path.of(
                s"src/test/resources/utxos/${input.transactionId.toHex}-${input.index}"
              ),
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )

            val stream = Files.readAllBytes(file)
            val output = mapper.readValue(stream, classOf[Utxo])
            Option(output.getReferenceScriptHash).foreach { scriptHash =>
                val file = Path.of(s"scripts/${scriptHash}")
                println(s"Copying script for UTXO $input to resources")
                Files.copy(
                  file,
                  Path.of(
                    s"src/test/resources/scripts/${scriptHash}"
                  ),
                  java.nio.file.StandardCopyOption.REPLACE_EXISTING
                )

            }
    }

    private def getTransactionOutput(
        output: Utxo,
        scriptRef: Option[ScriptRef]
    ): TransactionOutput = {
        val address = Address.fromBech32(output.getAddress)
        val datumOption: Option[DatumOption] =
            Option(output.getDataHash) -> Option(output.getInlineDatum) match
                case (_, Some(inlineDatum)) =>
                    Some(DatumOption.Inline(Data.fromCbor(inlineDatum.hexToBytes)))
                case (Some(dataHash), None) =>
                    Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHash))))
                case (None, None) => None
        TransactionOutput(
          address,
          output.toValue.toLedgerValue,
          datumOption,
          scriptRef
        )
    }

}
