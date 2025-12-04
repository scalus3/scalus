package scalus.bloxbean

import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.plutus.spec.{PlutusScript, PlutusV1Script, PlutusV2Script, PlutusV3Script}
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.*
import scalus.utils.Hex.hexToBytes

import scala.collection.mutable
import scala.jdk.OptionConverters.*
import scala.util.Try

/** Resolves UTXOs for transactions using scalus.cardano.ledger domain classes */
private[scalus] class ScalusUtxoResolver(
    utxoSupplier: UtxoSupplier,
    scriptSupplier: ScriptSupplier
) {

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
        inputUtxos.foreach { case (input, output) =>
            utxos.put(input, output)
        }

        val allInputs =
            transaction.body.value.inputs.toSet.view
                ++ transaction.body.value.referenceInputs.toSet.view
                ++ transaction.body.value.collateralInputs.toSet.view
        for input <- allInputs do
            if !utxos.contains(input) then
                utxoSupplier.getTxOutput(input.transactionId.toHex, input.index).toScala match
                    case Some(output) =>
                        // Get reference input script if available
                        val scriptRef = Option(output.getReferenceScriptHash).flatMap {
                            scriptHash =>
                                // FIXME: add support for native reference scripts
                                Try(scriptSupplier.getScript(scriptHash)).map {
                                    case s: PlutusV1Script =>
                                        ScriptRef(Script.PlutusV1(decodeToSingleCbor(s)))
                                    case s: PlutusV2Script =>
                                        ScriptRef(Script.PlutusV2(decodeToSingleCbor(s)))
                                    case s: PlutusV3Script =>
                                        ScriptRef(Script.PlutusV3(decodeToSingleCbor(s)))
                                }.toOption
                        }
                        val out = getTransactionOutput(output, scriptRef)
                        utxos.put(input, out)

                    case None =>
                        throw new IllegalStateException(
                          s"Reference UTXO not found for input $input"
                        )

        // Return resolved UTXOs for all inputs (regular and reference)
        allInputs.map { input =>
            val output = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"UTXO not found for input $input")
            )
            input -> output
        }.toMap
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
