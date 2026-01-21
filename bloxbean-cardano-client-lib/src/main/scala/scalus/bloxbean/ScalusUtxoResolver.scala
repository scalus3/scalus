package scalus.bloxbean

import com.bloxbean.cardano.client.api.UtxoSupplier
import scalus.cardano.ledger.*

import scala.collection.mutable
import scala.jdk.OptionConverters.*

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
                    case Some(bloxbeanUtxo) =>
                        val out = Interop.toTransactionOutput(
                          bloxbeanUtxo,
                          Interop.getScriptRef(bloxbeanUtxo, scriptSupplier)
                        )
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
}
