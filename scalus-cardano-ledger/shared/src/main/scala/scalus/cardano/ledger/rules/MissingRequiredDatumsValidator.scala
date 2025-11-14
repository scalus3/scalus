package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.AllResolvedScripts
import scala.collection.mutable

// // It's part of Babbage.missingRequiredDatums in cardano-ledger
object MissingRequiredDatumsValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException | TransactionException.DatumsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val utxos = state.utxos

        for
            allResolvedPlutusScriptsMap <- AllResolvedScripts.allResolvedPlutusScriptsMap(
              transaction,
              utxos
            )

            (inputHashes, txInsNoDataHash) = getInputDataHashesTxBody(
              transaction,
              utxos,
              allResolvedPlutusScriptsMap
            )

            txHashes = transaction.witnessSet.plutusData.value.toMap.keySet
            unmatchedDatumHashes = inputHashes -- txHashes
            allowedSupplementalDataHashes = getSupplementalDataHashes(transaction, utxos)
            supplementalDatumHashes = txHashes -- inputHashes
            notOkSupplementalDHs = supplementalDatumHashes -- allowedSupplementalDataHashes

            _ <-
                if txInsNoDataHash.nonEmpty || unmatchedDatumHashes.nonEmpty || notOkSupplementalDHs.nonEmpty
                then
                    failure(
                      TransactionException.DatumsException(
                        transaction.id,
                        txInsNoDataHash,
                        unmatchedDatumHashes,
                        notOkSupplementalDHs
                      )
                    )
                else success
        yield ()
    }

    private def getInputDataHashesTxBody(
        transaction: Transaction,
        utxos: Utxos,
        scriptsProvided: Map[ScriptHash, PlutusScript]
    ): (Set[DataHash], Set[TransactionInput]) = {
        val txBody = transaction.body.value
        val inputs = txBody.inputs.toSet.view
        val inputHashes = mutable.Set.empty[DataHash]
        val txInsNoDataHash = mutable.Set.empty[TransactionInput]

        for
            input <- inputs
            output <- utxos.get(input)
            scriptHash <- output.address.scriptHashOption
            script <- scriptsProvided.get(scriptHash)
        do
            output.datumOption match
                case Some(datumOption) =>
                    datumOption match
                        case DatumOption.Hash(hash) => inputHashes += hash
                        case DatumOption.Inline(_)  => // Inline datum, no hash to collect
                case None if script.language == Language.PlutusV3 => // Datum for Plutus V3 script is optional
                case None => txInsNoDataHash += input

        (inputHashes.toSet, txInsNoDataHash.toSet)
    }

    private def getSupplementalDataHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Set[DataHash] = {
        val txBody = transaction.body.value
        val txOutputs = txBody.outputs.view.map(_.value)
        val referenceOutputs = txBody.referenceInputs.toSeq.view.flatMap(utxos.get)
        val allOutputs = txOutputs ++ referenceOutputs

        (
          for
              output <- allOutputs
              datumOption <- output.datumOption
              dataHash <- datumOption.datHashOption
          yield dataHash
        ).toSet
    }
}
