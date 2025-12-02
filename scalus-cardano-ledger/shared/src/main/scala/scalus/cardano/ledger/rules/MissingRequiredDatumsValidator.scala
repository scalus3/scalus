package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.AllResolvedScripts
import scala.collection.mutable

// It's part of Babbage.missingRequiredDatums in cardano-ledger
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

            (neededDatumHashes, inputsWithMissingDatumHash) = getInputDataHashesTxBody(
              transaction,
              utxos,
              allResolvedPlutusScriptsMap
            )

            witnessDatumHashes = transaction.witnessSet.plutusData.value.toMap.keySet
            unmatchedDatumHashes = neededDatumHashes -- witnessDatumHashes
            allowedSupplementalDatumHashes = getSupplementalDatumHashes(transaction, utxos)
            actualSupplementalDatumHashes = witnessDatumHashes -- neededDatumHashes
            notAllowedSupplementalDatumHashes =
                actualSupplementalDatumHashes -- allowedSupplementalDatumHashes

            _ <-
                if inputsWithMissingDatumHash.nonEmpty || unmatchedDatumHashes.nonEmpty || notAllowedSupplementalDatumHashes.nonEmpty
                then
                    failure(
                      TransactionException.DatumsException(
                        transaction.id,
                        inputsWithMissingDatumHash,
                        unmatchedDatumHashes,
                        notAllowedSupplementalDatumHashes
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
        val neededDatumHashes = mutable.Set.empty[DataHash]
        val inputsWithMissingDatumHash = mutable.Set.empty[TransactionInput]

        for
            input <- inputs
            output <- utxos.get(input)
            scriptHash <- output.address.scriptHashOption
            script <- scriptsProvided.get(scriptHash)
        do
            output.datumOption match
                case Some(datumOption) =>
                    datumOption match
                        case DatumOption.Hash(hash) => neededDatumHashes += hash
                        case DatumOption.Inline(_)  => // Inline datum, no hash to collect
                case None if script.language == Language.PlutusV3 => // Datum for Plutus V3 script is optional
                case None => inputsWithMissingDatumHash += input

        (neededDatumHashes.toSet, inputsWithMissingDatumHash.toSet)
    }

    private def getSupplementalDatumHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Set[DataHash] = {
        val txBody = transaction.body.value
        val txOutputs = txBody.outputs.view.map(_.value)
        val referenceOutputs = txBody.referenceInputs.toSeq.view.flatMap(utxos.get)
        val collateralReturnOutput = txBody.collateralReturnOutput.map(_.value).view
        val allOutputs = txOutputs ++ referenceOutputs ++ collateralReturnOutput

        (
          for
              output <- allOutputs
              datumOption <- output.datumOption
              dataHash <- datumOption.dataHashOption
          yield dataHash
        ).toSet
    }
}
