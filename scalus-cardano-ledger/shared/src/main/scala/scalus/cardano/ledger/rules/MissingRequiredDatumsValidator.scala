package scalus.cardano.ledger.rules

import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.MissingRequiredDatumsValidator.success
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.ledger.*

import scala.collection.mutable

// // It's part of Babbage.missingRequiredDatums in cardano-ledger
object MissingRequiredDatumsValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val utxo = state.utxos

        val result = for {
            scriptsProvided <- AllResolvedScripts.allResolvedScriptsMap(transaction, utxo)
            (inputHashes, txInsNoDataHash) = getInputDataHashesTxBody(
              utxo,
              transaction,
              scriptsProvided
            )
            txHashes = transaction.witnessSet.plutusData.value.toMap.keys.toSet[ByteString]

            unmatchedDatumHashes = inputHashes -- txHashes
            allowedSupplementalDataHashes = getSupplementalDataHashes(utxo, transaction)
            supplementalDatumHashes = txHashes -- inputHashes
            (okSupplementalDHs, notOkSupplementalDHs) = supplementalDatumHashes.partition(
              allowedSupplementalDataHashes.contains
            )

        } yield {
            if txInsNoDataHash.nonEmpty then
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Unspendable UTxO without datum hash for transaction ${transaction.id}: $txInsNoDataHash"
                  )
                )
            else if unmatchedDatumHashes.nonEmpty then {
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Missing required datums for transaction ${transaction.id}: required=$unmatchedDatumHashes, provided=$txHashes"
                  )
                )
            } else if notOkSupplementalDHs.nonEmpty then
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Not allowed supplemental datums for transaction ${transaction.id}: notAllowed=$notOkSupplementalDHs, allowed=$okSupplementalDHs"
                  )
                )
            else success
        }

        result.flatten
    }

    private def getInputDataHashesTxBody(
        utxo: Utxos,
        transaction: Transaction,
        scriptsProvided: Map[ScriptHash, Script]
    ): (Set[ByteString], Set[TransactionInput]) = {
        val txBody = transaction.body.value
        val inputHashes = mutable.Set.empty[DataHash]
        val txInsNoDataHash = mutable.Set.empty[TransactionInput]

        for {
            input <- txBody.inputs.toSet
            resolvedInput <- utxo.get(input)
            if isTwoPhaseScriptAddress(resolvedInput.address, scriptsProvided)
        } do {
            resolvedInput match {
                case shelley: TransactionOutput.Shelley =>
                    shelley.datumHash match {
                        case Some(hash) => inputHashes += hash
                        case None       => txInsNoDataHash += input
                    }
                case babbage: TransactionOutput.Babbage =>
                    babbage.datumOption match {
                        case Some(DatumOption.Hash(hash)) =>
                            inputHashes += hash
                        case _ => txInsNoDataHash += input
                    }
            }
        }

        (inputHashes.toSet, txInsNoDataHash.toSet)
    }

    private def isTwoPhaseScriptAddress(
        address: Address,
        scriptsProvided: Map[ScriptHash, Script]
    ): Boolean = {
        def isPhaseTwo(s: Script) = s match {
            case script: PlutusScript  => true
            case Script.Native(script) => false
        }
        (for {
            scriptHash <- address.scriptHashOption
            script <- scriptsProvided.get(scriptHash)
        } yield isPhaseTwo(script)).getOrElse(false)
    }

    private def getSupplementalDataHashes(
        utxo: Utxos,
        transaction: Transaction
    ): Set[ByteString] = {
        val txBody = transaction.body.value
        val allowedHashes = mutable.Set.empty[ByteString]

        def handleHash(resolvedInput: TransactionOutput) =
            resolvedInput match {
                case shelley: TransactionOutput.Shelley =>
                    shelley.datumHash match {
                        case Some(hash) => allowedHashes += hash
                        case None       => // no datum no hash
                    }
                case babbage: TransactionOutput.Babbage =>
                    babbage.datumOption match {
                        case Some(DatumOption.Hash(hash)) => allowedHashes += hash
                        case _                            => // Inline datum or no datum
                    }
            }

        txBody.outputs.foreach(x => handleHash(x.value))
        txBody.referenceInputs.toSeq.flatMap(utxo.get).foreach(handleHash)

        allowedHashes.toSet
    }
}
