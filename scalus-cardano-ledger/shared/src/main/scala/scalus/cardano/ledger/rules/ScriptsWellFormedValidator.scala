package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.AllResolvedScripts

import scala.collection.View

// It's Babbage.validateScriptsWellFormed in cardano-ledger
object ScriptsWellFormedValidator extends STS.Validator {
    override final type Error = TransactionException.IllFormedScriptsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxos
        val body = event.body.value
        val majorProtocolVersion = MajorProtocolVersion(context.env.params.protocolVersion.major)

        val allWitnessesPlutusScripts = AllResolvedScripts.allWitnessesPlutusScriptsView(event)

        val allPlutusScriptsFromAllOutputs =
            val allOutputs = (body.outputs.view ++ body.collateralReturnOutput).map(_.value)

            for
                output <- allOutputs
                scriptRef <- output.scriptRef
                plutusScript <- scriptRef.script match
                    case plutusScript: PlutusScript => Some(plutusScript)
                    case _                          => None
            yield plutusScript

        val allInvalidWitnessesPlutusScriptHashes = findAllInvalidPlutusScripts(
          allWitnessesPlutusScripts,
          majorProtocolVersion
        )

        val allInvalidPlutusScriptHashesFromAllOutputs = findAllInvalidPlutusScripts(
          allPlutusScriptsFromAllOutputs,
          majorProtocolVersion
        )

        if allInvalidWitnessesPlutusScriptHashes.nonEmpty ||
            allInvalidPlutusScriptHashesFromAllOutputs.nonEmpty
        then
            failure(
              TransactionException.IllFormedScriptsException(
                transactionId,
                allInvalidWitnessesPlutusScriptHashes,
                allInvalidPlutusScriptHashesFromAllOutputs
              )
            )
        else success
    }

    private def findAllInvalidPlutusScripts(
        plutusScripts: View[PlutusScript],
        majorProtocolVersion: MajorProtocolVersion
    ): Set[ScriptHash] = {
        plutusScripts
            .filterNot(_.isWellFormed(majorProtocolVersion))
            .map(_.scriptHash)
            .toSet
    }
}
