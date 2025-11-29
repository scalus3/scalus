package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptsValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.NativeScriptsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val validityInterval = event.validityInterval
        val utxo = state.utxos

        for
            allNeededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(
              event,
              utxo
            )

            allProvidedReferenceNativeScripts <- AllResolvedScripts
                .allProvidedReferenceNativeScripts(
                  event,
                  utxo
                )

            allWitnessesNativeScripts = AllResolvedScripts.allWitnessesNativeScripts(event)

            validatorKeys = extractValidatorKeys(event)

            invalidWitnessesNativeScriptHashes = findInvalidNativeScriptHashes(
              allWitnessesNativeScripts,
              allNeededScriptHashes,
              validatorKeys,
              validityInterval
            )

            invalidProvidedReferenceNativeScriptHashes = findInvalidNativeScriptHashes(
              allProvidedReferenceNativeScripts,
              allNeededScriptHashes,
              validatorKeys,
              validityInterval
            )

            _ <-
                if invalidWitnessesNativeScriptHashes.nonEmpty ||
                    invalidProvidedReferenceNativeScriptHashes.nonEmpty
                then
                    failure(
                      TransactionException.NativeScriptsException(
                        transactionId,
                        invalidWitnessesNativeScriptHashes,
                        invalidProvidedReferenceNativeScriptHashes
                      )
                    )
                else success
        yield ()
    }

    private def extractValidatorKeys(
        event: Event
    ): Set[AddrKeyHash] = event.witnessSet.vkeyWitnesses.toSet.map(_.vkeyHash)

    private def findInvalidNativeScriptHashes(
        nativeScripts: Set[Script.Native],
        neededScriptHashes: Set[ScriptHash],
        validatorKeys: Set[AddrKeyHash],
        validityInterval: ValidityInterval
    ): Set[ScriptHash] = {
        for
            nativeScript <- nativeScripts if neededScriptHashes.contains(nativeScript.scriptHash) &&
                !nativeScript.script.evaluate(validatorKeys, validityInterval)
        yield nativeScript.scriptHash
    }
}
