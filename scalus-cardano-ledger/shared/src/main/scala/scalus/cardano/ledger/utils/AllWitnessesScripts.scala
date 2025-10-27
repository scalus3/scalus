package scalus.cardano.ledger
package utils

import scala.collection.View

object AllWitnessesScripts {
    def allWitnessesScriptsMap(transaction: Transaction): Map[ScriptHash, Script] =
        allWitnessesScriptsView(
          transaction
        ).map(script => script.scriptHash -> script).toMap

    def allWitnessesScriptHashes(transaction: Transaction): Set[ScriptHash] =
        allWitnessesScriptHashesView(transaction).toSet

    def allWitnessesScriptHashesView(transaction: Transaction): View[ScriptHash] = {
        allWitnessesScriptsView(transaction).map(_.scriptHash)
    }

    def allWitnessesScripts(transaction: Transaction): Set[Script] = allWitnessesScriptsView(
      transaction
    ).toSet

    def allWitnessesScriptsView(transaction: Transaction): View[Script] = {
        val witnessSet = transaction.witnessSet

        witnessSet.nativeScripts.toSortedSet.view ++
            witnessSet.plutusV1Scripts.toSortedSet.view ++
            witnessSet.plutusV2Scripts.toSortedSet.view ++
            witnessSet.plutusV3Scripts.toSortedSet.view

    }

    def allWitnessesPlutusScriptsMap(
        transaction: Transaction
    ): Map[ScriptHash, PlutusScript] =
        allWitnessesPlutusScriptsView(transaction).map(script => script.scriptHash -> script).toMap

    def allWitnessesPlutusScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = allWitnessesPlutusScriptHashesView(transaction).toSet

    def allWitnessesPlutusScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = allWitnessesPlutusScriptsView(transaction).map(_.scriptHash)

    def allWitnessesPlutusScripts(
        transaction: Transaction
    ): Set[PlutusScript] = allWitnessesPlutusScriptsView(transaction).toSet

    def allWitnessesPlutusScriptsView(
        transaction: Transaction
    ): View[PlutusScript] = {
        val witnessSet = transaction.witnessSet

        witnessSet.plutusV1Scripts.toSortedSet.view ++
            witnessSet.plutusV2Scripts.toSortedSet.view ++
            witnessSet.plutusV3Scripts.toSortedSet.view
    }

    def allWitnessesNativeScriptsMap(transaction: Transaction): Map[ScriptHash, Script.Native] =
        transaction.witnessSet.nativeScripts.toSortedSet.view
            .map(script => script.scriptHash -> script)
            .toMap

    def allWitnessesNativeScripts(transaction: Transaction): Set[Script.Native] =
        transaction.witnessSet.nativeScripts.toSortedSet

    def allWitnessesNativeScriptHashes(transaction: Transaction): Set[ScriptHash] = {
        allWitnessesNativeScriptHashesView(transaction).toSet
    }

    def allWitnessesNativeScriptHashesView(transaction: Transaction): View[ScriptHash] = {
        allWitnessesNativeScripts(transaction).view.map(_.scriptHash)
    }
}
