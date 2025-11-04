package scalus.cardano.ledger
package utils

import scala.collection.View

object AllWitnessesScripts {
    def allWitnessesScriptsMap(transaction: Transaction): Map[ScriptHash, Script] = {
        val witnessSet = transaction.witnessSet
        witnessSet.nativeScripts.toMap ++
            witnessSet.plutusV1Scripts.toMap ++
            witnessSet.plutusV2Scripts.toMap ++
            witnessSet.plutusV3Scripts.toMap
    }

    def allWitnessesScriptHashes(transaction: Transaction): Set[ScriptHash] =
        allWitnessesScriptsMap(transaction).keySet

    def allWitnessesScriptHashesView(transaction: Transaction): View[ScriptHash] =
        allWitnessesScriptHashes(transaction).view

    def allWitnessesScripts(transaction: Transaction): Set[Script] =
        allWitnessesScriptsMap(transaction).values.toSet

    def allWitnessesScriptsView(transaction: Transaction): View[Script] =
        allWitnessesScriptsMap(transaction).values.view

    def allWitnessesPlutusScriptsMap(
        transaction: Transaction
    ): Map[ScriptHash, PlutusScript] = {
        val witnessSet = transaction.witnessSet
        witnessSet.plutusV1Scripts.toMap ++
            witnessSet.plutusV2Scripts.toMap ++
            witnessSet.plutusV3Scripts.toMap
    }

    def allWitnessesPlutusScriptHashes(transaction: Transaction): Set[ScriptHash] =
        allWitnessesPlutusScriptsMap(transaction).keySet

    def allWitnessesPlutusScriptHashesView(transaction: Transaction): View[ScriptHash] =
        allWitnessesPlutusScriptHashes(transaction).view

    def allWitnessesPlutusScripts(transaction: Transaction): Set[PlutusScript] =
        allWitnessesPlutusScriptsMap(transaction).values.toSet

    def allWitnessesPlutusScriptsView(transaction: Transaction): View[PlutusScript] =
        allWitnessesPlutusScriptsMap(transaction).values.view

    def allWitnessesNativeScriptsMap(transaction: Transaction): Map[ScriptHash, Script.Native] =
        transaction.witnessSet.nativeScripts.toMap

    def allWitnessesNativeScripts(transaction: Transaction): Set[Script.Native] =
        transaction.witnessSet.nativeScripts.toMap.values.toSet

    def allWitnessesNativeScriptHashes(transaction: Transaction): Set[ScriptHash] = {
        allWitnessesNativeScriptsMap(transaction).keySet
    }

    def allWitnessesNativeScriptHashesView(transaction: Transaction): View[ScriptHash] =
        allWitnessesNativeScriptHashes(transaction).view
}
