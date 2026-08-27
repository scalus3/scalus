package scalus.cardano.ledger
package utils

import scala.collection.View

object AllWitnessesKeyHashes {
    def allWitnessesKeyHashes(transaction: Transaction): Set[AddrKeyHash] = {
        allWitnessesKeyHashesView(transaction).toSet
    }

    def allWitnessesKeyHashesView(transaction: Transaction): View[AddrKeyHash] = {
        allVkeyWitnessesKeyHashesView(transaction) ++
            allBootstrapWitnessesKeyHashesView(transaction)
    }

    def allVkeyWitnessesKeyHashes(transaction: Transaction): Set[AddrKeyHash] = {
        allVkeyWitnessesKeyHashesView(transaction).toSet
    }

    def allVkeyWitnessesKeyHashesView(transaction: Transaction): View[AddrKeyHash] = {
        transaction.witnessSet.vkeyWitnesses.toSet.view.map(_.vkeyHash)
    }

    def allBootstrapWitnessesKeyHashes(transaction: Transaction): Set[AddrKeyHash] = {
        allBootstrapWitnessesKeyHashesView(transaction).toSet
    }

    def allBootstrapWitnessesKeyHashesView(transaction: Transaction): View[AddrKeyHash] = {
        transaction.witnessSet.bootstrapWitnesses.toSet.view.map(_.addrKeyHash)
    }
}
