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
        transaction.witnessSet.vkeyWitnesses.toSortedSet.view.map(_.vkeyHash)
    }

    def allBootstrapWitnessesKeyHashes(transaction: Transaction): Set[AddrKeyHash] = {
        allBootstrapWitnessesKeyHashesView(transaction).toSet
    }

    // TODO implementation
    def allBootstrapWitnessesKeyHashesView(transaction: Transaction): View[AddrKeyHash] = {
        View.empty
    }
}
