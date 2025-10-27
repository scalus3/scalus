package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class RemoveInputsFromUtxoMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("RemoveInputsFromUtxoMutator success") {
        val context = Context()
        val state = State(
          utxo = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](1, 4).sample.get
        )
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(state.utxo.keySet)
                )
              )
            )
        }

        val result = RemoveInputsFromUtxoMutator.transit(context, state, transaction)
        assert(state.utxo.nonEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.isEmpty)
    }
}
