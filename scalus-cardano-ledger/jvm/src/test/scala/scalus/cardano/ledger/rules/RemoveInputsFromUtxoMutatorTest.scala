package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class RemoveInputsFromUtxoMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("RemoveInputsFromUtxoMutator success") {
        val context = Context()
        val state = State(
          utxos = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](1, 4).sample.get
        )
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(state.utxos.keySet)
                )
              )
            )
        }

        val result = RemoveInputsFromUtxoMutator.transit(context, state, transaction)
        assert(state.utxos.nonEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxos.isEmpty)
    }
}
