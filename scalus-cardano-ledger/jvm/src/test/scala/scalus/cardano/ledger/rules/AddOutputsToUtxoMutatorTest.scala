package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class AddOutputsToUtxoMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("AddOutputsToUtxoMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = genVectorOfSizeFromArbitrary[Sized[TransactionOutput]](1, 4).sample.get
                )
              )
            )
        }

        val result = AddOutputsToUtxoMutator.transit(context, state, transaction)
        assert(state.utxos.isEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxos.values.toSeq == transaction.body.value.outputs.map {
            _.value
        })
    }
}
