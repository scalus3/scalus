package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class EmptyInputsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("EmptyInputsValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  )
                )
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.toSet.nonEmpty)
    }

    test("EmptyInputsValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty
                )
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.isEmpty)
    }
}
