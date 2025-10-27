package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class InputsAndReferenceInputsDisjointValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("InputsAndReferenceInputsDisjointValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  ),
                  referenceInputs = TaggedSortedSet.empty
                )
              )
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(
          transaction.body.value.inputs.toSortedSet.nonEmpty && transaction.body.value.referenceInputs.toSortedSet.isEmpty
        )
    }

    test("InputsAndReferenceInputsDisjointValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(inputs),
                  referenceInputs = TaggedSortedSet.from(inputs)
                )
              )
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          transaction.body.value.inputs.toSortedSet.nonEmpty && transaction.body.value.referenceInputs.toSortedSet.nonEmpty
        )
        assert(
          transaction.body.value.inputs.toSortedSet == transaction.body.value.referenceInputs.toSortedSet
        )
    }
}
