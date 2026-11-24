package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class EmptyInputsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("EmptyInputsValidator rule success") {
        val transaction = txSpending(input(0), input(1))(TransactionWitnessSet.empty)

        val result = validate(EmptyInputsValidator, Map.empty, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.toSet.nonEmpty)
    }

    test("EmptyInputsValidator rule failure on empty inputs") {
        val transaction = txSpending()(TransactionWitnessSet.empty)

        val result = validate(EmptyInputsValidator, Map.empty, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.isEmpty)
    }
}
