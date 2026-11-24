package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class TransactionSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val maxTxSize = Context().env.params.maxTxSize

    test("TransactionSizeValidator rule success") {
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input(0))),
            outputs = IndexedSeq(Sized(Output(keyAddress(1), Value(Coin(1000000L))))),
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )

        val result = TransactionSizeValidator.validate(Context(), State(), transaction)
        assert(result.isRight)
        assert(transaction.toCborForFeeCalculation.length <= maxTxSize)
    }

    test("TransactionSizeValidator rule failure") {
        // 1000 inputs of about 37 bytes each exceed the maximum transaction size.
        val transaction = txSpending((0 until 1000).map(input)*)(TransactionWitnessSet.empty)

        val result = TransactionSizeValidator.validate(Context(), State(), transaction)
        assert(result.isLeft)
        assert(transaction.toCborForFeeCalculation.length > maxTxSize)
    }
}
