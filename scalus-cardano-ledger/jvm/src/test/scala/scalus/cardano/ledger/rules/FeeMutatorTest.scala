package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class FeeMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("FeeMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  fee = Arbitrary.arbitrary[Coin].sample.get
                )
              )
            )
        }

        val result = FeeMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(context.fee == transaction.body.value.fee)
    }
}
