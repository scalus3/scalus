package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class TooManyCollateralInputsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("TooManyCollateralInputsValidator success") {
        val context = Context()
        val state = State()
        val maxCollateralInputs = context.env.params.maxCollateralInputs.toInt

        val collateralInputs = (1 to maxCollateralInputs).map { i =>
            TransactionInput(
              Arbitrary.arbitrary[TransactionHash].sample.get,
              i
            )
        }.toSet

        val tx = randomTransactionWithIsValidField.copy(
          body = KeepRaw(
            randomTransactionWithIsValidField.body.value.copy(
              collateralInputs = TaggedSortedSet.from(collateralInputs)
            )
          )
        )

        val result = TooManyCollateralInputsValidator.validate(context, state, tx)
        assert(result.isRight)
    }

    test("TooManyCollateralInputsValidator failure") {
        val context = Context()
        val state = State()
        val collateralInputs = (1 to context.env.params.maxCollateralInputs.toInt + 1).map { i =>
            TransactionInput(
              Arbitrary.arbitrary[TransactionHash].sample.get,
              i
            )
        }.toSet

        val tx = randomTransactionWithIsValidField.copy(
          body = KeepRaw(
            randomTransactionWithIsValidField.body.value.copy(
              collateralInputs = TaggedSortedSet.from(collateralInputs)
            )
          )
        )

        val result = TooManyCollateralInputsValidator.validate(context, state, tx)
        assert(result.isLeft)
    }

}
