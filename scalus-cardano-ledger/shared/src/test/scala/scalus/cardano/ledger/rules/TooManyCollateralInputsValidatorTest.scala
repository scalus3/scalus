package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class TooManyCollateralInputsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val maxCollateralInputs = Context().env.params.maxCollateralInputs.toInt

    // The rule reads only the collateral inputs, so a fixed spending transaction serves.
    private def withCollateral(count: Int): Transaction =
        txSpending(input(0))(TransactionWitnessSet.empty).withBody(
          _.copy(collateralInputs = TaggedSortedSet.from((1 to count).map(input)))
        )

    test("TooManyCollateralInputsValidator success") {
        val tx = withCollateral(maxCollateralInputs)

        val result = validate(TooManyCollateralInputsValidator, Map.empty, tx)
        assert(result.isRight)
    }

    test("TooManyCollateralInputsValidator failure") {
        val tx = withCollateral(maxCollateralInputs + 1)

        val result = validate(TooManyCollateralInputsValidator, Map.empty, tx)
        assert(result.isLeft)
    }

}
