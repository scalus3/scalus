package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class TooManyCollateralInputsValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("TooManyCollateralInputs - success with max allowed inputs") {
        val context = Context()
        val maxCollateralInputs = context.env.params.maxCollateralInputs.toInt

        val collateralInputs = (1 to maxCollateralInputs).map { i =>
            Input(
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(TooManyCollateralInputsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(tx)
        assert(result.isRight)
    }

    test("TooManyCollateralInputs - failure with too many inputs") {
        val context = Context()
        val collateralInputs = (1 to context.env.params.maxCollateralInputs.toInt + 1).map { i =>
            Input(
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(TooManyCollateralInputsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(tx)
        assert(result.isLeft)
    }
}
