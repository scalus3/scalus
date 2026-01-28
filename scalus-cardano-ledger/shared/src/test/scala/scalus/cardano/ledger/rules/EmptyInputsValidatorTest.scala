package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class EmptyInputsValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("EmptyInputs - success with non-empty inputs") {
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(EmptyInputsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.toSet.nonEmpty)
    }

    test("EmptyInputs - failure on empty inputs") {
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(EmptyInputsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.isEmpty)
    }
}
