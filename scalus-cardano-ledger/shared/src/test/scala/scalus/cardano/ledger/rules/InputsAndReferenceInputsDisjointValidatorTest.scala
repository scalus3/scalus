package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class InputsAndReferenceInputsDisjointValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("InputsAndReferenceInputsDisjoint - success with disjoint inputs") {
        val transaction = {
            val tx = randomTransactionWithIsValidField
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(InputsAndReferenceInputsDisjointValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
        assert(
          transaction.body.value.inputs.toSet.nonEmpty && transaction.body.value.referenceInputs.toSet.isEmpty
        )
    }

    test("InputsAndReferenceInputsDisjoint - failure with overlapping inputs") {
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(inputs),
                  referenceInputs = TaggedSortedSet.from(inputs)
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(InputsAndReferenceInputsDisjointValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
        assert(
          transaction.body.value.inputs.toSet.nonEmpty && transaction.body.value.referenceInputs.toSet.nonEmpty
        )
        assert(
          transaction.body.value.inputs.toSet == transaction.body.value.referenceInputs.toSet
        )
    }
}
