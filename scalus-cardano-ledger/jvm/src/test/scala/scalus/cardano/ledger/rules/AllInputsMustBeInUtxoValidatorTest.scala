package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("AllInputsMustBeInUtxoValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  ),
                  collateralInputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  ),
                  referenceInputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  )
                )
              )
            )
        }
        val state = State(
          utxo = transaction.body.value.inputs.toSortedSet.view
              .concat(transaction.body.value.collateralInputs.toSortedSet)
              .concat(transaction.body.value.referenceInputs.toSortedSet)
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.toSortedSet.forall(state.utxo.contains))
        assert(transaction.body.value.collateralInputs.toSortedSet.forall(state.utxo.contains))
        assert(transaction.body.value.referenceInputs.toSortedSet.forall(state.utxo.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure") {
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
                  collateralInputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  ),
                  referenceInputs = TaggedSortedSet.from(
                    genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                  )
                )
              )
            )
        }

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(!transaction.body.value.inputs.toSortedSet.forall(state.utxo.contains))
        assert(!transaction.body.value.collateralInputs.toSortedSet.forall(state.utxo.contains))
        assert(!transaction.body.value.referenceInputs.toSortedSet.forall(state.utxo.contains))
    }
}
