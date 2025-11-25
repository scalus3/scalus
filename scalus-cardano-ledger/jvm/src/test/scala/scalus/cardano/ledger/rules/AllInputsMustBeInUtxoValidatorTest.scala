package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("AllInputsMustBeInUtxoValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomTransactionWithIsValidField
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
          utxos = (
            transaction.body.value.inputs.toSet.view ++
                transaction.body.value.collateralInputs.toSet.view ++
                transaction.body.value.referenceInputs.toSet.view
          )
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.collateralInputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.referenceInputs.toSet.forall(state.utxos.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure on inputs") {
        val context = Context()
        val transaction = {
            val tx = randomTransactionWithIsValidField
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
          utxos = (
            // missing inputs
            transaction.body.value.collateralInputs.toSet.view ++
                transaction.body.value.referenceInputs.toSet.view
          )
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.forall(!state.utxos.contains(_)))
        assert(transaction.body.value.collateralInputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.referenceInputs.toSet.forall(state.utxos.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure on collateralInputs") {
        val context = Context()
        val transaction = {
            val tx = randomTransactionWithIsValidField
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
          utxos = (
            transaction.body.value.inputs.toSet.view ++
                // missing collateralInputs
                transaction.body.value.referenceInputs.toSet.view
          )
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.collateralInputs.toSet.forall(!state.utxos.contains(_)))
        assert(transaction.body.value.referenceInputs.toSet.forall(state.utxos.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure on referenceInputs") {
        val context = Context()
        val transaction = {
            val tx = randomTransactionWithIsValidField
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
          utxos = (
            transaction.body.value.inputs.toSet.view ++
                transaction.body.value.collateralInputs.toSet.view
                // missing referenceInputs
          )
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.collateralInputs.toSet.forall(state.utxos.contains))
        assert(transaction.body.value.referenceInputs.toSet.forall(!state.utxos.contains(_)))
    }

}
