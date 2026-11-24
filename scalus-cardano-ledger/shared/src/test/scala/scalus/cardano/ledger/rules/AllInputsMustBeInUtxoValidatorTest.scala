package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // The rule checks only that each input is in the UTxO, so any fixed output serves.
    private def utxosOf(inputs: Set[TransactionInput]): Utxos =
        inputs.view.map(_ -> TransactionOutput(keyAddress(1), Value.ada(2))).toMap

    private def tx(
        inputs: Set[TransactionInput] = Set.empty,
        collateralInputs: Set[TransactionInput] = Set.empty,
        referenceInputs: Set[TransactionInput] = Set.empty
    ): Transaction = Transaction(
      emptyBody.copy(
        inputs = TaggedSortedSet.from(inputs),
        collateralInputs = TaggedSortedSet.from(collateralInputs),
        referenceInputs = TaggedSortedSet.from(referenceInputs)
      ),
      TransactionWitnessSet.empty
    )

    test("AllInputsMustBeInUtxoValidator success on inputs") {
        val transaction = tx(inputs = Set(input(0)))
        val utxos = utxosOf(Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, utxos, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on inputs") {
        val transaction = tx(inputs = Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxoValidator success on collateral inputs") {
        val transaction = tx(collateralInputs = Set(input(0)))
        val utxos = utxosOf(Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, utxos, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on collateral inputs") {
        val transaction = tx(collateralInputs = Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxoValidator success on reference inputs") {
        val transaction = tx(referenceInputs = Set(input(0)))
        val utxos = utxosOf(Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, utxos, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on reference inputs") {
        val transaction = tx(referenceInputs = Set(input(0)))

        val result = validate(AllInputsMustBeInUtxoValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test(
      "AllInputsMustBeInUtxoValidator success on empty inputs, collateralInputs, and referenceInputs"
    ) {
        val transaction = tx()

        val result = validate(AllInputsMustBeInUtxoValidator, Map.empty, transaction)
        assert(result.isRight)
    }
}
