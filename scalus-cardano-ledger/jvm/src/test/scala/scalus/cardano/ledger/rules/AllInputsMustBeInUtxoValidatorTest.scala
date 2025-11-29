package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("AllInputsMustBeInUtxoValidator success on inputs") {
        val context = Context()
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State(
          utxos = transaction.body.value.inputs.toSet.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on inputs") {
        val context = Context()
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State()

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxoValidator success on collateral inputs") {
        val context = Context()
        val collateralInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.from(Set(collateralInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State(
          utxos = transaction.body.value.collateralInputs.toSet.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on collateral inputs") {
        val context = Context()
        val collateralInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.from(Set(collateralInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State()

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxoValidator success on reference inputs") {
        val context = Context()
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State(
          utxos = transaction.body.value.referenceInputs.toSet.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxoValidator failure on reference inputs") {
        val context = Context()
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State()

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "AllInputsMustBeInUtxoValidator success on empty inputs, collateralInputs, and referenceInputs"
    ) {
        val context = Context()
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet.empty
        )
        val state = State()

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
    }
}
