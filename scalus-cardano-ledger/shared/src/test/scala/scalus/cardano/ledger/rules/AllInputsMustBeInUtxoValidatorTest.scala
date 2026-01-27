package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*

// Duplicates most of the `AllInputsMustBeInUtxoValidatorTest` in JVM ledger tests for now
object AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite, ArbitraryInstances {
    def allTests = Seq(
      allInputsMustBeInUtxoValidatorFailureOnInputs,
      allInputsMustBeInUtxoValidatorSuccessOnCollateralInputs,
      allInputsMustBeInUtxoValidatorFailureOnCollateralInputs,
      allInputsMustBeInUtxoValidatorSuccessOnReferenceInputs,
      allInputsMustBeInUtxoValidatorFailureOnReferenceInputs,
      allInputsMustBeInUtxoValidatorSuccessOnAlLKindsOfInputs
    )

    def allInputsMustBeInUtxoValidatorSuccessOnInputs: TestCase = {
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
        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isLeft)
        )
    }

    def allInputsMustBeInUtxoValidatorFailureOnInputs = {
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

        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isLeft)
        )
    }

    def allInputsMustBeInUtxoValidatorSuccessOnCollateralInputs = {
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

        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isRight)
        )
    }

    def allInputsMustBeInUtxoValidatorFailureOnCollateralInputs = {
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

        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isLeft)
        )
    }

    def allInputsMustBeInUtxoValidatorSuccessOnReferenceInputs = {
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
        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isRight)
        )
    }

    def allInputsMustBeInUtxoValidatorFailureOnReferenceInputs = {
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

        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isLeft)
        )
    }

    def allInputsMustBeInUtxoValidatorSuccessOnAlLKindsOfInputs = {
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
        TestCase(
          state,
          context,
          transaction,
          x => assert(x.isRight)
        )
    }
}
