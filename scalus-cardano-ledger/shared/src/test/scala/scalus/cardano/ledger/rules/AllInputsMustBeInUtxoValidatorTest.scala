package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class AllInputsMustBeInUtxoValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("AllInputsMustBeInUtxo - success on inputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )
        val utxos = transaction.body.value.inputs.toSet.view
            .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
            .toMap

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxo - failure on inputs") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          utxos = Map.empty,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxo - success on collateral inputs") {
        val collateralInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.from(Set(collateralInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )
        val utxos = transaction.body.value.collateralInputs.toSet.view
            .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
            .toMap

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxo - failure on collateral inputs") {
        val collateralInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.from(Set(collateralInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          utxos = Map.empty,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxo - success on reference inputs") {
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )
        val utxos = transaction.body.value.referenceInputs.toSet.view
            .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
            .toMap

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("AllInputsMustBeInUtxo - failure on reference inputs") {
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.from(Set(referenceInput)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          utxos = Map.empty,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("AllInputsMustBeInUtxo - success on empty inputs") {
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.empty,
            referenceInputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet.empty
        )

        val emulator = TestEmulatorFactory.create(
          utxos = Map.empty,
          validators = Seq(AllInputsMustBeInUtxoValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }
}
