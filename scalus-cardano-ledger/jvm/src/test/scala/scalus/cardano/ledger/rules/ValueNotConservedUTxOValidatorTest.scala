package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address

class ValueNotConservedUTxOValidatorTest extends AnyFunSuite, ArbitraryInstances {

    test("value is preserved in an empty transaction") {
        val context = Context()
        val tx = randomValidTransaction.copy(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
            )
          )
        )

        assert(
          ValueNotConservedUTxOValidator.validate(context, State(), tx).isRight,
          "Empty transaction should preserve value"
        )
    }

    test("value is not preserved when inputs and outputs are not equal") {
        val context = Context()
        val input = TransactionInput(
          transactionId = randomValidTransaction.id,
          index = 0
        )
        val resolvedOutput = TransactionOutput(
          address = arbitrary[Address].sample.get,
          value = Value.lovelace(123),
        )
        val output = TransactionOutput(
          address = arbitrary[Address].sample.get,
          value = Value.ada(1), // 1 ADA
        )
        val state = State(
          utxos = Map(input -> resolvedOutput)
        )
        val tx = randomValidTransaction.copy(
          body = KeepRaw(
            randomValidTransaction.body.value.copy(
              fee = Coin.zero,
              mint = None,
              inputs = TaggedSortedSet.from(Set(input)),
              outputs = IndexedSeq(Sized(output))
            )
          )
        )

        assert(ValueNotConservedUTxOValidator.validate(context, state, tx).isLeft)
    }

    private def randomValidTransaction = arbitrary[Transaction].sample.get.copy(isValid = true)
}
