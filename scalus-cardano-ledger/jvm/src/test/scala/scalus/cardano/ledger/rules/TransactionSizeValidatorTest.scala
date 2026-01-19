package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.cardano.address.ByronAddress
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedSet

class TransactionSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("TransactionSizeValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedMap.empty,
                plutusV1Scripts = TaggedSortedStrictMap.empty,
                plutusV2Scripts = TaggedSortedStrictMap.empty,
                plutusV3Scripts = TaggedSortedStrictMap.empty,
                plutusData = KeepRaw(TaggedSortedMap.empty),
                redeemers = None
              ),
              auxiliaryData = None,
              body = KeepRaw(
                tx.body.value.copy(
                  inputs =
                      TaggedSortedSet.from(Set(Arbitrary.arbitrary[TransactionInput].sample.get)),
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ByronAddress].sample.get,
                        Value(Coin(1000000L))
                      )
                    )
                  ),
                  votingProcedures = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  withdrawals = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  mint = None,
                  requiredSigners = TaggedSortedSet.empty,
                  collateralReturnOutput = None
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isRight)
    }

    test("TransactionSizeValidator rule failure") {
        val context = Context()
        val inputs = SortedSet.fill(1000) { // Arbitrary large number of inputs
            Arbitrary.arbitrary[TransactionInput].sample.get
        }

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet(inputs)
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isLeft)
    }
}
