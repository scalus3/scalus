package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ByronAddress
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

import scala.collection.immutable.SortedSet

class TransactionSizeValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("TransactionSize - success with small transaction") {
        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.withWitness(
              _.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedMap.empty,
                plutusV1Scripts = TaggedSortedStrictMap.empty,
                plutusV2Scripts = TaggedSortedStrictMap.empty,
                plutusV3Scripts = TaggedSortedStrictMap.empty,
                plutusData = KeepRaw(TaggedSortedMap.empty),
                redeemers = None
              )
            ).copy(
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(TransactionSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("TransactionSize - failure with too large transaction") {
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

        val emulator = TestEmulatorFactory.create(
          validators = Seq(TransactionSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
