package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class OutputsHaveTooBigValueStorageSizeValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("OutputsHaveTooBigValueStorageSize - TransactionOutputs success") {
        val output = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(OutputsHaveTooBigValueStorageSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSize - TransactionOutputs failure") {
        val output = Output(
          Arbitrary.arbitrary[ShelleyAddress].sample.get,
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(OutputsHaveTooBigValueStorageSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveTooBigValueStorageSize - CollateralReturnOutput success") {
        val collateralReturnOutput = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(OutputsHaveTooBigValueStorageSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSize - CollateralReturnOutput failure") {
        val collateralReturnOutput = Output(
          Arbitrary.arbitrary[ShelleyAddress].sample.get,
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(OutputsHaveTooBigValueStorageSizeValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
