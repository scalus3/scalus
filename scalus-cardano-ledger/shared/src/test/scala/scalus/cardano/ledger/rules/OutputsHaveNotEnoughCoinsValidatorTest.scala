package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class OutputsHaveNotEnoughCoinsValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("OutputsHaveNotEnoughCoins - TransactionOutputs success") {
        val output = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(
            Coin(1000000000L),
            genMultiAsset(
              minPolicies = 1,
              maxPolicies = 4,
              minAssets = 1,
              maxAssets = 4
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoins - TransactionOutputs failure for minimum ada") {
        val output = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1L))
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoins - TransactionOutputs failure for negative assets") {
        val output = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(
            Coin(1000000000L),
            MultiAsset.asset(
              Arbitrary.arbitrary[PolicyId].sample.get,
              Arbitrary.arbitrary[AssetName].sample.get,
              -1L
            )
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoins - CollateralReturnOutput success") {
        val collateralReturnOutput = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(
            Coin(1000000000L),
            genMultiAsset(
              minPolicies = 1,
              maxPolicies = 4,
              minAssets = 1,
              maxAssets = 4
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoins - CollateralReturnOutput failure for minimum ada") {
        val collateralReturnOutput = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1L))
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoins - CollateralReturnOutput failure for negative assets") {
        val collateralReturnOutput = Output(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.Key(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(
            Coin(1000000000L),
            MultiAsset.asset(
              Arbitrary.arbitrary[PolicyId].sample.get,
              Arbitrary.arbitrary[AssetName].sample.get,
              -1L
            )
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
          validators = Seq(OutputsHaveNotEnoughCoinsValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
