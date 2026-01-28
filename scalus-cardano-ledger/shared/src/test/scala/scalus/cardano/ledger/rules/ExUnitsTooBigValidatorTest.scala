package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.Data

class ExUnitsTooBigValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("ExUnitsTooBig - success with small exUnits") {
        val exUnits = ExUnits(1, 1)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(ExUnitsTooBigValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBig - success with no redeemers") {
        val tx =
            randomTransactionWithIsValidField
                .withWitness(TransactionWitnessSet(redeemers = None))

        val emulator = TestEmulatorFactory.create(
          validators = Seq(ExUnitsTooBigValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBig - failure with too large exUnits") {
        val exUnits = ExUnits(Long.MaxValue, Long.MaxValue)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(ExUnitsTooBigValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(tx)
        assert(result.isLeft)
    }
}
