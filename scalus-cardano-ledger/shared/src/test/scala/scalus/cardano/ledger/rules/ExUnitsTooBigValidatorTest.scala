package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.uplc.builtin.Data

class ExUnitsTooBigValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("ExUnitsTooBigValidator success") {
        val context = Context()
        val state = State()
        val exUnits = ExUnits(1, 1)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBigValidator success with no redeemers") {
        val context = Context()
        val state = State()
        val exUnits = ExUnits(1, 1)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(TransactionWitnessSet(redeemers = None))
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBigValidator failure") {
        val context = Context()
        val state = State()
        val exUnits = ExUnits(Long.MaxValue, Long.MaxValue)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isLeft)
    }

    // The ledger checks memory and steps independently (Alonzo pointWiseExUnits (<=)),
    // so exceeding either component alone must fail.
    test("ExUnitsTooBigValidator failure when only steps exceed the maximum") {
        val context = Context()
        val state = State()
        val max = context.env.params.maxTxExecutionUnits
        val exUnits = ExUnits(memory = max.memory / 2, steps = max.steps + 1)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isLeft)
    }

    test("ExUnitsTooBigValidator failure when only memory exceeds the maximum") {
        val context = Context()
        val state = State()
        val max = context.env.params.maxTxExecutionUnits
        val exUnits = ExUnits(memory = max.memory + 1, steps = max.steps / 2)
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isLeft)
    }

    test("ExUnitsTooBigValidator success at exactly the maximum") {
        val context = Context()
        val state = State()
        val max = context.env.params.maxTxExecutionUnits
        val tx =
            randomTransactionWithIsValidField
                .withWitness(
                  TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, max))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isRight)
    }
}
