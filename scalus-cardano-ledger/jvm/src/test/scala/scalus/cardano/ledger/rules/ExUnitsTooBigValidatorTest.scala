package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.cardano.ledger.RedeemerTag.Spend

class ExUnitsTooBigValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("ExUnitsTooBigValidator success") {
        val context = Context()
        val state = State()
        val exUnits = ExUnits(1, 1)
        val tx =
            randomTransactionWithIsValidField
                .copy(
                  witnessSet = TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBigValidator failure") {
        val context = Context()
        val state = State()
        val exUnits = ExUnits(Long.MaxValue, Long.MaxValue)
        val tx =
            randomTransactionWithIsValidField
                .copy(
                  witnessSet = TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(context, state, tx)
        assert(result.isLeft)
    }
}
