package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.uplc.builtin.Data

class ExUnitsTooBigValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val max = Context().env.params.maxTxExecutionUnits

    // The rule reads only the redeemers, so a fixed spending transaction serves.
    private def spendWith(exUnits: ExUnits): Transaction =
        txSpending(input(0))(
          TransactionWitnessSet(
            redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))
          )
        )

    test("ExUnitsTooBigValidator success") {
        val result = validate(ExUnitsTooBigValidator, Map.empty, spendWith(ExUnits(1, 1)))
        assert(result.isRight)
    }

    test("ExUnitsTooBigValidator success with no redeemers") {
        val tx = txSpending(input(0))(TransactionWitnessSet(redeemers = None))
        val result = validate(ExUnitsTooBigValidator, Map.empty, tx)
        assert(result.isRight)
    }

    test("ExUnitsTooBigValidator failure") {
        val exUnits = ExUnits(Long.MaxValue, Long.MaxValue)
        val result = validate(ExUnitsTooBigValidator, Map.empty, spendWith(exUnits))
        assert(result.isLeft)
    }

    // The ledger checks memory and steps independently (Alonzo pointWiseExUnits (<=)),
    // so exceeding either component alone must fail.
    test("ExUnitsTooBigValidator failure when only steps exceed the maximum") {
        val exUnits = ExUnits(memory = max.memory / 2, steps = max.steps + 1)
        val result = validate(ExUnitsTooBigValidator, Map.empty, spendWith(exUnits))
        assert(result.isLeft)
    }

    test("ExUnitsTooBigValidator failure when only memory exceeds the maximum") {
        val exUnits = ExUnits(memory = max.memory + 1, steps = max.steps / 2)
        val result = validate(ExUnitsTooBigValidator, Map.empty, spendWith(exUnits))
        assert(result.isLeft)
    }

    test("ExUnitsTooBigValidator success at exactly the maximum") {
        val result = validate(ExUnitsTooBigValidator, Map.empty, spendWith(max))
        assert(result.isRight)
    }
}
