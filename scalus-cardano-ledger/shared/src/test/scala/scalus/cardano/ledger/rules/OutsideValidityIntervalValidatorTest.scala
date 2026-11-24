package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class OutsideValidityIntervalValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // Each test sets its own slot, so it calls the validator with its own context, spec
    // [kit.rules.own-context]. The rule reads only the validity interval of the transaction.
    private def txWithin(validityInterval: ValidityInterval): Transaction =
        txSpending(input(0))(TransactionWitnessSet.empty).withBody(
          _.copy(
            validityStartSlot = validityInterval.invalidBefore,
            ttl = validityInterval.invalidHereafter
          )
        )

    test("OutsideValidityIntervalValidator ValidityInterval(None, None) success") {
        val context = Context()

        val validityInterval = ValidityInterval(
          invalidBefore = None,
          invalidHereafter = None
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(None, Some(1000)) slot = 999 success"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 999L))

        val validityInterval = ValidityInterval(
          invalidBefore = None,
          invalidHereafter = Some(1000L)
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(None, Some(1000)) slot = 1000 failure"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 1000L))

        val validityInterval = ValidityInterval(
          invalidBefore = None,
          invalidHereafter = Some(1000L)
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(Some(500), None) slot = 500 success"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 500L))

        val validityInterval = ValidityInterval(
          invalidBefore = Some(500L),
          invalidHereafter = None
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(Some(500), None) slot = 499 failure"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 499L))

        val validityInterval = ValidityInterval(
          invalidBefore = Some(500L),
          invalidHereafter = None
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(Some(500), Some(1000)) slot = 700 success"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 700L))

        val validityInterval = ValidityInterval(
          invalidBefore = Some(500L),
          invalidHereafter = Some(1000L)
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(Some(500), Some(1000)) slot = 499 failure"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 499L))

        val validityInterval = ValidityInterval(
          invalidBefore = Some(500L),
          invalidHereafter = Some(1000L)
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "OutsideValidityIntervalValidator ValidityInterval(Some(500), Some(1000)) slot = 1000 failure"
    ) {
        val context = Context(env = UtxoEnv.default.copy(slot = 1000L))

        val validityInterval = ValidityInterval(
          invalidBefore = Some(500L),
          invalidHereafter = Some(1000L)
        )

        val transaction = txWithin(validityInterval)

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
