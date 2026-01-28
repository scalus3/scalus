package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class OutsideValidityIntervalValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("OutsideValidityIntervalValidator ValidityInterval(None, None) success") {
        val context = Context()

        val validityInterval = ValidityInterval(
          invalidBefore = None,
          invalidHereafter = None
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

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

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  validityStartSlot = validityInterval.invalidBefore,
                  ttl = validityInterval.invalidHereafter
                )
              )
            )
        }

        val state = State()

        val result = OutsideValidityIntervalValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
