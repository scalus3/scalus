package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory

class OutsideValidityIntervalValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test("OutsideValidityInterval - ValidityInterval(None, None) success") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutsideValidityInterval - ValidityInterval(None, Some(1000)) slot = 999 success") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutsideValidityInterval - ValidityInterval(None, Some(1000)) slot = 1000 failure") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutsideValidityInterval - ValidityInterval(Some(500), None) slot = 500 success") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutsideValidityInterval - ValidityInterval(Some(500), None) slot = 499 failure") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutsideValidityInterval - ValidityInterval(Some(500), Some(1000)) slot = 700 success") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("OutsideValidityInterval - ValidityInterval(Some(500), Some(1000)) slot = 499 failure") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("OutsideValidityInterval - ValidityInterval(Some(500), Some(1000)) slot = 1000 failure") {
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

        val emulator = TestEmulatorFactory.create(
          context = context,
          validators = Seq(OutsideValidityIntervalValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
