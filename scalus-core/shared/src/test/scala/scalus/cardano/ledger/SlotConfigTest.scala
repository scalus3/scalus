package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.annotation.nowarn

/** Slot and epoch arithmetic, which every platform shares.
  *
  * The `java.time.Instant` conversions live on the JVM and Native `SlotConfig` only, so their tests
  * are in `scalus-core/jvm`'s `SlotConfigInstantTest`.
  */
@nowarn("msg=(toLong is redundant|long2double)")
class SlotConfigTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("epochOf derives correct epochs for mainnet") {
        val config = SlotConfig.mainnet
        // Shelley start: slot 4492800 = start of epoch 208
        assert(config.epochOf(4492800L).toLong == 208L)
        assert(config.epochOf(4492800L + 431999L).toLong == 208L)
        assert(config.epochOf(4492800L + 432000L).toLong == 209L)
        assert(config.epochOf(4492800L + 292L * 432000L).toLong == 500L)
        // Slots before the Shelley era are clamped to zeroEpoch
        assert(config.epochOf(0L).toLong == 208L)
    }

    test("epochOf derives correct epochs for preprod") {
        val config = SlotConfig.preprod
        // Shelley start: slot 86400 = start of epoch 4 (after 4 Byron epochs)
        assert(config.epochOf(86400L).toLong == 4L)
        assert(config.epochOf(86400L + 431999L).toLong == 4L)
        assert(config.epochOf(86400L + 432000L).toLong == 5L)
        assert(config.epochOf(0L).toLong == 4L)
    }

    test("epochOf derives correct epochs for preview") {
        val config = SlotConfig.preview
        // Preview has 1-day epochs (86400 slots)
        assert(config.epochOf(0L).toLong == 0L)
        assert(config.epochOf(86399L).toLong == 0L)
        assert(config.epochOf(86400L).toLong == 1L)
    }

    test("epochOf is the inverse of firstSlotOfEpoch at epoch boundaries") {
        val config = SlotConfig.mainnet
        forAll { (offset: Int) =>
            val epoch = 208L + math.abs(offset % 1000)
            val firstSlot = config.firstSlotOfEpoch(epoch)
            assert(config.epochOf(firstSlot).toLong == epoch)
            assert(config.epochOf(firstSlot + 431999L).toLong == epoch)
        }
    }
}
