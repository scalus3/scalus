package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant
import scala.annotation.nowarn

@nowarn("msg=(toLong is redundant|long2double)")
class SlotConfigTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("slotToInstant converts slot to Instant correctly for mainnet") {
        val config = SlotConfig.mainnet
        val slot = 4492800L // zeroSlot
        val instant = config.slotToInstant(slot)
        assert(instant.toEpochMilli == 1596059091000L)
    }

    test("slotToInstant converts slot to Instant correctly for preview") {
        val config = SlotConfig.preview
        val slot = 0L // zeroSlot
        val instant = config.slotToInstant(slot)
        assert(instant.toEpochMilli == 1666656000000L)
    }

    test("slotToInstant converts slot to Instant correctly for preprod") {
        val config = SlotConfig.preprod
        val slot = 86400L // zeroSlot
        val instant = config.slotToInstant(slot)
        assert(instant.toEpochMilli == 1654041600000L + 1728000000L)
    }

    test("instantToSlot converts Instant to slot correctly for mainnet") {
        val config = SlotConfig.mainnet
        val instant = Instant.ofEpochMilli(1596059091000L)
        val slot = config.instantToSlot(instant)
        assert(slot == 4492800L)
    }

    test("instantToSlot converts Instant to slot correctly for preview") {
        val config = SlotConfig.preview
        val instant = Instant.ofEpochMilli(1666656000000L)
        val slot = config.instantToSlot(instant)
        assert(slot == 0L)
    }

    test("instantToSlot converts Instant to slot correctly for preprod") {
        val config = SlotConfig.preprod
        val instant = Instant.ofEpochMilli(1654041600000L + 1728000000L)
        val slot = config.instantToSlot(instant)
        assert(slot == 86400L)
    }

    test("round-trip slot -> instant -> slot returns same slot") {
        val config = SlotConfig.mainnet
        val mainnetZeroSlot = 4492800L // mainnet zeroSlot
        forAll { (offset: Int) =>
            val slot = mainnetZeroSlot + math.abs(offset % 1000000)
            val instant = config.slotToInstant(slot)
            val resultSlot = config.instantToSlot(instant)
            assert(resultSlot == slot)
        }
    }

    test("round-trip instant -> slot -> instant returns same instant (within slot precision)") {
        val config = SlotConfig.mainnet
        val mainnetZeroSlot = 4492800L // mainnet zeroSlot
        // Instants aligned to slot boundaries round-trip exactly
        forAll { (offset: Int) =>
            val slot = mainnetZeroSlot + math.abs(offset % 1000000)
            val instant = config.slotToInstant(slot)
            val resultSlot = config.instantToSlot(instant)
            val resultInstant = config.slotToInstant(resultSlot)
            assert(resultInstant == instant)
        }
    }

    test("slotToInstant handles slots after zeroSlot") {
        val config = SlotConfig.mainnet
        val slot = 4492800L + 1000 // 1000 slots after zeroSlot
        val instant = config.slotToInstant(slot)
        // Each slot is 1000ms, so 1000 slots = 1000000ms later
        assert(instant.toEpochMilli == 1596059091000L + 1000000L)
    }

    test("instantToSlot handles time after zeroTime") {
        val config = SlotConfig.mainnet
        val instant = Instant.ofEpochMilli(1596059091000L + 1000000L) // 1000000ms after zeroTime
        val slot = config.instantToSlot(instant)
        // 1000000ms / 1000ms per slot = 1000 slots after zeroSlot
        assert(slot == 4492800L + 1000)
    }

    test("slotToInstant and slotToTime are consistent") {
        val config = SlotConfig.mainnet
        val mainnetZeroSlot = 4492800L // mainnet zeroSlot
        forAll { (offset: Int) =>
            val slot = mainnetZeroSlot + math.abs(offset % 1000000)
            val instant = config.slotToInstant(slot)
            val time = config.slotToTime(slot)
            assert(instant.toEpochMilli == time.toLong)
        }
    }

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

    test("instantToSlot and timeToSlot are consistent") {
        val config = SlotConfig.mainnet
        val mainnetZeroTime = 1596059091000L // mainnet zeroTime
        forAll { (offset: Int) =>
            val time = mainnetZeroTime + math.abs(offset % 1000000000L)
            val instant = Instant.ofEpochMilli(time)
            val slotFromInstant = config.instantToSlot(instant)
            val slotFromTime = config.timeToSlot(time)
            assert(slotFromInstant.toLong == slotFromTime.toLong)
        }
    }
}
