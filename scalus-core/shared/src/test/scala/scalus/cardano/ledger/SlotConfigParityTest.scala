package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.nowarn

/** Pins slot arithmetic to the same answers on every platform.
  *
  * `SlotConfig` has no shared definition: `jvm/`, `js/` and `native/` each declare their own class
  * under this same fully-qualified name, and they do not agree on types. JVM and Native take and
  * return `Long`; JavaScript takes and returns `Double`, and its `timeToSlot` is documented as
  * returning a *fractional* slot. Shared code that calls either one therefore compiles against two
  * different signatures, and every caller in `shared/` writes `.toLong` by hand to paper over it.
  *
  * Nothing is known to be wrong today - the values involved (times near 1.6e12, slots near 1e8)
  * stay far below 2^53, where `Double` is exact, and both platforms truncate toward zero. But that
  * is an invariant nobody wrote down and no test checked, in code that computes the validity bounds
  * a Plutus script sees. This suite writes it down.
  *
  * Every expectation below is a literal, computed by hand from the network constants rather than
  * from the formula under test - a test that recomputes `zeroTime + (slot - zeroSlot) * slotLength`
  * to check `slotToTime` passes for any implementation of it, including a wrong one.
  */
// The `.toLong` on each result, and the `Long` arguments, are the point: this is the exact idiom
// the thirteen callers in `shared/` use. On JVM and Native the conversions are identity; on
// JavaScript the argument widens to `Double` and the result truncates back. That widening is the
// deprecated `long2double`, which is the divergence under test, so its warning is expected here.
@nowarn("cat=deprecation")
class SlotConfigParityTest extends AnyFunSuite {

    test("mainnet: a slot maps to the time its epoch anchor implies") {
        val c = SlotConfig.mainnet
        // Shelley begins at slot 4492800 == 1596059091000 ms, the anchor the config is built on.
        assert(c.slotToTime(4492800L).toLong == 1596059091000L)
        // 100 slots later, at one second per slot.
        assert(c.slotToTime(4492900L).toLong == 1596059191000L)
    }

    test("mainnet: a time maps back to its slot, truncating within the slot") {
        val c = SlotConfig.mainnet
        assert(c.timeToSlot(1596059091000L).toLong == 4492800L)
        assert(c.timeToSlot(1596059191000L).toLong == 4492900L)
        // 999 ms into slot 4492900. This is the discriminating case: JVM and Native divide two
        // `Long`s and get 100; JavaScript divides two `Double`s and gets 100.999, which `.toLong`
        // truncates. Both must land on the slot that has begun, never the next one.
        assert(c.timeToSlot(1596059191999L).toLong == 4492900L)
    }

    test("mainnet: epochs are counted from the anchor epoch, not from zero") {
        val c = SlotConfig.mainnet
        // The anchor slot is the first slot of epoch 208, and anything at or before it clamps.
        assert(c.epochOf(4492800L).toLong == 208L)
        assert(c.epochOf(4492799L).toLong == 208L)
        // 432000 slots is one mainnet epoch, so the next boundary is 4924800.
        assert(c.epochOf(4924799L).toLong == 208L)
        assert(c.epochOf(4924800L).toLong == 209L)
        assert(c.firstSlotOfEpoch(209L).toLong == 4924800L)
    }

    test("preprod: anchored at slot 86400, epoch 4, after four Byron epochs") {
        val c = SlotConfig.preprod
        assert(c.slotToTime(86400L).toLong == 1655769600000L)
        assert(c.timeToSlot(1655769600000L).toLong == 86400L)
        assert(c.epochOf(86400L).toLong == 4L)
        assert(c.epochOf(518399L).toLong == 4L)
        assert(c.epochOf(518400L).toLong == 5L)
        assert(c.firstSlotOfEpoch(5L).toLong == 518400L)
    }

    test("preview: anchored at slot 0 with one-day epochs") {
        val c = SlotConfig.preview
        assert(c.slotToTime(0L).toLong == 1666656000000L)
        assert(c.timeToSlot(1666656000000L).toLong == 0L)
        // 86400 slots of one second is a day, which is a whole epoch on preview.
        assert(c.slotToTime(86400L).toLong == 1666742400000L)
        assert(c.epochOf(86399L).toLong == 0L)
        assert(c.epochOf(86400L).toLong == 1L)
        assert(c.firstSlotOfEpoch(1L).toLong == 86400L)
    }

    test("slotToTime and timeToSlot invert each other on a slot boundary") {
        for c <- Seq(SlotConfig.mainnet, SlotConfig.preprod, SlotConfig.preview) do
            for slot <- Seq(0L, 1L, 100000L, 150_000_000L) do
                assert(c.timeToSlot(c.slotToTime(slot)).toLong == slot)
    }

    test("the network anchors are the documented ones") {
        // Pins the constants the expectations above are computed from, so a change to a network
        // config fails here - naming the cause - rather than only in the arithmetic tests.
        assert(SlotConfig.mainnet.slotToTime(0L).toLong == 1596059091000L - 4492800L * 1000L)
        assert(SlotConfig.preview.slotToTime(0L).toLong == 1666656000000L)
        assert(SlotConfig.preprod.slotToTime(0L).toLong == 1655769600000L - 86400L * 1000L)
    }
}
