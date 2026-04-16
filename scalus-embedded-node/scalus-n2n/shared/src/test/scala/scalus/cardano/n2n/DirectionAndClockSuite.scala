package scalus.cardano.n2n

import org.scalatest.funsuite.AnyFunSuite

/** Small tests for the two supporting primitives that go alongside `Sdu`: [[Direction]]
  * and [[MonotonicClock]].
  */
class DirectionAndClockSuite extends AnyFunSuite {

    test("Direction.fromBit — 0 ⇒ Initiator, 1 ⇒ Responder; higher bits ignored") {
        assert(Direction.fromBit(0) == Direction.Initiator)
        assert(Direction.fromBit(1) == Direction.Responder)
        assert(Direction.fromBit(2) == Direction.Initiator) // bit 0 clear
        assert(Direction.fromBit(3) == Direction.Responder) // bit 0 set
    }

    test("Direction bit field — Initiator is 0, Responder is 1") {
        assert(Direction.Initiator.bit == 0)
        assert(Direction.Responder.bit == 1)
    }

    test("MonotonicClock.fixed always returns the supplied value") {
        val c = MonotonicClock.fixed(12345L)
        assert(c.nowMicros() == 12345L)
        assert(c.nowMicros() == 12345L)
    }

    test("MonotonicClock.counting increments by 1 µs per read") {
        val c = MonotonicClock.counting(start = 100L)
        assert(c.nowMicros() == 100L)
        assert(c.nowMicros() == 101L)
        assert(c.nowMicros() == 102L)
    }

    test("MonotonicClock.system is monotonically non-decreasing across successive reads") {
        val c = MonotonicClock.system
        val a = c.nowMicros()
        val b = c.nowMicros()
        val d = c.nowMicros()
        assert(a <= b && b <= d, s"expected non-decreasing, got $a, $b, $d")
    }
}
