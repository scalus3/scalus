package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.OnchainError
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit

class NormalizedIntervalTest extends AnyFunSuite with EvalTestKit {

    test("tryNormalize should return ClosedRange for finite bounds") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(100), true),
              to = IntervalBound(IntervalBoundType.Finite(200), true)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.ClosedRange(100, 200))
        }
    }

    test("tryNormalize should handle exclusive bounds for ClosedRange") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(10), false),
              to = IntervalBound(IntervalBoundType.Finite(20), false)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.ClosedRange(11, 19))
        }
    }

    test("tryNormalize should return FromNegInf for negative infinity lower bound") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.Finite(150), true)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.FromNegInf(150))
        }
    }

    test("tryNormalize should handle exclusive upper bound for FromNegInf") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.Finite(150), false)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.FromNegInf(149))
        }
    }

    test("tryNormalize should return ToPosInf for positive infinity upper bound") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(50), true),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.ToPosInf(50))
        }
    }

    test("tryNormalize should handle exclusive lower bound for ToPosInf") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(50), false),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.ToPosInf(51))
        }
    }

    test("tryNormalize should return Always for infinite bounds") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.tryNormalize === Option.Some(NormalizedInterval.Always)
        }
    }

    test("tryNormalize should return None for improper intervals") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(200), true),
              to = IntervalBound(IntervalBoundType.Finite(100), true)
            )
            interval.tryNormalize === Option.None
        }
    }

    test("tryNormalize should return None for NegInf to NegInf") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.NegInf, false)
            )
            interval.tryNormalize === Option.None
        }
    }

    test("tryNormalize should return None for PosInf lower bound") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.PosInf, false),
              to = IntervalBound(IntervalBoundType.Finite(100), true)
            )
            interval.tryNormalize === Option.None
        }
    }

    test("normalize should return ClosedRange for finite bounds") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(100), true),
              to = IntervalBound(IntervalBoundType.Finite(200), true)
            )
            interval.normalize === NormalizedInterval.ClosedRange(100, 200)
        }
    }

    test("normalize should handle exclusive bounds for ClosedRange") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(10), false),
              to = IntervalBound(IntervalBoundType.Finite(20), false)
            )
            interval.normalize === NormalizedInterval.ClosedRange(11, 19)
        }
    }

    test("normalize should return FromNegInf for negative infinity lower bound") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.Finite(150), true)
            )
            interval.normalize === NormalizedInterval.FromNegInf(150)
        }
    }

    test("normalize should handle exclusive upper bound for FromNegInf") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.Finite(150), false)
            )
            interval.normalize === NormalizedInterval.FromNegInf(149)
        }
    }

    test("normalize should return ToPosInf for positive infinity upper bound") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(50), true),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.normalize === NormalizedInterval.ToPosInf(50)
        }
    }

    test("normalize should handle exclusive lower bound for ToPosInf") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(50), false),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.normalize === NormalizedInterval.ToPosInf(51)
        }
    }

    test("normalize should return Always for infinite bounds") {
        assertEval {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.PosInf, false)
            )
            interval.normalize === NormalizedInterval.Always
        }
    }

    test("normalize should fail for improper intervals") {
        assertEvalFails[OnchainError] {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.Finite(200), true),
              to = IntervalBound(IntervalBoundType.Finite(100), true)
            )
            interval.normalize
        }
    }

    test("normalize should fail for NegInf to NegInf") {
        assertEvalFails[OnchainError] {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.NegInf, false),
              to = IntervalBound(IntervalBoundType.NegInf, false)
            )
            interval.normalize
        }
    }

    test("normalize should fail for PosInf lower bound") {
        assertEvalFails[OnchainError] {
            val interval = Interval(
              from = IntervalBound(IntervalBoundType.PosInf, false),
              to = IntervalBound(IntervalBoundType.Finite(100), true)
            )
            interval.normalize
        }
    }

    test("Eq instance should work correctly for ClosedRange") {
        assertEval {
            val interval1 = NormalizedInterval.ClosedRange(100, 200)
            val interval2 = NormalizedInterval.ClosedRange(100, 200)
            val interval3 = NormalizedInterval.ClosedRange(100, 300)

            (interval1 === interval2) &&
            (interval1 !== interval3)
        }
    }

    test("Eq instance should work correctly for different variants") {
        assertEval {
            val closedRange = NormalizedInterval.ClosedRange(100, 200)
            val fromNegInf = NormalizedInterval.FromNegInf(200)
            val toPosInf = NormalizedInterval.ToPosInf(100)
            val always = NormalizedInterval.Always

            (closedRange !== fromNegInf) &&
            (fromNegInf !== toPosInf) &&
            (toPosInf !== always) &&
            (always !== closedRange)
        }
    }

    test("Ord instance should order variants correctly") {
        assertEval {
            val closedRange = NormalizedInterval.ClosedRange(100, 200)
            val fromNegInf = NormalizedInterval.FromNegInf(200)
            val toPosInf = NormalizedInterval.ToPosInf(100)
            val always = NormalizedInterval.Always

            ((closedRange <=> fromNegInf) === Order.Less) &&
            ((fromNegInf <=> toPosInf) === Order.Less) &&
            ((toPosInf <=> always) === Order.Less) &&
            ((always <=> closedRange) === Order.Greater)
        }
    }

    test("Ord instance should compare fields within same variant") {
        assertEval {
            val range1 = NormalizedInterval.ClosedRange(100, 200)
            val range2 = NormalizedInterval.ClosedRange(100, 300)
            val range3 = NormalizedInterval.ClosedRange(200, 200)

            ((range1 <=> range2) === Order.Less) &&
            ((range1 <=> range3) === Order.Less) &&
            ((range2 <=> range1) === Order.Greater)
        }
    }

    test("Show instance should format ClosedRange correctly") {
        assertEval {
            val interval = NormalizedInterval.ClosedRange(100, 200)
            interval.show === "NormalizedInterval.ClosedRange(100, 200)"
        }
    }

    test("Show instance should format FromNegInf correctly") {
        assertEval {
            val interval = NormalizedInterval.FromNegInf(150)
            interval.show === "NormalizedInterval.FromNegInf(150)"
        }
    }

    test("Show instance should format ToPosInf correctly") {
        assertEval {
            val interval = NormalizedInterval.ToPosInf(50)
            interval.show === "NormalizedInterval.ToPosInf(50)"
        }
    }

    test("Show instance should format Always correctly") {
        assertEval {
            val interval = NormalizedInterval.Always
            interval.show === "NormalizedInterval.Always"
        }
    }
}
