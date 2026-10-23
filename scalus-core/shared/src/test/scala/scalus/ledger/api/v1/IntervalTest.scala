package scalus.cardano.onchain.plutus.v1

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.testing.kit.EvalTestKit

import scala.annotation.nowarn

@nowarn("msg=entirelyAfter .* is deprecated")
class IntervalTest
    extends AnyFunSuite
    with EvalTestKit
    with scalus.cardano.onchain.plutus.v1.ArbitraryInstances {

    test("`never` contains no values") {
        checkEval { (time: PosixTime) =>
            !Interval.never.contains(time)
        }
    }

    test("`always` contains all values") {
        checkEval { (time: PosixTime) =>
            Interval.always.contains(time)
        }
    }

    test("point interval contains only its point") {
        checkEval { (time: PosixTime) =>
            val interval = Interval.between(time, time)
            interval.contains(time) &&
            !interval.contains(time + 1) &&
            !interval.contains(time - 1)
        }
    }

    test("interval contains values between its bounds") {
        checkEval { (time: PosixTime) =>
            val interval = Interval.before(time)
            interval.contains(time) &&
            !interval.contains(time + 1) &&
            interval.contains(time - 1)
        }

        checkEval { (time: PosixTime) =>
            val interval = Interval.entirelyBefore(time)
            !interval.contains(time) &&
            !interval.contains(time + 1) &&
            interval.contains(time - 1)
        }

        checkEval { (time: PosixTime) =>
            val interval = Interval.after(time)
            interval.contains(time) &&
            interval.contains(time + 1) &&
            !interval.contains(time - 1)
        }

        forAll { (from: PosixTime, to: PosixTime) =>
            whenever(from < to) {
                val interval = Interval.between(from, to)
                interval.contains(from) &&
                interval.contains(to)
            }
        }

        forAll { (from: PosixTime, to: PosixTime) =>
            whenever(from < to) {
                val interval = Interval.entirelyBetween(from, to)
                !interval.contains(from) &&
                !interval.contains(to)
            }
        }

        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.contains(100) && interval.contains(BigInt(200))
        }

        assertEval {
            val interval = Interval.entirelyBetween(BigInt(100), BigInt(200))
            !interval.contains(100) && !interval.contains(BigInt(200)) &&
            interval.contains(101) && interval.contains(BigInt(199))
        }
    }

    test("isEntirelyAfter") {
        forAll { (from: PosixTime, to: PosixTime, testTime: PosixTime) =>
            whenever(from <= to) {
                val interval = Interval.between(from, to)

                if testTime < from then interval.isEntirelyAfter(testTime)
                else if testTime >= to then !interval.isEntirelyAfter(testTime)
                else interval.contains(testTime)
            }
        }
        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.isEntirelyAfter(BigInt(50)) && !interval.isEntirelyAfter(
              BigInt(150)
            ) && !interval
                .isEntirelyAfter(250)
        }
    }

    test("isEntirelyBefore") {
        forAll { (from: PosixTime, to: PosixTime, testTime: PosixTime) =>
            whenever(from <= to) {
                val interval = Interval.between(from, to)

                if testTime > to then interval.isEntirelyBefore(testTime)
                else if testTime <= from then !interval.isEntirelyBefore(testTime)
                else interval.contains(testTime)
            }
        }
        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.isEntirelyBefore(250) && !interval.isEntirelyBefore(BigInt(150)) && !interval
                .isEntirelyBefore(BigInt(50))
        }
    }

    test("isEntirelyBetween") {
        forAll { (from: PosixTime, to: PosixTime, after: PosixTime, before: PosixTime) =>
            val orderedFrom = from.min(to)
            val orderedTo = from.max(to)
            val orderedAfter = after.min(before)
            val orderedBefore = after.max(before)

            val interval = Interval.between(orderedFrom, orderedTo)
            val isEntirelyBetween = interval.isEntirelyBetween(orderedAfter, orderedBefore)

            isEntirelyBetween === (interval.isEntirelyAfter(orderedAfter) && interval
                .isEntirelyBefore(
                  orderedBefore
                ))
        }

        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            val isEntirelyBetween = interval.isEntirelyBetween(BigInt(50), BigInt(250))
            isEntirelyBetween === (interval.isEntirelyAfter(BigInt(50)) && interval
                .isEntirelyBefore(
                  BigInt(250)
                ))
        }
    }

    test("never interval isn't entirely after any time") {
        checkEval { (time: PosixTime) =>
            !Interval.never.isEntirelyAfter(time)
        }
    }

    test("never interval isn't entirely before any time") {
        checkEval { (time: PosixTime) =>
            !Interval.never.isEntirelyBefore(time)
        }
    }

    test("hull") {
        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(150), BigInt(250))
            val hull = Interval.hull(i1, i2)
            hull.from.boundType === IntervalBoundType.Finite(BigInt(100)) &&
            hull.to.boundType === IntervalBoundType.Finite(BigInt(250))
        }

        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(50), BigInt(150))
            val hull = Interval.hull(i1, i2)
            hull.from.boundType === IntervalBoundType.Finite(BigInt(50)) &&
            hull.to.boundType === IntervalBoundType.Finite(BigInt(200))
        }

        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(300), BigInt(400))
            val hull = Interval.hull(i1, i2)
            hull.from.boundType === IntervalBoundType.Finite(BigInt(100)) &&
            hull.to.boundType === IntervalBoundType.Finite(BigInt(400))
        }

        assertEval {
            val i1 = Interval.before(BigInt(200))
            val i2 = Interval.after(BigInt(150))
            val hull = Interval.hull(i1, i2)
            hull.from.boundType === IntervalBoundType.NegInf &&
            hull.to.boundType === IntervalBoundType.PosInf
        }

        assertEval {
            val i1 = Interval.never
            val i2 = Interval.between(BigInt(100), BigInt(200))
            val hull = Interval.hull(i1, i2)
            hull === i2
        }
    }

    test("intersection") {
        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(150), BigInt(250))
            val intersection = Interval.intersection(i1, i2)
            intersection === Interval.between(BigInt(150), BigInt(200))
        }

        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(50), BigInt(150))
            val intersection = Interval.intersection(i1, i2)
            intersection === Interval.between(BigInt(100), BigInt(150))
        }

        assertEval {
            val i1 = Interval.between(BigInt(100), BigInt(200))
            val i2 = Interval.between(BigInt(300), BigInt(400))
            val intersection = Interval.intersection(i1, i2)
            intersection.isNever
        }

        assertEval {
            val i1 = Interval.before(BigInt(200))
            val i2 = Interval.after(BigInt(150))
            val intersection = Interval.intersection(i1, i2)
            intersection === Interval.between(BigInt(150), BigInt(200))
        }

        assertEval {
            val i1 = Interval.never
            val i2 = Interval.between(BigInt(100), BigInt(200))
            val intersection = Interval.intersection(i1, i2)
            intersection.isNever
        }
    }

    test("isNever") {
        assertEval {
            Interval.between(BigInt(200), BigInt(100)).isNever
        }
        assertEval {
            Interval.never.isNever
        }
    }

    test("nonNever") {
        assertEval {
            Interval.between(BigInt(100), BigInt(200)).nonNever
        }
    }

    test("intersection keeps an exclusive lower bound at a shared time") {
        assertEval {
            val i1 = Interval.between(BigInt(5), BigInt(10))
            val i2 = Interval(IntervalBound.finiteExclusive(5), IntervalBound.finiteInclusive(10))
            Interval.intersection(i1, i2) === i2 && Interval.intersection(i2, i1) === i2
        }
    }

    test("hull keeps an inclusive lower bound at a shared time") {
        assertEval {
            val i1 = Interval.between(BigInt(5), BigInt(10))
            val i2 = Interval(IntervalBound.finiteExclusive(5), IntervalBound.finiteInclusive(10))
            Interval.hull(i1, i2) === i1 && Interval.hull(i2, i1) === i1
        }
    }

    test("isNever is true for a half-open interval at one time") {
        assertEval {
            Interval(IntervalBound.finiteExclusive(5), IntervalBound.finiteInclusive(5)).isNever &&
            Interval(IntervalBound.finiteInclusive(5), IntervalBound.finiteExclusive(5)).isNever &&
            Interval(IntervalBound.negInf, IntervalBound.negInf).isNever &&
            Interval(IntervalBound.posInf, IntervalBound.posInf).isNever
        }
    }

    // Bound times in 0..3 with a random inclusivity and no ordering, so bounds at one time with
    // mixed inclusivity, open intervals (t, t+1) and never-intervals are all common.
    // Every interval then changes membership only inside -1..4, so `probes` decides it.
    private val genSmallBound: Gen[IntervalBound] =
        for
            boundType <- Gen.frequency(
              (1, Gen.const(IntervalBoundType.NegInf)),
              (1, Gen.const(IntervalBoundType.PosInf)),
              (8, Gen.choose(0, 3).map(t => IntervalBoundType.Finite(BigInt(t))))
            )
            isInclusive <- Arbitrary.arbitrary[Boolean]
        yield IntervalBound(boundType, isInclusive)

    private val smallIntervals: Arbitrary[Interval] = Arbitrary(
      for from <- genSmallBound; to <- genSmallBound yield Interval(from, to)
    )

    test("intersection, hull and isNever agree with contains") {
        given Arbitrary[Interval] = smallIntervals
        checkEval { (a: Interval, b: Interval) =>
            val probes = PList(BigInt(-1), BigInt(0), BigInt(1), BigInt(2), BigInt(3), BigInt(4))
            val intersection = Interval.intersection(a, b)
            val hull = Interval.hull(a, b)
            probes.forall { t =>
                intersection.contains(t) === (a.contains(t) && b.contains(t)) &&
                (!(a.contains(t) || b.contains(t)) || hull.contains(t))
            } &&
            a.isNever === !probes.exists(t => a.contains(t))
        }
    }
}
