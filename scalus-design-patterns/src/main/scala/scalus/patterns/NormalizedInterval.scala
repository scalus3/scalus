package scalus.patterns

import scalus.Compile
import scalus.prelude.*
import scalus.ledger.api.v1.*
import scalus.builtin.{Builtins, FromData, ToData}

/** Datatype for eliminating meaningless ranges, without the redundant inclusiveness flag (instead
  * all range values are inclusive). Interval.never is represented as an improper interval.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/blob/main/validity-range-normalization/VALIDITY-RANGE-NORMALIZATION.md]]
  */
enum NormalizedInterval derives ToData, FromData:
    case ClosedRange(lower: PosixTime, upper: PosixTime)
    case FromNegInf(upper: PosixTime)
    case ToPosInf(lower: PosixTime)
    case Always

extension (self: Interval) {

    /** Try to normalize an `Interval` into a `NormalizedInterval`.
      *
      * @example
      *   {{{
      * // ClosedRange case
      * val closedInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.Finite(100), true),
      *   to = IntervalBound(IntervalBoundType.Finite(200), true)
      * )
      * closedInterval.tryNormalize // Some(ClosedRange(100, 200))
      *
      * // FromNegInf case
      * val fromNegInfInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.NegInf, false),
      *   to = IntervalBound(IntervalBoundType.Finite(150), true)
      * )
      * fromNegInfInterval.tryNormalize // Some(FromNegInf(150))
      *
      * // ToPosInf case
      * val toPosInfInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.Finite(50), true),
      *   to = IntervalBound(IntervalBoundType.PosInf, false)
      * )
      * toPosInfInterval.tryNormalize // Some(ToPosInf(50))
      *
      * // Always case
      * val alwaysInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.NegInf, false),
      *   to = IntervalBound(IntervalBoundType.PosInf, false)
      * )
      * alwaysInterval.tryNormalize // Some(Always)
      *
      * // Improper interval case
      * val improperInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.Finite(200), true),
      *   to = IntervalBound(IntervalBoundType.Finite(100), true)
      * )
      * improperInterval.tryNormalize // None
      *   }}}
      *
      * @return
      *   Some(NormalizedInterval) if the interval is proper, None otherwise i.e. Interval.never
      */
    inline def tryNormalize: Option[NormalizedInterval] =
        NormalizedInterval.tryNormalizedInterval(self)

    /** Normalize an `Interval` into a `NormalizedInterval` or fail if the interval is improper.
      *
      * @example
      *   {{{
      * // ClosedRange case
      * val closedInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.Finite(10), false),
      *   to = IntervalBound(IntervalBoundType.Finite(20), false)
      * )
      * closedInterval.normalize // ClosedRange(11, 19)
      *
      * // FromNegInf case
      * val fromNegInfInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.NegInf, false),
      *   to = IntervalBound(IntervalBoundType.Finite(100), false)
      * )
      * fromNegInfInterval.normalize // FromNegInf(99)
      *
      * // ToPosInf case
      * val toPosInfInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.Finite(75), false),
      *   to = IntervalBound(IntervalBoundType.PosInf, false)
      * )
      * toPosInfInterval.normalize // ToPosInf(76)
      *
      * // Always case
      * val alwaysInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.NegInf, false),
      *   to = IntervalBound(IntervalBoundType.PosInf, false)
      * )
      * alwaysInterval.normalize // Always
      *
      * // Improper interval case - throws error
      * val invalidInterval = Interval(
      *   from = IntervalBound(IntervalBoundType.PosInf, false),
      *   to = IntervalBound(IntervalBoundType.Finite(100), true)
      * )
      * invalidInterval.normalize // throws scalus.prelude.OnchainError: "Improper interval encountered"
      *   }}}
      *
      * @throws scalus.cardano.onchain.OnchainError
      *   if improper interval encountered i.e. Interval.never
      * @return
      *   NormalizedInterval
      */
    inline def normalize: NormalizedInterval = NormalizedInterval.normalizedInterval(self)
}

@Compile
object NormalizedInterval {
    given Show[NormalizedInterval] = {
        case ClosedRange(lower, upper) =>
            Builtins.appendString(
              "NormalizedInterval.ClosedRange(",
              Builtins.appendString(
                lower.show,
                Builtins.appendString(", ", Builtins.appendString(upper.show, ")"))
              )
            )

        case FromNegInf(upper) =>
            Builtins.appendString(
              "NormalizedInterval.FromNegInf(",
              Builtins.appendString(upper.show, ")")
            )

        case ToPosInf(lower) =>
            Builtins.appendString(
              "NormalizedInterval.ToPosInf(",
              Builtins.appendString(lower.show, ")")
            )

        case Always => "NormalizedInterval.Always"
    }

    given Eq[NormalizedInterval] = (lhs: NormalizedInterval, rhs: NormalizedInterval) =>
        lhs match
            case ClosedRange(x1, x2) =>
                rhs match
                    case ClosedRange(y1, y2) => (x1 === y1) && (x2 === y2)
                    case _                   => false
            case FromNegInf(x1) =>
                rhs match
                    case FromNegInf(y1) => x1 === y1
                    case _              => false
            case ToPosInf(x1) =>
                rhs match
                    case ToPosInf(y1) => x1 === y1
                    case _            => false
            case Always =>
                rhs match
                    case Always => true
                    case _      => false

    given Ord[NormalizedInterval] = (lhs: NormalizedInterval, rhs: NormalizedInterval) =>
        lhs match
            case ClosedRange(x1, x2) =>
                rhs match
                    case ClosedRange(y1, y2) =>
                        (x1 <=> y1) ifEqualThen (x2 <=> y2)
                    case _ => Order.Less
            case FromNegInf(x1) =>
                rhs match
                    case ClosedRange(_, _) => Order.Greater
                    case FromNegInf(y1)    => x1 <=> y1
                    case _                 => Order.Less
            case ToPosInf(x1) =>
                rhs match
                    case ClosedRange(_, _) => Order.Greater
                    case FromNegInf(_)     => Order.Greater
                    case ToPosInf(y1)      => x1 <=> y1
                    case Always            => Order.Less
            case Always =>
                rhs match
                    case Always => Order.Equal
                    case _      => Order.Greater

    def tryNormalizedInterval(interval: Interval): Option[NormalizedInterval] = {
        interval.from.boundType match
            case IntervalBoundType.NegInf =>
                interval.to.boundType match
                    case IntervalBoundType.NegInf => Option.None
                    case IntervalBoundType.Finite(upperTime) =>
                        Option.Some(
                          FromNegInf(
                            resolveUpper(upperTime, interval.to.isInclusive)
                          )
                        )
                    case IntervalBoundType.PosInf => Option.Some(Always)

            case IntervalBoundType.Finite(lowerTime) =>
                interval.to.boundType match
                    case IntervalBoundType.NegInf => Option.None
                    case IntervalBoundType.Finite(upperTime) =>
                        val lower = resolveLower(lowerTime, interval.from.isInclusive)
                        val upper = resolveUpper(upperTime, interval.to.isInclusive)

                        if lower > upper then Option.None
                        else Option.Some(ClosedRange(lower, upper))
                    case IntervalBoundType.PosInf =>
                        Option.Some(
                          ToPosInf(
                            resolveLower(lowerTime, interval.from.isInclusive)
                          )
                        )

            case IntervalBoundType.PosInf => Option.None
    }

    def normalizedInterval(interval: Interval): NormalizedInterval = {
        tryNormalizedInterval(interval) match
            case Option.Some(normalized) => normalized
            case Option.None             => fail("Improper interval encountered")
    }

    private def resolveLower(lower: BigInt, isInclusive: Boolean): BigInt = {
        if isInclusive then lower else lower + 1
    }

    private def resolveUpper(upper: BigInt, isInclusive: Boolean): BigInt = {
        if isInclusive then upper else upper - 1
    }
}
