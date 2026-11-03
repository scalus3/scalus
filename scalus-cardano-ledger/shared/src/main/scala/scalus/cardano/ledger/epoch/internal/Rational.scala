package scalus.cardano.ledger.epoch.internal

import scalus.cardano.ledger.{NonNegativeInterval, UnitInterval}

/** An exact rational over `BigInt` for the reward calculation, ledger-rules spec [RW-19], D74. The
  * denominator is positive; the terms are not reduced.
  */
private[epoch] final case class Rational(numerator: BigInt, denominator: BigInt) {
    require(denominator > 0, s"denominator must be positive, got $denominator")

    def +(that: Rational): Rational =
        Rational(
          numerator * that.denominator + that.numerator * denominator,
          denominator * that.denominator
        )

    def -(that: Rational): Rational =
        Rational(
          numerator * that.denominator - that.numerator * denominator,
          denominator * that.denominator
        )

    def *(that: Rational): Rational =
        Rational(numerator * that.numerator, denominator * that.denominator)

    /** `this / that`; `that` must not be 0. */
    def /(that: Rational): Rational =
        if that.numerator < 0 then
            Rational(-numerator * that.denominator, denominator * -that.numerator)
        else Rational(numerator * that.denominator, denominator * that.numerator)

    def <=(that: Rational): Boolean = numerator * that.denominator <= that.numerator * denominator

    def min(that: Rational): Rational = if this <= that then this else that

    /** The greatest integer at most `this`, the ledger's `floor`, toward −∞. */
    def floor: BigInt = {
        val (q, r) = numerator /% denominator
        if r < 0 then q - 1 else q
    }
}

private[epoch] object Rational {

    val zero: Rational = Rational(0, 1)

    val one: Rational = Rational(1, 1)

    def apply(n: BigInt): Rational = Rational(n, 1)

    def of(i: UnitInterval): Rational = Rational(i.numerator, i.denominator)

    def of(i: NonNegativeInterval): Rational = Rational(i.numerator, i.denominator)

    /** `n / d`, or 0 when `d` is 0 or below: the ledger's `%?` for `d = 0`, spec [RW-16]. */
    def ratio(n: BigInt, d: BigInt): Rational = if d <= 0 then zero else Rational(n, d)
}
