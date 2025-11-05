package scalus.prelude

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins.byteStringToInteger
import scalus.builtin.Builtins.integerToByteString
import scalus.builtin.Builtins.shiftByteString
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex
import scalus.prelude.Option.None
import scalus.prelude.Option.Some

import scala.annotation.tailrec

@Compile
object Math:

    /** Calculate the absolute value of an integer.
      *
      * @example
      *   {{{
      *   abs(-3) == 3
      *   abs(5) == 5
      *   }}}
      * @return
      *   The absolute value.
      */
    inline def abs(x: BigInt): BigInt = if x >= 0 then x else -x

    /** Returns the smaller of two integers.
      *
      * @example
      *   {{{
      *   min(1, 2) == 1
      *   min(-1, -5) == -5
      *   }}}
      * @return
      *   The minimum.
      */
    inline def min(x: BigInt, y: BigInt): BigInt = if x <= y then x else y

    /** Returns the larger of two integers.
      *
      * @example
      *   {{{
      *   man(1, 2) == 2
      *   man(-1, -5) == -1
      *   }}}
      * @return
      *   The maximum.
      */
    inline def max(x: BigInt, y: BigInt): BigInt = if x >= y then x else y

    /** Restrict the value of an integer between two min and max bounds
      *
      * @param self
      *   an integer to restrict
      * @param min
      *   lowest allowed value.
      * @param max
      *   highest allowed value.
      *
      * @example
      *   {{{
      *   clamp(self = 14, min = 0, max = 10) == 10
      *   clamp(self = -4, min = 0, max = 10) == 0
      *   clamp(self = 7, min = 0, max = 10) == 7
      *   }}}
      * @return
      *   `self` clamped to `[min, max]`.
      */
    def clamp(self: BigInt, min: BigInt, max: BigInt): BigInt =
        if self < min then min
        else if self > max then max
        else self

    /** The greatest common divisor of this and another integer.
      *
      * @param self
      *   A number to compute GCD with.
      * @param other
      *   The other number to compute GCD with.
      *
      * @example
      *   {{{
      *   gcd(42, 14) == 14
      *   gcd(14, 42) == 14
      *   gcd(0, 0) == 0
      *   }}}
      * @return
      *   The greatest common divisor.
      */
    def gcd(x: BigInt, y: BigInt): BigInt =
        @tailrec
        def go(a: BigInt, b: BigInt): BigInt =
            if b == BigInt(0) then a else go(b, a % b)
        go(x, y).absolute

    /** Calculates the square root of an integer using the babylonian method.
      *
      * @see
      *   [[https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method Babylonian method]].
      *
      * @note
      *   This function can be quite expensive to perform on-chain. Prefer using
      *   [[scalus.prelude.isSqrt]] whenever possible.
      *
      * @param radicand
      *   The number to take the square root of
      *
      * @example
      *   {{{
      *   sqrt(0) == Some(0)
      *   sqrt(25) == Some(5)
      *   sqrt(44203) == Some(210)
      *   sqrt(-42) == None
      *   }}}
      * @return
      *   The exact result or the smallest integer nearest to the square root. `None` for negative
      *   values.
      */
    def sqrt(radicand: BigInt): Option[BigInt] =
        if radicand < 0 then None
        else if radicand <= 1 then Some(radicand)
        else Some(sqrtBabylonian(radicand, radicand, (radicand + 1) / 2))

    /** Babylonian (Heron) method for integer square root.
      *
      * Internal helper, returns integer approximation.
      *
      * The basic idea is that if x is an overestimate to the square root of a non-negative real
      * number S then S/x will be an underestimate, or vice versa, and so the average of these two
      * numbers may reasonably be expected to provide a better approximation (though the formal
      * proof of that assertion depends on the inequality of arithmetic and geometric means that
      * shows this average is always an overestimate of the square root.
      *
      * @param radicand
      *   The number to take the square root of
      * @param x
      *   Current guess
      * @param y
      *   Next guess
      *
      * @return
      *   integer approximation of `sqrt(radicand)`
      */
    @annotation.tailrec
    private def sqrtBabylonian(radicand: BigInt, x: BigInt, y: BigInt): BigInt =
        if y >= x then x
        else sqrtBabylonian(radicand, y, (y + radicand / y) / 2)

    /** Checks if an integer has a given integer square root `x`.
      *
      * The check has constant time complexity `O(1)`.
      *
      * @example
      *   {{{
      *   isSqrt(0, 0) == true
      *   isSqrt(25, 5) == true
      *   isSqrt(25, -5) == false
      *   isSqrt(44203, 210) == true
      *   }}}
      * @return
      *   True if it is a perfect square, false otherwise.
      */
    def isSqrt(self: BigInt, x: BigInt): Boolean =
        val inc = x + 1
        x * x <= self && inc * inc > self

    /** Raises an integer to a power using the exponentiation by squaring method.
      *
      * @param base
      *   The base to be raised to a power.
      * @param exp
      *   The power's exponent to raise.
      *
      * @example
      *   {{{
      *   pow(3, 5) == 243
      *   pow(7, 2) == 49
      *   pow(3, -4) == 0
      *   pow(0, 0) == 1
      *   pow(513, 3) == 135005697
      *   }}}
      * @return
      *   `base` raised to the power of `exp`.
      */
    def pow(base: BigInt, exp: BigInt): BigInt = if exp < 0 then 0
    else
        @tailrec
        def go(ret: BigInt, n: BigInt, e: BigInt): BigInt =
            if e == BigInt(0) then ret
            else if e % 2 == BigInt(0) then go(ret, n * n, e / 2)
            else go(ret * n, n * n, (e - 1) / 2)
        go(1, base, exp)

    /** Calculates the power of `2` for a given exponent.
      *
      * @note
      *   Much cheaper than using `pow(base = 2, _)` for small exponents `0 < exp < 256`.
      * @param exp
      *   The power's exponent to raise.
      *
      * @example
      *   {{{
      *   exp2(-2) == 0
      *   exp2(0) == 1
      *   exp2(1) == 2
      *   exp2(4) == 16
      *   exp2(42) == 4398046511104
      *   }}}
      * @return
      *   `2` raised to the power of `exp`.
      */
    def exp2(exp: BigInt): BigInt = if exp < 0 then 0
    else
        byteStringToInteger(
          true,
          shiftByteString(hex"01", exp % BigInt(8)) ++
              integerToByteString(true, exp / BigInt(8), BigInt(0))
        )

    /** The integer logarithm in base `2`.
      *
      * @note
      *   Faster than `log(_, base = 2)` in this particular case.
      * @param self
      *   logarithm argument
      *
      * @example
      *   {{{
      *   log2(1) == 0
      *   log2(2) == 1
      *   log2(3) == 1
      *   log2(4) == 2
      *   log2(256) == 8
      *   log2(257) == 8
      *   log2(511) == 8
      *   log2(1025) == 10
      *   }}}
      * @return
      *   Greatest integer `k` such that `2^k <= self`.
      */
    def log2(self: BigInt): BigInt = if self <= 0 then 0
    else
        val s = integerToByteString(true, 0, self)
        val n =
            val b = s.at(0)
            if b < BigInt(2) then BigInt(8)
            else if b < BigInt(4) then BigInt(7)
            else if b < BigInt(8) then BigInt(6)
            else if b < BigInt(16) then BigInt(5)
            else if b < BigInt(32) then BigInt(4)
            else if b < BigInt(64) then BigInt(3)
            else if b < BigInt(128) then BigInt(2)
            else BigInt(1)
        s.length * BigInt(8) - n

    /** Computes a logarithm in a given base using integer divisions.
      *
      * @param self
      *   logarithm argument
      * @param base
      *   Base of logarithm.
      *
      * @example
      *   {{{
      *   log(10, base = 2) == 3
      *   log(42, base = 2) == 5
      *   log(42, base = 3) == 3
      *   log(5, base = 0) == 0
      *   log(4, base = 4) == 1
      *   log(4, base = 42) == 0
      *   }}}
      * @return
      *   Greatest integer `k` such that `base^k <= self`.
      */
    def log(self: BigInt, base: BigInt): BigInt = if base <= 0 then 0
    else
        @tailrec
        def go(ret: BigInt, n: BigInt): BigInt =
            if n < base then ret else go(1 + ret, n / base)
        go(ret = 0, n = self)

extension (self: BigInt)

    /** Calculate the absolute value of the integer.
      *
      * @param other
      *   The other number to compute ABS with.
      *
      * @see
      *   [[scalus.prelude.Math.abs]]
      */
    inline def absolute: BigInt = Math.abs(x = self)

    /** Returns the smaller of this and another integer.
      *
      * @param other
      *   The other number to compute MIN with.
      *
      * @see
      *   [[scalus.prelude.Math.min]]
      */
    inline def minimum(other: BigInt): BigInt = Math.min(self, other)

    /** Returns the larger of this and another integer.
      *
      * @param other
      *   The other number to compute MAX with.
      *
      * @see
      *   [[scalus.prelude.Math.maximum]]
      */
    inline def maximum(other: BigInt): BigInt = Math.max(self, other)

    /** Restrict the value of this integer between two bounds
      *
      * @param min
      *   lowest allowed value.
      * @param max
      *   highest allowed value.
      *
      * @see
      *   [[scalus.prelude.Math.clamp]]
      */
    inline def clamp(min: BigInt, max: BigInt): BigInt = Math.clamp(self, min, max)

    /** The greatest common factor aka divisor of this and another integer.
      *
      * @param other
      *   The other integer to compute GCD with.
      *
      * @see
      *   [[scalus.prelude.Math.gcd]]
      */
    inline def gcf(other: BigInt): BigInt = Math.gcd(self, other)

    /** Calculates the square root of this integer using the babylonian method.
      *
      * @see
      *   [[scalus.prelude.Math.sqrt]]
      */
    inline def sqRoot: Option[BigInt] = Math.sqrt(self)

    /** Checks if this integer has a given integer square root `x`.
      *
      * @param x
      *   square root to check.
      *
      * @see
      *   [[scalus.prelude.Math.isSqrt]]
      */
    inline def isSqrt(x: BigInt): Boolean = Math.isSqrt(self, x)

    /** Raises this integer to a power using the exponentiation by squaring method.
      *
      * @param exp
      *   The exponent of power to raise.
      *
      * @see
      *   [[scalus.prelude.Math.pow]]
      */
    inline def pow(exp: BigInt): BigInt = Math.pow(self, exp)

    /** Calculates the power of `2` for this exponent.
      *
      * @see
      *   [[scalus.prelude.Math.exp2]]
      */
    inline def exp2: BigInt = Math.exp2(self)

    /** The integer logarithm in base `2`.
      *
      * @see
      *   [[scalus.prelude.Math.log2]]
      */
    inline def log2: BigInt = Math.log2(self)

    /** Computes the logarithm in a given base using integer divisions.
      *
      * @param base
      *   Base of logarithm.
      *
      * @see
      *   [[scalus.prelude.Math.log]]
      */
    inline def logarithm(base: BigInt): BigInt = Math.log(self, base)
