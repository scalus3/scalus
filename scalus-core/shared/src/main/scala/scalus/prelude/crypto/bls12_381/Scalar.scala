package scalus.prelude.crypto.bls12_381

import scalus.Compile
import scalus.builtin.Builtins.{byteStringToInteger, integerToByteString}
import scalus.builtin.{ByteString, PlatformSpecific}
import scalus.prelude.Option.{None, Some}
import scalus.prelude.{require, Option}

import scala.annotation.tailrec

/** This is an opaque wrapper around BigInt that represents an element (integer) in the scalar field
  * of BLS12-381 curve. Use it when you need modular arithmetics over it.
  *
  * See ScalarTest for examples.
  *
  * @param unScalar
  *   underlying BigInt representation
  */
case class Scalar private[bls12_381] (private val unScalar: BigInt)

@Compile
object Scalar:
    /** The prime number defining the scalar field of the BLS12-381 curve. */
    val fieldPrime: BigInt = PlatformSpecific.bls12_381_scalar_period

    /** Returns a Scalar instance representing zero in the field. */
    def zero: Scalar = new Scalar(0)

    /** Returns a Scalar instance representing one in the field. */
    def one: Scalar = new Scalar(1)

    /** Creates a new Scalar from a BigInt value.
      *
      * @param n
      *   the BigInt value to convert
      * @return
      *   Some(Scalar) if the value is within the valid field range, None otherwise
      */
    def apply(n: BigInt): Option[Scalar] =
        if n >= 0 && n < fieldPrime then Some(new Scalar(n))
        else None

    /** Creates a new Scalar from a BigInt value without safety checks, throws when the argument is
      * not valid.
      *
      * @param n
      *   the BigInt value to convert
      * @return
      *   Scalar instance
      * @throws IllegalArgumentException
      *   if the value is not in the valid field range
      */
    def applyUnsafe(n: BigInt): Scalar =
        require(0 <= n && n < fieldPrime, "value not in the field")
        new Scalar(n)

    /** Creates a new Scalar from a string representation of a number.
      *
      * @param s
      *   the string representation of the number
      * @return
      *   Some(Scalar) if the value is within the valid field range, None otherwise
      */
    inline def apply(inline s: String): Option[Scalar] = Scalar(BigInt(s))

    /** Creates a Scalar from a big-endian byte string representation.
      *
      * @param bytes
      *   the big-endian byte string to convert
      * @return
      *   Some(Scalar) if the value is within the valid field range, None otherwise
      */
    def fromByteStringBigEndian(bytes: ByteString): Option[Scalar] =
        Scalar(byteStringToInteger(true, bytes))

    /** Creates a Scalar from a big-endian byte string representation without safety checks.
      *
      * @param bytes
      *   the big-endian byte string to convert
      * @return
      *   Scalar instance
      * @throws IllegalArgumentException
      *   if the value is not in the valid field range
      */
    def fromByteStringBigEndianUnsafe(bytes: ByteString): Scalar =
        Scalar.applyUnsafe(byteStringToInteger(true, bytes))

    /** Creates a Scalar from a little-endian byte string representation.
      *
      * @param bytes
      *   the little-endian byte string to convert
      * @return
      *   Some(Scalar) if the value is within the valid field range, None otherwise
      */
    def fromByteStringLittleEndian(bytes: ByteString): Option[Scalar] =
        Scalar(byteStringToInteger(false, bytes))

    /** Creates a Scalar from a little-endian byte string representation without safety checks.
      *
      * @param bytes
      *   the little-endian byte string to convert
      * @return
      *   Scalar instance
      * @throws IllegalArgumentException
      *   if the value is not in the valid field range
      */
    def fromByteStringLittleEndianUnsafe(bytes: ByteString): Scalar =
        Scalar.applyUnsafe(byteStringToInteger(false, bytes))

    extension (self: Scalar)
        /** Adds two Scalar elements in the finite field.
          *
          * @param addend
          *   the Scalar to add
          * @return
          *   a new Scalar representing the sum
          */
        infix def +(addend: Scalar): Scalar =
            new Scalar((self.unScalar + addend.unScalar) % fieldPrime)

        /** Multiplies two Scalar elements in the finite field.
          *
          * @param multiplier
          *   the Scalar to multiply by
          * @return
          *   a new Scalar representing the product
          */
        infix def *(multiplier: Scalar): Scalar =
            new Scalar(self.unScalar * multiplier.unScalar % fieldPrime)

        /** Divides one Scalar by another in the finite field.
          *
          * @param divisor
          *   the Scalar to divide by
          * @return
          *   Some(Scalar) representing the quotient, or None if divisor is zero
          */
        infix def /(divisor: Scalar): Option[Scalar] = {
            if divisor.unScalar == zero.unScalar then None
            else Some(self * divisor.scale(fieldPrime - 2))
        }

        /** Subtracts one Scalar from another in the finite field.
          *
          * @param subtrahend
          *   the Scalar to subtract
          * @return
          *   a new Scalar representing the difference
          */
        infix def -(subtrahend: Scalar): Scalar =
            val difference = self.unScalar - subtrahend.unScalar
            new Scalar(
              if difference >= 0 then difference
              else fieldPrime + difference
            )

        /** Raises the Scalar to a power using repeated squaring algorithm.
          *
          * @param e
          *   the exponent
          * @return
          *   a new Scalar representing `self^e`
          * @note
          *   Returns zero for negative exponents
          */
        def scale(e: BigInt): Scalar =
            if e < 0 then zero
            else if e == BigInt(0) then one
            else if e % 2 == BigInt(0) then (self * self).scale(e / 2)
            else self * (self * self).scale((e - 1) / 2)

        /** Optimized power operation for powers of 2.
          *
          * @param k
          *   the power of 2 exponent (i.e., calculating `self^(2^k)`)
          * @return
          *   a new Scalar representing `self^(2^k)`
          * @note
          *   Returns zero for negative k
          */
        def scale2(k: BigInt): Scalar =
            @tailrec
            def go(self: Scalar, k: BigInt): Scalar =
                if k == BigInt(0) then self
                else go(self * self, k - 1)

            if k < 0 then zero
            else go(self, k)

        /** Returns the additive inverse of this Scalar.
          *
          * @return
          *   a new Scalar representing `-self`
          */
        def unary_- : Scalar =
            if self.unScalar == BigInt(0) then self
            else new Scalar(fieldPrime - self.unScalar)

        /** Calculates the multiplicative inverse of this Scalar.
          *
          * @return
          *   Some(Scalar) representing the inverse, or None if this Scalar is zero
          */
        def recip: Option[Scalar] = one / self

        /** Converts this Scalar to its BigInt representation.
          *
          * @return
          *   the BigInt value representing this Scalar
          */
        def toInt: BigInt = self.unScalar

        /** Converts this Scalar to a big-endian byte string representation.
          *
          * @param size
          *   the desired size of the resulting byte string
          * @return
          *   ByteString representation in big-endian format
          */
        def toByteStringBigEndian(size: BigInt): ByteString =
            integerToByteString(true, size, self.unScalar)

        /** Converts this Scalar to a little-endian byte string representation.
          *
          * @param size
          *   the desired size of the resulting byte string
          * @return
          *   ByteString representation in little-endian format
          */
        def toByteStringLittleEndian(size: BigInt): ByteString =
            integerToByteString(false, size, self.unScalar)

end Scalar
