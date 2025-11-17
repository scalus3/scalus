package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.serialization.flat.{w7l, DecoderState, EncoderState, Flat}

import java.math.BigInteger

/** A case class representing an unsigned 64-bit integer.
  *
  * This type uses a signed `Long` underneath but provides operations that interpret the bits as an
  * unsigned 64-bit value, giving access to the full range 0 to 2^64-1.
  *
  * The underlying `Long` storage means that values from 0 to `Long.MAX_VALUE`
  * (9,223,372,036,854,775,807) are stored as positive longs, while values from `Long.MAX_VALUE + 1`
  * to `2^64 - 1` are stored as negative longs using two's complement representation.
  *
  * Example usage:
  * ```scala
  * val small = Word64(1000L)
  * val large = Word64.fromUnsignedString("18446744073709551615") // 2^64 - 1
  *
  * println(small.toUnsignedString) // "1000"
  * println(large.toUnsignedString) // "18446744073709551615"
  * println(large.toLong) // -1 (signed interpretation)
  *
  * // Arithmetic works correctly with unsigned semantics
  * val sum = small + large
  * println(sum.toUnsignedString) // "999"
  *
  * // Comparison uses unsigned semantics
  * println(large.compareUnsigned(small) > 0) // true
  * ```
  */
case class Word64(value: Long) {

    /** Returns the underlying Long value (signed interpretation).
      *
      * For values > Long.MAX_VALUE, this returns a negative number due to two's complement
      * representation.
      *
      * @return
      *   the underlying Long value
      */
    def toLong: Long = value

    /** Converts this Word64 to a BigInteger with unsigned interpretation.
      *
      * This always returns a non-negative BigInteger representing the true unsigned value.
      *
      * @return
      *   BigInteger representation of the unsigned value
      */
    def toBigInteger: BigInteger =
        if value >= 0 then BigInteger.valueOf(value)
        else BigInteger.valueOf(value).add(BigInteger.ONE.shiftLeft(64))

    /** Returns the string representation of this unsigned 64-bit value.
      *
      * @return
      *   string representation of the unsigned value
      */
    def toUnsignedString: String = java.lang.Long.toUnsignedString(value)

    /** Returns the hexadecimal string representation of this unsigned 64-bit value.
      *
      * @return
      *   hexadecimal string representation (without "0x" prefix)
      */
    def toHexString: String = java.lang.Long.toUnsignedString(value, 16)

    /** Compares this Word64 with another using unsigned comparison.
      *
      * @param other
      *   the Word64 to compare with
      * @return
      *   negative if this < other, zero if equal, positive if this > other
      */
    def compareUnsigned(other: Word64): Int =
        java.lang.Long.compareUnsigned(value, other.value)

    /** Returns true if this Word64 represents a value greater than Long.MAX_VALUE.
      *
      * Such values are stored as negative longs in two's complement.
      *
      * @return
      *   true if the value exceeds Long.MAX_VALUE
      */
    def isLargeUnsigned: Boolean = value < 0

    /** Unsigned addition. Overflow wraps around.
      *
      * @param other
      *   the value to add
      * @return
      *   the sum as Word64
      */
    def +(other: Word64): Word64 = Word64(value + other.value)

    /** Unsigned subtraction. Underflow wraps around.
      *
      * @param other
      *   the value to subtract
      * @return
      *   the difference as Word64
      */
    def -(other: Word64): Word64 = Word64(value - other.value)

    /** Unsigned multiplication. Overflow wraps around.
      *
      * @param other
      *   the value to multiply by
      * @return
      *   the product as Word64
      */
    def *(other: Word64): Word64 = Word64(value * other.value)

    /** Unsigned division.
      *
      * @param other
      *   the divisor
      * @return
      *   the quotient as Word64
      * @throws java.lang.ArithmeticException
      *   if other is zero
      */
    def divideUnsigned(other: Word64): Word64 =
        Word64(java.lang.Long.divideUnsigned(value, other.value))

    /** Unsigned remainder (modulo).
      *
      * @param other
      *   the divisor
      * @return
      *   the remainder as Word64
      * @throws java.lang.ArithmeticException
      *   if other is zero
      */
    def remainderUnsigned(other: Word64): Word64 =
        Word64(java.lang.Long.remainderUnsigned(value, other.value))

    /** Bitwise AND operation.
      *
      * @param other
      *   the value to AND with
      * @return
      *   the result as Word64
      */
    def &(other: Word64): Word64 = Word64(value & other.value)

    /** Bitwise OR operation.
      *
      * @param other
      *   the value to OR with
      * @return
      *   the result as Word64
      */
    def |(other: Word64): Word64 = Word64(value | other.value)

    /** Bitwise XOR operation.
      *
      * @param other
      *   the value to XOR with
      * @return
      *   the result as Word64
      */
    def ^(other: Word64): Word64 = Word64(value ^ other.value)

    /** Bitwise NOT operation.
      *
      * @return
      *   the bitwise complement as Word64
      */
    def unary_~ : Word64 = Word64(~value)

    /** Left shift operation.
      *
      * @param bits
      *   number of bits to shift (only lower 6 bits are used)
      * @return
      *   the result as Word64
      */
    def <<(bits: Int): Word64 = Word64(value << bits)

    /** Logical right shift operation (fills with zeros).
      *
      * @param bits
      *   number of bits to shift (only lower 6 bits are used)
      * @return
      *   the result as Word64
      */
    def >>>(bits: Int): Word64 = Word64(value >>> bits)

    /** Arithmetic right shift operation (sign extends, but use >>> for unsigned).
      *
      * @param bits
      *   number of bits to shift (only lower 6 bits are used)
      * @return
      *   the result as Word64
      */
    def >>(bits: Int): Word64 = Word64(value >> bits)
}

object Word64 {

    /** Creates a Word64 from an unsigned string representation.
      *
      * @param s
      *   string representation of an unsigned 64-bit integer
      * @return
      *   a Word64 representing the parsed value
      * @throws NumberFormatException
      *   if the string is not a valid unsigned long
      */
    def fromUnsignedString(s: String): Word64 =
        Word64(java.lang.Long.parseUnsignedLong(s))

    /** Creates a Word64 from a BigInteger.
      *
      * @param bi
      *   the BigInteger value
      * @return
      *   a Word64 representing the value
      * @throws IllegalArgumentException
      *   if the value is negative or exceeds 64 bits
      */
    def fromBigInteger(bi: BigInteger): Word64 =
        if bi.signum() < 0 then throw new IllegalArgumentException(s"Value $bi is negative")
        if bi.bitLength() > 64 then
            throw new IllegalArgumentException(s"Value $bi exceeds 64-bit range")
        Word64(bi.longValue())

    /** Creates a Word64 from an unsigned integer, promoting it to 64 bits.
      *
      * @param value
      *   the unsigned 32-bit integer
      * @return
      *   a Word64 representing the value
      */
    def fromUnsignedInt(value: Int): Word64 = Word64(java.lang.Integer.toUnsignedLong(value))

    /** Word64 representing zero */
    val Zero: Word64 = Word64(0L)

    /** Word64 representing the maximum 64-bit unsigned value (2^64 - 1) */
    val MaxValue: Word64 = Word64(-1L)

    /** Word64 representing one */
    val One: Word64 = Word64(1L)

    /** CBOR encoder for Word64.
      *
      * Encodes small values (≤ Long.MAX_VALUE) as CBOR integers, and large values as CBOR big
      * integers for proper unsigned semantics.
      */
    given Encoder[Word64] with
        def write(writer: Writer, w64: Word64): Writer =
            if w64.isLargeUnsigned then
                // For values > Long.MAX_VALUE, encode as BigInteger to preserve unsigned semantics
                writer.write(w64.toBigInteger)
            else
                // For smaller values, encode as Long
                writer.writeLong(w64.toLong)

    /** CBOR decoder for Word64.
      *
      * Handles both regular CBOR integers and big integers, ensuring proper unsigned interpretation
      * of the decoded value.
      */
    given Decoder[Word64] with
        def read(reader: Reader): Word64 =
            import io.bullet.borer.DataItem as DI
            reader.dataItem() match
                case DI.Int | DI.Long =>
                    Word64(reader.readLong())
                case DI.OverLong =>
                    // Handle big integers that exceed Long range
                    Word64.fromBigInteger(reader.read[BigInteger]())
                case other =>
                    reader.validationFailure(s"Expected integer for Word64, got $other")

    /** Returns the number of bytes required to encode in flat this unsigned 64-bit value.
      *
      * @see
      *   PlutusCore/Flat/Encoder/Size.hs:89:1
      */
    def flatBytesCount(a: Word64): Int =
        // For negative longs (unsigned values >= 2^63), all need 10 bytes
        if a.isLargeUnsigned then 10
        else {
            // For positive longs, use direct comparisons (no BigInt allocation)
            if a.value < 128L then 1
            else if a.value < 16384L then 2
            else if a.value < 2097152L then 3
            else if a.value < 268435456L then 4
            else if a.value < 34359738368L then 5
            else if a.value < 4398046511104L then 6
            else if a.value < 562949953421312L then 7
            else if a.value < 72057594037927936L then 8
            else 9 // < 2^63
        }

    /** Flat encoder/decoder for Word64 using variable-length encoding.
      */
    given Flat[Word64] with {
        inline def bitSize(a: Word64): Int = flatBytesCount(a) * 8

        // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
        inline def encode(a: Word64, encode: EncoderState): Unit =
            for b <- w7l(a.value) do encode.bits(8, b)

        def decode(decode: DecoderState): Word64 = {
            var bs = List.empty[Byte]
            while
                val b = decode.bits8(8)
                bs = b :: bs
                (b & 0x80) != 0
            do ()
            val r = bs.foldLeft(0L): (a, b) =>
                (a << 7) + (b & 0x7f)
            Word64(r)
        }
    }
}
