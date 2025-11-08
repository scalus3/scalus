package scalus.builtin

import scalus.Compile
import scalus.prelude.*
import scalus.serialization.flat.Flat
import scalus.uplc.CommonFlatInstances
import scalus.utils.Hex.toHex

import scala.annotation.threadUnsafe
import scala.compiletime.asMatchable

/** An immutable sequence of bytes.
  *
  * This class is used as a UPLC builtin type to represent binary data and it can be used in both
  * onchain and offchain code.
  *
  * @note
  *   methods of this class are only available offchain with the exception of `==` and `!=`.
  *
  * For onchain operations use the extension methods in the companion object
  */
class ByteString private[builtin] (val bytes: Array[Byte]) {

    /** Gets byte by index
      *
      * Offchain operation, not available onchain. Use [[ByteString.at]] for onchain access.
      */
    def apply(i: Int): Byte = bytes(i)

    override def toString: String = "\"" + toHex + "\""

    override def hashCode: Int = java.util.Arrays.hashCode(bytes)

    /** Compares two ByteStrings for equality
      *
      * Offchain operation, not available onchain. Use `==` for onchain equality checks.
      */
    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
        case _                => false
    }

    /** Converts the ByteString to a hexadecimal string representation
      *
      * Offchain operation, not available onchain.
      */
    @threadUnsafe val toHex: String = bytes.toHex

    /** Converts the ByteString to a binary string representation
      *
      * Offchain operation, not available onchain.
      */
    def toBinaryString: String = bytes.view
        .map(b => String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0'))
        .mkString("")

    /** Concatenates two ByteStrings and returns a new ByteString
      *
      * Offchain operation, not available onchain.
      */
    def concat(that: ByteString): ByteString = new ByteString(bytes ++ that.bytes)

    /** The length of the ByteString
      *
      * Offchain operation, not available onchain.
      */
    def size: Int = bytes.length
}

@Compile
object ByteString extends ByteStringOffchainApi, ByteStringFlatInstance {
    given Show[ByteString] = (x: ByteString) =>
        Builtins.appendString(Builtins.appendString("\"", Prelude.encodeHex(x)), "\"")

    given Eq[ByteString] = (x: ByteString, y: ByteString) => Builtins.equalsByteString(x, y)

    given Ord[ByteString] = (x: ByteString, y: ByteString) =>
        if Builtins.lessThanByteString(x, y) then Order.Less
        else if Builtins.equalsByteString(x, y) then Order.Equal
        else Order.Greater

    /** Encodes a BigInt value as a Big-Endian (most-significant bytes first) `ByteString`.
      * @param value
      *   is the value to encode.
      * @param size
      *   is the expected size in number of bytes.
      * @note
      *   This function fails (i.e. halts the program) if the value cannot fit in the given size.
      *   When the size is _too large_, the ByteString is left-padded with zeroes.
      * @example
      *   {{{
      *   ByteString.fromBigIntBigEndian(1_000_000, 3) == hex"0f4240"
      *   ByteString.fromBigIntBigEndian(1_000_000, 5) == hex"00000f4240"
      *   ByteString.fromBigIntBigEndian(0, 8) == hex"0000000000000000"
      *   ByteString.fromBigIntBigEndian(1_000_000, 1) => 💥
      *   }}}
      */
    inline def fromBigIntBigEndian(value: BigInt, size: BigInt = 0): ByteString =
        Builtins.integerToByteString(true, size, value)

    /** Encodes a BigInt value as a Little-Endian (least-significant bytes first) `ByteString`.
      *
      * @param value
      *   is the value to encode.
      * @param size
      *   is the expected size in number of bytes.
      * @example
      *   {{{
      *   ByteString.fromBigIntLittleEndian(1_000_000, 3) == hex"40420f"
      *   ByteString.fromBigIntLittleEndian(1_000_000, 5) == hex"40420f0000"
      *   ByteString.fromBigIntLittleEndian(0, 8) == hex"0000000000000000"
      *   ByteString.fromBigIntLittleEndian(1_000_000, 1) => 💥
      *   }}}
      */
    inline def fromBigIntLittleEndian(value: BigInt, size: BigInt = 0): ByteString =
        Builtins.integerToByteString(false, size, value)

    // TODO it's not truly overriding and doesn't work in generic function,
    //  see extension methods <, <=, >, >=, equiv, nonEquiv for [A: Ord]

    // These methods are available onchain
    // Because we make them inline and those are builtins
    // we don't need to compile this module (so no @Compile on ByteString)
    extension (self: ByteString)
        /** Checks if one 'ByteString' is less than another */
        inline infix def <(inline that: ByteString): Boolean =
            Builtins.lessThanByteString(self, that)

        /** Checks if one 'ByteString' is less than or equal to another */
        inline infix def <=(inline that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(self, that)

        /** Checks if one 'ByteString' is greater than another */
        inline infix def >(inline that: ByteString): Boolean =
            Builtins.lessThanByteString(that, self)

        /** Checks if one 'ByteString' is greater than or equal to another */
        inline infix def >=(inline that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(that, self)

        /** Checks if two 'ByteString's are equal */
        inline infix def equiv(inline that: ByteString): Boolean =
            Builtins.equalsByteString(self, that)

        /** Checks if two 'ByteString's are not equal */
        inline infix def nonEquiv(inline that: ByteString): Boolean =
            !Builtins.equalsByteString(self, that)

        /** Returns a new ByteString that is a slice of the original ByteString
          *
          * @param from
          *   the starting index of the slice (inclusive)
          * @param len
          *   the length of the slice
          * @example
          *   {{{
          *   hex"1234567890abcdef".slice(2, 4) // returns hex"567890ab"
          *   hex"1234567890abcdef".slice(5, 4) // returns hex"abcdef"
          *   hex"1234567890abcdef".slice(9, 4) // returns hex""
          *   hex"1234567890abcdef".slice(0, 0) // returns hex""
          *   }}}
          */
        inline def slice(from: BigInt, len: BigInt): ByteString =
            Builtins.sliceByteString(from, len, self)

        /** Takes the first `n` bytes from the ByteString
          *
          * @param n
          *   the number of bytes to take
          * @example
          *   {{{
          *   hex"1234567890abcdef".take(0) // returns hex""
          *   hex"1234567890abcdef".take(4) // returns hex"12345678"
          *   hex"1234567890abcdef".take(20) // returns hex"1234567890abcdef"
          *   hex"".take(20) // returns hex""
          *   }}}
          */
        inline def take(n: BigInt): ByteString = Builtins.sliceByteString(0, n, self)

        /** Drops the first `n` bytes from the ByteString
          *
          * @param n
          *   the number of bytes to drop
          * @example
          *   {{{
          *   hex"1234567890abcdef".drop(0) // returns hex"1234567890abcdef"
          *   hex"1234567890abcdef".drop(4) // returns hex"90abcdef"
          *   hex"1234567890abcdef".drop(20) // returns hex""
          *   hex"".drop(20) // returns hex""
          *   }}}
          */
        def drop(n: BigInt): ByteString = {
            self.slice(n, self.length - n)
        }

        /** Returns the byte at the specified index in the ByteString
          *
          * @throws BuiltinException
          *   if the index is out of bounds (offchain)
          */
        inline def at(index: BigInt): BigInt = Builtins.indexByteString(self, index)

        /** Returns the length of the ByteString */
        inline def length: BigInt = Builtins.lengthOfByteString(self)

        /** Checks if this ByteString is empty */
        inline def isEmpty: Boolean = self.length == BigInt(0)

        /** Checks if this ByteString is empty */
        inline def nonEmpty: Boolean = !isEmpty

        /** Concatenates two ByteStrings and returns a new ByteString */
        inline infix def ++(that: ByteString): ByteString =
            Builtins.appendByteString(self, that)

    extension (b: BigInt)
        /** Prepends a BigInt to a ByteString and returns a new ByteString */
        inline infix def +:(bs: ByteString): ByteString = Builtins.consByteString(b, bs)
}

private trait ByteStringFlatInstance {
    given Flat[ByteString] = CommonFlatInstances.given_Flat_ByteString
}
