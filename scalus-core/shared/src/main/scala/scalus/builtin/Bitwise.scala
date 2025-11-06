package scalus.builtin

import scalus.uplc.eval.BuiltinException

import scala.collection.mutable.ArrayBuffer

object IntegerToByteString:
    val maximumOutputLength: Int = 8192

    private def integerLog2(i: BigInt): Int =
        if i <= 0 then 0
        else i.bitLength - 1

    private enum IntegerToByteStringError:
        case NegativeInput, NotEnoughDigits

    /** Convert a [[BigInt]] into a [[ByteString]].
      *
      * The conversion uses fixed-width output and explicit endianness. If `lengthArg` is 0, the
      * result is a minimal-length encoding.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121 CIP-121]]
      *
      * @param endiannessArg
      *   `true` for big-endian output, `false` for little-endian output.
      *
      * @param lengthArg
      *   Desired output length in bytes. If zero, the result is minimally sized. If positive, the
      *   output must fit into the exact width, otherwise an exception is thrown.
      *
      * @param input
      *   Unsigned integer to convert. Negative integers are rejected.
      *
      * @throws BuiltinException
      *   If the requested length is negative, exceeds the maximum, or the integer cannot be
      *   represented in the requested number of bytes.
      *
      * @example
      *   {{{
      *   // Big-endian, length 2:
      *   integerToByteString(true, 2, 4660) == hex"1234"
      *
      *   // Little-endian, length 2:
      *   integerToByteString(false, 2, 4660) == hex"3412"
      *
      *   // Minimal representation:
      *   integerToByteString(true, 0, 4660) == hex"1234"
      *   }}}
      *
      * @return
      *   A byte string encoded with the requested width and endianness.
      *
      * @see
      *   [[scalus.builtin.ByteStringToInteger.byteStringToInteger]]
      */
    def integerToByteString(
        endiannessArg: Boolean,
        lengthArg: BigInt,
        input: BigInt
    ): ByteString =
        if lengthArg < 0 then
            throw new BuiltinException(
              s"integerToByteString: negative length argument\nLength requested: $lengthArg"
            )
        else if lengthArg > maximumOutputLength then
            throw new BuiltinException(
              s"integerToByteString: requested length is too long (maximum is $maximumOutputLength bytes)\nLength requested: $lengthArg"
            )
        else if lengthArg == 0 && integerLog2(input) >= 8 * maximumOutputLength then
            val bytesRequiredFor = (n: BigInt) => integerLog2(n) / 8 + 1
            throw new BuiltinException(
              s"integerToByteString: input too long (maximum is 2^${8 * maximumOutputLength}-1)\nLength required: ${bytesRequiredFor(input)}"
            )
        else
            val endianness = if endiannessArg then ByteOrder.BigEndian else ByteOrder.LittleEndian
            unsafeIntegerToByteString(endianness, lengthArg.toInt, input) match
                case Left(IntegerToByteStringError.NegativeInput) =>
                    throw new BuiltinException(
                      s"integerToByteString: cannot convert negative Integer\nInput: $input"
                    )
                case Left(IntegerToByteStringError.NotEnoughDigits) =>
                    throw new BuiltinException(
                      s"integerToByteString: cannot represent Integer in given number of bytes\nInput: $input\nBytes requested: $lengthArg"
                    )
                case Right(result) => result

    /** Internal CIP-121 conversion without range checks. */
    private def unsafeIntegerToByteString(
        requestedByteOrder: ByteOrder,
        requestedLength: Int,
        input: BigInt
    ): Either[IntegerToByteStringError, ByteString] =
        if input < 0 then Left(IntegerToByteStringError.NegativeInput)
        else if input == 0 then Right(ByteString.unsafeFromArray(new Array[Byte](requestedLength)))
        else if requestedLength == 0 then
            val result = requestedByteOrder match
                case ByteOrder.LittleEndian => goLENoLimit(input)
                case ByteOrder.BigEndian    => goLENoLimit(input).reverse
            Right(ByteString.unsafeFromArray(result.toArray))
        else
            val result = requestedByteOrder match
                case ByteOrder.LittleEndian => goLELimit(input, requestedLength)
                case ByteOrder.BigEndian    => goLELimit(input, requestedLength).map(_.reverse)
            result match
                case None        => Left(IntegerToByteStringError.NotEnoughDigits)
                case Some(bytes) => Right(ByteString.unsafeFromArray(bytes.toArray))

    /** Produce a little-endian byte sequence truncated/padded to a limit. */
    private def goLELimit(remaining: BigInt, requestedLength: Int): Option[ArrayBuffer[Byte]] =
        val builder = new ArrayBuffer[Byte](requestedLength)
        var current = remaining

        while current != 0 && builder.length < requestedLength do
            val byte = (current & 0xff).toByte
            builder += byte
            current = current >> 8

        if current == 0 then
            val result = builder.padTo(requestedLength, 0.toByte)
            Some(result)
        else None

    /** Produce minimal little-endian representation of a positive integer. */
    private def goLENoLimit(input: BigInt): ArrayBuffer[Byte] =
        val builder = new ArrayBuffer[Byte](input.bitLength / 8 + 1)
        var remaining = input

        var current = remaining

        while current != 0 do
            val byte = (current & 0xff).toByte
            builder += byte
            current = current >> 8

        builder

object ByteStringToInteger:
    enum ByteOrder:
        case LittleEndian, BigEndian

    /** Convert a [[ByteString]] into a non-negative [[BigInt]].
      *
      * Leading zero bytes are ignored. The interpretation is unsigned.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121 CIP-121]]
      *
      * @note
      *   This function mirrors `integerToByteString`.
      *
      * @param statedEndiannessArg
      *   `true` for big-endian interpretation, `false` for little-endian interpretation.
      *
      * @param input
      *   The byte string to convert. Empty or all-zero strings yield `0`.
      *
      * @example
      *   {{{
      *   // Big-endian
      *   byteStringToInteger(true, hex"1234") == 4660
      *
      *   // Little-endian
      *   byteStringToInteger(false, hex"3412") == 4660
      *
      *   // Leading zeros ignored
      *   byteStringToInteger(true,  hex"001234") == 4660
      *   }}}
      *
      * @return
      *   The unsigned integer represented by the input bytes.
      *
      * @see
      *   [[scalus.builtin.IntegerToByteString.integerToByteString]]
      */
    def byteStringToInteger(statedEndiannessArg: Boolean, input: ByteString): BigInt =
        val endianness = if statedEndiannessArg then ByteOrder.BigEndian else ByteOrder.LittleEndian
        unsafeByteStringToInteger(endianness, input)

    /** Internal CIP-121 conversion with explicit byte order handling. */
    private def unsafeByteStringToInteger(statedByteOrder: ByteOrder, input: ByteString): BigInt =
        val bytes = statedByteOrder match
            case ByteOrder.LittleEndian =>
                input.bytes.view.reverse
            case ByteOrder.BigEndian =>
                input.bytes.view
        val nonZeroInput = bytes.dropWhile(_ == 0)
        if nonZeroInput.isEmpty then BigInt(0)
        else goBE(nonZeroInput.toArray)

    /** Accumulate the big-endian integer value from a byte array. */
    private def goBE(input: Array[Byte]): BigInt =
        var result = BigInt(0)
        var i = 0
        while i < input.size do
            val byte = input(i).toInt & 0xff
            result = (result << 8) + BigInt(byte)
            i += 1
        result

    /** Drop trailing bytes matching a predicate when scanning from the end. */
    private def reverseTakeWhile(bs: ByteString, p: Byte => Boolean): Array[Byte] =
        var lastNonZeroIndex = bs.size - 1
        while lastNonZeroIndex >= 0 && p(bs.bytes(lastNonZeroIndex)) do lastNonZeroIndex -= 1
        if lastNonZeroIndex == -1 then Array.empty
        else bs.bytes.slice(0, lastNonZeroIndex + 1)

/** CIP-122 + shifts & rotations from CIP-123 */
object BitwiseLogicalOperations:
    /** Bitwise AND between two byte strings.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @note
      *   Bit 0 is the least-significant bit of the last byte.
      *
      * @param shouldPad
      *   `true` - padding semantics (pad the shorter input at high-index bytes). `false` -
      *   truncation semantics (truncate to the shorter input).
      *
      * @param lhs
      *   Left operand.
      *
      * @param rhs
      *   Right operand.
      *
      * @example
      *   {{{
      *   andByteString(false, hex"0f0f", hex"0fff") == hex"0f0f"
      *   andByteString(true,  hex"0f0f", hex"0fff") == hex"0f0f"
      *   }}}
      *
      * @return
      *   A new byte string representing the bitwise AND of the two inputs, using the chosen
      *   padding/truncation.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.orByteString]]
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.xorByteString]]
      */
    def andByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte & rhsByte).toByte)

    /** Bitwise OR between two byte strings.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @param shouldPad
      *   `true` - padding semantics (pad the shorter input at high-index bytes). `false` -
      *   truncation semantics (truncate to the shorter input).
      *
      * @param lhs
      *   Left operand.
      *
      * @param rhs
      *   Right operand.
      *
      * @example
      *   {{{
      *   orByteString(false, hex"0f0f", hex"f000") == hex"0000"
      *   orByteString(true,  hex"0f0f", hex"f000") == hex"ff0f"
      *   }}}
      *
      * @return
      *   A new byte string representing the bitwise OR of the two inputs, using the chosen
      *   padding/truncation.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.andByteString]]
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.xorByteString]]
      */
    def orByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte | rhsByte).toByte)

    /** Bitwise XOR between two byte strings.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @param shouldPad
      *   `true` - padding semantics (pad the shorter input at high-index bytes). `false` -
      *   truncation semantics (truncate to the shorter input).
      *
      * @param lhs
      *   Left operand.
      *
      * @param rhs
      *   Right operand.
      *
      * @example
      *   {{{
      *   xorByteString(false, hex"0f0f", hex"ffff") == hex"f0f0"
      *   xorByteString(true,  hex"0f0f", hex"ffff") == hex"f0f0"
      *   }}}
      *
      * @return
      *   A new byte string representing the bitwise XOR of the two inputs, using the chosen
      *   padding/truncation.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.andByteString]]
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.orByteString]]
      */
    def xorByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte ^ rhsByte).toByte)

    /** Bitwise NOT of a byte string (flip all bits).
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @param byteString
      *   Input byte string.
      *
      * @example
      *   {{{
      *   complementByteString(hex"00ff") == hex"ff00"
      *   }}}
      *
      * @return
      *   A new byte string where each byte is replaced by `0xff - byte`.
      */
    def complementByteString(byteString: ByteString): ByteString =
        transformByteString(byteString)(byte => (byte ^ 255).toByte)

    /** Read a single bit from a byte string. Bit index `0` refers to the least significant bit of
      * the last byte.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @note
      *   Bit indexed 0 is the least-significant bit of the last byte.
      *
      * @param byteString
      *   Input byte string.
      *
      * @param index
      *   Bit index in the range `[0, byteString.length * 8)`.
      *
      * @throws BuiltinException
      *   If the byte string is empty, or if the index is out of bounds.
      *
      * @example
      *   {{{
      *   // Last byte: 10010000
      *   readBit(hex"1098", 4) == true
      *   readBit(hex"1098", 0) == false
      *   }}}
      *
      * @return
      *   `true` if the bit is 1, `false` if the bit is 0.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.writeBits]]
      */
    def readBit(byteString: ByteString, index: BigInt): Boolean = {
        if byteString.isEmpty then
            throw new BuiltinException(
              s"readBit: Index out of bounds, because byte string is empty, actual: $index"
            )

        val bytes = byteString.bytes
        val bitLength = bytes.length * 8

        if index < 0 || index >= bitLength then
            throw new BuiltinException(
              s"readBit: Index out of bounds, expected: [0 .. $bitLength), actual: $index"
            )

        val currentIndex = index.toInt
        val byteIndex = (bitLength - 1 - currentIndex) / 8
        val bitIndex = currentIndex % 8
        ((bytes(byteIndex) >> bitIndex) & 1) == 1
    }

    /** Write multiple bits in a byte string.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]
      *
      * @note
      *   Bulk updates are intentionally more efficient than repeated `setBit` calls (see CIP-122
      *   design rationale).
      *
      * @note
      *   Bit indexed 0 is the least-significant bit of the last byte.
      *
      * @param byteString
      *   Input byte string to modify.
      *
      * @param indexes
      *   Sequence of bit indexes to change. Must be within bounds.
      *
      * @param bit
      *   The new bit value (`true` - set to 1, `false` - set to 0).
      *
      * @throws BuiltinException
      *   If the byte string is empty or any index lies outside the valid bit range.
      *
      * @example
      *   {{{
      *   writeBits(hex"00", List(0, 3), true) == hex"09" // Set bits 0 and 3 in a single byte
      *   }}}
      *
      * @return
      *   A new byte string with all specified bits updated.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.readBit]]
      */
    def writeBits(
        byteString: ByteString,
        indexes: Seq[BigInt],
        bit: Boolean
    ): ByteString = {
        if indexes.isEmpty then return byteString

        if byteString.isEmpty then
            throw new BuiltinException(
              s"writeBits: Indexes out of bounds, because byte string is empty, actual: $indexes"
            )

        val resultArray = byteString.bytes.clone()
        val bitLength = resultArray.length * 8
        val bitValue = if bit then 1 else 0

        for index <- indexes do
            if index < 0 || index >= bitLength then
                throw new BuiltinException(
                  s"writeBits: Index out of bounds, expected: [0 .. $bitLength), actual: $index"
                )

            val currentIndex = index.toInt
            val byteIndex = (bitLength - 1 - currentIndex) / 8
            val bitIndex = currentIndex % 8
            if bitValue == 1 then
                resultArray(byteIndex) = (resultArray(byteIndex) | (1 << bitIndex)).toByte
            else resultArray(byteIndex) = (resultArray(byteIndex) & ~(1 << bitIndex)).toByte

        ByteString.unsafeFromArray(resultArray)
    }

    /** Construct a byte string by repeating a single byte.
      *
      * @param length
      *   Number of bytes to generate. Must be within `[0, maximumOutputLength]`.
      *
      * @param byte
      *   Byte value in the range `[0, 255]`.
      *
      * @throws BuiltinException
      *   If the requested length or byte value is outside the permitted range.
      *
      * @example
      *   {{{
      *   replicateByte(4, 0xFF) == hex"ffffffff"
      *   replicateByte(0, 1) == hex""
      *   }}}
      *
      * @return
      *   A new byte string consisting of `length` copies of `byte`.
      *
      * @see
      *   [[scalus.builtin.IntegerToByteString.maximumOutputLength]]
      */
    def replicateByte(length: BigInt, byte: BigInt): ByteString = {
        if length < 0 || length > IntegerToByteString.maximumOutputLength then
            throw new BuiltinException(
              s"replicateByte: requested length out of bounds, expected: [0 .. ${IntegerToByteString.maximumOutputLength}], actual: $length"
            )

        if byte < 0 || byte > 255 then
            throw new BuiltinException(
              s"replicateByte: byte value out of bounds, expected: [0 .. 255], actual: $byte"
            )

        val lengthValue = length.toInt
        val byteValue = byte.toByte

        if lengthValue == 0 then return ByteString.empty
        ByteString.fill(lengthValue, byteValue)
    }

    /** Logical bit shift of a byte string.
      *
      * Positive `shift` performs a left shift. Negative `shift` performs a right shift. Bits
      * shifted out are discarded; zeros are shifted in.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123 CIP-123]]
      *
      * @note
      *   Shifts never sign-extend; they are logical, not arithmetic.
      *
      * @param byteString
      *   Input byte string.
      *
      * @param shift
      *   Number of bits to shift. Must fit in a 32-bit signed integer.
      *
      * @throws BuiltinException
      *   If the shift value does not fit into a 32-bit signed integer.
      *
      * @example
      *   {{{
      *   shiftByteString  hex"11ff"  4 == hex"1ff0"
      *   shiftByteString  hex"11ff" -4 == hex"011f"
      *   }}}
      *
      * @return
      *   The shifted byte string.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.rotateByteString]]
      */
    def shiftByteString(byteString: ByteString, shift: BigInt): ByteString = {
        if byteString.isEmpty || shift == 0 then return byteString

        val bytes = byteString.bytes
        val bytesLength = bytes.length

        if bytesLength * 8 < shift.abs then
            return ByteString.unsafeFromArray(new Array[Byte](bytesLength))

        if shift < Int.MinValue || shift > Int.MaxValue then
            throw new BuiltinException(
              s"shiftByteString: shift out of bounds, expected: [${Int.MinValue} .. ${Int.MaxValue}], actual: $shift"
            )

        val shiftValue = shift.toInt

        val resultArray =
            if shiftValue > 0 then shiftLeft(bytes, shiftValue)
            else shiftRight(bytes, shiftValue.abs)

        ByteString.unsafeFromArray(resultArray)
    }

    /** Rotate all bits of a byte string left or right, modulo its bit length.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123 CIP-123]]
      *
      * @note
      *   Rotation wraps around the bitstring width (`n * 8` bits).
      *
      * @param byteString
      *   Input byte string.
      *
      * @param rotation
      *   Number of bits to rotate. Positive - rotate left, negative - rotate right.
      *
      * @throws BuiltinException
      *   If the rotation remainder does not fit in a 32-bit signed integer.
      *
      * @example
      *   {{{
      *   rotateByteString(hex"80", 1) == hex"01"
      *   rotateByteString(hex"01", -1) == hex"80"
      *   }}}
      *
      * @return
      *   A byte string of identical length with bits rotated.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.shiftByteString]]
      */
    def rotateByteString(byteString: ByteString, rotation: BigInt): ByteString = {
        if byteString.isEmpty then return byteString

        val bytes = byteString.bytes
        val bytesLength = bytes.length
        val bitLength = bytesLength * 8
        val rotationRemainder = rotation % bitLength

        if rotationRemainder == 0 then return byteString

        if rotationRemainder < Int.MinValue || rotationRemainder > Int.MaxValue then
            throw new BuiltinException(
              s"rotateByteString: rotation remainder to big, expected: [${Int.MinValue} .. ${Int.MaxValue}], actual: $rotationRemainder"
            )

        val rotationValue = rotationRemainder.toInt
        val resultArray =
            if rotationValue > 0 then rotateLeft(bytes, rotationValue)
            else rotateRight(bytes, rotationValue.abs)

        ByteString.unsafeFromArray(resultArray)
    }

    /** Count the number of 1-bits in the byte string (Hamming weight).
      *
      * @param byteString
      *   Input byte string.
      *
      * @example
      *   {{{
      *   countSetBits(hex"0f0f") == 8
      *   countSetBits(hex"0000") == 0
      *   }}}
      *
      * @return
      *   Number of set bits.
      */
    def countSetBits(byteString: ByteString): Int = {
        if byteString.isEmpty then return 0

        val bytes = byteString.bytes
        val bytesLength = bytes.length

        var count = 0
        var index = 0
        while index < bytesLength do
            var value = bytes(index) & 0xff
            while value != 0 do
                count += value & 1
                value >>>= 1
            index += 1

        count
    }

    /** Find the index of the least significant 1-bit.
      *
      * Scans from bit 0 (LSB of last byte) upwards.
      *
      * @param byteString
      *   Input byte string.
      *
      * @example
      *   {{{
      *   findFirstSetBit(hex"10") == 4 // 00010000
      *   findFirstSetBit(hex"00") == -1
      *   }}}
      *
      * @return
      *   Index of the first set bit `>= 0`, or `-1` if all bits are zero.
      *
      * @see
      *   [[scalus.builtin.BitwiseLogicalOperations.countSetBits]]
      */
    def findFirstSetBit(byteString: ByteString): Int = {
        if byteString.isEmpty then return -1

        val bytes = byteString.bytes
        val bytesLength = bytes.length

        var totalBitIndex = 0
        var index = bytesLength - 1
        while index >= 0 do
            var value = bytes(index) & 0xff
            var localBitIndex = 0
            while value != 0 do
                if (value & 1) == 1 then return totalBitIndex + localBitIndex
                else localBitIndex += 1
                value >>>= 1
            totalBitIndex += 8

            index -= 1

        -1
    }

    /** Internal combine logic for AND/OR/XOR with CIP-122 padding/truncation rules. */
    private inline def combineByteStrings(
        shouldPad: Boolean,
        lhs: ByteString,
        rhs: ByteString
    )(inline op: (Byte, Byte) => Byte): ByteString = {
        val (shortArray, longArray) =
            if lhs.size < rhs.size then (lhs.bytes, rhs.bytes) else (rhs.bytes, lhs.bytes)

        val resultArray = new Array[Byte](if shouldPad then longArray.length else shortArray.length)

        var index = 0
        val shortArrayLength = shortArray.length
        while index < shortArrayLength do
            resultArray(index) = op(shortArray(index), longArray(index))
            index += 1

        if shouldPad && shortArray.length != longArray.length then
            System.arraycopy(
              longArray,
              shortArray.length,
              resultArray,
              shortArray.length,
              longArray.length - shortArray.length
            )

        ByteString.unsafeFromArray(resultArray)
    }

    /** Apply a unary byte transformation across the entire byte string. */
    private inline def transformByteString(
        byteString: ByteString
    )(inline op: Byte => Byte): ByteString = {
        val inputArray = byteString.bytes
        val inputArrayLength = inputArray.length
        val resultArray = new Array[Byte](inputArrayLength)

        var index = 0
        while index < inputArrayLength do
            resultArray(index) = op(inputArray(index))
            index += 1

        ByteString.unsafeFromArray(resultArray)
    }

    /** Internal left-shift implementation. */
    private def shiftLeft(inputBytes: Array[Byte], shift: Int): Array[Byte] = {
        val shiftMod = shift % 8
        val carryMask = ((1 << shiftMod) - 1).toByte
        val offsetBytes = shift / 8
        val bytesLength = inputBytes.length

        val resultBytes = new Array[Byte](bytesLength)

        var index = 0
        while index < bytesLength do
            val sourceIndex = index + offsetBytes

            if sourceIndex >= bytesLength then resultBytes(index) = 0
            else
                val src = inputBytes(sourceIndex)
                var dst = (src << shiftMod).toByte
                if sourceIndex + 1 < bytesLength then
                    dst = (dst | ((inputBytes(
                      sourceIndex + 1
                    ) >>> (8 - shiftMod)) & carryMask)).toByte
                resultBytes(index) = dst

            index += 1

        resultBytes
    }

    /** Internal right-shift implementation. */
    private def shiftRight(inputBytes: Array[Byte], shift: Int): Array[Byte] = {
        val shiftMod = shift % 8
        val carryMask = (0xff << (8 - shiftMod)).toByte
        val offsetBytes = shift / 8
        val bytesLength = inputBytes.length

        val resultBytes = new Array[Byte](bytesLength)

        var index = bytesLength - 1
        while index >= 0 do
            val sourceIndex = index - offsetBytes

            if sourceIndex < 0 then resultBytes(index) = 0
            else
                val src = inputBytes(sourceIndex)
                var dst = ((src & 0xff) >>> shiftMod).toByte
                if sourceIndex - 1 >= 0 then
                    dst =
                        (dst | ((inputBytes(sourceIndex - 1) << (8 - shiftMod)) & carryMask)).toByte
                resultBytes(index) = dst

            index -= 1

        resultBytes
    }

    /** Internal rotate-left implementation. */
    private def rotateLeft(inputBytes: Array[Byte], rotation: Int): Array[Byte] = {
        val shiftMod = rotation % 8
        val carryMask = ((1 << shiftMod) - 1).toByte
        val offsetBytes = rotation / 8
        val bytesLength = inputBytes.length

        val resultBytes = new Array[Byte](bytesLength)

        var index = 0
        while index < bytesLength do
            val sourceIndex = (index + offsetBytes) % bytesLength

            val src = inputBytes(sourceIndex)
            var dst = (src << shiftMod).toByte
            dst = (dst | ((inputBytes(
              if sourceIndex + 1 < bytesLength then sourceIndex + 1 else 0
            ) >>> (8 - shiftMod)) & carryMask)).toByte
            resultBytes(index) = dst

            index += 1

        resultBytes
    }

    /** Internal rotate-left implementation. */
    private def rotateRight(inputBytes: Array[Byte], rotation: Int): Array[Byte] = {
        val shiftMod = rotation % 8
        val carryMask = (0xff << (8 - shiftMod)).toByte
        val offsetBytes = rotation / 8
        val bytesLength = inputBytes.length
        val lastByteIndex = bytesLength - 1

        val resultBytes = new Array[Byte](bytesLength)

        var index = bytesLength - 1
        while index >= 0 do
            val diff = index - offsetBytes
            val sourceIndex = if diff >= 0 then diff else bytesLength + diff

            val src = inputBytes(sourceIndex)
            var dst = ((src & 0xff) >>> shiftMod).toByte
            dst = (dst | ((inputBytes(
              if sourceIndex > 0 then sourceIndex - 1 else lastByteIndex
            ) << (8 - shiftMod)) & carryMask)).toByte
            resultBytes(index) = dst

            index -= 1

        resultBytes
    }
