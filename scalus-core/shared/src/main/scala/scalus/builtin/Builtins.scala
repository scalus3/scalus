package scalus.builtin
import io.bullet.borer.Cbor

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import scalus.uplc.eval.BuiltinException

/** Class contains all Cardano Plutus built-in functions according to Plutus Specification.
  *
  * Functions of this class are treated specially by the Scalus compiler plugin. When used in
  * validator code, the compiler plugin will replace the function call with an actual Plutus
  * built-in function.
  *
  * Scalus Compiler plugin expects that this class to contain methods named exactly as in
  * [[scalus.uplc.DefaultFun]] with lowercase first letter. For example, for
  * [[scalus.uplc.DefaultFun.AddInteger]] there should be a method named `addInteger` etc.
  *
  * All the builtins are implemented according to semantics of the Plutus builtins. The
  * implementation is platform independent. All the platform specific code is in the
  * [[PlatformSpecific]].
  *
  * Only modify this class when a new builtin is added to [[scalus.uplc.DefaultFun]], or when you
  * know what you are doing.
  *
  * @see
  *   [[https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf]]
  */
private[builtin] abstract class AbstractBuiltins(using ps: PlatformSpecific):
    // Integers
    def addInteger(i1: BigInt, i2: BigInt): BigInt = i1 + i2
    def subtractInteger(i1: BigInt, i2: BigInt): BigInt = i1 - i2
    def multiplyInteger(i1: BigInt, i2: BigInt): BigInt = i1 * i2
    def divideInteger(i1: BigInt, i2: BigInt): BigInt =
        import java.math.{BigDecimal, RoundingMode}
        val r = new BigDecimal(i1.bigInteger)
            .divide(new BigDecimal(i2.bigInteger), RoundingMode.FLOOR)
            .toBigInteger()
        BigInt(r)
    def quotientInteger(i1: BigInt, i2: BigInt): BigInt = i1 / i2
    def remainderInteger(i1: BigInt, i2: BigInt): BigInt = i1 % i2
    def modInteger(i1: BigInt, i2: BigInt): BigInt =
        /*divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                                     where qr@(q,r) = quotRem n d */
        val r = i1 % i2
        if r.signum == -i2.signum then r + i2 else r
    def equalsInteger(i1: BigInt, i2: BigInt): Boolean = i1 == i2
    def lessThanInteger(i1: BigInt, i2: BigInt): Boolean = i1 < i2
    def lessThanEqualsInteger(i1: BigInt, i2: BigInt): Boolean = i1 <= i2
    // Bytestrings
    def appendByteString(a: ByteString, b: ByteString): ByteString =
        ByteString.unsafeFromArray(a.bytes ++ b.bytes)
    def consByteString(char: BigInt, byteString: ByteString): ByteString =
        if char < 0 || char > 255 then
            throw new BuiltinException(s"consByteString: invalid byte value: $char")
        ByteString.unsafeFromArray(char.toByte +: byteString.bytes)

    /** Returns a new ByteString that is a slice of the original ByteString
      *
      * @param from
      *   the starting index of the slice (inclusive)
      * @param len
      *   the length of the slice
      * @param bs
      *   the original ByteString to slice
      *
      * @example
      *   {{{
      *   sliceByteString(2, 4, hex"1234567890abcdef") // returns hex"567890ab"
      *   sliceByteString(5, 4, hex"1234567890abcdef") // returns hex"abcdef"
      *   sliceByteString(9, 4, hex"1234567890abcdef") // returns hex""
      *   sliceByteString(0, 0, hex"1234567890abcdef") // returns hex""
      *   }}}
      */
    def sliceByteString(from: BigInt, len: BigInt, bs: ByteString): ByteString =
        ByteString.unsafeFromArray(bs.bytes.drop(from.toInt).take(len.toInt))

    /** Returns the length of the ByteString */
    def lengthOfByteString(bs: ByteString): BigInt = bs.size

    /** Returns the byte at the specified index in the ByteString
      *
      * @throws scalus.uplc.eval.BuiltinException
      *   if the index is out of bounds (offchain)
      */
    def indexByteString(bs: ByteString, i: BigInt): BigInt =
        if i < 0 || i >= bs.size then
            throw new BuiltinException(
              s"index $i out of bounds for bytestring of length ${bs.size}"
            )
        else BigInt(bs.bytes(i.toInt) & 0xff)

    def equalsByteString(a: ByteString, b: ByteString): Boolean = a == b

    /** Checks if one 'ByteString' is less than another */
    def lessThanByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.size, b.size)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.size < b.size then true
        else false

    /** Checks if one 'ByteString' is less than or equal to another */
    def lessThanEqualsByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.size, b.size)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.size <= b.size then true
        else false
    // Cryptography and hashes

    def sha2_256(bs: ByteString): ByteString = ps.sha2_256(bs)
    def sha3_256(bs: ByteString): ByteString = ps.sha3_256(bs)
    def blake2b_256(bs: ByteString): ByteString = ps.blake2b_256(bs)
    def blake2b_224(bs: ByteString): ByteString = ps.blake2b_224(bs)
    def verifyEd25519Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ps.verifyEd25519Signature(pk, msg, sig)
    def verifyEcdsaSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifyEcdsaSecp256k1Signature(pk, msg, sig)
    def verifySchnorrSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifySchnorrSecp256k1Signature(pk, msg, sig)

    // Strings
    def appendString(s1: String, s2: String): String = s1 + s2
    def equalsString(s1: String, s2: String): Boolean = s1 == s2
    def encodeUtf8(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))
    def decodeUtf8(bs: ByteString): String =
        UTF8Decoder.decode(bs.bytes)

    // Bool
    def ifThenElse[A](cond: Boolean, a: A, b: A): A =
        if cond then a else b

    // Unit
    def chooseUnit[A]()(a: A): A = a

    // Tracing
    def trace[A](s: String)(a: A): A =
        // calculate the hash
        println(s)
        a

    // Pairs
    def fstPair[A, B](p: BuiltinPair[A, B]): A = p.fst
    def sndPair[A, B](p: BuiltinPair[A, B]): B = p.snd

    // Lists
    def chooseList[A, B](l: BuiltinList[A], e: B, ne: B): B =
        if l.isEmpty then e else ne
    def mkCons[A](a: A, l: BuiltinList[A]): BuiltinList[A] = a :: l
    def headList[A](l: BuiltinList[A]): A = l.head
    def tailList[A](l: BuiltinList[A]): BuiltinList[A] = l.tail
    def nullList[A](l: BuiltinList[A]): Boolean = l.isEmpty

    // Data
    def chooseData[A](d: Data, constrCase: A, mapCase: A, listCase: A, iCase: A, bCase: A): A =
        d match
            case Data.Constr(_, _) => constrCase
            case Data.Map(_)       => mapCase
            case Data.List(_)      => listCase
            case Data.I(_)         => iCase
            case Data.B(_)         => bCase

    def constrData(ctor: BigInt, args: BuiltinList[Data]): Data =
        Data.Constr(ctor.toLong, args.toList)
    def mapData(values: BuiltinList[BuiltinPair[Data, Data]]): Data =
        Data.Map(values.toList.map(p => (p.fst, p.snd)))
    def listData(values: BuiltinList[Data]): Data = Data.List(values.toList)
    def iData(value: BigInt): Data = Data.I(value)
    def bData(value: ByteString): Data = Data.B(value)
    def unConstrData(d: Data): BuiltinPair[BigInt, BuiltinList[Data]] = d match
        case Data.Constr(constr, args) => BuiltinPair(constr: BigInt, BuiltinList(args*))
        case _                         => throw new Exception(s"not a constructor but $d")
    def unListData(d: Data): BuiltinList[Data] = d match
        case Data.List(values) => BuiltinList(values*)
        case _                 => throw new Exception(s"not a list but $d")

    def unMapData(d: Data): BuiltinList[BuiltinPair[Data, Data]] = d match
        case Data.Map(values) => BuiltinList(values.map(BuiltinPair.apply)*)
        case _                => throw new Exception(s"not a list but $d")
    def unIData(d: Data): BigInt = d match
        case Data.I(value) => value
        case _             => throw new Exception(s"not an integer but $d")
    def unBData(d: Data): ByteString = d match
        case Data.B(value) => value
        case _             => throw new Exception(s"not a bytestring but $d")

    def equalsData(d1: Data, d2: Data): Boolean = d1 == d2

    def serialiseData(d: Data): ByteString =
        ByteString.fromArray(Cbor.encode(d).toByteArray)

    // Misc monomorphized constructors.
    // We could simply replace those with constants, but we use built-in functions for consistency
    // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    // See note [Representable built-in functions over polymorphic built-in types].
    def mkPairData(fst: Data, snd: Data): BuiltinPair[Data, Data] = BuiltinPair(fst, snd)
    def mkNilData(): BuiltinList[Data] = BuiltinList.empty
    def mkNilPairData(): BuiltinList[BuiltinPair[Data, Data]] = BuiltinList.empty

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
      * @throws scalus.uplc.eval.BuiltinException
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
      *   [[scalus.builtin.Builtins.byteStringToInteger]]
      */
    def integerToByteString(endianness: Boolean, length: BigInt, input: BigInt): ByteString = {
        IntegerToByteString.integerToByteString(endianness, length, input)
    }

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
      *   [[scalus.builtin.Builtins.integerToByteString]]
      */
    def byteStringToInteger(endianness: Boolean, input: ByteString): BigInt = {
        ByteStringToInteger.byteStringToInteger(endianness, input)
    }

    /** Bitwise logical AND for ByteStrings.
      * @see
      *   [CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise AND operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] & rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   andByteString(false, hex"0FFF", hex"FF") == hex"0F"
      * @example
      *   andByteString(true, hex"0FFF", hex"FF") == hex"0FFF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise AND operation.
      */
    def andByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.andByteString(shouldPad, lhs, rhs)

    /** Bitwise logical OR for ByteStrings.
      * @see
      *   [CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise OR operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] | rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   orByteString(false, hex"0FFF", hex"FF") == hex"FF"
      * @example
      *   orByteString(true, hex"0FFF", hex"FF") == hex"FFFF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise OR operation.
      */
    def orByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.orByteString(shouldPad, lhs, rhs)

    /** Bitwise logical XOR for ByteStrings.
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise XOR operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] ˆ rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   xorByteString(false, hex"0FFF", hex"FF") == hex"F0"
      * @example
      *   xorByteString(true, hex"0FFF", hex"FF") == hex"F0FF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise XOR operation.
      */
    def xorByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.xorByteString(shouldPad, lhs, rhs)

    /** Bitwise logical ComplementByteString for ByteStrings.
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a bitwise logical ComplementByteString operation on the input ByteString by
      * inverting each bit (Formally resultBit[i] = if inputBit[i] == 0 then 1 else 0).
      *
      * @example
      *   complementByteString(hex"FF") == hex"00"
      * @example
      *   complementByteString(hex"F0") == hex"0F"
      *
      * @param byteString
      *   The `ByteString` that to be bitwise logical completed(inverted).
      * @return
      *   The result of the bitwise logical ComplementByteString operation.
      */
    def complementByteString(byteString: ByteString): ByteString =
        BitwiseLogicalOperations.complementByteString(byteString)

    /** Bitwise logical ReadBit for ByteStrings.
      *
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Gets the value of the bit at the specified index in the input ByteString. The index must be
      * in the range [0 .. byteString.size * 8), otherwise BuiltinException will be thrown. Bit
      * indexing starts from the end of the ByteString.
      *
      * @example
      *   readBit(hex"0004", 2) == true
      * @example
      *   readBit(hex"0004", 15) == false
      * @example
      *   readBit(hex"0004", 16) throws BuiltinException
      *
      * @param byteString
      *   The `ByteString` that contains the bit to be read.
      * @param index
      *   The index of the bit to be read.
      * @throws scalus.uplc.eval.BuiltinException
      *   if the index is out of bounds.
      * @return
      *   The value of the bit at the specified index.
      */
    def readBit(byteString: ByteString, index: BigInt): Boolean =
        BitwiseLogicalOperations.readBit(byteString, index)

    /** Bitwise logical WriteBits for ByteStrings.
      *
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Sets the value of the bit at the specified indexes in a copy of the input ByteString. The
      * indexes must be in the range [0 .. byteString.size * 8), otherwise BuiltinException will be
      * thrown. Bit indexing starts from the end of the ByteString.
      *
      * @example
      *   writeBits(hex"0000", List(0, 1, 2, 3), true) == hex"000F"
      * @example
      *   writeBits(hex"000F", List(0, 1, 2, 3), false) == hex"0000"
      * @example
      *   writeBits(hex"000F", List(16), true) throws BuiltinException
      *
      * @param byteString
      *   The `ByteString` copy of that to be written.
      * @param indexes
      *   The indexes of the bits to be written.
      * @param bit
      *   The value of the bit to be written.
      * @throws scalus.uplc.eval.BuiltinException
      *   if the indexes are out of bounds.
      * @return
      *   The result of the bitwise logical WriteBits operation.
      */
    def writeBits(
        byteString: ByteString,
        indexes: BuiltinList[BigInt],
        bit: Boolean
    ): ByteString =
        BitwiseLogicalOperations.writeBits(byteString, indexes.toList, bit)

    /** Bitwise logical ReplicateByte for ByteStrings.
      *
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Replicates a byte `length` times and returns result as ByteString. Length must be
      * non-negative, otherwise BuiltinException will be thrown. The byte value must be in the range
      * [0 .. 255], otherwise BuiltinException will be thrown.
      *
      * @example
      *   replicateByte(0, 0xFF) == hex""
      * @example
      *   replicateByte(4, 0xFF) == hex"FFFFFFFF"
      * @example
      *   replicateByte(-1, 255) throws BuiltinException
      * @example
      *   replicateByte(1, -1) throws BuiltinException
      * @example
      *   replicateByte(1, 256) throws BuiltinException
      *
      * @param length
      *   The number of times to replicate the byte.
      * @param byte
      *   The value of the byte to be replicated.
      * @throws scalus.uplc.eval.BuiltinException
      *   if the length is negative or the byte value is out of bounds.
      * @return
      *   The result of the bitwise logical ReplicateByte operation.
      */
    def replicateByte(length: BigInt, byte: BigInt): ByteString =
        BitwiseLogicalOperations.replicateByte(length, byte)

    /** Bitwise logical shiftByteString for ByteStrings.
      *
      * @see
      *   [CIP-123] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
      *
      * Shifts the input ByteString by the specified number of bits. A positive shift value shifts
      * the ByteString to the left, while a negative shift value shifts the ByteString to the right.
      *
      * @example
      *   shiftByteString(hex"000F", 4) == hex"00F0"
      * @example
      *   shiftByteString(hex"000F", 16) == hex"0000"
      * @example
      *   shiftByteString(hex"000F", -4) == hex"0000"
      *
      * @param byteString
      *   The ByteString to be shifted.
      * @param shift
      *   The number of bits to shift the ByteString.
      * @return
      *   The result of the bitwise logical shiftByteString operation.
      */
    def shiftByteString(byteString: ByteString, shift: BigInt): ByteString =
        BitwiseLogicalOperations.shiftByteString(byteString, shift)

    /** Bitwise logical rotateByteString for ByteStrings.
      *
      * @see
      *   [CIP-123] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
      *
      * Rotates the input ByteString by the specified number of bits. A positive rotation value
      * rotates the ByteString to the left, while a negative rotation value rotates the ByteString
      * to the right. Rotation by more than the total number of bits is the same as the remainder
      * after division by number of bits.
      *
      * @example
      *   rotateByteString(hex"000F", 4) == hex"00F0"
      * @example
      *   rotateByteString(hex"000F", -4) == hex"F000"
      * @example
      *   rotateByteString(hex"000F", 16) == hex"000F"
      * @example
      *   rotateByteString(hex"000F", -16) == hex"000F"
      *
      * @param byteString
      *   The ByteString to be rotated.
      * @param rotation
      *   The number of bits to rotates the ByteString.
      * @return
      *   The result of the bitwise logical rotateByteString operation.
      */
    def rotateByteString(byteString: ByteString, rotation: BigInt): ByteString =
        BitwiseLogicalOperations.rotateByteString(byteString, rotation)

    /** Bitwise logical countSetBits for ByteStrings.
      *
      * @see
      *   [CIP-123] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
      *
      * Counts the number of set bits in the input ByteString.
      *
      * @example
      *   countSetBits(hex"000F") == 4
      * @example
      *   countSetBits(hex"0000") == 0
      * @example
      *   countSetBits(hex"0001") == 1
      *
      * @param byteString
      *   The ByteString to be counted.
      * @return
      *   The number of set bits in the ByteString.
      */
    def countSetBits(byteString: ByteString): BigInt =
        BitwiseLogicalOperations.countSetBits(byteString)

    /** Bitwise logical findFirstSetBit for ByteStrings.
      *
      * @see
      *   [CIP-123] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123).
      *
      * Finds the index of the first set bit in the input ByteString. The index is zero-based and
      * starts from the end of the ByteString. If no set bits are found, -1 is returned.
      *
      * @example
      *   findFirstSetBit(hex"") == -1
      * @example
      *   findFirstSetBit(hex"0000") == -1
      * @example
      *   findFirstSetBit(hex"0002") == 1
      * @example
      *   findFirstSetBit(hex"FFF2") == 1
      *
      * @param byteString
      *   The ByteString to be searched.
      * @return
      *   The index of the first set bit in the ByteString from the end.
      */
    def findFirstSetBit(byteString: ByteString): BigInt =
        BitwiseLogicalOperations.findFirstSetBit(byteString)

    def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        ps.bls12_381_G1_equal(p1, p2)

    def bls12_381_G1_add(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ps.bls12_381_G1_add(p1, p2)

    def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ps.bls12_381_G1_scalarMul(s, p)

    def bls12_381_G1_neg(p: BLS12_381_G1_Element): BLS12_381_G1_Element = ps.bls12_381_G1_neg(p)

    def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString =
        ps.bls12_381_G1_compress(p)

    def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_uncompress(bs)

    def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_hashToGroup(bs, dst)

    def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        ps.bls12_381_G2_equal(p1, p2)

    def bls12_381_G2_add(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ps.bls12_381_G2_add(p1, p2)

    def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ps.bls12_381_G2_scalarMul(s, p)

    def bls12_381_G2_neg(p: BLS12_381_G2_Element): BLS12_381_G2_Element = ps.bls12_381_G2_neg(p)

    def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString =
        ps.bls12_381_G2_compress(p)

    def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element =
        ps.bls12_381_G2_uncompress(bs)

    def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element =
        ps.bls12_381_G2_hashToGroup(bs, dst)

    /** The compressed form of the point at infinity in G2, 96 bytes long.
      */
    def bls12_381_G2_compressed_zero: ByteString = PlatformSpecific.bls12_381_G2_compressed_zero

    def bls12_381_G2_compressed_generator: ByteString =
        PlatformSpecific.bls12_381_G2_compressed_generator

    def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        ps.bls12_381_millerLoop(p1, p2)

    def bls12_381_mulMlResult(r1: BLS12_381_MlResult, r2: BLS12_381_MlResult): BLS12_381_MlResult =
        ps.bls12_381_mulMlResult(r1, r2)

    def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean =
        ps.bls12_381_finalVerify(p1, p2)

    def keccak_256(bs: ByteString): ByteString =
        ps.keccak_256(bs)

    /** Hashing primitive Ripemd_160 for ByteStrings.
      *
      * @see
      *   [CIP-127] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0127).
      *
      * Ripemd_160 hash function (https://en.wikipedia.org/wiki/RIPEMD).
      *
      * @param byteString
      *   The ByteString to be hashed.
      * @return
      *   The result of the Ripemd_160 hash function.
      */
    def ripemd_160(byteString: ByteString): ByteString =
        ps.ripemd_160(byteString)

private object UTF8Decoder {
    def decode(bytes: Array[Byte]): String = {
        val decoder: CharsetDecoder = StandardCharsets.UTF_8.newDecoder()
        decoder.onMalformedInput(CodingErrorAction.REPORT)
        decoder.onUnmappableCharacter(CodingErrorAction.REPORT)

        val inputBuffer: ByteBuffer = ByteBuffer.wrap(bytes)
        val outputBuffer: CharBuffer = CharBuffer.allocate(bytes.length)

        try
            val result = decoder.decode(inputBuffer, outputBuffer, true)
            if result.isUnderflow then
                outputBuffer.flip()
                outputBuffer.toString
            else throw new IllegalArgumentException("Invalid UTF-8 sequence")
        catch
            case e: java.nio.charset.CharacterCodingException =>
                throw new IllegalArgumentException("Invalid UTF-8 sequence", e)
    }
}
