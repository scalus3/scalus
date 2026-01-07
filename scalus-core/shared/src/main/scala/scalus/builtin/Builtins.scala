package scalus.builtin
import io.bullet.borer.Cbor
import scalus.prelude.List as PList

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

    // ============================================================================
    // Integer Operations
    // ============================================================================

    /** Add two arbitrary-precision integers.
      *
      * @example
      *   addInteger(3, 5) == 8
      */
    def addInteger(i1: BigInt, i2: BigInt): BigInt = i1 + i2

    /** Subtract two arbitrary-precision integers.
      *
      * @example
      *   subtractInteger(10, 3) == 7
      */
    def subtractInteger(i1: BigInt, i2: BigInt): BigInt = i1 - i2

    /** Multiply two arbitrary-precision integers.
      *
      * @example
      *   multiplyInteger(4, 5) == 20
      */
    def multiplyInteger(i1: BigInt, i2: BigInt): BigInt = i1 * i2

    /** Divide two integers using floor division (truncate toward negative infinity).
      *
      * For negative dividends with positive divisors (or vice versa), this differs from truncated
      * division (`quotientInteger`) by rounding toward negative infinity.
      *
      * @example
      *   divideInteger(7, 2) == 3
      * @example
      *   divideInteger(-7, 2) == -4 // floor division
      * @throws java.lang.ArithmeticException
      *   if divisor is zero
      */
    def divideInteger(i1: BigInt, i2: BigInt): BigInt =
        // Floor division: truncate toward negative infinity
        // Standard BigInt `/` truncates toward zero
        // Adjust by -1 when signs differ and there's a non-zero remainder
        val q = i1 / i2
        val r = i1 % i2
        if r != 0 && (i1 < 0) != (i2 < 0) then q - 1 else q

    /** Compute the quotient of two integers (truncate toward zero).
      *
      * This is standard truncated division, which rounds toward zero.
      *
      * @example
      *   quotientInteger(7, 2) == 3
      * @example
      *   quotientInteger(-7, 2) == -3 // truncated toward zero
      * @throws java.lang.ArithmeticException
      *   if divisor is zero
      */
    def quotientInteger(i1: BigInt, i2: BigInt): BigInt = i1 / i2

    /** Compute the remainder after truncated division (`quotientInteger`).
      *
      * The result has the same sign as the dividend. Satisfies:
      * `dividend == quotientInteger(dividend, divisor) * divisor + remainderInteger(dividend, divisor)`
      *
      * @example
      *   remainderInteger(7, 3) == 1
      * @example
      *   remainderInteger(-7, 3) == -1
      * @throws java.lang.ArithmeticException
      *   if divisor is zero
      */
    def remainderInteger(i1: BigInt, i2: BigInt): BigInt = i1 % i2

    /** Compute the modulus after floor division (`divideInteger`).
      *
      * The result has the same sign as the divisor. Satisfies:
      * `dividend == divideInteger(dividend, divisor) * divisor + modInteger(dividend, divisor)`
      *
      * @example
      *   modInteger(7, 3) == 1
      * @example
      *   modInteger(-7, 3) == 2 // result has sign of divisor
      * @throws java.lang.ArithmeticException
      *   if divisor is zero
      */
    def modInteger(i1: BigInt, i2: BigInt): BigInt =
        /*divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                                     where qr@(q,r) = quotRem n d */
        val r = i1 % i2
        if r.signum == -i2.signum then r + i2 else r

    /** Check if two integers are equal.
      *
      * @example
      *   equalsInteger(5, 5) == true
      */
    def equalsInteger(i1: BigInt, i2: BigInt): Boolean = i1 == i2

    /** Check if the first integer is strictly less than the second.
      *
      * @example
      *   lessThanInteger(3, 5) == true
      */
    def lessThanInteger(i1: BigInt, i2: BigInt): Boolean = i1 < i2

    /** Check if the first integer is less than or equal to the second.
      *
      * @example
      *   lessThanEqualsInteger(5, 5) == true
      */
    def lessThanEqualsInteger(i1: BigInt, i2: BigInt): Boolean = i1 <= i2

    /** Compute modular exponentiation: base^exponent mod modulus.
      *
      * Supports negative exponents via modular inverse.
      *
      * @param base
      *   the base integer
      * @param exponent
      *   the exponent (can be negative if base is invertible mod modulus)
      * @param modulus
      *   the modulus (must be positive)
      * @return
      *   base^exponent mod modulus as a non-negative integer
      * @throws scalus.uplc.eval.BuiltinException
      *   if modulus <= 0, or if exponent < 0 and base is not invertible mod modulus
      * @example
      *   expModInteger(2, 10, 1000) == 24 // 2^10 mod 1000
      * @example
      *   expModInteger(3, -1, 7) == 5 // modular inverse of 3 mod 7
      */
    def expModInteger(base: BigInt, exponent: BigInt, modulus: BigInt): BigInt =
        if modulus <= 0 then throw BuiltinException("expModInteger: modulus must be positive")
        else if modulus == 1 then BigInt(0)
        else if exponent >= 0 then platform.modPow(base, exponent, modulus)
        else
            // Negative exponent: compute modular inverse first
            // modInverse throws ArithmeticException if gcd(base, modulus) != 1
            try
                val inverse = base.modInverse(modulus)
                platform.modPow(inverse, -exponent, modulus)
            catch
                case _: ArithmeticException =>
                    throw BuiltinException(
                      s"expModInteger: $base is not invertible modulo $modulus"
                    )

    // ============================================================================
    // ByteString Operations
    // ============================================================================

    /** Concatenate two bytestrings.
      *
      * @example
      *   appendByteString(hex"1234", hex"5678") == hex"12345678"
      */
    def appendByteString(a: ByteString, b: ByteString): ByteString =
        ByteString.unsafeFromArray(a.bytes ++ b.bytes)

    /** Prepend a byte to a bytestring.
      *
      * @param char
      *   the byte value to prepend, must be in range [0, 255]
      * @param byteString
      *   the bytestring to prepend to
      * @throws scalus.uplc.eval.BuiltinException
      *   if byte value is outside [0, 255]
      *
      * @example
      *   consByteString(0x12, hex"3456") == hex"123456"
      */
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

    /** Check if two bytestrings are equal.
      *
      * @example
      *   equalsByteString(hex"1234", hex"1234") == true
      */
    def equalsByteString(a: ByteString, b: ByteString): Boolean = a == b

    /** Check if one bytestring is lexicographically less than another.
      *
      * Comparison is byte-by-byte, treating bytes as unsigned. Shorter bytestrings are considered
      * less than longer ones with the same prefix.
      *
      * @example
      *   lessThanByteString(hex"12", hex"1234") == true
      */
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

    /** Check if one bytestring is lexicographically less than or equal to another.
      *
      * @example
      *   lessThanEqualsByteString(hex"1234", hex"1234") == true
      */
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

    // ============================================================================
    // Cryptographic Hash Functions
    // ============================================================================

    /** Compute SHA2-256 hash.
      *
      * @return
      *   a 32-byte (256-bit) hash digest
      */
    def sha2_256(bs: ByteString): ByteString = ps.sha2_256(bs)

    /** Compute SHA3-256 hash.
      *
      * @return
      *   a 32-byte (256-bit) hash digest using Keccak-based SHA3
      */
    def sha3_256(bs: ByteString): ByteString = ps.sha3_256(bs)

    /** Compute BLAKE2b-256 hash.
      *
      * This is the primary hash function used in Cardano for script hashing and transaction IDs.
      *
      * @return
      *   a 32-byte (256-bit) hash digest
      */
    def blake2b_256(bs: ByteString): ByteString = ps.blake2b_256(bs)

    /** Compute BLAKE2b-224 hash.
      *
      * @return
      *   a 28-byte (224-bit) hash digest
      */
    def blake2b_224(bs: ByteString): ByteString = ps.blake2b_224(bs)

    /** Verify an Ed25519 signature.
      *
      * @param pk
      *   public key (32 bytes)
      * @param msg
      *   message to verify
      * @param sig
      *   signature (64 bytes)
      * @return
      *   true if the signature is valid
      * @throws scalus.uplc.eval.BuiltinException
      *   if key or signature has wrong length
      */
    def verifyEd25519Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ps.verifyEd25519Signature(pk, msg, sig)

    /** Verify an ECDSA signature on the secp256k1 curve.
      *
      * @param pk
      *   public key (33 bytes compressed)
      * @param msg
      *   message hash (32 bytes, must be pre-hashed)
      * @param sig
      *   signature (64 bytes, r||s format)
      * @return
      *   true if the signature is valid
      * @throws scalus.uplc.eval.BuiltinException
      *   if inputs have wrong lengths
      */
    def verifyEcdsaSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifyEcdsaSecp256k1Signature(pk, msg, sig)

    /** Verify a Schnorr signature on the secp256k1 curve (BIP-340).
      *
      * @param pk
      *   public key (32 bytes x-only)
      * @param msg
      *   message (arbitrary length)
      * @param sig
      *   signature (64 bytes)
      * @return
      *   true if the signature is valid
      * @throws scalus.uplc.eval.BuiltinException
      *   if inputs have wrong lengths
      */
    def verifySchnorrSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifySchnorrSecp256k1Signature(pk, msg, sig)

    // ============================================================================
    // String Operations
    // ============================================================================

    /** Concatenate two strings.
      *
      * @example
      *   appendString("Hello, ", "World!") == "Hello, World!"
      */
    def appendString(s1: String, s2: String): String = s1 + s2

    /** Check if two strings are equal. */
    def equalsString(s1: String, s2: String): Boolean = s1 == s2

    /** Encode a string as UTF-8 bytes.
      *
      * @example
      *   encodeUtf8("hello") == hex"68656c6c6f"
      */
    def encodeUtf8(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))

    /** Decode UTF-8 bytes to a string.
      *
      * @throws java.lang.IllegalArgumentException
      *   if the bytestring is not valid UTF-8
      */
    def decodeUtf8(bs: ByteString): String =
        UTF8Decoder.decode(bs.bytes)

    // ============================================================================
    // Control Flow
    // ============================================================================

    /** Conditional expression (if-then-else).
      *
      * Returns the second argument if the condition is true, third argument otherwise.
      *
      * @example
      *   ifThenElse(true, "yes", "no") == "yes"
      */
    def ifThenElse[A](cond: Boolean, a: A, b: A): A =
        if cond then a else b

    /** Choose based on unit value.
      *
      * Always returns the argument after consuming the unit input.
      */
    def chooseUnit[A]()(a: A): A = a

    // ============================================================================
    // Tracing (Debugging)
    // ============================================================================

    /** Trace a message and return a value.
      *
      * Logs the string message (for debugging) and returns the second argument unchanged. In
      * on-chain execution, traces are collected but don't affect the result.
      */
    def trace[A](s: String)(a: A): A =
        // calculate the hash
        println(s)
        a

    // ============================================================================
    // Pair Operations
    // ============================================================================

    /** Extract the first element of a pair. */
    def fstPair[A, B](p: BuiltinPair[A, B]): A = p.fst

    /** Extract the second element of a pair. */
    def sndPair[A, B](p: BuiltinPair[A, B]): B = p.snd

    // ============================================================================
    // Array Operations (CIP-0156)
    // ============================================================================

    /** Get the length of an array. */
    def lengthOfArray[A](a: BuiltinArray[A]): BigInt = a.length

    /** Convert a list to an array for O(1) indexed access. */
    def listToArray[A](a: BuiltinList[A]): BuiltinArray[A] = BuiltinArray.fromList(a)

    /** Access an element by index.
      *
      * @throws IndexOutOfBoundsException
      *   if index is out of bounds
      */
    def indexArray[A](a: BuiltinArray[A], n: BigInt): A = a(n)

    /** Multi-index array access from CIP-0156.
      *
      * Retrieves elements from an array at the specified indices.
      *
      * @param indices
      *   A list of zero-based indices, in the order elements should be retrieved. In case of
      *   repeated indices, the same element is returned multiple times.
      * @param arr
      *   The array to index.
      * @return
      *   A list containing the elements at the specified indices, in the same order.
      * @throws scalus.uplc.eval.BuiltinException
      *   If any index is out of bounds (< 0 or >= lengthOfArray).
      *
      * @example
      *   {{{
      *   val arr = listToArray(mkCons(iData(10), mkCons(iData(20), mkCons(iData(30), mkNilData()))))
      *   multiIndexArray(BuiltinList[BigInt](0, 2), arr)
      *   // returns BuiltinList(iData(10), iData(30))
      *   }}}
      */
    def multiIndexArray[A](indices: BuiltinList[BigInt], arr: BuiltinArray[A]): BuiltinList[A] =
        val len = arr.length
        val result = indices.toList.map { idx =>
            if idx < 0 || idx >= len then
                throw new BuiltinException(
                  s"multiIndexArray: index $idx out of bounds for array of length $len"
                )
            arr(idx)
        }
        BuiltinList.from(result)

    // ============================================================================
    // List Operations
    // ============================================================================

    /** Pattern match on a list - return one value for empty, another for non-empty.
      *
      * @param l
      *   the list to match on
      * @param e
      *   value to return if the list is empty
      * @param ne
      *   value to return if the list is non-empty
      */
    def chooseList[A, B](l: BuiltinList[A], e: B, ne: B): B =
        if l.isEmpty then e else ne

    /** Prepend an element to a list.
      *
      * @example
      *   mkCons(1, BuiltinList(2, 3)) == BuiltinList(1, 2, 3)
      */
    def mkCons[A](a: A, l: BuiltinList[A]): BuiltinList[A] = a :: l

    /** Get the first element of a list.
      *
      * @throws java.util.NoSuchElementException
      *   if the list is empty
      */
    def headList[A](l: BuiltinList[A]): A = l.head

    /** Get all elements except the first.
      *
      * @throws java.util.NoSuchElementException
      *   if the list is empty
      */
    def tailList[A](l: BuiltinList[A]): BuiltinList[A] = l.tail

    /** Check if a list is empty.
      *
      * @example
      *   nullList(BuiltinList.empty) == true
      */
    def nullList[A](l: BuiltinList[A]): Boolean = l.isEmpty

    /** Drop the first n elements from a list.
      *
      * If n is negative, no elements are dropped. If n is larger than the list length, an empty
      * list is returned.
      */
    def dropList[A](n: BigInt, l: BuiltinList[A]): BuiltinList[A] =
        if n.signum <= 0 then l
        else
            // For very large n, drop Int.MaxValue (same as Plutus reference impl)
            val dropCount = if n.isValidInt then n.toInt else Int.MaxValue
            BuiltinList.from(l.toList.drop(dropCount))

    // ============================================================================
    // Data Operations
    // ============================================================================

    /** Pattern match on a Data value by its constructor type.
      *
      * Returns one of five values depending on the Data constructor: Constr, Map, List, I
      * (integer), or B (bytestring).
      *
      * @param d
      *   the Data value to match
      * @param constrCase
      *   value to return if d is a Constr
      * @param mapCase
      *   value to return if d is a Map
      * @param listCase
      *   value to return if d is a List
      * @param iCase
      *   value to return if d is an Integer
      * @param bCase
      *   value to return if d is a ByteString
      */
    def chooseData[A](d: Data, constrCase: A, mapCase: A, listCase: A, iCase: A, bCase: A): A =
        d match
            case Data.Constr(_, _) => constrCase
            case Data.Map(_)       => mapCase
            case Data.List(_)      => listCase
            case Data.I(_)         => iCase
            case Data.B(_)         => bCase

    /** Construct a Data.Constr value.
      *
      * @param ctor
      *   constructor index (must be non-negative)
      * @param args
      *   list of constructor arguments
      */
    def constrData(ctor: BigInt, args: BuiltinList[Data]): Data =
        Data.Constr(ctor, PList.from(args.toList))

    /** Construct a Data.Map value.
      *
      * @param values
      *   list of key-value pairs
      */
    def mapData(values: BuiltinList[BuiltinPair[Data, Data]]): Data =
        Data.Map(PList.from(values.toList.map(p => (p.fst, p.snd))))

    /** Construct a Data.List value.
      *
      * @param values
      *   list of Data elements
      */
    def listData(values: BuiltinList[Data]): Data = Data.List(PList.from(values.toList))

    /** Construct a Data.I (integer) value. */
    def iData(value: BigInt): Data = Data.I(value)

    /** Construct a Data.B (bytestring) value. */
    def bData(value: ByteString): Data = Data.B(value)

    /** Deconstruct a Data.Constr value.
      *
      * @return
      *   a pair of (constructor index, list of arguments)
      * @throws java.lang.Exception
      *   if the Data is not a Constr
      */
    def unConstrData(d: Data): BuiltinPair[BigInt, BuiltinList[Data]] = d match
        case Data.Constr(constr, args) => BuiltinPair(constr, BuiltinList.from(args.toScalaList))
        case _                         => throw new Exception(s"not a constructor but $d")

    /** Deconstruct a Data.List value.
      *
      * @return
      *   the list of Data elements
      * @throws java.lang.Exception
      *   if the Data is not a List
      */
    def unListData(d: Data): BuiltinList[Data] = d match
        case Data.List(values) => BuiltinList.from(values.toScalaList)
        case _                 => throw new Exception(s"not a list but $d")

    /** Deconstruct a Data.Map value.
      *
      * @return
      *   the list of key-value pairs
      * @throws java.lang.Exception
      *   if the Data is not a Map
      */
    def unMapData(d: Data): BuiltinList[BuiltinPair[Data, Data]] = d match
        case Data.Map(values) => BuiltinList.from(values.toScalaList.map(BuiltinPair.apply))
        case _                => throw new Exception(s"not a list but $d")

    /** Deconstruct a Data.I value.
      *
      * @return
      *   the integer value
      * @throws java.lang.Exception
      *   if the Data is not an Integer
      */
    def unIData(d: Data): BigInt = d match
        case Data.I(value) => value
        case _             => throw new Exception(s"not an integer but $d")

    /** Deconstruct a Data.B value.
      *
      * @return
      *   the bytestring value
      * @throws java.lang.Exception
      *   if the Data is not a ByteString
      */
    def unBData(d: Data): ByteString = d match
        case Data.B(value) => value
        case _             => throw new Exception(s"not a bytestring but $d")

    /** Check if two Data values are structurally equal. */
    def equalsData(d1: Data, d2: Data): Boolean = d1 == d2

    /** Serialize a Data value to CBOR format.
      *
      * @return
      *   the CBOR-encoded bytestring
      * @since Plutus
      *   V2
      */
    def serialiseData(d: Data): ByteString =
        ByteString.fromArray(Cbor.encode(d).toByteArray)

    /** Create a pair of Data values.
      *
      * This is a monomorphic constructor for pairs of Data.
      */
    def mkPairData(fst: Data, snd: Data): BuiltinPair[Data, Data] = BuiltinPair(fst, snd)

    /** Create an empty list of Data values.
      *
      * This is a monomorphic constructor for empty lists of Data.
      */
    def mkNilData(): BuiltinList[Data] = BuiltinList.empty

    /** Create an empty list of Data pairs.
      *
      * This is a monomorphic constructor for empty lists of Data pairs, useful for building Maps.
      */
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

    // ============================================================================
    // BLS12-381 Elliptic Curve Operations (CIP-0381)
    // ============================================================================

    /** Check if two G1 points are equal.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        ps.bls12_381_G1_equal(p1, p2)

    /** Add two points in the G1 group.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_add(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ps.bls12_381_G1_add(p1, p2)

    /** Scalar multiplication of a G1 point.
      *
      * Equivalent to repeated addition of the point with itself `s` times.
      *
      * @param s
      *   the scalar multiplier
      * @param p
      *   the point to multiply
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ps.bls12_381_G1_scalarMul(s, p)

    /** Negate a G1 point.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_neg(p: BLS12_381_G1_Element): BLS12_381_G1_Element = ps.bls12_381_G1_neg(p)

    /** Compress a G1 point to its 48-byte compressed form.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString =
        ps.bls12_381_G1_compress(p)

    /** Uncompress a 48-byte compressed G1 point.
      *
      * @throws scalus.uplc.eval.BuiltinException
      *   if the bytestring is not a valid compressed G1 point
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_uncompress(bs)

    /** Hash a bytestring to a G1 point using the specified domain separation tag.
      *
      * @param bs
      *   the bytestring to hash
      * @param dst
      *   domain separation tag (up to 255 bytes)
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_hashToGroup(bs, dst)

    /** Check if two G2 points are equal.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        ps.bls12_381_G2_equal(p1, p2)

    /** Add two points in the G2 group.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_add(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ps.bls12_381_G2_add(p1, p2)

    /** Scalar multiplication of a G2 point.
      *
      * Equivalent to repeated addition of the point with itself `s` times.
      *
      * @param s
      *   the scalar multiplier
      * @param p
      *   the point to multiply
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ps.bls12_381_G2_scalarMul(s, p)

    /** Negate a G2 point.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_neg(p: BLS12_381_G2_Element): BLS12_381_G2_Element = ps.bls12_381_G2_neg(p)

    /** Compress a G2 point to its 96-byte compressed form.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString =
        ps.bls12_381_G2_compress(p)

    /** Uncompress a 96-byte compressed G2 point.
      *
      * @throws scalus.uplc.eval.BuiltinException
      *   if the bytestring is not a valid compressed G2 point
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element =
        ps.bls12_381_G2_uncompress(bs)

    /** Hash a bytestring to a G2 point using the specified domain separation tag.
      *
      * @param bs
      *   the bytestring to hash
      * @param dst
      *   domain separation tag (up to 255 bytes)
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element =
        ps.bls12_381_G2_hashToGroup(bs, dst)

    /** The compressed form of the point at infinity (zero) in G2, 96 bytes long.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_compressed_zero: ByteString = PlatformSpecific.bls12_381_G2_compressed_zero

    /** The compressed form of the generator point in G2, 96 bytes long.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_G2_compressed_generator: ByteString =
        PlatformSpecific.bls12_381_G2_compressed_generator

    /** Compute the Miller loop for a pairing.
      *
      * This is the first step in computing a bilinear pairing e(P, Q) where P is in G1 and Q is in
      * G2.
      *
      * @param p1
      *   a point in G1
      * @param p2
      *   a point in G2
      * @return
      *   an intermediate Miller loop result
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        ps.bls12_381_millerLoop(p1, p2)

    /** Multiply two Miller loop results.
      *
      * Used to combine multiple pairings: e(P1,Q1) * e(P2,Q2) = mulMlResult(millerLoop(P1,Q1),
      * millerLoop(P2,Q2))
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_mulMlResult(r1: BLS12_381_MlResult, r2: BLS12_381_MlResult): BLS12_381_MlResult =
        ps.bls12_381_mulMlResult(r1, r2)

    /** Final verification step for pairing equality check.
      *
      * Returns true if e(P1, Q1) == e(P2, Q2) where r1 = millerLoop(P1, Q1) and r2 = millerLoop(P2,
      * Q2).
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]
      * @since Plutus
      *   V3
      */
    def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean =
        ps.bls12_381_finalVerify(p1, p2)

    /** Multi-scalar multiplication on G1.
      *
      * Computes ∑(i=0 to N-1) scalar_i × point_i efficiently using Pippenger's algorithm. Both
      * input lists must be non-empty and have equal length.
      *
      * @param scalars
      *   List of scalars (integers)
      * @param points
      *   List of G1 group elements
      * @return
      *   The sum of scalar multiplications
      * @throws BuiltinException
      *   if either list is empty or lists have different lengths
      * @since Plutus
      *   V4 (CIP-133)
      */
    def bls12_381_G1_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[BLS12_381_G1_Element]
    ): BLS12_381_G1_Element =
        ps.bls12_381_G1_multiScalarMul(scalars, points)

    /** Multi-scalar multiplication on G2.
      *
      * Computes ∑(i=0 to N-1) scalar_i × point_i efficiently using Pippenger's algorithm. Both
      * input lists must be non-empty and have equal length.
      *
      * @param scalars
      *   List of scalars (integers)
      * @param points
      *   List of G2 group elements
      * @return
      *   The sum of scalar multiplications
      * @throws BuiltinException
      *   if either list is empty or lists have different lengths
      * @since Plutus
      *   V4 (CIP-133)
      */
    def bls12_381_G2_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[BLS12_381_G2_Element]
    ): BLS12_381_G2_Element =
        ps.bls12_381_G2_multiScalarMul(scalars, points)

    /** Compute Keccak-256 hash.
      *
      * This is the original Keccak hash (as used in Ethereum), not the NIST SHA-3 standard.
      *
      * @return
      *   a 32-byte (256-bit) hash digest
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0158 CIP-158]]
      * @since Plutus
      *   V3
      */
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

    // MaryEraValue builtins (CIP-0153)

    /** Insert or update a token amount in a value.
      *
      * @param currency
      *   The currency symbol (policy ID), max 32 bytes.
      * @param token
      *   The token name, max 32 bytes.
      * @param amount
      *   The amount to set. Use 0 to remove the token.
      * @param value
      *   The value to modify.
      * @return
      *   A new value with the token inserted/updated.
      */
    def insertCoin(
        currency: ByteString,
        token: ByteString,
        amount: BigInt,
        value: BuiltinValue
    ): BuiltinValue =
        scalus.uplc.eval.BuiltinValueOps.insertCoin(currency, token, amount, value)

    /** Lookup a token amount in a value.
      *
      * @param currency
      *   The currency symbol (policy ID).
      * @param token
      *   The token name.
      * @param value
      *   The value to search.
      * @return
      *   The amount of the token, or 0 if not found.
      */
    def lookupCoin(currency: ByteString, token: ByteString, value: BuiltinValue): BigInt =
        scalus.uplc.eval.BuiltinValueOps.lookupCoin(currency, token, value)

    /** Merge two values by adding corresponding token amounts.
      *
      * @param v1
      *   The first value.
      * @param v2
      *   The second value.
      * @return
      *   A new value with merged amounts.
      */
    def unionValue(v1: BuiltinValue, v2: BuiltinValue): BuiltinValue =
        scalus.uplc.eval.BuiltinValueOps.unionValue(v1, v2)

    /** Check if v1 contains at least the amounts in v2.
      *
      * @param v1
      *   The container value.
      * @param v2
      *   The value to check for containment.
      * @return
      *   true if v1 contains at least as much of every token as v2.
      */
    def valueContains(v1: BuiltinValue, v2: BuiltinValue): Boolean =
        scalus.uplc.eval.BuiltinValueOps.valueContains(v1, v2)

    /** Convert a BuiltinValue to Data representation.
      *
      * @param value
      *   The value to convert.
      * @return
      *   The Data representation of the value.
      */
    def valueData(value: BuiltinValue): Data =
        scalus.uplc.eval.BuiltinValueOps.toData(value)

    /** Convert Data to a BuiltinValue.
      *
      * @param data
      *   The data to convert. Must be in Map ByteString (Map ByteString Integer) format.
      * @return
      *   The BuiltinValue.
      */
    def unValueData(data: Data): BuiltinValue =
        scalus.uplc.eval.BuiltinValueOps.fromData(data)

    /** Multiply all token amounts in a value by a scalar.
      *
      * @param scalar
      *   The multiplier.
      * @param value
      *   The value to scale.
      * @return
      *   A new value with all amounts multiplied.
      */
    def scaleValue(scalar: BigInt, value: BuiltinValue): BuiltinValue =
        scalus.uplc.eval.BuiltinValueOps.scaleValue(scalar, value)

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
