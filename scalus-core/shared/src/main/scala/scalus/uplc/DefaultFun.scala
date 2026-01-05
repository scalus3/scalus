package scalus.uplc

import scalus.serialization.flat.{DecoderState, EncoderState, Flat}

/** Plutus Core built-in functions.
  *
  * This enum defines all available built-in functions in Untyped Plutus Core (UPLC). Built-in
  * functions provide primitive operations that cannot be expressed in the lambda calculus itself,
  * such as arithmetic, cryptographic operations, and data structure manipulation.
  *
  * ==Plutus Version Compatibility==
  *
  * Built-in functions are available in different Plutus versions:
  *   - '''Plutus V1''': Original set of builtins (integers, bytestrings, strings, Data, etc.)
  *   - '''Plutus V2''': Added `SerialiseData`
  *   - '''Plutus V3''': Added BLS12-381, bitwise operations, integer/bytestring conversions,
  *     additional hash functions (CIP-381, CIP-121, CIP-122, CIP-123, CIP-127)
  *   - '''Plutus V4''': Added array builtins, MaryEraValue builtins, DropList (CIP-153, CIP-156,
  *     CIP-158)
  *
  * ==Type Signatures==
  *
  * Built-in functions have specific type signatures. Polymorphic builtins require explicit type
  * instantiation via `force` before application.
  *
  * ==Categories==
  *
  * The builtins are organized into the following categories:
  *   - '''Integer operations''': Arithmetic and comparison on arbitrary-precision integers
  *   - '''ByteString operations''': Manipulation of byte arrays
  *   - '''Cryptographic functions''': Hashing and signature verification
  *   - '''String operations''': UTF-8 string manipulation
  *   - '''Control flow''': Conditionals and unit handling
  *   - '''Pairs and Lists''': Data structure operations
  *   - '''Data type''': Operations on the universal `Data` type
  *   - '''BLS12-381''': Pairing-friendly elliptic curve operations (Plutus V3+)
  *   - '''Bitwise operations''': Bit-level manipulation of bytestrings (Plutus V3+)
  *   - '''Value operations''': Multi-asset value manipulation (Plutus V4+)
  *
  * @see
  *   [[https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf Plutus Core Specification]]
  * @see
  *   [[scalus.builtin.Builtins]] for the Scala implementations
  */
enum DefaultFun extends Enum[DefaultFun]:

    // ============================================================================
    // Integer Operations
    // ============================================================================
    // All integer builtins operate on arbitrary-precision integers (BigInt).
    // Available since Plutus V1.

    /** Add two integers.
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * @example
      *   `addInteger(3, 5)` returns `8`
      * @since Plutus
      *   V1
      */
    case AddInteger

    /** Subtract two integers.
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * @example
      *   `subtractInteger(10, 3)` returns `7`
      * @since Plutus
      *   V1
      */
    case SubtractInteger

    /** Multiply two integers.
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * @example
      *   `multiplyInteger(4, 5)` returns `20`
      * @since Plutus
      *   V1
      */
    case MultiplyInteger

    /** Divide two integers using floor division (truncate toward negative infinity).
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * For negative dividends with positive divisors (or vice versa), this differs from truncated
      * division (quotientInteger) by rounding toward negative infinity.
      *
      * @example
      *   `divideInteger(7, 2)` returns `3`
      * @example
      *   `divideInteger(-7, 2)` returns `-4` (floor division)
      * @throws BuiltinException
      *   if divisor is zero
      * @since Plutus
      *   V1
      */
    case DivideInteger

    /** Compute the quotient of two integers (truncate toward zero).
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * This is standard truncated division, which rounds toward zero.
      *
      * @example
      *   `quotientInteger(7, 2)` returns `3`
      * @example
      *   `quotientInteger(-7, 2)` returns `-3` (truncated toward zero)
      * @throws BuiltinException
      *   if divisor is zero
      * @since Plutus
      *   V1
      */
    case QuotientInteger

    /** Compute the remainder after truncated division (quotientInteger).
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * The result has the same sign as the dividend. Satisfies:
      * `dividend = quotientInteger(dividend, divisor) * divisor + remainderInteger(dividend, divisor)`
      *
      * @example
      *   `remainderInteger(7, 3)` returns `1`
      * @example
      *   `remainderInteger(-7, 3)` returns `-1`
      * @throws BuiltinException
      *   if divisor is zero
      * @since Plutus
      *   V1
      */
    case RemainderInteger

    /** Compute the modulus after floor division (divideInteger).
      *
      * '''Type:''' `Integer -> Integer -> Integer`
      *
      * The result has the same sign as the divisor. Satisfies:
      * `dividend = divideInteger(dividend, divisor) * divisor + modInteger(dividend, divisor)`
      *
      * @example
      *   `modInteger(7, 3)` returns `1`
      * @example
      *   `modInteger(-7, 3)` returns `2` (result has sign of divisor)
      * @throws BuiltinException
      *   if divisor is zero
      * @since Plutus
      *   V1
      */
    case ModInteger

    /** Check if two integers are equal.
      *
      * '''Type:''' `Integer -> Integer -> Bool`
      *
      * @example
      *   `equalsInteger(5, 5)` returns `true`
      * @since Plutus
      *   V1
      */
    case EqualsInteger

    /** Check if the first integer is strictly less than the second.
      *
      * '''Type:''' `Integer -> Integer -> Bool`
      *
      * @example
      *   `lessThanInteger(3, 5)` returns `true`
      * @since Plutus
      *   V1
      */
    case LessThanInteger

    /** Check if the first integer is less than or equal to the second.
      *
      * '''Type:''' `Integer -> Integer -> Bool`
      *
      * @example
      *   `lessThanEqualsInteger(5, 5)` returns `true`
      * @since Plutus
      *   V1
      */
    case LessThanEqualsInteger

    // ============================================================================
    // ByteString Operations
    // ============================================================================
    // ByteStrings are immutable sequences of bytes.
    // Available since Plutus V1.

    /** Concatenate two bytestrings.
      *
      * '''Type:''' `ByteString -> ByteString -> ByteString`
      *
      * @example
      *   `appendByteString(hex"1234", hex"5678")` returns `hex"12345678"`
      * @since Plutus
      *   V1
      */
    case AppendByteString

    /** Prepend a byte to a bytestring.
      *
      * '''Type:''' `Integer -> ByteString -> ByteString`
      *
      * The integer must be in the range [0, 255].
      *
      * @example
      *   `consByteString(0x12, hex"3456")` returns `hex"123456"`
      * @throws BuiltinException
      *   if byte value is outside [0, 255]
      * @since Plutus
      *   V1
      */
    case ConsByteString

    /** Extract a slice from a bytestring.
      *
      * '''Type:''' `Integer -> Integer -> ByteString -> ByteString`
      *
      * Parameters are: start index (inclusive), length, and source bytestring. If start is beyond
      * the end, returns empty. If length exceeds available bytes, returns as many bytes as
      * available.
      *
      * @example
      *   `sliceByteString(2, 4, hex"1234567890abcdef")` returns `hex"567890ab"`
      * @example
      *   `sliceByteString(5, 10, hex"1234567890abcdef")` returns `hex"abcdef"` (truncated)
      * @since Plutus
      *   V1
      */
    case SliceByteString

    /** Get the length of a bytestring in bytes.
      *
      * '''Type:''' `ByteString -> Integer`
      *
      * @example
      *   `lengthOfByteString(hex"1234")` returns `2`
      * @since Plutus
      *   V1
      */
    case LengthOfByteString

    /** Get the byte at a specific index in a bytestring.
      *
      * '''Type:''' `ByteString -> Integer -> Integer`
      *
      * Returns an unsigned byte value in [0, 255]. Index is 0-based.
      *
      * @example
      *   `indexByteString(hex"1234", 0)` returns `0x12`
      * @throws BuiltinException
      *   if index is out of bounds
      * @since Plutus
      *   V1
      */
    case IndexByteString

    /** Check if two bytestrings are equal.
      *
      * '''Type:''' `ByteString -> ByteString -> Bool`
      *
      * @example
      *   `equalsByteString(hex"1234", hex"1234")` returns `true`
      * @since Plutus
      *   V1
      */
    case EqualsByteString

    /** Check if the first bytestring is lexicographically less than the second.
      *
      * '''Type:''' `ByteString -> ByteString -> Bool`
      *
      * Comparison is byte-by-byte, treating bytes as unsigned. Shorter bytestrings are considered
      * less than longer ones with the same prefix.
      *
      * @example
      *   `lessThanByteString(hex"12", hex"1234")` returns `true`
      * @since Plutus
      *   V1
      */
    case LessThanByteString

    /** Check if the first bytestring is lexicographically less than or equal to the second.
      *
      * '''Type:''' `ByteString -> ByteString -> Bool`
      *
      * @example
      *   `lessThanEqualsByteString(hex"1234", hex"1234")` returns `true`
      * @since Plutus
      *   V1
      */
    case LessThanEqualsByteString

    // ============================================================================
    // Cryptographic Hash Functions
    // ============================================================================
    // Hash functions produce fixed-size digests from arbitrary input.

    /** Compute SHA2-256 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 32-byte (256-bit) hash digest.
      *
      * @example
      *   `sha2_256(hex"")` returns the SHA2-256 hash of empty input
      * @since Plutus
      *   V1
      */
    case Sha2_256

    /** Compute SHA3-256 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 32-byte (256-bit) hash digest using the Keccak-based SHA3.
      *
      * @since Plutus
      *   V1
      */
    case Sha3_256

    /** Compute BLAKE2b-256 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 32-byte (256-bit) hash digest. This is the primary hash function used in Cardano
      * for script hashing and transaction IDs.
      *
      * @since Plutus
      *   V1
      */
    case Blake2b_256

    /** Verify an Ed25519 signature.
      *
      * '''Type:''' `ByteString -> ByteString -> ByteString -> Bool`
      *
      * Parameters are: public key (32 bytes), message, signature (64 bytes).
      *
      * @example
      *   `verifyEd25519Signature(pubKey, message, signature)` returns `true` if valid
      * @throws BuiltinException
      *   if key or signature has wrong length
      * @since Plutus
      *   V1
      */
    case VerifyEd25519Signature

    /** Verify an ECDSA signature on the secp256k1 curve.
      *
      * '''Type:''' `ByteString -> ByteString -> ByteString -> Bool`
      *
      * Parameters are: public key (33 bytes compressed), message hash (32 bytes), signature (64
      * bytes, r||s format).
      *
      * @note
      *   The message must be pre-hashed (typically with SHA-256 or Keccak-256).
      * @throws BuiltinException
      *   if inputs have wrong lengths
      * @since Plutus
      *   V2
      */
    case VerifyEcdsaSecp256k1Signature

    /** Verify a Schnorr signature on the secp256k1 curve (BIP-340).
      *
      * '''Type:''' `ByteString -> ByteString -> ByteString -> Bool`
      *
      * Parameters are: public key (32 bytes x-only), message (arbitrary), signature (64 bytes).
      *
      * @since Plutus
      *   V2
      */
    case VerifySchnorrSecp256k1Signature

    // ============================================================================
    // String Operations
    // ============================================================================
    // Strings are sequences of Unicode characters, encoded as UTF-8.
    // Available since Plutus V1.

    /** Concatenate two strings.
      *
      * '''Type:''' `String -> String -> String`
      *
      * @example
      *   `appendString("Hello, ", "World!")` returns `"Hello, World!"`
      * @since Plutus
      *   V1
      */
    case AppendString

    /** Check if two strings are equal.
      *
      * '''Type:''' `String -> String -> Bool`
      *
      * @since Plutus
      *   V1
      */
    case EqualsString

    /** Encode a string as UTF-8 bytes.
      *
      * '''Type:''' `String -> ByteString`
      *
      * @example
      *   `encodeUtf8("hello")` returns `hex"68656c6c6f"`
      * @since Plutus
      *   V1
      */
    case EncodeUtf8

    /** Decode UTF-8 bytes to a string.
      *
      * '''Type:''' `ByteString -> String`
      *
      * @throws BuiltinException
      *   if the bytestring is not valid UTF-8
      * @since Plutus
      *   V1
      */
    case DecodeUtf8

    // ============================================================================
    // Control Flow
    // ============================================================================

    /** Conditional expression (if-then-else).
      *
      * '''Type:''' `∀a. Bool -> a -> a -> a`
      *
      * Requires one `force` to instantiate the type variable. Returns the second argument if the
      * condition is true, third argument otherwise.
      *
      * @example
      *   `force(ifThenElse) true x y` evaluates to `x`
      * @since Plutus
      *   V1
      */
    case IfThenElse

    /** Choose based on unit value.
      *
      * '''Type:''' `∀a. Unit -> a -> a`
      *
      * Requires one `force` to instantiate the type variable. Always returns the second argument
      * after evaluating the first (which must be unit).
      *
      * @since Plutus
      *   V1
      */
    case ChooseUnit

    // ============================================================================
    // Tracing (Debugging)
    // ============================================================================

    /** Trace a message and return a value.
      *
      * '''Type:''' `∀a. String -> a -> a`
      *
      * Requires one `force` to instantiate the type variable. Logs the string message (for
      * debugging) and returns the second argument unchanged.
      *
      * @note
      *   In on-chain execution, traces are typically collected but don't affect the result.
      * @since Plutus
      *   V1
      */
    case Trace

    // ============================================================================
    // Pair Operations
    // ============================================================================
    // Pairs are polymorphic 2-tuples.
    // Available since Plutus V1.

    /** Extract the first element of a pair.
      *
      * '''Type:''' `∀a b. Pair a b -> a`
      *
      * Requires two `force` applications to instantiate both type variables.
      *
      * @since Plutus
      *   V1
      */
    case FstPair

    /** Extract the second element of a pair.
      *
      * '''Type:''' `∀a b. Pair a b -> b`
      *
      * Requires two `force` applications to instantiate both type variables.
      *
      * @since Plutus
      *   V1
      */
    case SndPair

    // ============================================================================
    // List Operations
    // ============================================================================
    // Lists are polymorphic, homogeneous, immutable linked lists.
    // Available since Plutus V1.

    /** Pattern match on a list (empty vs non-empty).
      *
      * '''Type:''' `∀a b. List a -> b -> b -> b`
      *
      * Requires two `force` applications. Returns the second argument if the list is empty, third
      * argument otherwise.
      *
      * @since Plutus
      *   V1
      */
    case ChooseList

    /** Prepend an element to a list.
      *
      * '''Type:''' `∀a. a -> List a -> List a`
      *
      * Requires one `force` application.
      *
      * @example
      *   `mkCons(1, mkCons(2, nil))` creates list `[1, 2]`
      * @since Plutus
      *   V1
      */
    case MkCons

    /** Get the first element of a non-empty list.
      *
      * '''Type:''' `∀a. List a -> a`
      *
      * Requires one `force` application.
      *
      * @throws BuiltinException
      *   if the list is empty
      * @since Plutus
      *   V1
      */
    case HeadList

    /** Get all elements except the first from a non-empty list.
      *
      * '''Type:''' `∀a. List a -> List a`
      *
      * Requires one `force` application.
      *
      * @throws BuiltinException
      *   if the list is empty
      * @since Plutus
      *   V1
      */
    case TailList

    /** Check if a list is empty.
      *
      * '''Type:''' `∀a. List a -> Bool`
      *
      * Requires one `force` application.
      *
      * @since Plutus
      *   V1
      */
    case NullList

    // ============================================================================
    // Data Type Operations
    // ============================================================================
    // The Data type is a universal serializable representation used for script datums,
    // redeemers, and encoding arbitrary Plutus data structures.
    // Available since Plutus V1.

    /** Pattern match on the Data type.
      *
      * '''Type:''' `∀a. Data -> a -> a -> a -> a -> a -> a`
      *
      * Requires one `force` application. Arguments after Data are: constr case, map case, list
      * case, integer case, bytestring case. Returns the argument corresponding to the Data
      * constructor.
      *
      * @since Plutus
      *   V1
      */
    case ChooseData

    /** Construct a Data value representing a constructor application.
      *
      * '''Type:''' `Integer -> List Data -> Data`
      *
      * @example
      *   `constrData(0, [iData(42)])` creates `Constr 0 [I 42]`
      * @since Plutus
      *   V1
      */
    case ConstrData

    /** Construct a Data value representing a map.
      *
      * '''Type:''' `List (Pair Data Data) -> Data`
      *
      * @since Plutus
      *   V1
      */
    case MapData

    /** Construct a Data value representing a list.
      *
      * '''Type:''' `List Data -> Data`
      *
      * @since Plutus
      *   V1
      */
    case ListData

    /** Construct a Data value representing an integer.
      *
      * '''Type:''' `Integer -> Data`
      *
      * @example
      *   `iData(42)` creates `I 42`
      * @since Plutus
      *   V1
      */
    case IData

    /** Construct a Data value representing a bytestring.
      *
      * '''Type:''' `ByteString -> Data`
      *
      * @example
      *   `bData(hex"1234")` creates `B #1234`
      * @since Plutus
      *   V1
      */
    case BData

    /** Extract constructor index and arguments from a Constr Data value.
      *
      * '''Type:''' `Data -> Pair Integer (List Data)`
      *
      * @throws BuiltinException
      *   if the Data is not a Constr
      * @since Plutus
      *   V1
      */
    case UnConstrData

    /** Extract the map from a Map Data value.
      *
      * '''Type:''' `Data -> List (Pair Data Data)`
      *
      * @throws BuiltinException
      *   if the Data is not a Map
      * @since Plutus
      *   V1
      */
    case UnMapData

    /** Extract the list from a List Data value.
      *
      * '''Type:''' `Data -> List Data`
      *
      * @throws BuiltinException
      *   if the Data is not a List
      * @since Plutus
      *   V1
      */
    case UnListData

    /** Extract the integer from an Integer Data value.
      *
      * '''Type:''' `Data -> Integer`
      *
      * @throws BuiltinException
      *   if the Data is not an Integer
      * @since Plutus
      *   V1
      */
    case UnIData

    /** Extract the bytestring from a ByteString Data value.
      *
      * '''Type:''' `Data -> ByteString`
      *
      * @throws BuiltinException
      *   if the Data is not a ByteString
      * @since Plutus
      *   V1
      */
    case UnBData

    /** Check if two Data values are equal.
      *
      * '''Type:''' `Data -> Data -> Bool`
      *
      * Performs deep structural equality comparison.
      *
      * @since Plutus
      *   V1
      */
    case EqualsData

    /** Serialize a Data value to CBOR bytes.
      *
      * '''Type:''' `Data -> ByteString`
      *
      * Produces CBOR-encoded representation of the Data value.
      *
      * @since Plutus
      *   V2
      */
    case SerialiseData

    // ============================================================================
    // Data Constructors
    // ============================================================================
    // Monomorphized constructors for common empty structures.
    // Available since Plutus V1.

    /** Create a pair of Data values.
      *
      * '''Type:''' `Data -> Data -> Pair Data Data`
      *
      * This is a monomorphized version of pair construction for Data.
      *
      * @since Plutus
      *   V1
      */
    case MkPairData

    /** Create an empty list of Data values.
      *
      * '''Type:''' `Unit -> List Data`
      *
      * @since Plutus
      *   V1
      */
    case MkNilData

    /** Create an empty list of Data pairs.
      *
      * '''Type:''' `Unit -> List (Pair Data Data)`
      *
      * Useful for constructing empty maps.
      *
      * @since Plutus
      *   V1
      */
    case MkNilPairData

    // ============================================================================
    // BLS12-381 Elliptic Curve Operations
    // ============================================================================
    // BLS12-381 is a pairing-friendly elliptic curve used for zero-knowledge proofs
    // and advanced cryptographic protocols.
    // Available since Plutus V3 (CIP-0381).
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0381 CIP-381]]

    // G1 Group Operations (48-byte compressed points)

    /** Add two G1 curve points.
      *
      * '''Type:''' `G1_Element -> G1_Element -> G1_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_add

    /** Negate a G1 curve point.
      *
      * '''Type:''' `G1_Element -> G1_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_neg

    /** Scalar multiplication on G1.
      *
      * '''Type:''' `Integer -> G1_Element -> G1_Element`
      *
      * Multiplies the point by a scalar integer.
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_scalarMul

    /** Check equality of two G1 points.
      *
      * '''Type:''' `G1_Element -> G1_Element -> Bool`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_equal

    /** Hash arbitrary data to a G1 curve point.
      *
      * '''Type:''' `ByteString -> ByteString -> G1_Element`
      *
      * Uses hash-to-curve with a domain separation tag (DST). First argument is the message, second
      * is the DST.
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_hashToGroup

    /** Compress a G1 point to 48 bytes.
      *
      * '''Type:''' `G1_Element -> ByteString`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_compress

    /** Decompress a 48-byte representation to a G1 point.
      *
      * '''Type:''' `ByteString -> G1_Element`
      *
      * @throws BuiltinException
      *   if the bytes don't represent a valid G1 point
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G1_uncompress

    // G2 Group Operations (96-byte compressed points)

    /** Add two G2 curve points.
      *
      * '''Type:''' `G2_Element -> G2_Element -> G2_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_add

    /** Negate a G2 curve point.
      *
      * '''Type:''' `G2_Element -> G2_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_neg

    /** Scalar multiplication on G2.
      *
      * '''Type:''' `Integer -> G2_Element -> G2_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_scalarMul

    /** Check equality of two G2 points.
      *
      * '''Type:''' `G2_Element -> G2_Element -> Bool`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_equal

    /** Hash arbitrary data to a G2 curve point.
      *
      * '''Type:''' `ByteString -> ByteString -> G2_Element`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_hashToGroup

    /** Compress a G2 point to 96 bytes.
      *
      * '''Type:''' `G2_Element -> ByteString`
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_compress

    /** Decompress a 96-byte representation to a G2 point.
      *
      * '''Type:''' `ByteString -> G2_Element`
      *
      * @throws BuiltinException
      *   if the bytes don't represent a valid G2 point
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_G2_uncompress

    // Pairing Operations

    /** Compute the Miller loop for pairing.
      *
      * '''Type:''' `G1_Element -> G2_Element -> MlResult`
      *
      * Computes the intermediate pairing result, which can be combined with `mulMlResult` and
      * verified with `finalVerify`.
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_millerLoop

    /** Multiply two Miller loop results.
      *
      * '''Type:''' `MlResult -> MlResult -> MlResult`
      *
      * Used to combine multiple pairing computations before final verification.
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_mulMlResult

    /** Perform final pairing verification.
      *
      * '''Type:''' `MlResult -> MlResult -> Bool`
      *
      * Returns true if the final exponentiation of both results are equal, i.e., checks that
      * `e(P1, Q1) * ... = e(P2, Q2) * ...`.
      *
      * @since Plutus
      *   V3 (CIP-381)
      */
    case Bls12_381_finalVerify

    /** Multi-scalar multiplication on G1.
      *
      * '''Type:''' `[Integer] -> [G1_Element] -> G1_Element`
      *
      * Computes the sum ∑(i=0 to N-1) scalar_i × point_i efficiently using Pippenger's algorithm.
      * Both input lists must be non-empty and have equal length.
      *
      * @throws BuiltinException
      *   if either list is empty or lists have different lengths
      * @since Plutus
      *   V4 (CIP-133)
      */
    case Bls12_381_G1_multiScalarMul

    /** Multi-scalar multiplication on G2.
      *
      * '''Type:''' `[Integer] -> [G2_Element] -> G2_Element`
      *
      * Computes the sum ∑(i=0 to N-1) scalar_i × point_i efficiently using Pippenger's algorithm.
      * Both input lists must be non-empty and have equal length.
      *
      * @throws BuiltinException
      *   if either list is empty or lists have different lengths
      * @since Plutus
      *   V4 (CIP-133)
      */
    case Bls12_381_G2_multiScalarMul

    // ============================================================================
    // Additional Hash Functions
    // ============================================================================

    /** Compute Keccak-256 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 32-byte hash. This is the hash function used by Ethereum.
      *
      * @note
      *   This is Keccak-256, not the NIST SHA3-256 standard (which differs in padding).
      * @since Plutus
      *   V3
      */
    case Keccak_256

    /** Compute BLAKE2b-224 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 28-byte (224-bit) hash digest.
      *
      * @since Plutus
      *   V3
      */
    case Blake2b_224

    // ============================================================================
    // Integer/ByteString Conversions (CIP-121)
    // ============================================================================
    // Bidirectional conversion between integers and bytestrings.
    // Available since Plutus V3.
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121 CIP-121]]

    /** Convert an integer to a bytestring.
      *
      * '''Type:''' `Bool -> Integer -> Integer -> ByteString`
      *
      * Parameters:
      *   - First Bool: `true` for big-endian, `false` for little-endian
      *   - First Integer: target length in bytes (0 for minimal representation)
      *   - Second Integer: the unsigned integer to convert (must be non-negative)
      *
      * @example
      *   `integerToByteString(true, 2, 4660)` returns `hex"1234"`
      * @example
      *   `integerToByteString(false, 2, 4660)` returns `hex"3412"` (little-endian)
      * @throws BuiltinException
      *   if integer is negative or doesn't fit in specified length
      * @since Plutus
      *   V3 (CIP-121)
      */
    case IntegerToByteString

    /** Convert a bytestring to an unsigned integer.
      *
      * '''Type:''' `Bool -> ByteString -> Integer`
      *
      * Parameters:
      *   - Bool: `true` for big-endian, `false` for little-endian
      *   - ByteString: the bytes to interpret as an unsigned integer
      *
      * @example
      *   `byteStringToInteger(true, hex"1234")` returns `4660`
      * @since Plutus
      *   V3 (CIP-121)
      */
    case ByteStringToInteger

    // ============================================================================
    // Bitwise Logical Operations (CIP-122)
    // ============================================================================
    // Bitwise operations on bytestrings.
    // Available since Plutus V3.
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122 CIP-122]]

    /** Bitwise AND of two bytestrings.
      *
      * '''Type:''' `Bool -> ByteString -> ByteString -> ByteString`
      *
      * The Bool controls padding behavior for different-length inputs:
      *   - `false`: result has length of shorter input (truncating)
      *   - `true`: result has length of longer input (padding with remaining bytes)
      *
      * @example
      *   `andByteString(false, hex"0FFF", hex"FF")` returns `hex"0F"`
      * @example
      *   `andByteString(true, hex"0FFF", hex"FF")` returns `hex"0FFF"`
      * @since Plutus
      *   V3 (CIP-122)
      */
    case AndByteString

    /** Bitwise OR of two bytestrings.
      *
      * '''Type:''' `Bool -> ByteString -> ByteString -> ByteString`
      *
      * @example
      *   `orByteString(false, hex"0FFF", hex"FF")` returns `hex"FF"`
      * @example
      *   `orByteString(true, hex"0FFF", hex"FF")` returns `hex"FFFF"`
      * @since Plutus
      *   V3 (CIP-122)
      */
    case OrByteString

    /** Bitwise XOR of two bytestrings.
      *
      * '''Type:''' `Bool -> ByteString -> ByteString -> ByteString`
      *
      * @example
      *   `xorByteString(false, hex"0FFF", hex"FF")` returns `hex"F0"`
      * @since Plutus
      *   V3 (CIP-122)
      */
    case XorByteString

    /** Bitwise complement (NOT) of a bytestring.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Inverts all bits in the bytestring.
      *
      * @example
      *   `complementByteString(hex"F0")` returns `hex"0F"`
      * @since Plutus
      *   V3 (CIP-122)
      */
    case ComplementByteString

    /** Read a single bit from a bytestring.
      *
      * '''Type:''' `ByteString -> Integer -> Bool`
      *
      * Bit indexing starts from the end (least significant bit of last byte is index 0).
      *
      * @example
      *   `readBit(hex"0004", 2)` returns `true`
      * @throws BuiltinException
      *   if index is out of bounds
      * @since Plutus
      *   V3 (CIP-122)
      */
    case ReadBit

    /** Write multiple bits in a bytestring.
      *
      * '''Type:''' `ByteString -> List Integer -> Bool -> ByteString`
      *
      * Sets all bits at the specified indices to the given boolean value.
      *
      * @example
      *   `writeBits(hex"0000", [0, 1, 2, 3], true)` returns `hex"000F"`
      * @throws BuiltinException
      *   if any index is out of bounds
      * @since Plutus
      *   V3 (CIP-122)
      */
    case WriteBits

    /** Create a bytestring by repeating a byte value.
      *
      * '''Type:''' `Integer -> Integer -> ByteString`
      *
      * First integer is length, second is the byte value [0, 255].
      *
      * @example
      *   `replicateByte(4, 0xFF)` returns `hex"FFFFFFFF"`
      * @throws BuiltinException
      *   if length is negative or byte is outside [0, 255]
      * @since Plutus
      *   V3 (CIP-122)
      */
    case ReplicateByte

    // ============================================================================
    // Bitwise Shift and Rotate Operations (CIP-123)
    // ============================================================================
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0123 CIP-123]]

    /** Shift bits in a bytestring.
      *
      * '''Type:''' `ByteString -> Integer -> ByteString`
      *
      * Positive shift moves bits left (toward higher indices), negative moves right. Bits shifted
      * out are lost; new bits are zero.
      *
      * @example
      *   `shiftByteString(hex"000F", 4)` returns `hex"00F0"`
      * @example
      *   `shiftByteString(hex"000F", -4)` returns `hex"0000"`
      * @since Plutus
      *   V3 (CIP-123)
      */
    case ShiftByteString

    /** Rotate bits in a bytestring.
      *
      * '''Type:''' `ByteString -> Integer -> ByteString`
      *
      * Positive rotation moves bits left; bits shifted out wrap around to the right.
      *
      * @example
      *   `rotateByteString(hex"000F", 4)` returns `hex"00F0"`
      * @example
      *   `rotateByteString(hex"000F", -4)` returns `hex"F000"`
      * @since Plutus
      *   V3 (CIP-123)
      */
    case RotateByteString

    /** Count the number of set bits (population count).
      *
      * '''Type:''' `ByteString -> Integer`
      *
      * @example
      *   `countSetBits(hex"000F")` returns `4`
      * @since Plutus
      *   V3 (CIP-123)
      */
    case CountSetBits

    /** Find the index of the first set bit.
      *
      * '''Type:''' `ByteString -> Integer`
      *
      * Returns -1 if no bits are set. Index 0 is the least significant bit.
      *
      * @example
      *   `findFirstSetBit(hex"0002")` returns `1`
      * @example
      *   `findFirstSetBit(hex"0000")` returns `-1`
      * @since Plutus
      *   V3 (CIP-123)
      */
    case FindFirstSetBit

    // ============================================================================
    // RIPEMD-160 Hash (CIP-127)
    // ============================================================================

    /** Compute RIPEMD-160 hash.
      *
      * '''Type:''' `ByteString -> ByteString`
      *
      * Produces a 20-byte (160-bit) hash digest. Used in Bitcoin address derivation.
      *
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0127 CIP-127]]
      * @since Plutus
      *   V3
      */
    case Ripemd_160

    // ============================================================================
    // List Extensions (CIP-158)
    // ============================================================================

    /** Drop the first n elements from a list.
      *
      * '''Type:''' `∀a. Integer -> List a -> List a`
      *
      * Requires one `force` application. If n is negative, returns the original list. If n exceeds
      * the list length, returns an empty list.
      *
      * @example
      *   `dropList(2, [1, 2, 3, 4])` returns `[3, 4]`
      * @see
      *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0158 CIP-158]]
      * @since Plutus
      *   V4
      */
    case DropList

    // ============================================================================
    // Array Operations (CIP-156)
    // ============================================================================
    // Arrays provide O(1) indexed access, complementing linked lists.
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0156 CIP-156]]

    /** Get the length of an array.
      *
      * '''Type:''' `∀a. Array a -> Integer`
      *
      * Requires one `force` application.
      *
      * @since Plutus
      *   V4 (CIP-156)
      */
    case LengthOfArray

    /** Convert a list to an array.
      *
      * '''Type:''' `∀a. List a -> Array a`
      *
      * Requires one `force` application.
      *
      * @since Plutus
      *   V4 (CIP-156)
      */
    case ListToArray

    /** Access an element by index.
      *
      * '''Type:''' `∀a. Array a -> Integer -> a`
      *
      * Requires one `force` application.
      *
      * @throws BuiltinException
      *   if index is out of bounds
      * @since Plutus
      *   V4 (CIP-156)
      */
    case IndexArray

    /** Access multiple elements by indices.
      *
      * '''Type:''' `∀a. List Integer -> Array a -> List a`
      *
      * Requires one `force` application. Returns elements at the specified indices in order.
      *
      * @throws BuiltinException
      *   if any index is out of bounds
      * @since Plutus
      *   V4 (CIP-156)
      */
    case MultiIndexArray

    // ============================================================================
    // MaryEraValue Operations (CIP-153)
    // ============================================================================
    // Native multi-asset value operations using a dedicated Value type.
    // More efficient than encoding values as Data for on-chain manipulation.
    // @see [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0153 CIP-153]]

    /** Insert or update a token amount in a Value.
      *
      * '''Type:''' `ByteString -> ByteString -> Integer -> Value -> Value`
      *
      * Parameters: currency symbol (policy ID, max 32 bytes), token name (max 32 bytes), amount,
      * and the Value to modify. Use amount 0 to remove a token.
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case InsertCoin

    /** Look up a token amount in a Value.
      *
      * '''Type:''' `ByteString -> ByteString -> Value -> Integer`
      *
      * Returns 0 if the token is not present.
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case LookupCoin

    /** Merge two Values by adding corresponding token amounts.
      *
      * '''Type:''' `Value -> Value -> Value`
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case UnionValue

    /** Check if one Value contains at least the amounts in another.
      *
      * '''Type:''' `Value -> Value -> Bool`
      *
      * Returns true if the first Value has at least as much of every token as the second Value.
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case ValueContains

    /** Convert a Value to its Data representation.
      *
      * '''Type:''' `Value -> Data`
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case ValueData

    /** Convert Data to a Value.
      *
      * '''Type:''' `Data -> Value`
      *
      * The Data must have the structure `Map ByteString (Map ByteString Integer)`.
      *
      * @throws BuiltinException
      *   if Data doesn't have the expected structure
      * @since Plutus
      *   V4 (CIP-153)
      */
    case UnValueData

    /** Scale all token amounts in a Value by a multiplier.
      *
      * '''Type:''' `Integer -> Value -> Value`
      *
      * @since Plutus
      *   V4 (CIP-153)
      */
    case ScaleValue

object DefaultFun {
    given Flat[DefaultFun] with
        def bitSize(a: DefaultFun): Int = 7

        def encode(a: DefaultFun, encode: EncoderState): Unit =
            val code = a match
                case AddInteger            => 0
                case SubtractInteger       => 1
                case MultiplyInteger       => 2
                case DivideInteger         => 3
                case QuotientInteger       => 4
                case RemainderInteger      => 5
                case ModInteger            => 6
                case EqualsInteger         => 7
                case LessThanInteger       => 8
                case LessThanEqualsInteger => 9

                case AppendByteString         => 10
                case ConsByteString           => 11
                case SliceByteString          => 12
                case LengthOfByteString       => 13
                case IndexByteString          => 14
                case EqualsByteString         => 15
                case LessThanByteString       => 16
                case LessThanEqualsByteString => 17

                case Sha2_256               => 18
                case Sha3_256               => 19
                case Blake2b_256            => 20
                case VerifyEd25519Signature => 21

                case AppendString => 22
                case EqualsString => 23
                case EncodeUtf8   => 24
                case DecodeUtf8   => 25

                case IfThenElse => 26

                case ChooseUnit => 27

                case Trace => 28

                case FstPair => 29
                case SndPair => 30

                case ChooseList => 31
                case MkCons     => 32
                case HeadList   => 33
                case TailList   => 34
                case NullList   => 35

                case ChooseData                      => 36
                case ConstrData                      => 37
                case MapData                         => 38
                case ListData                        => 39
                case IData                           => 40
                case BData                           => 41
                case UnConstrData                    => 42
                case UnMapData                       => 43
                case UnListData                      => 44
                case UnIData                         => 45
                case UnBData                         => 46
                case EqualsData                      => 47
                case MkPairData                      => 48
                case MkNilData                       => 49
                case MkNilPairData                   => 50
                case SerialiseData                   => 51
                case VerifyEcdsaSecp256k1Signature   => 52
                case VerifySchnorrSecp256k1Signature => 53
                case Bls12_381_G1_add                => 54
                case Bls12_381_G1_neg                => 55
                case Bls12_381_G1_scalarMul          => 56
                case Bls12_381_G1_equal              => 57
                case Bls12_381_G1_compress           => 58
                case Bls12_381_G1_uncompress         => 59
                case Bls12_381_G1_hashToGroup        => 60
                case Bls12_381_G2_add                => 61
                case Bls12_381_G2_neg                => 62
                case Bls12_381_G2_scalarMul          => 63
                case Bls12_381_G2_equal              => 64
                case Bls12_381_G2_compress           => 65
                case Bls12_381_G2_uncompress         => 66
                case Bls12_381_G2_hashToGroup        => 67
                case Bls12_381_millerLoop            => 68
                case Bls12_381_mulMlResult           => 69
                case Bls12_381_finalVerify           => 70
                case Bls12_381_G1_multiScalarMul     => 92
                case Bls12_381_G2_multiScalarMul     => 93
                case Keccak_256                      => 71
                case Blake2b_224                     => 72

                case IntegerToByteString  => 73
                case ByteStringToInteger  => 74
                case AndByteString        => 75
                case OrByteString         => 76
                case XorByteString        => 77
                case ComplementByteString => 78
                case ReadBit              => 79
                case WriteBits            => 80
                case ReplicateByte        => 81

                case ShiftByteString  => 82
                case RotateByteString => 83
                case CountSetBits     => 84
                case FindFirstSetBit  => 85
                case Ripemd_160       => 86

                // Plutus 1.53 new builtins
                case DropList => 88

                // Array builtins
                case LengthOfArray   => 89
                case ListToArray     => 90
                case IndexArray      => 91
                case MultiIndexArray => 101

                // MaryEraValue builtins (CIP-0153)
                case InsertCoin    => 94
                case LookupCoin    => 95
                case UnionValue    => 96
                case ValueContains => 97
                case ValueData     => 98
                case UnValueData   => 99
                case ScaleValue    => 100

            encode.bits(7, code.toByte)

        def decode(decode: DecoderState): DefaultFun =
            decode.bits8(7) match
                case 0  => AddInteger
                case 1  => SubtractInteger
                case 2  => MultiplyInteger
                case 3  => DivideInteger
                case 4  => QuotientInteger
                case 5  => RemainderInteger
                case 6  => ModInteger
                case 7  => EqualsInteger
                case 8  => LessThanInteger
                case 9  => LessThanEqualsInteger
                case 10 => AppendByteString
                case 11 => ConsByteString
                case 12 => SliceByteString
                case 13 => LengthOfByteString
                case 14 => IndexByteString
                case 15 => EqualsByteString
                case 16 => LessThanByteString
                case 17 => LessThanEqualsByteString
                case 18 => Sha2_256
                case 19 => Sha3_256
                case 20 => Blake2b_256
                case 21 => VerifyEd25519Signature
                case 22 => AppendString
                case 23 => EqualsString
                case 24 => EncodeUtf8
                case 25 => DecodeUtf8
                case 26 => IfThenElse
                case 27 => ChooseUnit
                case 28 => Trace
                case 29 => FstPair
                case 30 => SndPair
                case 31 => ChooseList
                case 32 => MkCons
                case 33 => HeadList
                case 34 => TailList
                case 35 => NullList
                case 36 => ChooseData
                case 37 => ConstrData
                case 38 => MapData
                case 39 => ListData
                case 40 => IData
                case 41 => BData
                case 42 => UnConstrData
                case 43 => UnMapData
                case 44 => UnListData
                case 45 => UnIData
                case 46 => UnBData
                case 47 => EqualsData
                case 48 => MkPairData
                case 49 => MkNilData
                case 50 => MkNilPairData
                case 51 => SerialiseData
                case 52 => VerifyEcdsaSecp256k1Signature
                case 53 => VerifySchnorrSecp256k1Signature
                case 54 => Bls12_381_G1_add
                case 55 => Bls12_381_G1_neg
                case 56 => Bls12_381_G1_scalarMul
                case 57 => Bls12_381_G1_equal
                case 58 => Bls12_381_G1_compress
                case 59 => Bls12_381_G1_uncompress
                case 60 => Bls12_381_G1_hashToGroup
                case 61 => Bls12_381_G2_add
                case 62 => Bls12_381_G2_neg
                case 63 => Bls12_381_G2_scalarMul
                case 64 => Bls12_381_G2_equal
                case 65 => Bls12_381_G2_compress
                case 66 => Bls12_381_G2_uncompress
                case 67 => Bls12_381_G2_hashToGroup
                case 68 => Bls12_381_millerLoop
                case 69 => Bls12_381_mulMlResult
                case 70 => Bls12_381_finalVerify
                case 92 => Bls12_381_G1_multiScalarMul
                case 93 => Bls12_381_G2_multiScalarMul
                case 71 => Keccak_256
                case 72 => Blake2b_224
                case 73 => IntegerToByteString
                case 74 => ByteStringToInteger
                case 75 => AndByteString
                case 76 => OrByteString
                case 77 => XorByteString
                case 78 => ComplementByteString
                case 79 => ReadBit
                case 80 => WriteBits
                case 81 => ReplicateByte
                case 82 => ShiftByteString
                case 83 => RotateByteString
                case 84 => CountSetBits
                case 85 => FindFirstSetBit
                case 86 => Ripemd_160
                // Plutus 1.53 new builtins
                case 88 => DropList
                // Array builtins
                case 89  => LengthOfArray
                case 90  => ListToArray
                case 91  => IndexArray
                case 101 => MultiIndexArray
                // MaryEraValue builtins (CIP-0153)
                case 94  => InsertCoin
                case 95  => LookupCoin
                case 96  => UnionValue
                case 97  => ValueContains
                case 98  => ValueData
                case 99  => UnValueData
                case 100 => ScaleValue
                case c   => throw new Exception(s"Invalid builtin function code: $c")

}

given DefaultFunOrdering: Ordering[DefaultFun] with
    def compare(x: DefaultFun, y: DefaultFun): Int = x.ordinal - y.ordinal
