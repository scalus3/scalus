package scalus.builtin

import scalus.Compile

/** Provides access to platform-specific built-in functions.
  *
  * This is used to access cryptographic primitives and other platform-specific functionality that
  * may vary between JVM, JS, and other platforms.
  */
inline def platform: PlatformSpecific = summon[PlatformSpecific]

/** This is the platform specific part of the builtins. This is mostly cryptographic primitives that
  * have different implementations on different platforms.
  */
trait PlatformSpecific:
    def sha2_256(bs: ByteString): ByteString
    def sha2_512(bs: ByteString): ByteString
    def sha3_256(bs: ByteString): ByteString
    def blake2b_224(bs: ByteString): ByteString
    def blake2b_256(bs: ByteString): ByteString
    def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean
    def signEd25519(privateKey: ByteString, msg: ByteString): ByteString

    /** Verify an ECDSA signature made using the SECP256k1 curve.
      *
      * @note
      *
      * There are additional well-formation requirements for the arguments beyond their length:
      *
      *   - The first byte of the public key must correspond to the sign of the `y` coordinate: this
      *     is `0x02` if `y` is even, and `0x03` otherwise.
      *   - The remaining bytes of the public key must correspond to the `x` coordinate, as a
      *     big-endian integer.
      *   - The first 32 bytes of the signature must correspond to the big-endian integer
      *     representation of _r_.
      *   - The last 32 bytes of the signature must correspond to the big-endian integer
      *     representation of _s_.
      *
      * While this primitive `accepts` a hash, any caller should only pass it hashes that they
      * computed themselves: specifically, they should receive the `message` from a sender and hash
      * it, rather than receiving the `hash` from said sender. Failure to do so can be
      * [dangerous](https://bitcoin.stackexchange.com/a/81116/35586). Other than length, we make no
      * requirements of what hash gets used.
      * @param pk
      *   Public key (33 bytes)
      * @param msg
      *   Message (32 bytes)
      * @param sig
      *   Signature (64 bytes)
      */
    def verifyEcdsaSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

    /** Verify a Schnorr signature made using the SECP256k1 curve.
      *
      * @note
      *
      * There are additional well-formation requirements for the arguments beyond their length.
      * Throughout, we refer to co-ordinates of the point `R`.
      *
      *   - The bytes of the public key must correspond to the `x` coordinate, as a big-endian
      *     integer, as specified in BIP-340.
      *   - The first 32 bytes of the signature must correspond to the `x` coordinate, as a
      *     big-endian integer, as specified in BIP-340.
      *   - The last 32 bytes of the signature must correspond to the bytes of `s`, as a big-endian
      *     integer, as specified in BIP-340.
      *
      * @see
      *   [BIP-340](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
      *
      * @param pk
      *   Public key (32 bytes)
      * @param msg
      *   Message (arbitrary length)
      * @param sig
      *   Signature (64 bytes)
      */
    def verifySchnorrSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

    // BLS12_381 operations

    def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean

    /** Adds two G1 group elements
      * @param p1
      *   G1 element
      * @param p2
      *   G1 element
      * @return
      *   p1 + p2
      */
    def bls12_381_G1_add(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): BLS12_381_G1_Element

    /** Multiplication of group elements by scalars. In the blst library the arguments are the other
      * way round, but scalars acting on the left is more consistent with standard mathematical
      * practice.
      *
      * @param s
      *   scalar
      * @param p
      *   group element
      * @return
      *   s * p
      */
    def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element

    /** Negates a G1 group element
      *
      * @param p
      *   G1 element
      * @return
      *   -p
      */
    def bls12_381_G1_neg(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element

    /** Compress a G1 element to a bytestring. This serialises a curve point to its x coordinate
      * only. The compressed bytestring is 48 bytes long, with three spare bits used to convey extra
      * information about the point, including determining which of two possible y coordinates the
      * point has and whether the point is the point at infinity.
      * @see
      *   https://github.com/supranational/blst#serialization-format
      *
      * @param p
      *   G1 element to compress
      * @return
      *   Compressed bytestring
      */
    def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString

    /** Uncompress a bytestring to get a G1 point. This will fail if any of the following are true.
      *   - The bytestring is not exactly 48 bytes long.
      *   - The most significant three bits are used incorrectly.
      *   - The bytestring encodes a field element which is not the x coordinate of a point on the
      *     E1 curve.
      *   - The bytestring does represent a point on the E1 curve, but the point is not in the G1
      *     subgroup.
      */
    def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element

    def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element

    def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean

    def bls12_381_G2_add(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): BLS12_381_G2_Element

    def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element

    def bls12_381_G2_neg(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element

    def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString

    def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element

    def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element

    def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult

    def bls12_381_mulMlResult(r1: BLS12_381_MlResult, r2: BLS12_381_MlResult): BLS12_381_MlResult

    def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean

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
      * @throws scalus.uplc.eval.BuiltinException
      *   if either list is empty or lists have different lengths
      */
    def bls12_381_G1_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[BLS12_381_G1_Element]
    ): BLS12_381_G1_Element

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
      * @throws scalus.uplc.eval.BuiltinException
      *   if either list is empty or lists have different lengths
      */
    def bls12_381_G2_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[BLS12_381_G2_Element]
    ): BLS12_381_G2_Element

    def keccak_256(bs: ByteString): ByteString

    def ripemd_160(byteString: ByteString): ByteString

    /** Modular exponentiation: base^exp mod modulus.
      *
      * This is platform-specific because Scala.js BigInt.modPow has bugs with very large numbers.
      *
      * @param base
      *   the base
      * @param exp
      *   the exponent (must be non-negative)
      * @param modulus
      *   the modulus (must be positive)
      * @return
      *   base^exp mod modulus
      */
    def modPow(base: BigInt, exp: BigInt, modulus: BigInt): BigInt

    /** Read a file from the filesystem and return its contents as a byte array.
      *
      * @note
      *   This method is only available in Node.js, JVM, and Native environments. DO NOT use this
      *   method in code that is intended to run in a browser. For browser-compatible code, avoid
      *   file I/O operations or use browser-specific APIs like FileReader.
      *
      * @param path
      *   The path to the file to read
      * @return
      *   The contents of the file as a byte array
      */
    def readFile(path: String): Array[Byte]

    /** Write bytes to a file, creating or truncating the file if it exists.
      *
      * @note
      *   This method is only available in Node.js, JVM, and Native environments. DO NOT use this
      *   method in code that is intended to run in a browser. For browser-compatible code, avoid
      *   file I/O operations or use browser-specific APIs like FileWriter.
      *
      * @param path
      *   The path to the file to write
      * @param bytes
      *   The bytes to write to the file
      */
    def writeFile(path: String, bytes: Array[Byte]): Unit

    /** Append bytes to a file, creating the file if it doesn't exist.
      *
      * @note
      *   This method is only available in Node.js, JVM, and Native environments. DO NOT use this
      *   method in code that is intended to run in a browser. For browser-compatible code, avoid
      *   file I/O operations or use browser-specific APIs.
      *
      * @param path
      *   The path to the file to append to
      * @param bytes
      *   The bytes to append to the file
      */
    def appendFile(path: String, bytes: Array[Byte]): Unit

@Compile
object PlatformSpecific:
    val bls12_381_scalar_period: BigInt =
        BigInt("52435875175126190479447740508185965837690552500527637822603658699938581184513")

    val bls12_381_G1_compressed_zero: ByteString =
        ByteString.fromHex(
          "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )

    val bls12_381_G1_compressed_generator: ByteString =
        ByteString.fromHex(
          "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        )

    val bls12_381_G2_compressed_zero: ByteString =
        ByteString.fromHex(
          "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )

    val bls12_381_G2_compressed_generator: ByteString =
        ByteString.fromHex(
          "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
        )
