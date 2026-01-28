package scalus.uplc.builtin

import scalus.crypto.ed25519.{NativeEd25519Signer, SigningKey}
import scalus.crypto.{Keccak, Ripemd160}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element, MLResult}

import java.nio.file.{Files, Paths}
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

@link("sodium")
@extern
object LibSodium {
    def sodium_init(): Int = extern

    def crypto_hash_sha256_bytes(): CSize = extern

    def crypto_hash_sha256(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    def crypto_hash_sha512_bytes(): CSize = extern

    def crypto_hash_sha512(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    // SHA3-256
    def crypto_hash_sha3_256_bytes(): CSize = extern
    def crypto_hash_sha3_256(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    // BLAKE2b with custom output length
    def crypto_generichash_blake2b_bytes_min(): CSize = extern
    def crypto_generichash_blake2b_bytes_max(): CSize = extern
    def crypto_generichash_blake2b(
        out: Ptr[Byte],
        outlen: CSize,
        in: Ptr[Byte],
        inlen: CUnsignedLongLong,
        key: Ptr[Byte],
        keylen: CSize
    ): CInt = extern

    def crypto_sign_ed25519_verify_detached(
        sig: Ptr[Byte],
        msg: Ptr[Byte],
        msglen: CUnsignedLongLong,
        key: Ptr[Byte]
    ): CInt = extern

}

object Sodium {
    import LibSodium.*
    private inline def hashWithSodium(
        input: Array[Byte],
        hashLen: Int,
        hashFunc: (Ptr[Byte], Ptr[Byte], CUnsignedLongLong) => CInt
    ): Array[Byte] =
        val output = new Array[Byte](hashLen)
        hashFunc(output.atUnsafe(0), input.atUnsafe(0), input.length.toULong)
        output

    def sha256(input: Array[Byte]): Array[Byte] =
        hashWithSodium(
          input,
          crypto_hash_sha256_bytes().toInt,
          LibSodium.crypto_hash_sha256
        )

    def sha2_512(input: Array[Byte]): Array[Byte] =
        hashWithSodium(
          input,
          crypto_hash_sha512_bytes().toInt,
          LibSodium.crypto_hash_sha512
        )

    private def blake2b(
        input: Array[Byte],
        outputLength: Int,
        key: Array[Byte] = Array.empty
    ): Array[Byte] = {
        val output = new Array[Byte](outputLength)
        val keyPtr = if key.nonEmpty then key.atUnsafe(0) else null

        LibSodium.crypto_generichash_blake2b(
          output.atUnsafe(0),
          outputLength.toCSize,
          input.atUnsafe(0),
          input.length.toULong,
          keyPtr,
          key.length.toCSize
        )
        output
    }

    def blake2b224(input: Array[Byte], key: Array[Byte] = Array.empty): Array[Byte] =
        blake2b(input, 28, key) // 224 bits = 28 bytes

    def blake2b256(input: Array[Byte], key: Array[Byte] = Array.empty): Array[Byte] =
        blake2b(input, 32, key) // 256 bits = 32 bytes

    def verifyEd25519Signature(
        pubKey: Array[Byte],
        msg: Array[Byte],
        signature: Array[Byte]
    ): Boolean = {
        require(pubKey.length == 32, s"Invalid public key length ${pubKey.length}")
        require(signature.length == 64, s"Invalid signature length ${signature.length}")
        LibSodium.crypto_sign_ed25519_verify_detached(
          signature.atUnsafe(0),
          msg.atUnsafe(0),
          msg.length.toULong,
          pubKey.atUnsafe(0)
        ) == 0
    }
}

/** Native bindings for libsecp256k1 */
@link("secp256k1")
@extern
private object LibSecp256k1 {

    /** Context for secp256k1 operations */
    type Context = Ptr[Byte]

    /** Public key structure */
    type PublicKey = CArray[Byte, Nat.Digit2[Nat._6, Nat._4]]

    /** Signature structure */
    type ECDSASignature = CArray[Byte, Nat.Digit2[Nat._6, Nat._4]]
    type SchnorrSignature = CArray[Byte, Nat.Digit2[Nat._6, Nat._4]]

    /** Create a secp256k1 context object */
    def secp256k1_context_create(flags: CUnsignedInt): Context = extern

    /** Destroy a secp256k1 context object */
    def secp256k1_context_destroy(ctx: Context): Unit = extern

    /** Parse a serialized public key */
    def secp256k1_ec_pubkey_parse(
        ctx: Context,
        pubkey: Ptr[PublicKey],
        input: Ptr[Byte],
        inputlen: CSize
    ): CInt = extern

    def secp256k1_ecdsa_signature_parse_compact(
        ctx: Context,
        sig: Ptr[ECDSASignature],
        input: Ptr[Byte]
    ): CInt = extern

    /** Verify an ECDSA signature */
    def secp256k1_ecdsa_verify(
        ctx: Context,
        sig: Ptr[ECDSASignature],
        msg32: Ptr[Byte],
        pubkey: Ptr[PublicKey]
    ): CInt = extern

    /** Verify a Schnorr signature */
    def secp256k1_schnorrsig_verify(
        ctx: Context,
        sig64: Ptr[Byte],
        msg32: Ptr[Byte],
        msglen: CSize,
        pubkey: Ptr[PublicKey]
    ): CInt = extern
}

/** Implementation of Cardano secp256k1 signature verification builtins */
object Secp256k1Builtins:
    private val SECP256K1_CONTEXT_VERIFY = 0x0101.toUInt
    private lazy val ctx = {
        val ctx = LibSecp256k1.secp256k1_context_create(SECP256K1_CONTEXT_VERIFY)
        assert(ctx != null, "Failed to create secp256k1 context")
        ctx
    }

    /** Verify an ECDSA secp256k1 signature according to Cardano specification
      *
      * @param message
      *   The 32-byte message hash to verify
      * @param signature
      *   The DER-encoded ECDSA signature
      * @param publicKey
      *   The 33/65-byte compressed/uncompressed public key
      * @return
      *   True if signature is valid, false otherwise
      *
      * Implementation follows CIP-0049 for ECDSA verification
      */
    def verifyEcdsaSecp256k1Signature(
        message: Array[Byte],
        signature: Array[Byte],
        publicKey: Array[Byte]
    ): Boolean = {
        // Input validation
        require(publicKey.length == 33, s"Invalid public key length ${publicKey.length}")
        require(message.length == 32, s"Invalid message length ${message.length}")
        require(signature.length == 64, s"Invalid signature length ${signature.length}")

        // Create context

        assert(ctx != null, "Failed to create secp256k1 context")

        // Parse public key
        val pubkey = stackalloc[LibSecp256k1.PublicKey]()
        // Allocate space for parsed signature
        val sigPtr = stackalloc[LibSecp256k1.ECDSASignature]()

        // Parse the ECDSA signature
        if LibSecp256k1.secp256k1_ecdsa_signature_parse_compact(
              ctx,
              sigPtr,
              signature.atUnsafe(0)
            ) == 0
        then throw new IllegalArgumentException("Failed to parse signature")

        if LibSecp256k1.secp256k1_ec_pubkey_parse(
              ctx,
              pubkey,
              publicKey.atUnsafe(0),
              publicKey.length.toCSize
            ) != 1
        then throw new IllegalArgumentException("Failed to parse public key")

        // Verify signature
        LibSecp256k1.secp256k1_ecdsa_verify(
          ctx,
          sigPtr,
          message.atUnsafe(0),
          pubkey
        ) == 1
    }

    /** Verify a Schnorr secp256k1 signature according to Cardano specification
      *
      * @param message
      *   The message to verify (any length)
      * @param signature
      *   The 64-byte Schnorr signature
      * @param publicKey
      *   The 32-byte x-only public key
      * @return
      *   True if signature is valid, false otherwise
      *
      * Implementation follows BIP-0340 for Schnorr verification
      */
    def verifySchnorrSecp256k1Signature(
        message: Array[Byte],
        signature: Array[Byte],
        publicKey: Array[Byte]
    ): Boolean = {
        // Input validation
        require(publicKey.length == 32, s"Invalid public key length ${publicKey.length}")
        require(signature.length == 64, s"Invalid signature length ${signature.length}")

        val ecPubKey = Array(0x02.toByte) ++ publicKey

        // Parse x-only public key
        val pubkey = stackalloc[LibSecp256k1.PublicKey]()
        if LibSecp256k1.secp256k1_ec_pubkey_parse(
              ctx,
              pubkey,
              ecPubKey.atUnsafe(0),
              ecPubKey.length.toCSize
            ) != 1
        then throw new IllegalArgumentException("Failed to parse public key")

        LibSecp256k1.secp256k1_schnorrsig_verify(
          ctx,
          signature.atUnsafe(0),
          message.atUnsafe(0),
          message.length.toCSize,
          pubkey
        ) == 1
    }

/** Native bindings for libblst (BLS12-381) */
@link("blst")
@extern
private object LibBlst {
    // Structure sizes in bytes:
    // blst_p1: 144 bytes (3 x 48-byte coordinates in Jacobian form)
    // blst_p1_affine: 96 bytes (2 x 48-byte coordinates)
    // blst_p2: 288 bytes (3 x 96-byte coordinates in Jacobian form)
    // blst_p2_affine: 192 bytes (2 x 96-byte coordinates)
    // blst_fp12: 576 bytes (12 x 48-byte field elements)
    // blst_scalar: 32 bytes

    // P1 (G1) operations
    def blst_p1_add(out: Ptr[Byte], a: Ptr[Byte], b: Ptr[Byte]): Unit = extern
    def blst_p1_mult(out: Ptr[Byte], p: Ptr[Byte], scalar: Ptr[Byte], nbits: CSize): Unit = extern
    def blst_p1_cneg(p: Ptr[Byte], cbit: CBool): Unit = extern
    def blst_p1_in_g1(p: Ptr[Byte]): CBool = extern
    def blst_p1_compress(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_p1_uncompress(out: Ptr[Byte], in: Ptr[Byte]): CInt = extern // BLST_ERROR
    def blst_p1_from_affine(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_p1_to_affine(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_hash_to_g1(
        out: Ptr[Byte],
        msg: Ptr[Byte],
        msg_len: CSize,
        DST: Ptr[Byte],
        DST_len: CSize,
        aug: Ptr[Byte],
        aug_len: CSize
    ): Unit = extern

    // P2 (G2) operations
    def blst_p2_add(out: Ptr[Byte], a: Ptr[Byte], b: Ptr[Byte]): Unit = extern
    def blst_p2_mult(out: Ptr[Byte], p: Ptr[Byte], scalar: Ptr[Byte], nbits: CSize): Unit = extern
    def blst_p2_cneg(p: Ptr[Byte], cbit: CBool): Unit = extern
    def blst_p2_in_g2(p: Ptr[Byte]): CBool = extern
    def blst_p2_compress(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_p2_uncompress(out: Ptr[Byte], in: Ptr[Byte]): CInt = extern
    def blst_p2_from_affine(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_p2_to_affine(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern
    def blst_hash_to_g2(
        out: Ptr[Byte],
        msg: Ptr[Byte],
        msg_len: CSize,
        DST: Ptr[Byte],
        DST_len: CSize,
        aug: Ptr[Byte],
        aug_len: CSize
    ): Unit = extern

    // Pairing operations
    def blst_miller_loop(ret: Ptr[Byte], Q: Ptr[Byte], P: Ptr[Byte]): Unit = extern
    def blst_fp12_mul(ret: Ptr[Byte], a: Ptr[Byte], b: Ptr[Byte]): Unit = extern
    def blst_fp12_finalverify(gt1: Ptr[Byte], gt2: Ptr[Byte]): CBool = extern

    // Scalar operations for reducing BigInt
    def blst_scalar_from_bendian(out: Ptr[Byte], in: Ptr[Byte]): Unit = extern

    // Point equality
    def blst_p1_is_equal(a: Ptr[Byte], b: Ptr[Byte]): CBool = extern
    def blst_p2_is_equal(a: Ptr[Byte], b: Ptr[Byte]): CBool = extern
}

/** BLS12-381 implementation using libblst */
object Blst:
    import LibBlst.*

    // Structure sizes
    private val P1_SIZE = 144
    private val P1_AFFINE_SIZE = 96
    private val P2_SIZE = 288
    private val P2_AFFINE_SIZE = 192
    private val FP12_SIZE = 576
    private val SCALAR_SIZE = 32
    private val G1_COMPRESSED_SIZE = 48
    private val G2_COMPRESSED_SIZE = 96

    // Helper to copy bytes from native pointer to Array[Byte]
    private inline def copyToArray(src: Ptr[Byte], len: Int): Array[Byte] =
        val arr = new Array[Byte](len)
        var i = 0
        while i < len do
            arr(i) = src(i)
            i += 1
        arr

    // Helper to copy Array[Byte] to native pointer
    private inline def copyFromArray(src: Array[Byte], dst: Ptr[Byte]): Unit =
        var i = 0
        while i < src.length do
            dst(i) = src(i)
            i += 1

    // G1 operations

    def g1Equal(p1: G1Element, p2: G1Element): Boolean =
        java.util.Arrays.equals(p1.compressed, p2.compressed)

    def g1Add(p1: G1Element, p2: G1Element): G1Element =
        val affine1 = stackalloc[Byte](P1_AFFINE_SIZE)
        val affine2 = stackalloc[Byte](P1_AFFINE_SIZE)
        val jac1 = stackalloc[Byte](P1_SIZE)
        val jac2 = stackalloc[Byte](P1_SIZE)
        val result = stackalloc[Byte](P1_SIZE)
        val compressed = stackalloc[Byte](G1_COMPRESSED_SIZE)

        // Uncompress inputs to affine
        blst_p1_uncompress(affine1, p1.compressed.atUnsafe(0))
        blst_p1_uncompress(affine2, p2.compressed.atUnsafe(0))

        // Convert to Jacobian
        blst_p1_from_affine(jac1, affine1)
        blst_p1_from_affine(jac2, affine2)

        // Add
        blst_p1_add(result, jac1, jac2)

        // Compress result
        blst_p1_compress(compressed, result)

        G1Element(copyToArray(compressed, G1_COMPRESSED_SIZE))

    def g1ScalarMul(scalar: BigInt, p: G1Element): G1Element =
        val affine = stackalloc[Byte](P1_AFFINE_SIZE)
        val jac = stackalloc[Byte](P1_SIZE)
        val result = stackalloc[Byte](P1_SIZE)
        val compressed = stackalloc[Byte](G1_COMPRESSED_SIZE)
        val scalarBytes = stackalloc[Byte](SCALAR_SIZE)

        // Reduce scalar modulo the curve order (Java BigInteger.mod always returns non-negative)
        val reduced =
            BigInt(scalar.bigInteger.mod(PlatformSpecific.bls12_381_scalar_period.bigInteger))
        val bytes = reduced.toByteArray

        // Pad or trim to 32 bytes (big-endian from BigInt)
        val padded = new Array[Byte](SCALAR_SIZE)
        if bytes.length <= SCALAR_SIZE then
            System.arraycopy(bytes, 0, padded, SCALAR_SIZE - bytes.length, bytes.length)
        else
            // bytes.length > 32, take the last 32 bytes
            System.arraycopy(bytes, bytes.length - SCALAR_SIZE, padded, 0, SCALAR_SIZE)

        // blst expects little-endian scalar, reverse from big-endian
        val leBytes = padded.reverse
        copyFromArray(leBytes, scalarBytes)

        // Uncompress point
        blst_p1_uncompress(affine, p.compressed.atUnsafe(0))
        blst_p1_from_affine(jac, affine)

        // Multiply
        blst_p1_mult(result, jac, scalarBytes, (SCALAR_SIZE * 8).toCSize)

        // Compress result
        blst_p1_compress(compressed, result)

        G1Element(copyToArray(compressed, G1_COMPRESSED_SIZE))

    def g1Neg(p: G1Element): G1Element =
        val affine = stackalloc[Byte](P1_AFFINE_SIZE)
        val jac = stackalloc[Byte](P1_SIZE)
        val compressed = stackalloc[Byte](G1_COMPRESSED_SIZE)

        // Uncompress
        blst_p1_uncompress(affine, p.compressed.atUnsafe(0))
        blst_p1_from_affine(jac, affine)

        // Negate (cbit=true means negate)
        blst_p1_cneg(jac, true)

        // Compress result
        blst_p1_compress(compressed, jac)

        G1Element(copyToArray(compressed, G1_COMPRESSED_SIZE))

    def g1Compress(p: G1Element): ByteString =
        p.toCompressedByteString

    def g1Uncompress(bs: ByteString): G1Element =
        require(
          bs.size == G1_COMPRESSED_SIZE,
          s"Invalid length for G1 compressed point: expected $G1_COMPRESSED_SIZE, got ${bs.size}"
        )
        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit not set for G1 point"
        )

        val affine = stackalloc[Byte](P1_AFFINE_SIZE)
        val jac = stackalloc[Byte](P1_SIZE)

        val err = blst_p1_uncompress(affine, bs.bytes.atUnsafe(0))
        if err != 0 then
            throw new IllegalArgumentException(s"Failed to uncompress G1 point: error $err")

        // Convert to Jacobian to check group membership
        blst_p1_from_affine(jac, affine)
        if !blst_p1_in_g1(jac) then
            throw new IllegalArgumentException("Point is not in G1 subgroup")

        G1Element(bs.bytes.clone())

    def g1HashToGroup(msg: ByteString, dst: ByteString): G1Element =
        require(
          dst.size <= 255,
          s"DST must be <= 255 bytes, got ${dst.size}"
        )

        val jac = stackalloc[Byte](P1_SIZE)
        val compressed = stackalloc[Byte](G1_COMPRESSED_SIZE)

        blst_hash_to_g1(
          jac,
          msg.bytes.atUnsafe(0),
          msg.size.toCSize,
          dst.bytes.atUnsafe(0),
          dst.size.toCSize,
          null, // no augmentation
          0.toCSize
        )

        blst_p1_compress(compressed, jac)

        G1Element(copyToArray(compressed, G1_COMPRESSED_SIZE))

    // G2 operations

    def g2Equal(p1: G2Element, p2: G2Element): Boolean =
        java.util.Arrays.equals(p1.compressed, p2.compressed)

    def g2Add(p1: G2Element, p2: G2Element): G2Element =
        val affine1 = stackalloc[Byte](P2_AFFINE_SIZE)
        val affine2 = stackalloc[Byte](P2_AFFINE_SIZE)
        val jac1 = stackalloc[Byte](P2_SIZE)
        val jac2 = stackalloc[Byte](P2_SIZE)
        val result = stackalloc[Byte](P2_SIZE)
        val compressed = stackalloc[Byte](G2_COMPRESSED_SIZE)

        blst_p2_uncompress(affine1, p1.compressed.atUnsafe(0))
        blst_p2_uncompress(affine2, p2.compressed.atUnsafe(0))

        blst_p2_from_affine(jac1, affine1)
        blst_p2_from_affine(jac2, affine2)

        blst_p2_add(result, jac1, jac2)

        blst_p2_compress(compressed, result)

        G2Element(copyToArray(compressed, G2_COMPRESSED_SIZE))

    def g2ScalarMul(scalar: BigInt, p: G2Element): G2Element =
        val affine = stackalloc[Byte](P2_AFFINE_SIZE)
        val jac = stackalloc[Byte](P2_SIZE)
        val result = stackalloc[Byte](P2_SIZE)
        val compressed = stackalloc[Byte](G2_COMPRESSED_SIZE)
        val scalarBytes = stackalloc[Byte](SCALAR_SIZE)

        // Reduce scalar modulo the curve order (Java BigInteger.mod always returns non-negative)
        val reduced =
            BigInt(scalar.bigInteger.mod(PlatformSpecific.bls12_381_scalar_period.bigInteger))
        val bytes = reduced.toByteArray

        val padded = new Array[Byte](SCALAR_SIZE)
        if bytes.length <= SCALAR_SIZE then
            System.arraycopy(bytes, 0, padded, SCALAR_SIZE - bytes.length, bytes.length)
        else System.arraycopy(bytes, bytes.length - SCALAR_SIZE, padded, 0, SCALAR_SIZE)

        // blst expects little-endian scalar, reverse from big-endian
        val leBytes = padded.reverse
        copyFromArray(leBytes, scalarBytes)

        blst_p2_uncompress(affine, p.compressed.atUnsafe(0))
        blst_p2_from_affine(jac, affine)

        blst_p2_mult(result, jac, scalarBytes, (SCALAR_SIZE * 8).toCSize)

        blst_p2_compress(compressed, result)

        G2Element(copyToArray(compressed, G2_COMPRESSED_SIZE))

    def g2Neg(p: G2Element): G2Element =
        val affine = stackalloc[Byte](P2_AFFINE_SIZE)
        val jac = stackalloc[Byte](P2_SIZE)
        val compressed = stackalloc[Byte](G2_COMPRESSED_SIZE)

        blst_p2_uncompress(affine, p.compressed.atUnsafe(0))
        blst_p2_from_affine(jac, affine)

        blst_p2_cneg(jac, true)

        blst_p2_compress(compressed, jac)

        G2Element(copyToArray(compressed, G2_COMPRESSED_SIZE))

    def g2Compress(p: G2Element): ByteString =
        p.toCompressedByteString

    def g2Uncompress(bs: ByteString): G2Element =
        require(
          bs.size == G2_COMPRESSED_SIZE,
          s"Invalid length for G2 compressed point: expected $G2_COMPRESSED_SIZE, got ${bs.size}"
        )
        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit not set for G2 point"
        )

        val affine = stackalloc[Byte](P2_AFFINE_SIZE)
        val jac = stackalloc[Byte](P2_SIZE)

        val err = blst_p2_uncompress(affine, bs.bytes.atUnsafe(0))
        if err != 0 then
            throw new IllegalArgumentException(s"Failed to uncompress G2 point: error $err")

        blst_p2_from_affine(jac, affine)
        if !blst_p2_in_g2(jac) then
            throw new IllegalArgumentException("Point is not in G2 subgroup")

        G2Element(bs.bytes.clone())

    def g2HashToGroup(msg: ByteString, dst: ByteString): G2Element =
        require(
          dst.size <= 255,
          s"DST must be <= 255 bytes, got ${dst.size}"
        )

        val jac = stackalloc[Byte](P2_SIZE)
        val compressed = stackalloc[Byte](G2_COMPRESSED_SIZE)

        blst_hash_to_g2(
          jac,
          msg.bytes.atUnsafe(0),
          msg.size.toCSize,
          dst.bytes.atUnsafe(0),
          dst.size.toCSize,
          null,
          0.toCSize
        )

        blst_p2_compress(compressed, jac)

        G2Element(copyToArray(compressed, G2_COMPRESSED_SIZE))

    // Pairing operations

    def millerLoop(g1: G1Element, g2: G2Element): MLResult =
        val g1Affine = stackalloc[Byte](P1_AFFINE_SIZE)
        val g2Affine = stackalloc[Byte](P2_AFFINE_SIZE)
        val fp12 = stackalloc[Byte](FP12_SIZE)

        blst_p1_uncompress(g1Affine, g1.compressed.atUnsafe(0))
        blst_p2_uncompress(g2Affine, g2.compressed.atUnsafe(0))

        blst_miller_loop(fp12, g2Affine, g1Affine)

        MLResult(copyToArray(fp12, FP12_SIZE))

    def mulMlResult(r1: MLResult, r2: MLResult): MLResult =
        val fp12_1 = stackalloc[Byte](FP12_SIZE)
        val fp12_2 = stackalloc[Byte](FP12_SIZE)
        val result = stackalloc[Byte](FP12_SIZE)

        copyFromArray(r1.fp12Bytes, fp12_1)
        copyFromArray(r2.fp12Bytes, fp12_2)

        blst_fp12_mul(result, fp12_1, fp12_2)

        MLResult(copyToArray(result, FP12_SIZE))

    def finalVerify(r1: MLResult, r2: MLResult): Boolean =
        val fp12_1 = stackalloc[Byte](FP12_SIZE)
        val fp12_2 = stackalloc[Byte](FP12_SIZE)

        copyFromArray(r1.fp12Bytes, fp12_1)
        copyFromArray(r2.fp12Bytes, fp12_2)

        blst_fp12_finalverify(fp12_1, fp12_2)

    def g1MultiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G1Element]
    ): G1Element = {
        // Use zip behavior: take minimum length, return identity for empty
        // Start with zero element
        var result = g1Uncompress(
          ByteString.fromHex(
            "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
          )
        )

        scalars.zip(points).foreach { case (scalar, point) =>
            val product = g1ScalarMul(scalar, point)
            result = g1Add(result, product)
        }
        result
    }

    def g2MultiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G2Element]
    ): G2Element = {
        // Use zip behavior: take minimum length, return identity for empty
        // Start with zero element
        var result = g2Uncompress(
          ByteString.fromHex(
            "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
          )
        )

        scalars.zip(points).foreach { case (scalar, point) =>
            val product = g2ScalarMul(scalar, point)
            result = g2Add(result, product)
        }
        result
    }

object Builtins extends Builtins(using NativePlatformSpecific)
class Builtins(using ps: PlatformSpecific) extends AbstractBuiltins(using ps)

trait NativePlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.sha256(bs.bytes))

    override def sha2_512(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.sha2_512(bs.bytes))

    override def sha3_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Keccak.sha3_256(bs.bytes))

    override def blake2b_224(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.blake2b224(bs.bytes))

    override def blake2b_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.blake2b256(bs.bytes))

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        Sodium.verifyEd25519Signature(pk.bytes, msg.bytes, sig.bytes)

    override def signEd25519(privateKey: ByteString, msg: ByteString): ByteString =
        require(privateKey.size == 32, s"Invalid private key length ${privateKey.size}")
        val signingKey = SigningKey.unsafeFromByteString(privateKey)
        NativeEd25519Signer.sign(signingKey, msg)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        Secp256k1Builtins.verifyEcdsaSecp256k1Signature(msg.bytes, sig.bytes, pk.bytes)

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        Secp256k1Builtins.verifySchnorrSecp256k1Signature(msg.bytes, sig.bytes, pk.bytes)

    // BLS12_381 operations
    override def bls12_381_G1_equal(p1: G1Element, p2: G1Element): Boolean =
        Blst.g1Equal(p1, p2)

    override def bls12_381_G1_add(
        p1: G1Element,
        p2: G1Element
    ): G1Element =
        Blst.g1Add(p1, p2)

    override def bls12_381_G1_scalarMul(s: BigInt, p: G1Element): G1Element =
        Blst.g1ScalarMul(s, p)

    override def bls12_381_G1_neg(p: G1Element): G1Element =
        Blst.g1Neg(p)

    override def bls12_381_G1_compress(p: G1Element): ByteString =
        Blst.g1Compress(p)

    override def bls12_381_G1_uncompress(bs: ByteString): G1Element =
        Blst.g1Uncompress(bs)

    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): G1Element =
        Blst.g1HashToGroup(bs, dst)

    override def bls12_381_G2_equal(p1: G2Element, p2: G2Element): Boolean =
        Blst.g2Equal(p1, p2)

    override def bls12_381_G2_add(
        p1: G2Element,
        p2: G2Element
    ): G2Element =
        Blst.g2Add(p1, p2)

    override def bls12_381_G2_scalarMul(s: BigInt, p: G2Element): G2Element =
        Blst.g2ScalarMul(s, p)

    override def bls12_381_G2_neg(p: G2Element): G2Element =
        Blst.g2Neg(p)

    override def bls12_381_G2_compress(p: G2Element): ByteString =
        Blst.g2Compress(p)

    override def bls12_381_G2_uncompress(bs: ByteString): G2Element =
        Blst.g2Uncompress(bs)

    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): G2Element =
        Blst.g2HashToGroup(bs, dst)

    override def bls12_381_millerLoop(
        p1: G1Element,
        p2: G2Element
    ): MLResult =
        Blst.millerLoop(p1, p2)

    override def bls12_381_mulMlResult(
        r1: MLResult,
        r2: MLResult
    ): MLResult =
        Blst.mulMlResult(r1, r2)

    override def bls12_381_finalVerify(p1: MLResult, p2: MLResult): Boolean =
        Blst.finalVerify(p1, p2)

    override def bls12_381_G1_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G1Element]
    ): G1Element =
        Blst.g1MultiScalarMul(scalars, points)

    override def bls12_381_G2_multiScalarMul(
        scalars: Seq[BigInt],
        points: Seq[G2Element]
    ): G2Element =
        Blst.g2MultiScalarMul(scalars, points)

    override def keccak_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Keccak.keccak256(bs.bytes))

    override def ripemd_160(byteString: ByteString): ByteString =
        ByteString.unsafeFromArray(Ripemd160.ripemd160(byteString.bytes))

    /** Custom modular exponentiation using square-and-multiply algorithm.
      *
      * This implementation avoids relying on BigInt.modPow which has bugs in Scala Native for very
      * large numbers.
      */
    override def modPow(base: BigInt, exp: BigInt, modulus: BigInt): BigInt =
        if exp == 0 then BigInt(1)
        else
            var result = BigInt(1)
            var b = base mod modulus
            var e = exp
            while e > 0 do
                if (e mod 2) == 1 then result = (result * b) mod modulus
                e = e >> 1
                b = (b * b) mod modulus
            // Ensure result is non-negative
            if result < 0 then result + modulus else result

    override def readFile(path: String): Array[Byte] = {
        Files.readAllBytes(Paths.get(path))
    }

    override def writeFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(Paths.get(path), bytes.toArray)
        ()
    }

    override def appendFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(
          Paths.get(path),
          bytes.toArray,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.APPEND
        )
        ()
    }
}

object NativePlatformSpecific extends NativePlatformSpecific

given PlatformSpecific = NativePlatformSpecific
