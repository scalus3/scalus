package scalus.builtin

import scalus.crypto.ed25519.{NativeEd25519Signer, SigningKey}
import scalus.crypto.{Keccak, Ripemd160}

import java.nio.file.{Files, Paths}
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

@link("sodium")
@extern
object LibSodium {
    def sodium_init(): Int = extern

    def crypto_hash_sha256_bytes(): CSize = extern

    def crypto_hash_sha256(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

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

object Builtins extends Builtins(using NativePlatformSpecific)
class Builtins(using ps: PlatformSpecific) extends AbstractBuiltins(using ps)

trait NativePlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.sha256(bs.bytes))

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
    override def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        ???

    override def bls12_381_G1_add(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G1_Element
    ): BLS12_381_G1_Element =
        ???

    override def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ???

    override def bls12_381_G1_neg(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = ???

    override def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString = ???

    override def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element =
        ???
    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ???
    override def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        ???
    override def bls12_381_G2_add(
        p1: BLS12_381_G2_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_G2_Element =
        ???
    override def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ???
    override def bls12_381_G2_neg(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = ???
    override def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString = ???
    override def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element =
        ???
    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element =
        ???
    override def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        ???
    override def bls12_381_mulMlResult(
        r1: BLS12_381_MlResult,
        r2: BLS12_381_MlResult
    ): BLS12_381_MlResult =
        ???
    override def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean =
        ???
    override def keccak_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Keccak.keccak256(bs.bytes))

    override def ripemd_160(byteString: ByteString): ByteString =
        ByteString.unsafeFromArray(Ripemd160.ripemd160(byteString.bytes))

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
