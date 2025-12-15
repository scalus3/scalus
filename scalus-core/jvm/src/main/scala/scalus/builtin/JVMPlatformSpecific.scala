package scalus.builtin

import org.bitcoin.NativeSecp256k1
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.jcajce.provider.digest.{Keccak, RIPEMD160, SHA3}
import scalus.crypto.ed25519.{JvmEd25519Signer, SigningKey}
import scalus.utils.Utils
import supranational.blst.{P1, P2, PT}

import java.math.BigInteger
import java.nio.file.{Files, Paths}

object Builtins extends Builtins(using JVMPlatformSpecific)
class Builtins(using ps: PlatformSpecific) extends AbstractBuiltins(using ps)

object JVMPlatformSpecific extends JVMPlatformSpecific
trait JVMPlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Utils.sha2_256(bs.bytes))

    override def sha3_256(bs: ByteString): ByteString =
        val digestSHA3 = new SHA3.Digest256()
        ByteString.unsafeFromArray(digestSHA3.digest(bs.bytes))

    override def blake2b_224(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(224)
        digest.update(bs.bytes, 0, bs.size)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def blake2b_256(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(256)
        digest.update(bs.bytes, 0, bs.size)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        // parity byte 0x02 for compressed public key
        require(NativeSecp256k1.isValidPubKey(0x02 +: pk.bytes), s"Invalid public key ${pk}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        NativeSecp256k1.schnorrVerify(sig.bytes, msg.bytes, pk.bytes)
    }

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        val pubKeyParams =
            try new Ed25519PublicKeyParameters(pk.bytes, 0)
            catch
                case e: IllegalArgumentException =>
                    return false
        val verifier = new Ed25519Signer()
        verifier.init(false, pubKeyParams)
        verifier.update(msg.bytes, 0, msg.size)
        verifier.verifySignature(sig.bytes)

    override def signEd25519(privateKey: ByteString, message: ByteString): ByteString =
        require(privateKey.size == 32, s"Invalid private key length ${privateKey.size}")
        val signingKey = SigningKey.unsafeFromByteString(privateKey)
        JvmEd25519Signer.sign(signingKey, message)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(
          pk.size == 33,
          s"Invalid public key length ${pk.size}, expected 33, ${pk.toHex}"
        )
        require(NativeSecp256k1.isValidPubKey(pk.bytes), s"Invalid public key ${pk}")
        require(msg.size == 32, s"Invalid message length ${msg.size}, expected 32")
        require(sig.size == 64, s"Invalid signature length ${sig.size}, expected 64")

        val r = BigInt(new BigInteger(1, sig.bytes, 0, 32)) // avoid copying the array
        val s = BigInt(new BigInteger(1, sig.bytes, 32, 32)) // avoid copying the array
        val rsSize = r.toByteArray.length + s.toByteArray.length
        val totalSize = 4 + rsSize
        val signature = Array(
          0x30.toByte,
          totalSize.toByte,
          0x2.toByte,
          r.toByteArray.length.toByte
        ) ++ r.toByteArray ++ Array(0x2.toByte, s.toByteArray.length.toByte) ++ s.toByteArray
        NativeSecp256k1.verify(msg.bytes, signature, pk.bytes)
    }

    // BLS12_381 operations
    override def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        p1 == p2

    override def bls12_381_G1_add(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = BLS12_381_G1_Element(p1.value.dup.add(p2.value))

    override def bls12_381_G1_scalarMul(
        s: BigInt,
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = {
        val scalar = s.bigInteger.mod(PlatformSpecific.bls12_381_scalar_period.bigInteger)
        BLS12_381_G1_Element(p.value.dup.mult(scalar))
    }

    override def bls12_381_G1_neg(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = {
        BLS12_381_G1_Element(p.value.dup.neg())
    }

    override def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString =
        p.toCompressedByteString

    override def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element = {
        require(
          bs.size == 48,
          s"Invalid length of bytes for compressed point of G1: expected 48, actual: ${bs.size}, byteString: $bs"
        )

        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit isn't set for point in G1, byteString: $bs"
        )

        val p = new P1(bs.bytes)
        if !p.in_group() then throw new IllegalArgumentException("Invalid point")
        BLS12_381_G1_Element(p)
    }

    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element = {
        require(
          dst.size <= 255,
          s"Invalid length of bytes for dst parameter of hashToGroup of G1, expected: <= 255, actual: ${dst.size}"
        )

        val p = new P1()
        p.hash_to(bs.bytes, new String(dst.bytes, "Latin1"))
        BLS12_381_G1_Element(p)
    }

    override def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        p1 == p2

    override def bls12_381_G2_add(
        p1: BLS12_381_G2_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = BLS12_381_G2_Element(p1.value.dup.add(p2.value))

    override def bls12_381_G2_scalarMul(
        s: BigInt,
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = {
        val scalar = s.bigInteger.mod(PlatformSpecific.bls12_381_scalar_period.bigInteger)
        BLS12_381_G2_Element(p.value.dup.mult(scalar))
    }

    override def bls12_381_G2_neg(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = {
        BLS12_381_G2_Element(p.value.dup.neg())
    }

    override def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString =
        p.toCompressedByteString

    override def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element = {
        require(
          bs.size == 96,
          s"Invalid length of bytes for compressed point of G2: expected 96, actual: ${bs.size}, byteString: $bs"
        )

        require(
          (bs.bytes(0) & 0x80) != 0,
          s"Compressed bit isn't set for point in G2, byteString: $bs"
        )

        val p = new P2(bs.bytes)
        if !p.in_group() then throw new IllegalArgumentException("Invalid point")
        BLS12_381_G2_Element(p)
    }

    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element = {
        require(
          dst.size <= 255,
          s"Invalid length of bytes for dst parameter of hashToGroup of G2, expected: <= 255, actual: ${dst.size}"
        )

        val p = new P2()
        p.hash_to(bs.bytes, new String(dst.bytes, "Latin1"))
        BLS12_381_G2_Element(p)
    }

    override def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult = {
        val pt = new PT(p1.value, p2.value)
        BLS12_381_MlResult(pt)
    }

    override def bls12_381_mulMlResult(
        r1: BLS12_381_MlResult,
        r2: BLS12_381_MlResult
    ): BLS12_381_MlResult = {
        val pt = r1.value.dup.mul(r2.value)
        BLS12_381_MlResult(pt)
    }

    override def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean = {
        PT.finalverify(p1.value, p2.value)
    }

    override def keccak_256(bs: ByteString): ByteString = {
        val digest = new Keccak.Digest256()
        ByteString.unsafeFromArray(digest.digest(bs.bytes))
    }

    override def ripemd_160(byteString: ByteString): ByteString = {
        val digest = new RIPEMD160.Digest()
        ByteString.unsafeFromArray(digest.digest(byteString.bytes))
    }

    override def readFile(path: String): Array[Byte] = {
        Files.readAllBytes(Paths.get(path))
    }

    override def writeFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(Paths.get(path), bytes)
        ()
    }

    override def appendFile(path: String, bytes: Array[Byte]): Unit = {
        Files.write(
          Paths.get(path),
          bytes,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.APPEND
        )
        ()
    }
}

given PlatformSpecific = JVMPlatformSpecific
